	
C---------------------------------------------------------------
      SUBROUTINE vfsout(dp,ieroty,sconc,sconc1,sconc2,Area,pL,qp,tp,
     C                  tc,D,ti,nhyet,nhyd)
C---------------------------------------------------------------
C      version 3.0.1, Last ModIFied: See ModIFications below
C      WRITTEN FOR: ASAE'99 Toronto paper, March 8, 2002 
C      Written by: R. Munoz-Carpena (rmc)   &   J. E. Parsons, BAE (jep)
C                  University of Florida        BAE, NC State University
C                  Gainesville, FL 32611        Raleigh, NC 27695-7625(USA)
C                  e-mail: carpena@ufl.edu      
C---------------------------------------------------------------
C---------------------------------------------------------------
c OUTPUT FOR VFSMOD INPUT FILES
C---------------------------------------------------------------
	IMPLICIT DOUBLE PRECISION (a-h,o-z)
	COMMON/hydgph/u(5000,2),qh(5000,3)
	COMMON/rain/rfix,rti(5000),rfi(5000),rcum(5000,2),ref(5000,2),ncum
C---------------------------------------------------------------
C OUTPUT OF VFSMOD INPUT FILE: *.isd
C---------------------------------------------------------------
	npart=7
	coarse=1.0d0
	IF (ieroty.eq.1) THEN
	ci= sconc1/1000.d0
	ELSEIF (ieroty.eq.2) THEN
	ci= sconc2/1000.d0
	ELSEIF (ieroty.eq.3) THEN
	ci= sconc/1000.d0
	ELSE
	ci= sconc1/1000.d0
	END IF
c--rmc  05/08/03 when runoff is small, sediment concentration by sediment
c----- yields methods that DO not consider runoff in calculation (Foster's
c----- ,CREAMS) can be very large. Override user selection of the method
c----- and slect Williams' that considers runoff and typiCALLy avoids this
c----- problem. Issue warning.
	IF(ci.ge.0.25d0) THEN
      ci=sconc1/1000.d0
      WRITE(*,160)
      WRITE(*,*)'WARNING: small runoff in this case produces large',
     C    ' sediment concentration with the sediment yield method #', 
     C    ieroty,'selected. Using Williams method instead--see manual'  
      WRITE(*,160)
      WRITE(10,*)
      WRITE(10,160)
      WRITE(10,*)'WARNING: small runoff in this case produces large',
     C    ' sediment concentration with the sediment yield method #', 
     C    ieroty,'selected. Using Williams method instead--see manual'  
      WRITE(10,160)
      END IF
	por=0.434d0
	WRITE (15,101) Npart,coarse,ci,por
	dpp=dp/10000.d0
	sg=2.65d0
	WRITE (15,102)dpp,sg

C---------------------------------------------------------------
c OUTPUT of VFSMOD runoff hydrograph: *.iro
C---------------------------------------------------------------
	swidth=Area*10000.d0/pL
	slength=pL
	WRITE (12,103) swidth,slength
	nbcroff=nhyd
	bcropeak=qp
      nstep1=100
      IF(nhyd.le.nstep1)THEN
      WRITE (12,104)nbcroff+1,bcropeak
      WRITE(2,*)' '
      WRITE(2,250)ti
      DO 20 ii=1, nbcroff-1
      tt=qh(ii,1)*3600.d0
      IF (ii.eq.1) THEN
      WRITE(12,105)tt,qh(ii,2)
      ELSE 
      WRITE(12,106)tt,qh(ii,2)
      END IF
20    CONTINUE
      ELSE
      nWRITE1=nhyd/nstep1+1
      WRITE(12,104)nhyd/nWRITE1+2,bcropeak
      DO 29 ii=1,nhyd-1
      tt=qh(ii,1)*3600.d0
      IF (ii.eq.1) THEN
      WRITE(12,105)tt,qh(ii,2)
      ELSE
      DO 25 k=1,nstep1
      IF(ii.eq.k*nWRITE1) THEN
      WRITE(12,106)tt,qh(ii,2)
c     WRITE(*,'(2i4,2e12.5)')tt,qh(ii,2)
      END IF
25    CONTINUE
      END IF
29    CONTINUE            
      END IF
c---WRITE 0 entry after last step
      tEND1=qh(nhyd,1)*3600.d0
      WRITE(12,106)tEND1,qh(nhyd,2)      
      WRITE(12,107)tEND1+300.d0,0.d0
C---------------------------------------------------------------
c OUTPUT VFSMOD rainfall hyetograph: *.irn
C---------------------------------------------------------------
      nstep2=100
      IF(nhyet.le.nstep2)THEN
      WRITE(14,201)nhyet+1,rfix
      DO 31 ii=1,nhyet-1
      tt=rti(ii)*3600.d0
      IF (ii.eq.1) THEN
      WRITE(14,203) tt,rfi(ii)
      ELSE
      WRITE(14,204) tt,rfi(ii)
      END IF
31    CONTINUE
      ELSE
      nWRITE2=nhyet/nstep2+1
      WRITE(14,201)nhyet/nWRITE2+2,rfix
      DO 33 ii=1,nhyet-1
      tt=rti(ii)*3600.d0
      IF (ii.eq.1) THEN
      WRITE(14,203) tt,rfi(ii)
      ELSE
      DO 32 k=1,nstep2
      IF(ii.eq.k*nWRITE2) THEN
      WRITE(14,204) tt,rfi(ii)
C     WRITE(*,'(2i4,2e12.5)')ii,k,tt,rfi(ii)
      END IF
32    CONTINUE
      END IF
33    CONTINUE            
      END IF
c---WRITE 0 entry after last step
      tEND2=rti(nhyet)*3600.d0
      WRITE(14,204)tEND2,rfi(nhyet)      
      WRITE(14,205)tEND2+300.d0,0.d0
C---------------------------------------------------------------
C  OUTPUT MESSAGE AT END OF PROGRAM 
C---------------------------------------------------------------
	WRITE(*,*)
	WRITE(*,*)'...FINISHED...','UH v3.0.1 2/2012'
	WRITE(*,*)
C---------------------------------------------------------------
c  FORMAT STATEMENTS
C---------------------------------------------------------------
101	FORMAT (2x,i4,2x,f8.1,2x,f11.4,2x,f7.4,8x,
     C   'Npart, Coarse, Ci(g/cm3), Por')
102	FORMAT(2x,f10.7,2x,f7.1,21x,'Dp(cm), SG(g/cm3)')
103	FORMAT(2x,f7.1,2x,f7.1,21x,'Swidth(m), Slength(m)')
104	FORMAT(2x,i4,2x,e12.5,19x,'nbcroff, bcropeak (m3/s)')
105	FORMAT(2x,e12.5,2x,e12.5,10x,' time(s), ro(m3/s)')
106	FORMAT(2x,e12.5,2x,e12.5)
107	FORMAT(2x,e12.5,2x,e12.5,/,30('-'))
160	FORMAT(72('-'))
201	FORMAT(i4,2x,e12.5,20x,' NRAIN, RPEAK(m/s)')
203	FORMAT(2x,e12.5,3x,e12.5,10x,'time(s), rainfall rate (m/s)')
204	FORMAT(2x,e12.5,3x,e12.5)
205	FORMAT(2x,e12.5,3x,e12.5,/,30('-'))
250	FORMAT('Time to ponding=',f8.3,' h')
260	FORMAT('Duration of rainfall excess=',f8.3,' h')
270	FORMAT('Time to peak after shIFting=',f8.3,' h')
280	FORMAT('Time correction to match hyetograph=',f8.3,' h')

	RETURN
	END SUBROUTINE vfsout
