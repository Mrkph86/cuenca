C -----------------------------------------------------------------------------------
C   Program: 	
C -----------------------------------------------------------------------------------
      SUBROUTINE tab_hyd(Q,Area,mref,nref,qp,nhyd,Dstep,NA)
C -----------------------------------------------------------------------------------
C Calculation of hydrograph by convolution (Chow, 1987) of SCS unit
C hydrograph and excess hyetograph
C   mref: number of unit hydrograph steps
C   nref: number of excess hyetograph steps 
C -----------------------------------------------------------------------------------
C      version 3.0.1, Last ModIFied: See ModIFications below
C      WRITTEN FOR: ASAE'99 Toronto paper, March 8, 2002 
C      Written by: R. Munoz-Carpena (rmc)   &   J. E. Parsons, BAE (jep)
C                  University of Florida        BAE, NC State University
C                  Gainesville, FL 32611        Raleigh, NC 27695-7625(USA)
C                  e-mail: carpena@ufl.edu      
C -----------------------------------------------------------------------------------
C   DECLARE VARIABLES
C -----------------------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (a-h, o-z)
      COMMON/BLK1/SS(600,10),SS1(600,10),Hydro(600,3)
      COMMON/hydgph/u(5000,2),qh(5000,3)
      COMMON/rain/rfix,rti(5000),rfi(5000),rcum(5000,2),ref(5000,2),ncum
      DIMENSION qhstep(600,3)
C -----------------------------------------------------------------------------------
      !WRITE(2,205)mref,nref !MAC 04/10/12
      cqdepth5=0.
      DO 40 i=1,mref
c     WRITE(2,'(2f10.4)')(u(i,j),j=1,2)
      cqdepth5=u(i,2)*360.d0/Area+cqdepth5
40    CONTINUE
      dt5=u(2,1)-u(1,1)
      unitq=cqdepth5*dt5
c     DO 50 i=1,nref
c     WRITE(2,'(2f10.4)')(ref(i,j),j=1,2)
c50   CONTINUE

C ---Apply convolution of the u and ref values to obtained hydrograph
      Def=u(2,1)-u(1,1)
      qp=0.d0
      DO 70 k=1,nref+mref-1
      qh(k,1)=ref(1,1)+(k-1)*Def
      qh(k,2)=0.d0
      qh(k,3)=0.d0
      DO 60 i=1,k
      qh(k,2)=qh(k,2)+ref(i,2)*u(k-i+1,2)     
60    CONTINUE
           
      qp=dmax1(qh(k,2),qp)
70    CONTINUE
      qdepth=qp*360./Area
      qh(1,3)=0.
      DO 61 i=2,k-1
      qh(i,3)=qh(i-1,3)+qh(i,2)*3600*Def !MAC 04/10/12 Accumulated (m^3)
61    CONTINUE
C -----------------------------------------------------------------------------------
!      WRITE(*,*)"Hidrograph"
!      DO 80 i=1,k-1
!      WRITE(2,'(3f10.4)')(qh(i,j),j=1,2),qh(i,2)*360.d0/Area !MAC 04/10/12 (h, m^3/s, mm/h)
!      WRITE(*,*)(qh(i,j),j=1,3)!MAC 04/10/12 (h,m3/s,m^3)
!80    CONTINUE
      nhyd=k-1
      
!MAC 04/11/12 Interpolate the hydrograph to a time step defined by user (Dstep(h))-
!      WRITE(*,*)"Hidrograph Aggregated"
      i=1
	qhstep(i,1)=0
      DO while (qhstep(i,1)<=qh(nhyd,1))
      IF (qh(1,1)<qhstep(i,1).and.qhstep(i,1)<qh(nhyd,1)) THEN
      j=1
      DO while (qhstep(i,1)>qh(j,1))
      j=j+1    
      END DO    
      qhstep(i,3)=(qhstep(i,1)-qh(j-1,1))*(qh(j,3)-qh(j-1,3))
      qhstep(i,3)=qhstep(i,3)/(qh(j,1)-qh(j-1,1))
      qhstep(i,3)=qh(j-1,3)+qhstep(i,3)
      qhstep(i,2)=(qhstep(i,3)-qhstep(i-1,3))/Dstep
      qhstep(i,1)=Dstep*i
      !WRITE(*,*)(qhstep(i,jj),jj=1,3)
      ELSE !This is just for the beginning, where the is no value
      qhstep(i,3)=0
      qhstep(i,2)=0
      qhstep(i,1)=Dstep*i
      END IF
      !WRITE(*,*)(qhstep(i,jj),jj=1,3)
      i=i+1
      qhstep(i,1)=Dstep*i
      END DO
	!MAC 04/11/12 just to consider the last pulse
	IF (qhstep(i-1,1)<qh(nhyd,1).and.qhstep(i,1)>=qh(nhyd,1)) THEN
	qhstep(i,3)=qh(nhyd,3)
	qhstep(i,2)=(qhstep(i,3)-qhstep(i-1,3))/Dstep
	END IF
	!WRITE(*,*)(qhstep(i,jj),jj=1,3)
		
	!Save in SS qh !MAC 04/10/12
!	DO 62 j=1,i
      !SS storages hydrograph in CFS 
C -----------------------------------------------------------------------------------
C   CONVERSION
C -----------------------------------------------------------------------------------
!	SS(j,NA)=SS(j,NA)+(qhstep(j,2)/(0.3048**3)) !Unit conversion from m^3/s to CFS
!	Hydro(j,2)=(qhstep(j,2)/(0.3048**3))
!	Hydro(j,2)=(qhstep(j,2)) !Metric Units
!	Hydro(j,1)=qhstep(j,1)
!     WRITE (*,*) Hydro(j,1),Hydro(j,2)
!62	CONTINUE

!	IF (SS(600,NA)<i) SS(600,NA)=i
	
      !Call MREAD(NA,AA)
      !AA(600)
      !AA(600)=Numero de posiciones
      !Call MWRITE(NA,AA)
      
      RETURN
      END SUBROUTINE tab_hyd
