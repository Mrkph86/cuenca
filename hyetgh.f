!--------------------------------------------------------------------
C  Disaggregation Daily Precipitation
!--------------------------------------------------------------------
C ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      SUBROUTINE hyetgh(jstype,P,D,volro,qdepth,vol,qp,Area,xIa,rtpeak,
     C          er,er1,erCoolm,ti,nref,tc,a1,b1,bigE,raimax30,nhyet)
C --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
c      version 3.0.1, Last ModIFied: See ModIFications below
C      WRITTEN FOR: ASAE'99 Toronto paper, March 8, 2002 
C      Written by: R. Munoz-Carpena (rmc)   &   J. E. Parsons, BAE (jep)
C                  University of Florida        BAE, NC State University
C                  Gainesville, FL 32611        Raleigh, NC 27695-7625(USA)
C                  e-mail: carpena@ufl.edu      
c !---------------------------------------------------------------------------
c ! Where P'(t)= is the cumulative hyetograph for the given duration
c !       Pd is the total rainfall for the given period (mm)
c !       D is the storm duration in hours
c !       t is time in hours
c !
c ! storm type II or III - from Haan
c ! hyetograph for 24 hour storms
c !
c ! P(t)        T (  24.04  ) ^0.75
c ! ---- = 0.5+---(---------)
c ! P24        24 (2|T|+0.04)
c ! 
c !  where T=t -12  (with t in hours)
c !        P24 is the 24h storm
c ! 
c ! storm type I  - fitted from SCS tabular data - rmc 03/04/99
c !
c ! hyetograph for 24 hour storms
c !
c !                   (     -0.1617    ) ^0.5853
c !       | 0.4511+ T (----------------)       ;for [-3.0163|T|+0.013]<0
c ! P(t)  |           (-3.0163|T|+0.013)
c ! ---- =| 
c ! P24   |  
c !       |   0.5129                           ;for [-3.0163|T|+0.013]>0
c !  
c !  where T=t -9.995  (with t in hours)
c !  
c ! storm type IA  - fitted from SCS tabular data - rmc 03/04/99
c !
c ! hyetograph for 24 hour storms
c !
c ! P(t)             (     0.0843     ) ^0.4228
c ! ---- = 0.3919+ T (----------------)
c ! P24              (120.39|T|+0.3567)
c !  
c !  where T=t -7.96  (with t in hours)
c !  
c ! For any storm of any duration (from Haan et. al.(1994), eq. 3.7)
c !
c ! P'(t)    P(tmid+t-D/2) - P(tmid-D/2)
c ! ----- = --------------------------
c ! Pd       P(tmid+D/2) - P(tmid-D/2)
c !  
c ! where where tmid=12. Alternatively (Munoz-Carpena and Parsons,2004), 
c ! tmid=12.00 for storm type II & III, tmid=9.995 for storm type I, and
c ! tmid=7.960 for storm type IA
c !
c !----------------------------------------------------------------
c  Storm type I and IA - fitted equations from tabular data on Haan's
c !----------------------------------------------------------------

      IMPLICIT DOUBLE PRECISION (a-h, o-z)
      CHARACTER*4 stype(5)
      COMMON/rain/rfix,rti(5000),rfi(5000),rcum(5000,2),ref(5000,2),ncum
      DIMENSION rainh(5000),rainh30(5000)
      DATA stype/'I','IA','II','III','user'/

c      PRINT *,'xxxx'
c      PRINT *,'volro (mm),qpeak (mm/h)=',volro,qdeph
c      PRINT *,stype(jstype)

      pd = P
C ---rmc 24/3/99 - hyetograph using unit hydrograph time step, Def
      Def=0.24d0*tc !MAC 04/10/12
	!Def=Dstep !MAC 04/10/12
      dtime=Def
      ndtime=D/dtime+1
C ---- 
      pcumtot=0.d0
      refcum=0.d0
      IFlag=0
      ti=0.d0
      bigE=0.d0
      raimax=0.d0
      raimax30=0.d0
      nref=0

C ---v3 09/2011 rmc
C -- calculate scaling factors for D<24 h, based on eq. 3-7, Haan et.al. (1994)
c**> set Cooly (1980) a1,b1 coef.
       
c      IF (jstype.eq.3) jstype=4
      IF (stype(jstype).eq.'I  ') THEN
      tmid=9.995d0
      a1 = 15.03d0
      b1 = 0.578d0
      ELSE IF (stype(jstype).eq.'IA ') THEN
      tmid=7.96d0
      a1 = 12.98d0
      b1 = 0.7488d0
      ELSE IF (stype(jstype).eq.'II ') THEN
      tmid=11.8d0
      a1 = 17.9d0
      b1 = 0.4134d0
      ELSE IF (stype(jstype).eq.'III') THEN
      tmid=12.d0
      a1 = 21.51d0
      b1 = 0.2811d0
      ELSE
      tmid=rcum(1,1)
      END IF
C --  scaling factors for other storm durations and volumen
      tminus=tmid-d*.50d0
      tplus=tmid+d*.5d0
      pm=SCStorm(jstype,tminus)
      pp=SCStorm(jstype,tplus)
      !WRITE(*,*)"PRECIPITATION (h, mm, mm/h)" !MAC 04/10/12
C -- rain time step loop ---       
	!WRITE(*,*)ndtime
      DO 3 i=1,ndtime
      smalle=0.d0
      rti(i)=(i-1)*dtime
      tsmall=tmid+rti(i)-d*.5d0
      ptp=SCStorm(jstype,tsmall)
C --------------------------------------------------------------------------------------------------------------------------------------
c  Calculate cumulative hyetograph for any duration and volume
C --------------------------------------------------------------------------------------------------------------------------------------
      cumtotal=pd*(ptp-pm)/(pp-pm)      
c     WRITE(*,'(6f9.4)')rti(i),ptp,cumtotal,
c    C        SCStorm(jstype,rti(i)),SCStorm(3,rti(i)) 
c     WRITE(*,'(6f9.4)')rti(i),pd,ptp,pm

      IF(cumtotal.gt.xIa.and.IFlag.eq.0) THEN
      IFlag=1
      ti=(rti(i)-rti(i-1))/(cumtotal-pcumtot)*
     C   (xIa-pcumtot)+rti(i-1) 
      END IF
C ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
c  Calculate instantaneous hyetograph and rainfall energy term for USLE
C ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      rainh(i)=cumtotal-pcumtot
	  raterain=rainh(i)/Def
	!MAC 04/10/12
	!WRITE(*,'(6f9.4)')rti(i),cumtotal,raterain !Time(hr) PAcum (mm) RatePreci(m/hr)
      IF (rainh(i).gt.0.d0) THEN
c ---> english units ft-tons/acre-inch
      IF ((rainh(i)/25.4d0/dtime).gt.3.d0) THEN
      smalle=1074.d0
      ELSE
      smalle=(rainh(i)/25.4d0)*
     C (916.d0+331.d0*dlog10(rainh(i)/25.4d0/dtime))
      END IF
c           smalle=
c     1      1099.d0 * (1.d0-0.72*exp(-1.27*(rainh(i)/25.4d0/dtime)))               
c           PRINT*,rti(i),smalle
      bigE=bigE+smalle
c ---> metric units
c           bigE=bigE+11.9d0+8.73d0*dlog10(rainh(i)/dtime)
      END IF
      IF (rainh(i).gt.raimax) THEN
      raimax=rainh(i)
      rtpeak=rti(i)
      END IF
C --rmc - I30 calculation after Chow et al, 1987
      IF(i.gt.2) THEN
      rainh30(i)=rainh(i-2)+rainh(i-1)+rainh(i)
      END IF
      IF (rainh30(i).gt.raimax30) THEN
      raimax30=rainh30(i)
      rtpeak30=rti(i)
      END IF
      pcumtot=cumtotal
      rfi1=rainh(i)/dtime
      rfi(i)=rfi1/3600.d0/1000.d0
C --rmC 08/24/11-- excess rainfall hyetograph for tabular hydrograph
      IF(cumtotal.ge.xIa) THEN
      nref=nref+1
           !Write(*,*)nref
      ref(nref,1)=rti(i) 
      ref(nref,2)=(cumtotal-xIa)**2.d0/(cumtotal+4.d0*xIa)-refcum
      refcum=(cumtotal-xIa)**2.d0/(cumtotal+4.d0*xIa)
      END IF
c     WRITE(*,202)rti(i)*3600,rfi(i),cumtotal,refcum,ref(nref,2)
c     WRITE(10,202)rti(i),rainh(i),tsmall,ptp,cumtotal,rfi1,smallE
c202  FORMAT(2x,f8.2,2x,e8.3,2x,f7.3,2x,f7.3,2x,f7.3,2x,f7.3,2x,
c     C            f7.3)
3     CONTINUE
C --rmc-08/24/11-- number of hyetograph steps
      nhyet=i-1
C ---rmc 03/11/99---
      rfix=raimax/(dtime*3600.d0)/1000.d0
      rfix30=raimax30*2.d0
      rI30=rfix30/25.4d0

C ---rmc 08/24/11-- DOne hyet - computing musle param
C --------------------------------------------------------------------------------------------------------------------------------------
c  compute R for musle
c    er=> Foster et al. 1977b, units N/h 
c    er1=> Williams, units Mg h/ha N
C --------------------------------------------------------------------------------------------------------------------------------------
c**convert bigE to SI metric - multiply by 1.702 / 100
c** units Rst=N/h
      bigEm=0.006700d0*bigE
      rst=1.702d0*(bigE/100.d0)*(raimax30/25.4d0/dtime)*dtime/0.5d0
      rro=volro*(qdeph)**(1.d0/3.d0)
      er=0.5d0* rst + 0.35d0*rro
C rmc 03/28/99- er1 = 9.05d0*(vol*qp)**0.56d0
      er1 = 9.05d0*(vol*qp)**0.56d0/Area
c**   Cooley (1980) -> er for design storms, EI/100 = R ft tonsf/ac 
c**                    1/0.67 * R for J/m^2
      erCooly=a1*(P/25.4d0)**(2.119d0*rti(ndtime)**0.0086d0)
     C           /(rti(ndtime)**b1)
      erCoolm=erCooly*1.702d0
C --------------------------------------------------------------------------------------------------------------------------------------
c results
C --------------------------------------------------------------------------------------------------------------------------------------
      
      !WRITE(10,5)Def*60.d0,nhyet !MAC 04/10/12
5     FORMAT(/,1x,'SCS ',f4.1,'-MIN HYETOGRAPH (25 of',i5,
     C  1x,'steps PRINTed)',/,/,  
     C  2x,'No.',3x,'Time(hr)',3x,'Rainfall(mm)',1x,'Rain30(mm)',
     C  1x,'Eff.rain(mm)')
      
C --PRINT hyetograph results 25 time steps only
      maxstep=24
      nWRITE=ndtime/maxstep
      crainh=0.d0
      cref=0.d0
      iref=nhyet-nref
	!MAC 04/10/12
!     DO 8 i=1,ndtime
!     crainh=crainh+rainh(i)
!     PRINT *,i
!     PRINT *,i,iref, cref, ref
 !    PRINT *,i,crainh,rainh(i)
 !    IF(i.ge.iref) cref=cref+ref((i-iref),2) 
!     DO 7 k=1,maxstep
!     IF(i.eq.k*nWRITE) THEN
!     WRITE(*,*) k,rti(i),crainh,rainh30(i),cref
!     WRITE(10,10)k,rti(i),crainh,rainh30(i),cref
!     END IF
!7    END DO
!8    END DO
     
      
      !WRITE(10,10)k,rti(ndtime),crainh,crainh30,cref
10    FORMAT(2x,i3,2x,f8.3,3x,3f10.3)

      !WRITE(10,15) cumtotal,P,refcum,raimax30,rfix30
15    FORMAT(/,2x,'Computed Total Rain =',f10.1,' mm'/,
     C   2x,'  Actual Total Rain =',f10.1,' mm',/,
     C   2x,'  Total Rain Excess =',f10.1,' mm',/,
     C   2x,'  raimax30          =',f10.1,' mm',/,
     C   2x,'  I30               =',f10.1,' mm/h')
      
      RETURN
      END SUBROUTINE hyetgh
