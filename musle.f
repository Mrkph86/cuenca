	  
C --------------------------------------------------------------
      SUBROUTINE musle(er,er1,erCoolm,ek,Y,pl,cfact,pfact,A,vol,tc,rain,
     C D,soilty,dp,sconc,sconc1,sconc2,om,aa1,b1,bigE,raimax30,qp)
C --------------------------------------------------------------
C      version 3.0.1, Last ModIFied: See ModIFications below
C      WRITTEN FOR: ASAE'99 Toronto paper, March 8, 2002 
C      Written by: R. Munoz-Carpena (rmc)   &   J. E. Parsons, BAE (jep)
C                  University of Florida        BAE, NC State University
C                  Gainesville, FL 32611        Raleigh, NC 27695-7625(USA)
C                  e-mail: carpena@ufl.edu      
C --------------------------------------------------------------
	  IMPLICIT DOUBLE PRECISION (a-h, o-z)
	  CHARACTER*20 soilty
	  CHARACTER*20 types(21)
	  DIMENSION d50(21),sand(21),silt(21),Tf(21),Sf(21),Pf(21)

	  DATA types/'Clay','Silty clay','Sandy clay','Silty clay loam',
     C  'Clay loam','Sandy clay loam','Silt','Silt loam','Loam',
     C  'Very fine sandy loam','Fine sandy loam','Sandy loam',
     C  'Coarse sandy loam','Loamy very fine sand','Loamy fine sand',
     C  'Loamy sand','Loamy coarse sand','Very fine sand',
     C  'Fine sand','Sand','Coarse sand'/
	  DATA d50/23.d0,24.d0,66.d0,25.d0,
     C         18.d0,91.d0,19.d0,27.d0,35.d0,
     C         35.d0, 80.d0,98.d0,
     C         160.d0,90.d0,120.d0,
     C         135.d0,180.d0,140.d0,160.d0,170.d0,200.d0/   
	  DATA sand/20.d0,10.d0,50.d0,15.d0,35.d0,
     C          55.d0,5.d0,20.d0,45.d0,60.d0,
     C          60.d0,60.d0,60.d0,84.d0,84.d0,
     C          84.d0,84.d0,90.d0,90.d0,90.d0,
     C          90.d0/
	  DATA silt/30.d0,45.d0,10.d0,50.d0,30.d0,
     C          20.d0,85.d0,60.d0,35.d0,25.d0,
     C          25.d0,25.d0,25.d0,8.d0,8.d0,
     C           8.d0,8.d0,5.d0,5.d0,5.d0,
     C           5.d0/
	  DATA tf/0.01287d0,0.01870d0,0.01714d0,0.02606d0,0.0236d0,
     C        0.02778d0,0.05845d0,0.04259d0,0.03618d0,0.03877d0,
     C        0.03205d0,0.02549d0,0.01914d0,0.03726d0,0.02301d0,
     C        0.01624d0,0.00982d0,0.04401d0,0.02173d0,0.01481d0,
     C        0.00827d0/
	  DATA sf/0.065d0,0.065d0,0.065d0,0.065d0,0.065d0,
     C        0.065d0,0.065d0,0.065d0,0.0325d0,-0.035d0,
     C        0.d0,0.0325d0,0.0325d0,-0.0325d0,0.d0,
     C        0.0325d0,0.0325d0,-0.0325d0,0.d0,0.0325d0,
     C        0.0325d0/
	  DATA pf/0.075d0,0.075d0,0.075d0,0.050d0,0.050d0,
     C        0.05d0,0.025d0,0.025d0,0.025d0,0.d0,
     C        0.d0,0.d0,0.d0,-0.025d0,-0.025d0,
     C        -0.025d0,-0.025d0,-0.05d0,-0.05d0,-0.05d0,
     C        -0.05d0/

C --------------------
c  Compute R for musle
C --------------------
c     er=> Foster et al. 1977b, units 
c     er1=> Williams, units Mg h/ha N
C --------------------
c**convert bigE to SI metric - multiply by 1.702 / 100
c** units Rst=N/h, Runoff volume: vol(m3), volro (mm)
      volro=vol/(A*10000.d0/1000.d0)
      qpdepth=qp*360.d0/A
      Def=0.24d0*tc
      dtime=Def
      ndtime=D/dtime+1.d0
      bigEm=0.006700d0*bigE
      rst=1.702d0*(bigE/100.d0)*(raimax30/25.4d0/dtime)*dtime/0.5d0
      rro=volro*(qpdepth)**(1.d0/3.d0)
      er=0.5d0* rst + 0.35d0*rro
      er1 = 9.05d0*(vol*qp)**0.56d0/A
C rmc03/28/99-- er1 = 9.05d0*(vol*qp)**0.56d0
C --------------------
c**   Cooley (1980) -> er for design storms, EI/100 = R ft tonsf/ac 
c**                    1/0.67 * R for J/m^2
C --------------------
      erCooly=aa1*(rain/25.4d0)**(2.119d0*D**0.0086d0)/(D**b1)
      erCoolm=erCooly*1.702d0
      WRITE(10,17)
17    FORMAT(/,/,1x,'RAINFALL ENERGY FACTOR R FOR EROSION CALCULATIONS')
c     C  ,'RAINFALL ENERGY FACTOR R FOR EROSION CALCULATIONS')
      WRITE(10,22) bigE,bigEm,volro,qpdepth,rst,rro,er,A,vol,qp,er1
22    FORMAT(/,2x,'a) Foster et al. (1977)',
     C   /,4x,'E=  ',f10.3,' ft-tonf/acre =',f10.3,' MJ/ha',
     C   /,4x,'Volume Runoff=',f10.4,' mm; qpeak =',f10.4,' mm/h',
     C   /,4x,'Factors in Rm: Rstorm=',f10.4,'; Rrunoff=',f10.4,
     C   /,4x,'Rm (Foster)=',f10.4,' N/h',
     C   /,/,2x,'b) Williams (1975)',
     C   /,4x,'Watershed area=',f10.3,' ha',
     C   /,4x,'Volume of runoff=',f10.4,' m3; qdeph=',f10.4,' m3/s',
     C   /,4x,'Rw (Williams)=',f10.4,' N/h')

      WRITE(10,24) aa1,b1,rain,D,erCooly, erCoolm
24    FORMAT(/,2x,'c) Cooley (1980) - Design Storm ',
     C   /,4x,'a1=', f10.4, ' b1=',f10.4,
     C   /,4x,'Rain=', f10.3, ' mm   D=',f10.3,' hr',
     C   /,4x,'Rst =',f10.3,' ft-tonf/acre =',f10.3,' N/ha')
C ------------------
c  erGLEAMS, from GLEAMS daily rain 
C ---------------------
      gei=7.87d0*(rain/25.4d0)**1.51d0
	  geim=1.702d0*gei
	  WRITE(10,32) rain,gei,geim
32	  FORMAT(/,2x,'d) GLEAMS/ daily CREAMS',
     C   /,4x,'Rain  =',f6.2,' mm',
     C   /,4x,'R_GLM =',f10.2,' From GLEAMS - Wischmeier',
     C   /,4x,'R_GLM =',f10.4,' N/h,  Converted to Metric')

C -------------------
c  Soil type selection
C -------------------
	  DO i=1, 21
	  IF (soilty.eq.types(I)) THEN
	  isoil=i
c     PRINT *,'picked=>',isoil,soilty,types(i),d50(isoil)
	  END IF

	  END DO
	  WRITE(10,98)

   98 FORMAT(/,/,1x,'ERODIBILITY K AND PARTICLE SIZE SELECTION',
     C  /,/,4x,'Table for computing Ksoil (from GLEAMS and KINEROS)',/,
     C  4x,' i',4x,'Soil Type',9x,'%Sand',2x,'%Silt',3x,
     C  'Tex.F.',4x,'Str.F.',4x,'Per.F.',4x,'D50(um)')

	  DO i=1,21
	  WRITE(10,99)i,types(i),sand(i),silt(i),tf(i),sf(i),pf(i),d50(i)
	  END DO
   99 FORMAT(4x,i2,2x,a20,2x,f4.0,2x,f4.0,2x,f8.5,2x,f8.4,2x,f7.3,2x,
     C  f6.1)

C ------------
c K FACTOR (calculate internally IF user did set -1 in input file)
C ------------
      IF(dp.le.0.d0) THEN
      dp=d50(isoil)
      END IF
c	om=2.d0
      IF ((ek.lt.0.d0)) THEN
	ek=tf(isoil)*(12.d0-om)+sf(isoil)+pf(isoil)
c --  convert to metric units - kg/N * h/m^2
	ek=0.1317d0*ek
      END IF

c***  save english version
      ekeng=ek/0.1317d0

C ------------
c S FACTOR
C ------------
	  theta=DATAn(Y)
	  s=dsin(theta)
c ** Usle 
c	  bigS=65.4d0*s**2+4.56d0*s+0.065d0
c from haan p261
      IF (s.lt.0.09) THEN
	  bigS=10.8*s +0.03
	  ELSE
	  bigS=16.8*s -0.5
	  END IF
	  IF (pl.lt.0.7) THEN
	  bigS=3.0*s**0.8 +0.56
	  END IF

C ------------
c L FACTOR after McCool, p262 Haan
C ------------
c	  IF (x.lt.3.d0) THEN
c	  x=0.3d0
c	  ELSEIF (x.eq.4.d0) THEN
c	  x=0.4d0
c	  ELSE
c	  x=0.5d0
c	  END IF
c * use distance not length along slope
      slopeL = pl*cos(theta)
	  beta=11.16d0*s/(3.d0*s**0.8d0+0.56)
	  x=beta/(1.d0+beta)
	  bigL=(slopeL/22.d0)**x

C ------------
c Final sediment yield calculations
C ------------
	  A0=er*ek*bigL*bigS*cfact*pfact        
	  A1=er1*ek*bigL*bigS*cfact*pfact
	  A2=geim*ek*bigL*bigS*cfact*pfact
	  A3=erCoolm*ek*bigL*bigS*cfact*pfact
C ------------
c concentrations of sediment in g/L
C ------------
	  IF(volro.le.0)THEN
	  sconc=0.d0
	  sconc1=sconc
	  sconc2=sconc
	  sconc3=sconc
	  ELSE
      sconc= A0*A*10000.d0/vol
      sconc1=A1*A*10000.d0/vol
      sconc2=A2*A*10000.d0/vol
      sconc3=A3*A*10000.d0/vol
	  END IF

C ----------------------
c  PRINT summary
C ----------------------
      WRITE(10,19) soilty,ek,ekeng,om,dp
      WRITE(10,21) slopeL,beta,bigL,bigS,cfact,pfact
      WRITE(10,23)
c*** convert kg/m^2 to Eng Units ton/ac
      ccc=0.00110231131d0/0.000247105381d0
      WRITE(10,26) 'Rm (Foster)  ',A0,A0*ccc,sconc,er,ek,bigL,bigS,
     C                    cfact,pfact
      WRITE(10,25) 'Rw (Williams)',A1,A1*ccc,sconc1,er1
      WRITE(10,25) 'Rm (GLEAMS)  ',A2,A2*ccc,sconc2,geim
      WRITE(10,25) 'Rst (Cooley) ',A3,A3*ccc,sconc3,ercoolm

19	  FORMAT(/,2x,'For the selected soil type: ',a20,
     C  /,5x,'K=',f10.3,' kg-h/N-m^2',2x,' Eng. K=',f10.3,
     C       2x,'% Org Matter=',f6.1,
     C  /,4x,' dp (d50)=',f12.2,' um')
21	  FORMAT(/,/,1x,'MISCELLANEOUS CALCS:',/,
     C  /,4x,' SlopeL  =',f10.3,' m',2x,'Beta    =',f10.3,
     C  /,4x,' L-Factor=',f10.3,4x,'S-Factor=',f10.3,
     C  /,4x,' C-fact  =',f10.2,4x,'P-fact  =',f10.2)
23	  FORMAT(/,/,1x,'FINAL CALCS:',
     C   /,/,2x,'Method',16x,'Soil Loss A',10x,'Sediment ',2x,'R-Factor'
     C   ,2x,'K-Factor',2x,'L-Factor',1x,'S-Factor',1x,'C-Factor',1x,
     C   'P-Factor',/,2x,18x,'kg/m^2',4x,'EngUnits t/ac',2x,'Conc g/l',
     C    3x,'N/ha',6x,'kg-N/N-m^2')
25	  FORMAT(
     C  1x,A13,2x,f10.2,4x,f10.2,4x,f10.2,2x,f10.2)
26	  FORMAT(
     C  1x,A13,2x,f10.2,4x,f10.2,4x,f10.2,2x,f10.2,2x,f7.3,
     C    4(1x,f8.3))
 
	  RETURN
	  END SUBROUTINE musle
