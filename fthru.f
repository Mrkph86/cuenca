C PROGRAM 16 ! Based on Hromadka book pag 210
C ------------------------------------------------------------
      SUBROUTINE fthru(SS1,m,n,NUT,NDAT,Hydro,mn1,mn2,mn3) ! 
!     SUBROUTINE fthru(NUT,NDAT) ! From the book (8.29.18)
C ------------------------------------------------------------	  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C THIS SUBROUTINE ROUTS FLOW THROUGH A FLOW THRU BASIN                               C 
C USING FIVE-MINUTE INTERVALS.  EXPLICIT ALGORITHM IS USED.                          C 
C VARIABLES:                                                                         C 
C ***** Line 1: NA,DEADS,S0,V0,NBASIN,TIME1,TIME2 *****                              C 	
C NA:    	Stream "A" number. This stream is the one to be modeled                  C		
C DEADS:  Dead storage volume (m^3)                                                  C
C S0:    	Initial dead storage volume (m^3)                                        C
C V0:    	Initial basin volume (above PL of outlet) (m^3)                          C			
C NBASIN:    Number of basin data points (zero at D=0).                              C
C            Allowable values [4 – 20]                                               C		
C TIME1:    	Time for Beginning of results (hrs)                                C					
C TIME2:	    Time for End of results (hrs)                                        C					
C ***** Line 2: BD(I),BQ(I),BV(I),I=1,NBASIN *****                                   C							
C BD(I):    	Basin Depth (m). Allowable values [0-76]                           C			
C BQ(I):    	Basin outflow (m^3/s). Allowable values [0-2831]                   C					
C BV(I):    	Basin Volume (m^2-m=m^3). Allowable values [0-123348184 m^3]       C						
C I=1    									                         C
C NBASIN:	Number of basin data points (zero at D=0).                               C
C            Allowable values [4 – 20]	                                           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C -----------------------------------------------------------------------
C   DECLARE VARIABLES
C -----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (a-h, o-z)
      COMMON/BLK1/SS(600,10),SS1(600,10),Hydro(600,3) !(8.29.18)
      !DIMENSION SS(600,10),SS1(600,10),Hydro(600,3)
!      COMMON/BLK1/SS
      DIMENSION A(600)
      DIMENSION BD(20),BQ(20),BV(20),AA(20),BB(20)
C ------------------------------------------------------------------------
      V0=0.D0
      S0=0.D0
      SS=SS1 ! SS (8.29.18) - should I activite this (8.30.2018)
      READ(NDAT,*)NA,DEADS,S0,V0,NBASIN,TIME1,TIME2
      READ(NDAT,*)(BD(I),BQ(I),BV(I),I=1,NBASIN)
      WRITE(NUT,901)NA,DEADS,S0,V0
C ------------------------------------------------------------------------	  
      !J1=1
      !DO 201 I=1,NBASIN (old code miguel)
      !J1=J1+3
C -----------------------------------------------------------------------
C   CONVERSION
C -----------------------------------------------------------------------       
C!      J1=1
C!      DO 201 I=1,NBASIN ! (marco)
C!      BD(I)=BD(I)/(0.3048) !To obtain feet
C!      BQ(I)=BQ(I)/(0.3048**3) !To obtain cubic feet per second
C!      BV(I)=BV(I)/(1233.8184) !To obtain acre-feet from m^3 (Basin Storage)
C!      J1=J1+3
C!201   CONTINUE ! IF I active the DO 
C!      READ(NDAT,*)(BD(I),BQ(I),BV(I),I=1,NBASIN)
     
901   FORMAT(/,10X,'ROUT RUNOFF HYDROGRAPH FROM STREAM NUMBER: ',I2,
     C /,10X,'THROUGH A FLOW-THROUGH DETENTION BASIN',/,
     C  10X, 'USING FIVE-MINUTE UNIT INTERVALS:',/,
     C  10X, 'SPECIFIED BASIN CONDITIONS ARE AS FOLLOWS:',/,
     C  10X, 'DEAD STORAGE (AP) = ',F44.3,/,
     C  10X, 'SPECIFIED DEAD STORAGE (AF) FILLED = ',F27.3,/,
     C  10X, 'SPECIFIED EFFECTIVE VOLUME (AF) FILLED ABOVE OUTLET = ',
     C  F10.3) 
      WRITE(NUT,903)
903   FORMAT(//,10X,' BASIN DEPTH VERSUS OUTFLOW AND STORAGE ',
     C  'INFORMATION:'
     C  ,//,11X,' INTERVAL    DEPTH   OUTFLOW    STORAGE'
     C  ,/,11X,'  NUMBER      (FT)      (CFS)     (AF)')
      WRITE(NUT,905)(I,BD(I),BQ(I),BV(I),I=1,NBASIN)
905   FORMAT(10X,I7,2X,F10.2,F10.2,F10.3)
C ----------------------------------------------------------------------
C   GRAPHICS
C ----------------------------------------------------------------------
      WRITE(NUT,921)NA,NA
921   FORMAT(///,20X,'  INFLOW',/,20X,'(STREAM',I2,')',/,3(25X,'|',/)
     C  25X,'V',15x,'Effective depth',/,
     C  20X,  ' -----------',
     C  9X,' | (and Volume)',/,
     C  20X,'|',11X,'|',4X,'|    |',/,20X,'|',11X,'|',4X,'|....',
     C  'V........',/,
     C  20X,  '| detention |<-->|        outflow',/,
     C  20X,  '|   basin   |    |........._____',/,
     C  20X,  ' -----------     |    ^    |    \',/,
     C  20X,  '      |          | storage |   basin outlet',/,
     C  20X,  '      V           ---------',/,
     C  22X,  'OUTFLOW',/,21X,'(STREAM',I2,')',//)

908    FORMAT(11X,'BASIN ROUTING MODEL RESULTS(5-MINUTE INTERVALS):'
     C  ,//,11X,'TIME     DEAD-STORAGE INFLOW   EFFECTIVE  OUTFLOW ',
     C        'EFFECTIVE'
     C  ,/,11X,'(HRS)     FILLED(AF)  (CFS)    DEPTH(FT)  (CFS)  ',
     C' VOLUME(AF)')
C READ IN STREAM NUMBER NA
       CALL MREAD(NA,A)
C INITIALIZE VARIABLES
       I=1        
       QBASIN=BQ(NBASIN)
       NB=NBASIN-1
       VBASIN=BV(NBASIN)
       TIME=0.D0
       STORE=0.D0
       VOLUME=0.D0
       NUMBER=A(600)
       ZERO=0.D0
C      WRITE(NUT,403)
       WRITE(NUT,908)
C      WRITE(NUT,480)
C ----------------------------------------------------------------------
C   MODEL DEAD STORAGE
C ----------------------------------------------------------------------
       IF(DEADS.EQ.0.)GO TO 100
       IF(S0.GE.DEADS)GO TO 100
       STORE=S0
       DO 220 I=1,NUMBER
       STORE=STORE+A(I)/145.2
       TIME=TIME+.083333
       X=STORE-DEADS
       IF(X)10,10,155
10     IF(TIME.LT.TIME1.OR.TIME.GT.TIME2)GO TO 220
C ------------------------------------------------------------------------
       WRITE(NUT,907)TIME,STORE,A(I),ZERO,ZERO,ZERO
907    FORMAT(10X,F7.3,F13.3,F9.1,F10.2,F9.1,F11.3)
220    A(I)=0.D0
C ALL FLOW HELD IN BASIN
       GO TO 2000
C DEAD STORAGE REMAINING
155    ATEMP=A(I)
       A(I)=X*145.2
       ATEMP=ATEMP-A(I)
       STORE=DEADS
       TIME=TIME-.08333
!       IF(TIME.GE.TIME1.AND.TIME.LE.TIME2) CONTINUE
C ------------------------------------------------------------------------ 
       IF(TIME.GE.TIME1.AND.TIME.LE.TIME2) WRITE(NUT,930)ATEMP,A(I)
930    FORMAT(/,11X,'DEAD STORAGE FILLED WITH UNIT INFLOW(CFS) = ',F15.1
     C  ,/,11X,'REMAINING UNIT FLOW IS = ',F34.1,' CFS ',/)
C ROUT THRU BASIN
       GO TO 110
C FLOW THRU BASIN MODE.-
100    VOLUME=V0
110    CONTINUE
C FIND INITIAL BASIN DEPTH AND OUTFLOW
       DO 115 II=1,NB
       IF(VOLUME.LT.BV(II+1))GO TO 116
115    CONTINUE
c rmc 114     TI=TIME+.083333
114    TI=TIME+.083333
C ------------------------------------------------------------------------ 
       WRITE(NUT,909)TI
909    FORMAT(10X,F7.3,5X,
     C '*BASIN CAPACITY EXCEEDED; BASIN DATA IS EXTRAPOLATED*')
       II=NB
116    TEMP=(VOLUME-BV(II))/(BV(II+1)-BV(II))
       D0=BD(II)+TEMP*(BD(II+1)-BD(II))
C ------------------------------------------------------------------------ 
C GET INITIAL VALUES
C ------------------------------------------------------------------------ 
       O2=0.D0
       S2=0.D0
       S1=BV(II)+TEMP*(BV(II+1)-BV(II))
       O1=BQ(II)+TEMP*(BQ(II+1)-BQ(II))
       CON=60./43560.*5./2.
       DO 1011 K=1,NBASIN
       AA(K)=BV(K)-BQ(K)*CON
1011   BB(K)=BV(K)+BQ(K)*CON
       AA(1)=BB(1)
       CON=CON*2.
       ATEMP=S1-O1*CON/2.
       DO 1000 K=I,576
       QQ=CON*A(K)
       TEMP=QQ+ATEMP
!      CALL SEE(TEMP,B1,B2,I1,I2,BB,NBASIN,TIME)
       CALL SEE(TEMP,B1,B2,I1,I2,NUT,BB,NBASIN,TIME) ! Original from the book
       RATIO=(TEMP-B1)/(B2-B1)
       DEPTH2=BD(I1)+RATIO*(BD(I2)-BD(I1))
       S2=BV(I1)+RATIO*(BV(I2)-BV(I1))
       O2=BQ(I1)+RATIO*(BQ(I2)-BQ(I1))
       ATEMP=S2-O2*CON/2.
       TIME=TIME+.0833333
       OAVG=(O1+O2)/2.
       O1=O2
       IF(TIME.LT.TIME1.OR.TIME.GT.TIME2)GO TO 1000
C ------------------------------------------------------------------------
       WRITE(NUT,907)TIME,DEADS,A(K),DEPTH2,OAVG,S2
1000   A(K)=OAVG
       A(600)=576
2000   CONTINUE
       CALL MWRITE(NA,A)
C ------------------------------------------------------------------------
C HYDROGRAPH TO EXPORT
C ------------------------------------------------------------------------
!       A(600)=0
!       Hydro(:,2)=A
!	  Hydro(:,2)=A*(0.3048**3) !To convert in m^3/s
!       DO 716 I=1,mn2
!       IF(I==1) THEN 
!       Hydro(I,1)=0.083333
!       ELSE
!       J=I-1
!       Hydro(I,1)=Hydro(J,1)+0.083333
!       END IF
!       WRITE(*,*) Hydro(I,1), Hydro(I,2), Hydro(I,3)
!716    CONTINUE    
!       A(600)=0
!       Hydro(:,2)=A
!       SS1=SS
       RETURN
       END SUBROUTINE fthru
