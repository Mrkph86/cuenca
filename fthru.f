C PROGRAM 16 ! Based on Hromadka book pag 210
C ------------------------------------------------------------
      !SUBROUTINE fthru(SS1,m,n,NDAT,Hydro,mn1,mn2,mn3) ! ARGU = NDAT (9.25.17)
      SUBROUTINE fthru(NUT,NDAT) ! From the book 
C ------------------------------------------------------------
C-THIS SUBROUTINE ROUTS FLOW THROUGH A FLOW THRU BASIN
C-USING FIVE-MINUTE INTERVALS.  EXPLICIT ALGORITHM IS USED.
C -----------------------------------------------------------------------
C VARIABLES:
C       DEADS = DEAD STORAGE VOLUME
C       S0 = INITIAL DEAD STORAGE VOLUME
C       V0 = INITIAL BASIN VOLUME(ABOVE PL OF OUTLET)
C       STORE = DEAD STORAGE VOLUME VARIABLE
C       VOLUME = EFFECTIVE STORAGE VARIABLE
C       NBASIN = NUMBER OF BASIN DATA POINTS(ZERO AT D=0.)
C       BD.BQ.BV = BASIN DEPTHS,QS,AND VOLUMES
C       NA = STREAM NUMBER FROM MEMORY
C       TIME1 = LOWER BOUND ON MODEL OUTPUT
C       TIME2 = UPPER BOUND ON MODEL OUTPUT
C -----------------------------------------------------------------------
      DIMENSION A(600)
      DIMENSION BD(20),BQ(20),BV(20),AA(20),BB(20)
!      COMMON/BLK1/SS(600,10)
! C ------------------------------------------------------------------------       
! C-  INITIALIZE VARIABLES
! C ------------------------------------------------------------------------
      ! REAL(8),DIMENSION(m,n) :: SS1
      ! INTEGER,VALUE :: m
      ! INTEGER,VALUE :: n
      ! !INTEGER,VALUE :: mn
      ! INTEGER,VALUE :: mn1
      ! REAL(8),DIMENSION(mn2,mn3) :: Hydro
      ! INTEGER,VALUE :: mn2
      ! INTEGER,VALUE :: mn3
! C ------------------------------------------------------------------------
      V0=0.
      S0=0.
!      SS=SS1 ! SS
      READ(NDAT,*)NA,DEADS,S0,V0,NBASIN,TIME1,TIME2
      READ(NDAT,*)(BD(I),BQ(I),BV(I),I=1,NBASIN)
      WRITE(NUT,901)NA,DEADS,S0,V0
      !J1=1
      !DO 201 I=1,NBASIN (old code miguel)
      !    J1=J1+3
C ----------------Conversions------ (9.25.17)
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
C       GRAPHICS
C ----------------------------------------------------------------------
!Now We are not going to WRITE the Flood.ans---------------------------- 
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

908    FORMAT(11X,'BASIN ROUTING MODEL RESULTS(5-MIMUTE INTERVALS):'
     C  ,//,11X,' TIME   DEAD-STORAGE  INFLOW   EFFECTIVE  OUTFLOW ',
     C      'EFFECTIVE'
     C  ,/,11X,' (HRS)    FILLED (AF)   (CFS)   DEPTH (FT)  (CFS)   ',
     C'VOLUME(AF)')
C-READ IN STREAM NUMBER NA
       CALL MREAD(NA,A)
C-INITIALIZE VARIABLES
       I=1        
       QBASIN=BQ(NBASIN)
       NB=NBASIN-1
       VBASIN=BV(NBASIN)
       TIME=0.
       STORE=0.
       VOLUME=0.
       NUMBER=A(600)
       ZERO=0.
c      WRITE(NUT,403)
!Now We are not going to WRITE the Flood.ans---------------------------- 
       WRITE(NUT,908)
C-----------------------------------------------------------------------
C      WRITE(NUT,480)
C-----------------------------------------------------------------------
C MODEL DEAD STORAGE
C-----------------------------------------------------------------------
       IF(DEADS.EQ.0.)GO TO 100
       IF(S0.GE.DEADS)GO TO 100
       STORE=S0
       DO 220 I=1,NUMBER
       STORE=STORE+A(I)/145.2
       TIME=TIME+.083333
       X=STORE-DEADS
       IF(X)10,10,155
10     IF(TIME.LT.TIME1.OR.TIME.GT.TIME2)GO TO 220
!Now We are not going to WRITE the Flood.ans---------------------------- 
       WRITE(NUT,907)TIME,STORE,A(I),ZERO,ZERO,ZERO
907    FORMAT(10X,F7.3,F13.3,F9.1,F10.2,F9.1,F11.3)
220    A(I)=0.
C-ALL FLOW HELD IN BASIN
       GO TO 2000
C-DEAD STORAGE REMAINING
155    ATEMP=A(I)
       A(I)=X*145.2
       ATEMP=ATEMP-A(I)
       STORE=DEADS
       TIME=TIME-.08333
!       IF(TIME.GE.TIME1.AND.TIME.LE.TIME2) CONTINUE
!Now We are not going to WRITE the Flood.ans---------------------------- 
       IF(TIME.GE.TIME1.AND.TIME.LE.TIME2) WRITE(NUT,930)ATEMP,A(I)
930    FORMAT(/,11X,'DEAD STORAGE FILLED WITH UNIT INFLOW(CFS) = ',F10.1
     C  ,/,11X,'REMAINING UNIT FLOW IS = ',F29.1,' CFS ',/)
C-ROUT THRU BASIN
       GO TO 110
C-FLOW THRU BASIN MODE.-
100    VOLUME=V0
110    CONTINUE
C-FIND INITIAL BASIN DEPTH AND OUTFLOW
       DO 115 II=1,NB
       IF(VOLUME.LT.BV(II+1))GO TO 116
115    CONTINUE
c rmc 114     TI=TIME+.083333
114    TI=TIME+.083333
!Now We are not going to WRITE the Flood.ans---------------------------- 
       WRITE(NUT,909)TI
909    FORMAT(10X,F7.3,5X,
     C '*BASIN CAPACITY EXCEEDED; BASIN DATA IS EXTRAPOLATED*')
       II=NB
116    TEMP=(VOLUME-BV(II))/(BV(II+1)-BV(II))
       D0=BD(II)+TEMP*(BD(II+1)-BD(II))
C.....GET INITIAL VALUES
       O2=0.
       S2=0.
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
!Now We are not going to WRITE the Flood.ans---------------------------- 
       WRITE(NUT,907)TIME,DEADS,A(K),DEPTH2,OAVG,S2
1000   A(K)=OAVG
       A(600)=576
2000   CONTINUE
       CALL MWRITE(NA,A)
       RETURN
C ------------------------------------------------------------------------
C-Hydrograph to export
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
       END SUBROUTINE fthru
