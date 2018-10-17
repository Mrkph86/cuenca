C     PROGRAM 19 - Based on Hromadka book 231 pag
C  ----------------------------------------------------------------------
      SUBROUTINE CONVEX(NUT,NDAT)
!      SUBROUTINE convex(SS1,m,n,NDAT,Hydro,mn1,mn2) !ARGU = NDAT (9.25.17)
! !------------------------------------------------------------------------
      ! REAL(8),DIMENSION(m,n) :: SS1
      ! INTEGER,VALUE :: m
      ! INTEGER,VALUE :: n
      ! !INTEGER,VALUE :: mn
! ! EXPORT Hydrograph, Date (hours) StreamA(CFS)
      ! REAL(8),DIMENSION(mn1,mn2) :: Hydro
      ! INTEGER,VALUE :: mn1
      ! INTEGER,VALUE :: mn2
! C ------------------------------------------------------------------------
!      COMMON/BLK1/SS(600,10)
C  ----------------------------------------------------------------------
C-THIS SUBROUTINE MODELS CHANNEL ROUTING BY THE SIMPLER CONVEX METHOD
C-WHERE A CSTAR VALUE AS ESTIMATED DUE TO IRREGULAR DELT VALUES
C ------------------------------------------------------------------------
      COMMON/BLK10/B(600)
      DIMENSION A(600)
C ------------------------------------------------------------------------
C-INITIALIZE VARIABLES
C ------------------------------------------------------------------------
      TIME=0.
      SS=SS1
C ------------------------------------------------------------------------
C-INPUT DATA
C ------------------------------------------------------------------------
      READ(NDAT,*)NA,C,V0,TIME1,TIME2   !(1.25.18)
      READ(NDAT,*)BB,Z,E1,E2,XL,XN    ! (1.25.18)
C ------------------CONVERSION--------------------------------------------      
!      (V0=V0)/0.3048 !To convert m to feet
!      BB=BB/0.3048 !To convert m to feet
!      E1=E1/0.3048 !To convert m to feet
!      E2=E2/0.3048 !To convert m to feet
!      XL=XL/0.3048 !To convert m to feet      
      
!Now We are not going to WRITE the Flood.ans----------------------------
      WRITE(NUT,901)NA
901   FORMAT(//,10X,'MODEL CHANNEL ROUTING BY CONVEX METHOD WHERE',/,
     C 10X, 'A MODIFIED C-ROUTING COEFFICIENT IS ESTIMATED IN ORDER',/,
     C 10X, 'TO ROUT THE STREAM',I2,' INFLOW HYDROGRAPH BY 5-MINUTE',/,
     C 10X, 'INTERVALS (reference: National Engineering Handbook,',/,
     C 10X, 'Hydrology, Chapter 17, page 17-52, August,1972,',/,
     C 10X, 'U.S. Department of Commerce).',/) 
      IF(C.GT.0.)WRITE(NUT,903)C
903   FORMAT(10X,'USER-SPECIFIED CHANNEL ROUTING COEFFICIENT = ',F6.3,/)
      IF(V0.GT.0.)WRITE(NUT,904)V0
904   FORMAT(10X,'USER-SPECIFIED CHANNEL AVG VELOCITY(FPS) = ',F7.3,/)
      WRITE(NUT,905)BB,Z,E1,E2,XL,XN
905   FORMAT(10X,'ASSUMED REGULAR CHANNEL INFORMATION:',/,
     C  17X,'BASEWIDTH(FT) = ',F17.2,/,
     C  17X,'CHANNEL Z = ',F21.2,/,
     C  17X,'UPSTREAM ELEVATION = ',F12.2,/,
     C  17X,'DOWNSTREAM ELEVATION = ',F10.2,/,
     C  17X,'CHANNEL LENGTH(FT) = ',F12.2,/,
     C  17X,'MANNINGS FACTOR= ',F16.3,/)
      DO 10 I=1,600
10    B(I)=0.
      CALL MREAD(NA,A)
      ISUM=0
      QSUM=0.
      NUMBER=A(600)
C ------------------------------------------------------------------------
C-FIND QMAX OF STREAM
C ------------------------------------------------------------------------
      QMAX=0.
      DO 12 I=1,NUMBER
      IF(A(I).GT.QMAX)QMAX=A(I)
12    CONTINUE
      F=QMAX/2.
      IF(C.GT.0.)GO TO 100
C ------------------------------------------------------------------------
C-FIND SUM OF Q50
C ------------------------------------------------------------------------
      DO 20 I=1,NUMBER
      IF(A(I).LT.F)GO TO 20
      QSUM=QSUM+A(I)
      ISUM=ISUM+1
20    CONTINUE
      X=ISUM
      QAVG=QSUM/X
      CALL TRAPV(QAVG,BB,Z,E1,E2,XL,XN,V)
      C=V/(V+1.7)
!Now We are not going to WRITE the Flood.ans----------------------------
      WRITE(NUT,909)QMAX,QAVG,QAVG,V,C ! QAVG is repeated twice
909   FORMAT(/,11X,'CHANNEL ROUTING COEFFICIENT ESTIMATED:',/,
     C 14X,'MAXIMUM INFLOW(CFS) = ',F37.2,/,
     C 14X,'AVERAGE FLOWRATE IN EXCESS OF 50% MAXIMUM INFLOW = ',F8.2,/,
     C 14X,'CHANNEL NORMAL VELOCITY FOR Q = ',F5.2,' CFS = ',F15.2,'FPS' !confuse about this values
     C ,/,14X,'ESTIMATED CHANNEL ROUTING COEFFICIENT = ',F19.3,/)
908   FORMAT(/,11X,'CONVEX METHOD CHANNEL ROUTING RESULTS:',//,
     C 11X,' MODEL      INFLOW      OUTFLOW',/,
     C 11X,' TIME     (STREAM',I2,')   (STREAM',I2,')',/,
     C 11X,' (HRS)       (CFS)        (CFS)')
100   IF(V0.GT.0.)V=V0
      TT=XL/3600./V
      DELT=TT*C
      EX=(.08333+.5*DELT)/ (1.5*DELT)
      CSTAR=1.-(1.-C)**EX
      CIN=CSTAR
      COUT=1.-CSTAR
      X=DELT*12.
      NUM=X
      IF((NUMBER+NUM+1).LE.576)GO TO 180
      NUMBER=NUMBER-NUM-1
180   CONTINUE
      Y=NUM
      DA=X-Y
      DB=1.-DA
!Now We are not going to WRITE the Flood.ans----------------------------
      WRITE(NUT,911)CSTAR
911   FORMAT(10X,' MODIFIED CHANNEL ROUTING COEFFICIENT FOR 5-MINUTE ',/,
     C 14X,'UNIT INTERVALS IS CSTAR = ',F33.3,/)
C ------------------------------------------------------------------------
C-ROUTING LOOP AND FIND NEW QMAX
C ------------------------------------------------------------------------
      WRITE(NUT,908)NA,NA
      QMAX=0.
      NUMB1=NUMBER+NUM+1
      QOUT=0.
      QIN=A(1)
      DO 200 I=1,NUMB1
      II=NUM+I+1
      QOUT=QOUT*COUT+QIN*CIN
      B(II)=B(II)+DA*QOUT
      II=II-1
      B(II)=B(II)+DB*QOUT
      TIME=TIME+.083333
      IF(TIME.LT.TIME1.OR.TIME.GT.TIME2)GO TO 200
!Now We are not going to WRITE the Flood.ans----------------------------
      WRITE(NUT,913)TIME,A(I),B(I)
913   FORMAT(10X,F7.3,1X,2F12.1)
200   QIN=A(I+1)
      B(600)=NUMBER+NUM+1
      DO 300 I=1,600
300   A(I)=B(I)
      CALL MWRITE(NA,A)
C ------------------------------------------------------------------------
C Hydrograph to export
C ------------------------------------------------------------------------
!      Hydro(:,2)=A*(0.3048**3) !To convert in m^3/S
!      Hydro(600,2)=0
!      DO 715 I=1,600
!      WRITE(*,*) A(I)
!715   CONTINUE
!      DO 716 I=1,B(600)
!      DO 716 I=1,mn1
!      IF(I==1) THEN 
!      Hydro(I,1)=0.083333
!      Hydro(I,2)=A(I)
!      ELSE
!      J=I-1
!      Hydro(I,1)=Hydro(J,1)+0.083333
!      Hydro(I,2)=A(I)
!      END IF
!      WRITE(*,*) Hydro(I,1), Hydro(I,2), Hydro(I,3)
!716   CONTINUE 
!      SS1=SS
      RETURN
      END SUBROUTINE CONVEX
