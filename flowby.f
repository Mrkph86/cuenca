C PROGRAM 17    ! Based on Hromadka book pag 217
C--------------------------------------------------------------
      SUBROUTINE FLOWBY(NUT,NDAT)
C      SUBROUTINE flowby(SS1,m,n,NDAT,Hydro,mn1,mn2) !   ARGU = NDAT (9.25.17)
C--------------------------------------------------------------
C-THIS SUBROUTINE USES A FIVE-MINUTE UNIT EXPLICIT MODEL
C-TO SIMULATE A FLOWBY BASIN,
C C-VARIABLES:
C
C       NA=STREAM NUMBER ENTERING THE SYSTEM
C       NB=STREAM NUMBER FOR FLOW EXCESS
C       QCAP=FLOWBY Q (m^3/s)
C       TIME1,TIME2=OUTPUT LIMITS
C
C-MEMORY ALLOCATION
      DIMENSION A(600),B(600)
! COMMON/BLK1/SS(600,10)
! C ------------------------------------------------------------------------
      ! REAL(8),DIMENSION(m,n) :: SS1
      ! INTEGER,VALUE :: m
      ! INTEGER,VALUE :: n
      ! !INTEGER,VALUE :: mn
! ! EXPORT Hydrograph, Date (hours) StreamA(m^3/s) StreamB(m^3/s)
      ! REAL(8),DIMENSION(mn1,mn2) :: Hydro
      ! INTEGER,VALUE :: mn1
      ! INTEGER,VALUE :: mn2
C ------------------------------------------------------------------------
      SS=SS1
      !Hydro=Hydro1 ! M.P test 10.1.17
	  !!this units
!--------------
C-READ IN STREAM DATA
      READ(NDAT,*)NA,NB,QCAP,TIME1,TIME2
C------- Conversion -------  (9.25.17)
!      QCAP=(QCAP)/(0.3048**3)!To obtain QCAP in CFS, The book works with
	  !!this units
      CALL MREAD(NA,A)
      IF(NB.GT.0)  CALL MREAD(NB,B)
C ------------------------------------------------------------------------
C     GRAPHICS
C ------------------------------------------------------------------------
921   FORMAT(32X, 'INFLOW',/,31X,'(STREAM',I2,') ',/,
     C 3(35X,'|',/),21X,' ---------    |'/,
     C 21X,'|',9X,'|',3X,'|',/,21X,'|',9X,'|<--* < =flowby Structure',/,
     C 21X, '|  basin  |   | (Maximum flowby Q = ',F5.1,' cfs)',/,
     C 21X,'| storage |   |',/,21X,' ---------    |',/,
     C  2(35X,'|',/),35X,'V',/,30X,' STREAM',I2,/,32X,'FLOWBY',///)
923    FORMAT(20X,'     INFLOW              INFLOW',/,
     C 20X,'     (STREAM',I2,')         (STREAM',I2,')',/,
     C 27X,'|',19X,'|',/,27X,'|',19X,'|',/,
     C 27X,'|',4X,'flow excess',4X,'|',/,
     C 27X,'<-----------------------------------* <=flowby structure',/,
     C 27X,'|',19X,'|',1X,'(flowby Q = ',F8.1,' cfs)',/,
     C 27X,'|',19X,'|',/,27X,'|',19X,'|',/,27X,'|',19X,'|',/,
     C 27X,'V',19X,'V',/,25X,'STREAM',I2,12X,'STREAM',I2,/,
     C 20X,'+ FLOW EXCESS',13X,'FLOWBY',//)
C ----------------------------------------------------------------------
C-INITIALIZE VARIABLES
C ----------------------------------------------------------------------
      TIME=0.
      NUMBER=A(600)
      IF(NUMBER.GT.576)NUMBER=576
      STORE=0.
906   FORMAT(//,10X,'FLOWBY BASIN MODELING RESULTS:',//,
     C 14X,' MODEL        INFLOW    INFLOW   OUTFLOW    FLOWBY',/,
     C 14X,'  TIME   ',4('   (STREAM',I2,')'),/,
     C 14X,' (HRS)  ',4X,4('    (CFS)  '))
908   FORMAT(//,10X,'FLOWBY BASIN MODELING RESULTS:',//,
     C 14X,'  MODEL  STREAM',I2,'  STREAM',I2,'      BASIN',/,
     C 14X,'   TIME   INFLOW    FLOWBY      VOLUME',/,
     C 14X,' (HRS)     (CFS)     (CFS)       (AF)',/)
C ----------------------------------------------------------------------
C READ MODEL DATA
C ----------------------------------------------------------------------
!Now We are not going to WRITE the Flood.ans----------------------------
      WRITE(NUT,901)NA       
901   FORMAT(//,10X,'MODEL STREAM NUMBER',I2,' FLOWING PAST A',
     C' FLOWBY STRUCTURE:')
      WRITE (NUT,905) NA, QCAP
      IF(NB.EQ.0)WRITE(NUT,902)
902   FORMAT(10X,'FLOW EXCESS IS ASSUMED TO BE PERMANENTLY STORED.',//)
      IF(NB.NE.0)WRITE(NUT,903)NB
903   FORMAT(10X,'FLOW EXCESS IS ASSUMED TO BE ADDED TO STREAM NUMBER',
     C I2,//)
905   FORMAT(10X,'FLOWRATES IN STREAM #',I2,' WHICH ARE GREATER THAN',/
     C ,10X,F8.1,' CFS ARE ASSUMED TO BE EXCESS FLOWS ')
      IF(NB.EQ.0)WRITE(NUT,921)NA,QCAP,NA
      IF(NB.GT.0)WRITE(NUT,923)NB,NA,QCAP,NB,NA
      IF(NB.EQ.0)WRITE(NUT,908)NA,NA
      IF(NB.GT.0)WRITE(NUT,906)NB,NA,NB,NA
C ----------------------------------------------------------------------------
C-MODEL FLOWBY EFFECTS
C ----------------------------------------------------------------------------
      IF(NB.GT.0)GO TO 199
C ----------------------------------------------------------------------------
C-MODEL DEAD STORAGE
C ----------------------------------------------------------------------------
      DO 100 I=1,NUMBER
      TIME=TIME+.08333
      Z=A(I)
      X=Z-QCAP
      IF(X)99,99,50
50    STORE=STORE+X/145.2
      A(I)=QCAP
      !Hydro(I,3)=(Z-QCAP)
      !Hydro(I,3)=(Z-QCAP)*(0.3048**3)!To obtain in m^3/S (1.26.18)
99    IF(TIME.LT.TIME1.OR.TIME.GT.TIME2)GO TO 100
!Now We are not going to WRITE the Flood.ans----------------------------
      WRITE(NUT,907)TIME,Z,A(I),STORE
907   FORMAT(10X,F7.3,F9.1,F10.1,F13.3)
100   CONTINUE
      GO TO 1000
199   DO 200 I=1,NUMBER
      Z=A(I)
      ZB=B(I)
      TIME=TIME+.0833333
      X=Z-QCAP
      IF(X)198,198,150
150   B(I)=B(I)+X
      A(I)=QCAP
        !Export Hydrograph to a permanent storage
		!WRITE(*,*) "HOLA1"
!        Hydro(I,3)=B(I)*(0.3048**3)!To obtain in m^3/S
		!Hydro(I,3)=B(I)
198   IF(TIME.LT.TIME1.OR.TIME.GT.TIME2)GO TO 200
!Now We are not going to WRITE the Flood.ans----------------------------
      WRITE(NUT,909)TIME,ZB,Z,B(I),A(I)
909   FORMAT(10X,F7.3,3X,4F10.1)
200   CONTINUE
      NUMB=B(600)
      IF(NUMBER.GT.NUMB)NUMB=NUMBER
      B(600)=NUMB
1000  CONTINUE
      CALL MWRITE(NA,A)
      IF(NB.GT.0) CALL MWRITE(NB,B)
              !Export Hydrograph to Stream B
			  !WRITE(*,*) "HOLA2"
!              Hydro(:,3)=B*(0.3048**3)!To obtain in m^3/S
			  !Hydro(:,3)=B
C ------------------------------------------------------------------------
C Hydrograph to export
C ------------------------------------------------------------------------
!      B(600)=0
!      Hydro(:,3)=B
!      Hydro(600,3)=0
!      A(600)=0
!      WRITE(*,*) "HOLA3"
!      Hydro(:,2)=A*(0.3048**3)!To obtain in m^3/S
!      Hydro(:,2)=A
!      DO 716 I=1,mn1
!      IF(I==1) THEN 
!      Hydro(I,1)=0.083333
!      ELSE
!      J=I-1
!      WRITE(*,*) "HOLA4"
!      Hydro(I,1)=Hydro(J,1)+0.083333
!      Hydro(I,2)=A(I)*(0.3048**3)!To obtain in m^3/S
!      Hydro(I,2)=A(I)
!      END IF
!      WRITE(*,*) Hydro(I,1), Hydro(I,2), Hydro(I,3)
!716   CONTINUE    
!      SS1=SS ! Just to update its value
      RETURN
      END SUBROUTINE FLOWBY
