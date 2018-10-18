C   PROGRAM 20 - Based on Hromadka book 237 pag.        
C ---------------------------------------------------------------------------
      SUBROUTINE piper(m,n,mn1,mn2) !ARGU = NDAT (9.25.17)
C ---------------------------------------------------------------------------
C  THIS SUBROUTINE PERFORMS NORMAL DEPTH ROUTING
C ---------------------------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C NA: Stream "A" number. This stream is the one to be modeled				  C 	
C XL: Piper length - the length of the  longest watercourse (FEET) 		  C 			
C XN: Basin Factor (Manning's Friction Factor) [0.008 - 0.999]              C 		
C E1: Upstream elevation (m) [-30 to 3000]							      C 
C E2: Downstream elevation (m) [-60 to 3000]							      C 
C D:  Piper diameter (m) [0.3-30]							                  C 
C TIME1:  Time for Beginning of results (hrs)							      C 
C TIME2:	Time for End of results (hrs)                                     C 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ---------------------------------------------------------------------------
C   DECLARE VARIABLES
C ---------------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION(a-h,o-z)
      !COMMON/BLK1/SS(600,10),SS1(600,10),Hydro(600,3)
      COMMON/NUT/NUT
      COMMON/NDAT/NDAT
      COMMON/BLK10/B(600)
      DIMENSION F(21,2)
      DIMENSION A(600)                                          
      DATA F/0.,.05,.1,.15,.2,.25,.3,.35,.4,.45,.5,.55,.6,.65,.7,.75,
     C  .8,.85,.9,.95,1.,0.,.52,.63,.715,.78,.832,.88,.912,
     C  .945,.97,1.,1.025,1.045,1.06,1.08,1.095,1.11,1.12,1.13,
     C  1.136,1.14/
!     EXPORT Hydrograph, NDATe (hours) StreamA(CFS)
C ---------------------------------------------------------------------------
      SS=SS1
C ---------------------------------------------------------------------------
      READ(NDAT,*)NA,XL,XN,E1,E2,D,TIME1,TIME2 
C ---------------------------------------------------------------------------
C   CONVERSION - To convert in m^3/S
C ---------------------------------------------------------------------------
!      XL=XL/0.3048 !To convert in m^3/S
!      E1=E1/0.3048
!      E2=E2/0.3048
!      D=D/0.3048
C ---------------------------------------------------------------------------
      WRITE(NUT,901)NA
901   FORMAT(/,11X,'MODEL PIPEFLOW ROUTING OF STREAM',I2,' WHERE',
     C /,11X,'STORAGE EFFECTS ARE NEGLECTED WITHIN THE PIPE, FLOW',/,
     C 11X,'VELOCITIES ARE ESTIMATED BY ASSUMING STEADY FLOW FOR',/,
     C 11X,'EACH UNIT INTERVAL(NORMAL DEPTH), AND FLOWS IN EXCESS',/,
     C 11X,'OF (.82)(DIAMETER) ARE PONDED AT THE UPSTREAM INLET:',/)
      WRITE(NUT,306)XL,XN,E1,E2,D
c rmc 305     WRITE(NUT,306)XL,XN,E1,E2,D      
306   FORMAT(20X,'PIPELENGTH(FT) = ',F18.2,/, 
     C 20X,'MANNINGS FACTOR = ',F17.3,/,
     C 20X,'UPSTREAM ELEVATION(FT) = ',F10.2,5X,/,
     C 20X,'DOWNSTREAM ELEVATION(FT) = ',F8.2,/,
     C 20X,'PIPE DIAMETER(FT) = ',F15.2,/)
C ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
C INITIALIZE VARIABLES
C ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      DO 10 I=1,600
10    B(I)=0.d0
      CALL MREAD(NA,A)
      TIME=0.
      STORE=0.
      S=SQRT((E1-E2)/XL)
      QCAP=35.628/XN*.013*S*D**2.6667
      NUMBER=A(600)
      VCAP=QCAP/.7854/D/D
      XA=XL/300.d0
      XB=300.d0/43560.d0
      WRITE(NUT,905)
905   FORMAT(/,13X,'NORMAL DEPTH VELOCITY PIPE ROUTING RESULTS:',//,
     C 10X,'  TIME     INFLOW   VELOCITY   OUTFLOW    UPSTREAM',/,
     C 10X,' (HRS)     (CFS)     (FPS)      (CFS)    PONDING(AF)')
C ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
C MAIN LOOP
C ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      DO 550 I=1,576
      IF(I.GT.NUMBER.AND.STORE.LT..001)GO TO 1000
      Q=A(I)
      !MAC 4/9/12 Next conditional is to consider IF before the hydrograph
      !there are records with value=0
      !IF (Q.EQ.0.AND.I.LT.NUMBER) THEN
      !B(I)=0.d0
      !GO TO 550
      !END IF
      IF(Q.LT.QCAP.AND.STORE.LE.0.d0)GO TO 510
C ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
C PIPE IS UNDER PRESSURE
C ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      V=VCAP
      STORE=STORE+(Q-QCAP)*XB
      Q=QCAP
      IF(STORE.GE.0.d0)GO TO 520
      Q=QCAP+STORE*145.2d0
      STORE=0.d0
C ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
C OPEN FLOW
C ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
510   QQ=Q/QCAP
      INDEX=QQ*20.d0+1.d0
      V=(QQ-F(INDEX,1))/.05*(F(INDEX+1,2)-F(INDEX,2))+F(INDEX,2)
      V=V*VCAP
520   XNUM=XA/V !IF(V.GT.0.)XNUM=XA/V - old code 
      !XNUM=0.   !IF(V.LE.0.)XNUM=0.   - old code
      NUM=XNUM
      ZNUM=NUM
      DA=XNUM-ZNUM
      DB=1.d0-DA
      II=I+NUM+1
      IF(II.GT.576)GO TO 522
      B(II)=B(II)+DA*Q
      II=I+NUM
      B(II)=B(II)+DB*Q
522   TIME=TIME+.083333d0
      IF(TIME.LT.TIME1.OR.TIME.GT.TIME2)GO TO 550
      WRITE(NUT,921)TIME,A(I),V,B(I),STORE  ! STORE???
921   FORMAT(10X,F7.3,3F10.1,F13.3)
550   CONTINUE
1000  B(600)=I

C     CHECK Next line WITH THE BOOK, MIGUEL CAMPO CHANGE THE LIMIT TO B(600)-1
C     BECAUSE IT IS WRITING EXTRANGES PEAKS AFTER THE HYDROGRAPH FINISH
      DO 1100 I=1,600
!      DO 1100 I=1,B(600)-1
!            !WRITE(*,*) B(I)
!1100    CONTINUE
1100	A(I)=B(I)
      CALL MWRITE(NA,A)
C ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
!     WRITE (*,*) "Hydrograph PIPE"
!     DO 715 I=1,600
!     WRITE(*,*) A(I)
!715  CONTINUE
C ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
C HYDROGRAPH TO EXPORT
C ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
!      Hydro(:,2)=A
!      Hydro(:,2)=A*(0.3048**3) !To convert in m^3/s
!      Hydro(600,2)=0
!      DO 716 I=1,A(600)-1
!      DO 716 I=1,mn1
!      IF(I==1) THEN 
!      Hydro(I,1)=0.083333d0
!      Hydro(I,2)=A(I)
!      ELSE
!      J=I-1
!      Hydro(I,1)=Hydro(J,1)+0.083333d0
!      Hydro(I,2)=A(I)
!      END IF
!      WRITE(*,*) Hydro(I,1), Hydro(I,2), Hydro(I,3)
!716   CONTINUE 
!      SS1=SS
      RETURN
      END SUBROUTINE PIPER
