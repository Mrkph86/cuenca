C     PROGRAM 18  ! Based on Hromadka book pag 222
C -----------------------------------------------------------------------
!      SUBROUTINE MOVE(NUT,NDAT)
      SUBROUTINE MOVE(SS1,m,n,NUT,NDAT,Hydro,mn1,mn2) ! ARGU = NDAT (9.25.17)
C -----------------------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C THIS SUBROUTINE MOVES STREAM NA FORWARD IN TIME BY DELT HOURS         C
C VARIABLES:                                                            C
C NA:     Stream "A" number. This stream is the one to be modeled	      C						
C DELT:   Duration of the translation,hrs [0.1 - 48]					  C			
C TIME1:  Time for Beginning of results (hrs)							  C	
C TIME2:  Time for End of results (hrs)                                 C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C -----------------------------------------------------------------------
C  DECLARE VARIABLES
C -----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (a-h, o-z)
      COMMON/BLK1/SS(600,10),SS1(600,10),Hydro(600,3) !(8.29.18)
      !DIMENSION SS(600,10),SS1(600,10),Hydro(600,3)
!      COMMON/BLK1/SS
      COMMON/BLK10/B(600)
      DIMENSION A(600)
!     EXPORT Hydrograph, Date (hours) StreamA(CFS)
C ------------------------------------------------------------------------
C  INITIALIZE VARIABLES
C ------------------------------------------------------------------------	  
      SS=SS1
C ------------------------------------------------------------------------
C   INPUT DATA
C ------------------------------------------------------------------------      
      READ(NDAT,*)NA,DELT,TIME1,TIME2 ! (9.25.17)
      CALL MREAD(NA,A)
C ------------------------------------------------------------------------
      WRITE(NUT,901)NA,DELT
901   FORMAT(/,10X,'MOVE STREAM NUMBER',I2,' FORWARD IN TIME',
     C  ' BY',F7.3,' HOURS:',/)      
      WRITE(NUT,903)NA,NA,DELT
903   FORMAT(10X,' MODEL        STREAM',I2,3X,' STREAM',I2,/,
     C  10X,' TIME          (CFS)      MOVED',F7.3,' HOURS')
      DO 20 I=1,600
20    B(I)=0.D0
      NUMBER=A(600)
      XM=DELT*12.
      M=XM
      TIME=0.D0 
C (M=NUMBER OF INTERVALS MOVED FORWARD)
      NUM1=NUMBER+M
      IF(NUM1.LT.576)GO TO 50
C HYDROGRAPH EXCEEDS 576; REDUCE NUMBER
      NUMBER=NUMBER+(576-NUM1)
      NUM1=576
C MOVE HYDROGRAPH FORWARD
50    XA=M
      XA=XM-XA
      XB=1.-XA
      DO 100 I=1,NUMBER
      J=I+M+1
      JJ=I+M
      B(J)=A(I)*XA
      B(JJ)=A(I)*XB+B(JJ)
      TIME=TIME+.083333
      IF(TIME.LT.TIME1.OR.TIME.GT.TIME2)GO TO 100
C ------------------------------------------------------------------------ 
      WRITE(NUT,921)TIME,A(I),B(I)
921   FORMAT(10X,F7.3,3X,F10.1,F10.1)
100   CONTINUE
C WRITE RESULTS TO MEMORY
      B(600)=J
      CALL MWRITE(NA,B)
C ------------------------------------------------------------------------
C HYDROGRAPH TO EXPORT
C ------------------------------------------------------------------------
!     Hydro(:,2)=B*(0.3048**3)!To obtain in m^3/S
!     Hydro(600,2)=0
!     DO 716 I=1,J
!     DO 716 I=1,mn1
!     IF(I==1) THEN 
!     Hydro(I,1)=0.083333
!     Hydro(I,2)=B(I)
!     ELSE
!     J=I-1
!     Hydro(I,1)=Hydro(J,1)+0.083333
!     Hydro(I,2)=B(I)
!     END IF
!     WRITE(*,*) Hydro(I,1), Hydro(I,2), Hydro(I,3)
!716  CONTINUE 
!     SS1=SS
      RETURN
      END SUBROUTINE MOVE
