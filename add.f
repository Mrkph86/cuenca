C     PROGRAM 18  ! Based on Hromadka book pag 222
C -------------------------------------------------------------------------
      SUBROUTINE ADD(m,n,mn1,mn2) ! ARGU = NDAT (9.25.17)
C -------------------------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C THIS SUBROUTINE ADDS A STREAM DATA BANK TO ANOTHER STREAM DATA BANK     C
C VARIABLES:                                                              C
C NUMA:  Streams to be added								                C
C NUMS:  Stream to receive flow from another stream                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C -------------------------------------------------------------------------
C   DECLARE VARIABLES
C --------------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (a-h, o-z)
      COMMON/BLK1/SS(600,10),SS1(600,10),Hydro(600,3)
      COMMON/NUT/NUT
      COMMON/NDAT/NDAT
      DIMENSION A(600),B(600)
!     EXPORT Hydrograph, Date (hours) StreamA(CFS)
C ----------------------------------------------------------------------------
      SS=SS1
      READ(NDAT,*)NUMA,NUMS
      CALL MREAD(NUMS,A)
      CALL MREAD(NUMA,B)
      NUMBA=A(600)
      NUMBS=B(600) ! SHOULD WE ADD THE m,n variables here. 
      NUMBER=NUMBA
      IF(NUMBS.GT.NUMBA)NUMBER=NUMBS
      DO 100 I=1,NUMBER
100   A(I)=A(I)+B(I)
      A(600)=NUMBER
      CALL MWRITE(NUMS,A)
C -------------------------------------------------------------------------------
C OUTPUT
C -------------------------------------------------------------------------------
      WRITE(NUT,101)NUMA,NUMS
101   FORMAT(10X,'STREAM NUMBER',I2,' ADDED TO STREAM NUMBER',I2)
      RETURN
C -------------------------------------------------------------------------------
C HYDROGRAPH TO EXPORT
C -------------------------------------------------------------------------------
!      Hydro(:,2)=A*(0.3048**3)!To obtain in m^3/S
!      Hydro(600,2)=0
!      DO 716 I=1,NUMBER
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
      END SUBROUTINE ADD
