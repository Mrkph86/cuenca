C ------------------------------------------------------------------------
C  PROGRAM 21 - Based on Hromadka book 
C ------------------------------------------------------------------------
      SUBROUTINE MREAD(icol,TEMP)
! Fix TEMP values to icol column of SS
C ------------------------------------------------------------------------   
C   DECLARE VARIABLES
C ------------------------------------------------------------------------      
      IMPLICIT DOUBLE PRECISION (a-h, o-z)
!      COMMON/BLK1/SS(600,10),SS1(600,10),Hydro(600,3) !9.4.18
      DIMENSION SS(600,10),SS1(600,10),Hydro(600,3)
      DIMENSION A(600)
      COMMON/BLK1/SS
      DIMENSION TEMP(600)
C ------------------------------------------------------------------------
      DO 100 i=1,600
         TEMP(i)=SS(i,icol)
100   CONTINUE

      RETURN
      END SUBROUTINE MREAD
