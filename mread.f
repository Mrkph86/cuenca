C-------------------------------------------------------
! PROGRAM 21 - Based on Hromadka book 
C-----------------------------------------
      SUBROUTINE MREAD(icol,TEMP)
! Fix TEMP values to icol column of SS
      DIMENSION TEMP(600)
      COMMON/BLK1/SS(600,10)

      DO 100 i=1,600
         TEMP(i)=SS(i,icol)
100   CONTINUE

      RETURN
      END SUBROUTINE MREAD
