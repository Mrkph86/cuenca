C-------------------------------------------------------
! PROGRAM 21 - Based on Hromadka book 
C-------------------------------------------------------
      SUBROUTINE MWRITE(icol,TEMP)
C For each column (icol) of holding matrix (SS), fixes value to TEMP vector
C-------------------------------------------------------
      DIMENSION TEMP(600)
      COMMON/BLK1/SS(600,10)
c      WRITE(*,*) 'TEST:inside of SUBROUTINE MWRITE'
      DO 100 i=1,600
        SS(i,icol)=TEMP(i)
100   CONTINUE
      RETURN
      END SUBROUTINE MWRITE
