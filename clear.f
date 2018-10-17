CC PROGRAM 18  -  Based on Hromadka book pag 222
C -----------------------------------------------------------------------
      SUBROUTINE CLEAR(m,n,mn1,mn2) ! ARGU = NDAT 
C -----------------------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C THIS SUBROUTINE CLEARS A SPECIFIED STREAM DATA BANK                    C
C Variables:                                                             C
C                                                                        C
C K:	Stream number to be set to 0                                       C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ------------------------------------------------------------------------
C   DECLARE VARIABLES
C ------------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (a-h, o-z)
      COMMON/BLK1/SS(600,10),SS1(600,10),Hydro(600,3)
      COMMON/NUT/NUT
      COMMON/NDAT/NDAT
      DIMENSION A(600)
!     EXPORT Hydrograph, Date (hours) StreamA(CFS)
C ------------------------------------------------------------------------
      SS=SS1
!	CLEAR THE K STREAM IN STREAM MATRIX SS
      READ(NDAT,*)K
      CALL MREAD(K,A)
      DO 30 I=1,600
30    A(I)=0.D0
      CALL MWRITE(K,A)
C ------------------------------------------------------------------------      
C OUTPUT
C ------------------------------------------------------------------------
      WRITE(NUT,101)K
101   FORMAT(10X,'STREAM NUMBER',I2,' IS SET TO ZERO.')
C ------------------------------------------------------------------------
C HYDROGRAPH TO EXPORT
C ------------------------------------------------------------------------      
      Hydro(:,2)=A*(0.3048**3)!To obtain in m^3/S
      Hydro(600,2)=0
C ------------------------------------------------------------------------       
      DO 716 I=1,mn1
      IF(I==1) THEN 
      Hydro(I,1)=0.083333
      ELSE
      J=I-1
      Hydro(I,1)=Hydro(J,1)+0.083333
      END IF
      WRITE(*,*) Hydro(I,1), Hydro(I,2), Hydro(I,3)
716   CONTINUE 
      SS1=SS
C ------------------------------------------------------------------------  
      RETURN
      END SUBROUTINE clear
