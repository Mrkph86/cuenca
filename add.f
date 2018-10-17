C     PROGRAM 18  ! Based on Hromadka book pag 222
C -----------------------------------------------------------------------
      SUBROUTINE ADD(NUT,NDAT)
!     SUBROUTINE add(SS1,m,n,NDAT,Hydro,mn1,mn2) ! ARGU = NDAT (9.25.17)
C -----------------------------------------------------------------------
C-THIS SUBROUTINE ADDS A STREAM DATA BANK
C-TO ANOTHER STREAM DATA BANK
C -----------------------------------------------------------------------
      DIMENSION A(600)
      DIMENSION B(600)
      ! COMMON/BLK1/SS(600,10)
! !------------------------------------------------------------------------
      ! REAL(8),DIMENSION(m,n) :: SS1
      ! INTEGER,VALUE :: m
      ! INTEGER,VALUE :: n
      ! !INTEGER,VALUE :: mn
! ! EXPORT Hydrograph, Date (hours) StreamA(CFS)
      ! REAL(8),DIMENSION(mn1,mn2) :: Hydro
      ! INTEGER,VALUE :: mn1
      ! INTEGER,VALUE :: mn2
! !------------------------------------------------------------------------
      SS=SS1
      READ(NDAT,*)NUMA,NUMS
      CALL MREAD(NUMS,A)
      CALL MREAD(NUMA,B)
      NUMBA=A(600)
      NUMBS=B(600)
      NUMBER=NUMBA
      IF(NUMBS.GT.NUMBA)NUMBER=NUMBS
      DO 100 I=1,NUMBER
100   A(I)=A(I)+B(I)
      A(600)=NUMBER
      CALL MWRITE(NUMS,A)
C-OUTPUT
!Now We are not going to WRITE the Flood.ans----------------------------
      WRITE(NUT,101)NUMA,NUMS
101   FORMAT(10X,'STREAM NUMBER',I2,' ADDED TO STREAM NUMBER',I2)
      RETURN
C -----------------------------------------------------------------------
C Hydrograph to export
C ------------------------------------------------------------------------
!      Hydro(:,2)=A*(0.3048**3)!To obtain in m^3/S
!      Hydro(600,2)=0
!      DO 716 I=1,NUMBER
!      DO 716 I=1,mn1
!      IF(I==1) THEN 
!              Hydro(I,1)=0.083333
!              Hydro(I,2)=A(I)
!      ELSE
!      J=I-1
!              Hydro(I,1)=Hydro(J,1)+0.083333
!              Hydro(I,2)=A(I)
!      END IF
!          WRITE(*,*) Hydro(I,1), Hydro(I,2), Hydro(I,3)
!716   CONTINUE 
!      SS1=SS
      END SUBROUTINE add
