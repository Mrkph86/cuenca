C     PROGRAM 18  ! Based on Hromadka book pag 222
C--------------------------------------------------------------
      SUBROUTINE CLEAR(NUT,NDAT)
!    SUBROUTINE clear(SS1,m,n,NDAT,Hydro,mn1,mn2) ! ARGU = NDAT 9.25.17
C----------------------------------------------------------
C       THIS SUBROUTINE CLEARS A SPECIFIED STREAM DATA BANK
      DIMENSION A(600)
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
!	CLEAR THE K STREAM IN STREAM MATRIX SS
      READ(NDAT,*)K
      CALL MREAD(K,A)
      DO 30 I=1,600
30    A(I)=0.
      CALL MWRITE(K,A)
C-OUTPUT
!Now We are not going to WRITE the Flood.ans----------------------------
      WRITE(NUT,101)K
101   FORMAT(10X,'STREAM NUMBER',I2,' IS SET TO ZERO.')
C ------------------------------------------------------------------------
C Hydrograph to export
C ------------------------------------------------------------------------      
!      Hydro(:,2)=A*(0.3048**3)!To obtain in m^3/S
!      Hydro(600,2)=0
!      DO 716 I=1,mn1
!      IF(I==1) THEN 
!      Hydro(I,1)=0.083333
!      ELSE
!      J=I-1
!      Hydro(I,1)=Hydro(J,1)+0.083333
!      END IF
!      WRITE(*,*) Hydro(I,1), Hydro(I,2), Hydro(I,3)
!716   CONTINUE 
!      SS1=SS
      RETURN
      END SUBROUTINE clear
