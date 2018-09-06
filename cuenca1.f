C PROGRAM MAIN - Marco Pazmiño-Hernandez, Rafael Muñoz-Carpena, Greg Kiker
C -------------------------------------------------------------------------
      PROGRAM CUENCA
C ------------------------------------------------------------------------
C   MAIN DRIVER FOR CUENCA ROUTING PROGRAM   
C ------------------------------------------------------------------------
C   DECLARE VARIABLES
C ------------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (a-h, o-z)
!      REAL(8) SS,SS1,Hydro
!      REAL(8),COMMON/BLK1/SS(600,10),SS1(600,10),Hydro(600,3)
!      COMMON/BLK1/SS(600,10),SS1(600,10),Hydro(600,3)
      DIMENSION SS(600,10),SS1(600,10),Hydro(600,3)
      DIMENSION A(600)
      COMMON/BLK1/SS
      !DIMENSION SS1(600,10)
      !DIMENSION Hydro(600,3)
!      COMMON/BLK1/SS,SS1,Hydro  
C      DIMENSION PP(m1,n1)
C ------------------------------------------------------------------------
C   OPENING THE CUENCA FILES
C ------------------------------------------------------------------------     
      OPEN(5,file="CUENCA.NDAT")
      OPEN(6,file="CUENCA.ANS")
      OPEN(7,file="CUENCA.SSS")
      OPEN(8,file="CUENCA.RAN")
C -----------------------------------------------------------------------
C  INITIAL FILES
C -----------------------------------------------------------------------
      NDAT=5 ! (file="CUENCA.DAT")
      NUT=6  ! (file="CUENCA.ANS")
      SSS=7   ! (file="CUENCA.SSS") ! error SS=7
      RAN=8  ! (file="CUENCA.RAN")
C -----------------------------------------------------------------------
C  INITIALIZE STREAM DATA BANKS TO ZERO
C -----------------------------------------------------------------------
      DO 10 I=1,600
      A(I)=0.0
10    CONTINUE
      CALL MWRITE(1,A)
      CALL MWRITE(2,A)
      CALL MWRITE(3,A)
      CALL MWRITE(4,A)
      CALL MWRITE(5,A)

      WRITE(NUT,701)
      WRITE(NUT,702)
      WRITE(NUT,701)
C -----------------------------------------------------------------------
C  PROCESS INPUT DATA FILE !Subroutines Cuenca
C -----------------------------------------------------------------------
!      KODE=1  ???
      DO WHILE (KODE.NE.999)
        READ(NDAT,*,ERR=50) ZN1, ZN2, KODE ! Nodes Initial point (ZN) END point (ZN) and the KODE program used - KODE should be in file="CUENCA.DAT"
        IF(KODE.NE.999)THEN
	    !CALL PROCESSES(NUT,NDAT,KODE)      
	    WRITE(NUT,601)
	    WRITE(NUT,600)ZN1, ZN2, KODE
	    WRITE(NUT,601)	
          WRITE(NUT,701)
          CALL PROCESSES(SS1,m,n,PP,m1,n1,NUT,NDAT,RAN,ZN1,ZN2,KODE,
     &     Hydro,mn1,mn2,mn3)
	   ELSE 
	    WRITE(NUT,701)
	    WRITE(NUT,904)
904       FORMAT(20X,'>>> END  OF  CUENCA  ROUTING  ANALYSIS <<<')
C ------------------------------------------------------------------------------      
	    WRITE(7,701)
          WRITE(7,905)
905       FORMAT(20X,'>>>>> SS - FILE TEMPORAL DATA <<<<<',/,
     C    1X,76('='),/, 
     C    6X,'SS',13X,'SS',13X,'SS',14X,'SS',14X,'SS',/,
     C    6X,'(1)',11X,'(2)',13X,'(3)',13X,'(4)',13X,'(5)')
          DO 1200 I=1, 600
            WRITE (7,*) SS(I,1),SS(I,2),SS(I,3),SS(I,4),SS(I,5) ! where this I are coming from?
1200      CONTINUE   
	    STOP
        END IF 
      END DO
50    WRITE(NUT,602)
1000  CONTINUE

      CLOSE(5) ! close "CUENCA.NDAT" file
      CLOSE(6) ! close "CUENCA.ANS" file
      CLOSE(7) ! close "CUENCA.SSS" file
      CLOSE(8) ! close "CUENCA.RAN" file
C ------------------------------------------------------------------------------      
600   FORMAT(3X,'FLOW PROCESS FROM NODE ',F10.2,' TO NODE ',F8.2,
     C' IS CODE = ',I3)
601   FORMAT(1X,76('*'))
602   FORMAT(1X,'*** FATAL READING ERROR - CHECK DATA INPUT ***')
701   FORMAT(1X,76('='))
702   FORMAT(19X,'C U E N C A   R O U T I N G   A N A L Y S I S')
703   FORMAT(1X,76(':'))

      STOP
      END 
C  ----------------------------------------------------------------------------    
C  SUBROUTINE PROCESSES - ! PROGRAM 15  - Based CUENCA
C  ----------------------------------------------------------------------------
       SUBROUTINE PROCESSES(SS1,m,n,PP,m1,n1,NUT,NDAT,RAN,ZN1,ZN2,KODE,
     & Hydro,mn1,mn2,mn3)
C ------------------------------------------------------------------------
C   DECLARE VARIABLES
C ------------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (a-h, o-z)
!      REAL(8) SS,SS1,Hydro
!      REAL(8),COMMON/BLK1/SS(600,10),SS1(600,10),Hydro(600,3)
!      COMMON/BLK1/SS(600,10),SS1(600,10),Hydro(600,3)
      DIMENSION SS(600,10),SS1(600,10),Hydro(600,3)
      !DIMENSION SS1(600,10)
      !DIMENSION Hydro(600,3)
      DIMENSION A(600)
      COMMON/BLK1/SS
!      COMMON/BLK1/SS,SS1,Hydro
C  ----------------------------------------------------------------------------
C  INITIALIZE KODE PROCESSES - ! book example
C  ----------------------------------------------------------------------------
c100   READ (NDAT,*) KODE
c      WRITE(NUT,701)
c      WRITE(NUT,702)
c      WRITE(NUT,703)
      !IF(KODE.EQ.1)CALL unith(NUT,NDAT)
      ! IF(KODE.EQ.1)CALL unith(NUT,NDAT,RAN,ZN1,ZN2,KODE) !TEST 2.14.18 
      ! IF(KODE.EQ.2)CALL flowby(NUT,NDAT) 
      ! IF(KODE.EQ.3)CALL fthru(NUT,NDAT)
      ! IF(KODE.EQ.4)CALL piper(NUT,NDAT)
      ! IF(KODE.EQ.5)CALL convex(NUT,NDAT)
      ! IF(KODE.EQ.6)CALL clear(NUT,NDAT)
      ! IF(KODE.EQ.7)CALL add(NUT,NDAT)
      ! IF(KODE.EQ.8)CALL split(NUT,NDAT)
      ! IF(KODE.EQ.9)CALL move(NUT,NDAT)
C  ----------------------------------------------------------------------------
C  INITIALIZE CUENCA KODE PROCESSES 
C  ----------------------------------------------------------------------------
      IF(KODE.EQ.1)CALL unith(SS1,m,n,PP,m1,n1,NUT,NDAT,RAN,ZN1,ZN2,
     CKODE,Hydro,mn1,mn2)
      IF(KODE.EQ.2)CALL flowby(SS1,m,n,NUT,NDAT,Hydro,mn1,mn2) 
      IF(KODE.EQ.3)CALL fthru(SS1,m,n,NUT,NDAT,Hydro,mn1,mn2,mn3)
      IF(KODE.EQ.4)CALL piper(SS1,m,n,NUT,NDAT,Hydro,mn1,mn2)
      IF(KODE.EQ.5)CALL convex(SS1,m,n,NUT,NDAT,Hydro,mn1,mn2)
      IF(KODE.EQ.6)CALL clear(SS1,m,n,NUT,NDAT,Hydro,mn1,mn2)
      IF(KODE.EQ.7)CALL add(SS1,m,n,NUT,NDAT,Hydro,mn1,mn2)
      IF(KODE.EQ.8)CALL split(SS1,m,n,NUT,NDAT,Hydro,mn1,mn2)
      IF(KODE.EQ.9)CALL move(SS1,m,n,NUT,NDAT,Hydro,mn1,mn2)
      ! IF(KODE.EQ.10)CALL hydrog(SS1,m,n,NDAT,Hydro,mn1,mn2)   
      ! IF(KODE.EQ.11)CALL pdmc(SS2,m1,n1,NDAT,m2 )                     
      ! IF(KODE.EQ.12)CALL uhcn(SS1,m,n,PP,m1,n1,NDAT,mn,Hydro1,mn1,mn2)
c     GO TO 100
1000  CONTINUE
C  ----------------------------------------------------------------------------
701   FORMAT(1X,76('='))
702   FORMAT(19X,'C U E N C A   R O U T I N G   A N A L Y S I S')
703   FORMAT(1X,76(':'))
      
      END SUBROUTINE PROCESSES
	  
!END PROGRAM CUENCA
