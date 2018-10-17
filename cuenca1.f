C   PROGRAM MAIN - Marco Pazmiño-Hernandez, Rafael Muñoz-Carpena, Greg Kiker
C -------------------------------------------------------------------------
      PROGRAM CUENCA
C ------------------------------------------------------------------------
C     MAIN DRIVER FOR CUENCA ROUTING PROGRAM     
      DIMENSION A(600)
      COMMON/BLK1/SS(600,10)
C      CHARACTER(LEN=35) :: CCC          
C      DIMENSION SS(600,10)
C      INTEGER m,n,m1,n1,mn1,mn2
      
C ------------------------------------------------------------------------
C   Opening the files
C ------------------------------------------------------------------------     
      OPEN(5,file="CUENCA.DAT")
      OPEN(6,file="CUENCA.ANS")
      !OPEN(7,file="SS.txt")
      !OPEN(9,file="par.txt") ! values of m,n,m1,n1,mn1,mn2 .txt file
      
C------------------------------------------------------------------------
C  INITIALIZE VARIABLES
C------------------------------------------------------------------------
      NUT=6  ! (file="CUENCA.ANS")
      NDAT=5 ! (file="CUENCA.DAT")
c     par=9  !(file="par.txt")
c     READ(par,*)m,n,m1,n1,mn1,mn2

C------------------------------------------------------------------------
C  INITIALIZE STREAM DATA BANKS TO ZERO
C------------------------------------------------------------------------
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
C------------------------------------------------------------------------
C  PROCESS DATA FILE !Subroutines Cuenca
C------------------------------------------------------------------------
!     KODE=1
      DO WHILE (KODE.NE.999)
      READ(NDAT,*,ERR=50) ZN1, ZN2, KODE ! Nodes Initial point (ZN) END point (ZN) and the KODE program used - KODE should be in file="CUENCA.ANS"
	IF(KODE.NE.999)THEN
      CALL PROCESSES(NUT,NDAT,KODE)
	WRITE(NUT,601)
	WRITE(NUT,600)ZN1, ZN2, KODE
	WRITE(NUT,601)	
      WRITE(NUT,701)	
	ELSE 
	WRITE(*,*) 'END OF CUENCA ROUTING ANALYSIS'
	DO 1200 I=1, 600
      WRITE (7,*) SS(I,1),SS(I,2),SS(I,3),SS(I,4),SS(I,5)
1200  CONTINUE   
	STOP
	END IF
      END DO
50    WRITE(NUT,602)

1000  CONTINUE

      CLOSE(5)! close "CUENCA.DAT" file
      CLOSE(6)! close "CUENCA.ANS" file
      !CLOSE(7)! close "SS.txt" file
      !CLOSE(9)! close "par.txt" file
      
600   FORMAT(3X,'FLOW PROCESS FROM NODE ',F10.2,' TO NODE ',F8.2,
     C' IS CODE = ',I3)
601   FORMAT(1X,76('*'))
602   FORMAT(1X,'*** FATAL READING ERROR - CHECK DATA INPUT ***')
701   FORMAT(1X,76('='))
702   FORMAT(19X,'C U E N C A   R O U T I N G   A N A L Y S I S')
703   FORMAT(1X,76(':'))

      STOP
      END 
C -----------------------------------------------------------------------------    
C SUBROUTINE PROCESSES - ! PROGRAM 15  - Based CUENCA
C  ----------------------------------------------------------------------------
      SUBROUTINE PROCESSES(NUT,NDAT,KODE) ! Processes SUBROUTINE  
C  ----------------------------------------------------------------------------
C  INITIALIZE VARIABLES
C  ----------------------------------------------------------------------

c100   READ (NDAT,*) KODE
c      WRITE(NUT,701)
c      WRITE(NUT,702)
c      WRITE(NUT,703)
      IF(KODE.EQ.1)CALL unith(NUT,NDAT)
      IF(KODE.EQ.2)CALL flowby(NUT,NDAT) 
      IF(KODE.EQ.3)CALL fthru(NUT,NDAT)
      IF(KODE.EQ.4)CALL piper(NUT,NDAT)
      IF(KODE.EQ.5)CALL convex(NUT,NDAT)
      IF(KODE.EQ.6)CALL clear(NUT,NDAT)
      IF(KODE.EQ.7)CALL add(NUT,NDAT)
      IF(KODE.EQ.8)CALL split(NUT,NDAT)
      IF(KODE.EQ.9)CALL move(NUT,NDAT)
      !IF(KODE.EQ.10)CALL hydrog(SS1,m,n,NDAT,Hydro,mn1,mn2)   
      !IF(KODE.EQ.11)CALL pdmc(SS2,m1,n1,NDAT,m2 )                     
      !IF(KODE.EQ.12)CALL uhcn(SS1,m,n,PP,m1,n1,NDAT,mn,Hydro1,mn1,mn2)
c      GO TO 100
1000  CONTINUE

701   FORMAT(1X,76('='))
702   FORMAT(19X,'C U E N C A   R O U T I N G   A N A L Y S I S')
703   FORMAT(1X,76(':'))
      
      END SUBROUTINE PROCESSES
	  
!END PROGRAM CUENCA
