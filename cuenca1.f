C PROGRAM MAIN - Marco Pazmiño-Hernandez, Rafael Muñoz-Carpena, Greg Kiker
C -------------------------------------------------------------------------
      PROGRAM CUENCA
C ------------------------------------------------------------------------
C   MAIN DRIVER FOR CUENCA ROUTING PROGRAM   
C ------------------------------------------------------------------------
C   DECLARE VARIABLES
C ------------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (a-h, o-z)
      COMMON/BLK1/SS(600,10),SS1(600,10),Hydro(600,3)! do I have to integrate ss1 and hydro here
      DIMENSION A(600)
      COMMON/NUT/NUT
      COMMON/NDAT/NDAT
C -----------------------------------------------------------------------
C  INITIAL FILES
C -----------------------------------------------------------------------
      NDAT=5  ! (file="CUENCA.NDAT")
      NUT=6   ! (file="CUENCA.ANS")
!      SSS=7  ! (file="CUENCA.SSS") 
      RAN=8   ! (file="CUENCA.RAN")
C ------------------------------------------------------------------------
C   OPENING THE CUENCA FILES
C ------------------------------------------------------------------------     
      OPEN(NDAT,file="CUENCA.NDAT") ! (file="DATA")
      OPEN(NUT,file="CUENCA.ANS")   ! (file="RESULT")
      OPEN(RAN,file="CUENCA.RAN")   ! (file="RAIN DATA")
!      OPEN(SSS,file="CUENCA.SSS")  ! (file="TEMP SS - RESULT")
C -----------------------------------------------------------------------
C  INITIALIZE STREAM DATA BANKS TO ZERO
C -----------------------------------------------------------------------
      DO 10 I=1,600
      A(I)=0.D0
10    CONTINUE
      CALL MWRITE(1,A)
      CALL MWRITE(2,A)
      CALL MWRITE(3,A)
      CALL MWRITE(4,A)
      CALL MWRITE(5,A)
C ------------------------------------------------------------------------
      WRITE(NUT,701)
      WRITE(NUT,702)
      WRITE(NUT,701)
C ------------------------------------------------------------------------
C  PROCESS INPUT DATA FILE !Subroutines Cuenca
C ------------------------------------------------------------------------
      KODE=1
      DO WHILE (KODE.NE.999)
        READ(NDAT,*,ERR=50) ZN1, ZN2, KODE ! Nodes Initial point (ZN) END point (ZN) and the KODE program used - KODE should be in file="CUENCA.NDAT"
        IF(KODE.NE.999)THEN
          CALL PROCESSES(m,n,PP,m1,n1,RAN,ZN1,ZN2,KODE,
     &    mn1,mn2,mn3)     
	    WRITE(NUT,601)
	    WRITE(NUT,600)ZN1, ZN2, KODE
	    WRITE(NUT,601)	
          WRITE(NUT,701)
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
C ------------------------------------------------------------------------------
      CLOSE(NDAT) ! close "CUENCA.NDAT" file
      CLOSE(NUT)  ! close "CUENCA.ANS" file
      CLOSE(RAN)  ! close "CUENCA.RAN" file
!      CLOSE(SSS) ! close "CUENCA.SSS" file
C ------------------------------------------------------------------------------      
600   FORMAT(3X,'FLOW PROCESS FROM NODE ',F10.2,' TO NODE ',F8.2,
     C' IS CODE = ',I3)
601   FORMAT(1X,76('*'))
602   FORMAT(1X,'*** FATAL READING ERROR - CHECK DATA INPUT ***')
701   FORMAT(1X,76('='))
702   FORMAT(19X,'C U E N C A   R O U T I N G   A N A L Y S I S')
c703   FORMAT(1X,76(':'))
      STOP
      END 
C  ----------------------------------------------------------------------------    
C   SUBROUTINE PROCESSES - ! PROGRAM 15  - Based CUENCA
C  ----------------------------------------------------------------------------
       SUBROUTINE PROCESSES(m,n,PP,m1,n1,RAN,ZN1,ZN2,KODE,
     & mn1,mn2,mn3)
C ------------------------------------------------------------------------
C   DECLARE VARIABLES
C ------------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (a-h, o-z)
      COMMON/BLK1/SS(600,10),SS1(600,10),Hydro(600,3)
      COMMON/NDAT/NDAT
C  ----------------------------------------------------------------------------
C   INITIALIZE CUENCA KODE PROCESSES 
C  ----------------------------------------------------------------------------
      IF(KODE.EQ.1)CALL unith(m,n,PP,m1,n1,RAN,ZN1,ZN2,KODE,
     C mn1,mn2)
      IF(KODE.EQ.2)CALL flowby(m,n,mn1,mn2) 
      IF(KODE.EQ.3)CALL fthru(m,n,mn1,mn2,mn3)
      IF(KODE.EQ.4)CALL piper(m,n,mn1,mn2)
      IF(KODE.EQ.5)CALL convex(m,n,mn1,mn2)
      IF(KODE.EQ.6)CALL clear(m,n,mn1,mn2)
      IF(KODE.EQ.7)CALL add(m,n,mn1,mn2)
      IF(KODE.EQ.8)CALL split(m,n,mn1,mn2)
      IF(KODE.EQ.9)CALL move(m,n,mn1,mn2)
!      IF(KODE.EQ.10)CALL hydrog(m,n,mn1,mn2)   
!      IF(KODE.EQ.11)CALL pdmc(SS2,m1,n1,m2)                     
!      IF(KODE.EQ.12)CALL uhcn(m,n,PP,m1,n1,mn,Hydro1,mn1,mn2)
C  ----------------------------------------------------------------------------
c701   FORMAT(1X,76('='))
c702   FORMAT(19X,'C U E N C A   R O U T I N G   A N A L Y S I S')
c703   FORMAT(1X,76(':'))
C  ----------------------------------------------------------------------------
      END SUBROUTINE PROCESSES
	  
!END PROGRAM CUENCA
