C   PROGRAM MAIN - Marco Pazmiño-Hernandez, Rafael Muñoz-Carpena, Greg Kiker
C -------------------------------------------------------------------------
      PROGRAM CUENCA
C ------------------------------------------------------------------------
C     MAIN DRIVER FOR CUENCA ROUTING PROGRAM     
      DIMENSION A(600)
      CHARACTER(LEN=35) :: CCC     
      COMMON/BLK1/SS(600,10)
c      DIMENSION SS(600,10)
      !INTEGER m,n,m1,n1,mn1,mn2
      
C ------------------------------------------------------------------------
C   Opening the files
C ------------------------------------------------------------------------     
      OPEN(5,file="CUENCA.DAT")
      OPEN(6,file="CUENCA.ANS")
      OPEN(7,file="SS.txt")
      !OPEN(9,file="par.txt") ! values of m,n,m1,n1,mn1,mn2 .txt file
      
C------------------------------------------------------------------------
C  INITIALIZE VARIABLES
C------------------------------------------------------------------------
      NUT=6  ! (file="CUENCA.ANS")
      NDAT=5 ! (file="CUENCA.DAT")
      par=9  !(file="par.txt")
      !READ(par,*)m,n,m1,n1,mn1,mn2

C------------------------------------------------------------------------
C  INITIALIZE STREAM DATA BANKS TO ZERO
C------------------------------------------------------------------------
      DO 10 I=1,600
10    A(I)=0.
      CALL MWRITE(1,A)
      CALL MWRITE(2,A)
      CALL MWRITE(3,A)
      CALL MWRITE(4,A)
      CALL MWRITE(5,A)

	  WRITE(NUT,701)
	  WRITE(NUT,702)
	  WRITE(NUT,701)

C--------------------------------------------------------------------------
C  PROCESS DATA FILE
C--------------------------------------------------------------------------
	  DO WHILE (KODE.NE.999)
      	READ(NDAT,*,ERR=50) ZN1, ZN2, KODE ! Nodes Initial point (ZN) end point (ZN) and the KODE program used - KODE should be in file="CUENCA.ANS"
	  	IF(KODE.NE.999)THEN
	  			CALL Processes(KODE,m,n,m1,n1,mn1,mn2)
			  	WRITE(NUT,601)
	  			WRITE(NUT,600)ZN1, ZN2, KODE
	  			WRITE(NUT,601)	!601 (format) 	
	  		ELSE 
	  			write(*,*) 'END OF CUENCA ROUTING ANALYSIS'
	  			Do 1200 I=1, 600
       				write (7,*) SS(I,1),SS(I,2),SS(I,3),SS(I,4),SS(I,5)
1200  			continue   
	  			STOP
	  	ENDIF
      END DO
50    WRITE(NUT,602)

1000  CONTINUE

      CLOSE(5)! close "CUENCA.DAT" file
      CLOSE(6)! close "CUENCA.ANS" file
      !CLOSE(7)! close "SS.txt" file
      !CLOSE(9)! close "par.txt" file
      
      
600   FORMAT(3X,'FLOW PROCESS FROM NODE ',F8.2,' TO NODE ',F8.2,
     C' IS CODE = ',I3)
601   FORMAT(1X,76('*'))
602   FORMAT(1X,'***FATAL READING ERROR - CHECK DATA INPUT***')
701   FORMAT(4('===================') )
702   FORMAT(15X,'F L O O D   R O U T I N G   A N A L Y S I S')
703   FORMAT(4(':::::::::::::::::::'))

      STOP
      END 

C -----------------------------------------------------------------------------    
C SUBROUTINE PROCESSES - ! PROGRAM 15  - Based CUENCA on Hromadka book pag 197
C  ----------------------------------------------------------------------------
      SUBROUTINE PROCESSES(KODE,m,n,m1,n1,mn1,mn2,mn3) ! Processes SUBROUTINE  
C  ----------------------------------------------------------------------------
      DIMENSION A(600)
      CHARACTER(LEN=35) :: CCC     
      COMMON/BLK1/SS(600,10)
      
C  ----------------------------------------------------------------------      
      REAL(8),DIMENSION(m,n) :: SS1
      INTEGER,VALUE :: m
      INTEGER,VALUE :: n
      REAL(8),DIMENSION(mn1,mn2) :: Hydro  ! it was 'Hydro1'
      INTEGER,VALUE :: mn1
      INTEGER,VALUE :: mn2
      REAL(8),DIMENSION(m1,n1) :: PP
      INTEGER,VALUE :: m1
      INTEGER,VALUE :: n1
      
C  ----------------------------------------------------------------------
C  INITIALIZE VARIABLES
C  ----------------------------------------------------------------------
      
      NUT=6 ! (file="CUENCA.ANS")
      NDAT=5 ! (file="CUENCA.DAT")
      !par=9  !(file="par.txt") ! values of m,n,m1,n1,mn1,mn2 .txt file
      !
     !!  READ(NDAT,*)NA,KTYPE,XL,LCA,HH,XN,AREA,VSL,KODE1,BASCON,SLP,
     !!C R5,R30,R1,R3,R6,R24,SS,KSTORM,KSOIL,PV,PF,PM,PD,NUT,
     !!  !C R5,R30,R1,R3,R6,R24,KSOIL,PV,PF,PM,PD,
     !!C FX5,FX30,FX1,FX3,FX6,FX24,DAOPT,TIME1,TIME2
     !! 
     !!  AX=AREA
     !!  XLX=XL
     !!  XLCAX=XLCA
      
     !! CALL uhcn(SS1,m,n,PP,m1,n1,NDAT,Hydro1,mn1,mn2) 

C  PROCESS DATA FILE !Subroutines Cuenca
	  IF(KODE.EQ.1)CALL unith(SS1,m,n,PP,m1,n1,NDAT,Hydro,mn1,mn2)
	  IF(KODE.EQ.2)CALL flowby(SS1,m,n,NDAT,Hydro,mn1,mn2) !		_ = contain the same variables
        IF(KODE.EQ.3)CALL fthru(SS1,m,n,NDAT,Hydro,mn1,mn2,mn3)
        IF(KODE.EQ.4)CALL piper(SS1,m,n,NDAT,Hydro,mn1,mn2)
        IF(KODE.EQ.5)CALL convex(SS1,m,n,NDAT,Hydro,mn1,mn2)
	  IF(KODE.EQ.6)CALL clear(SS1,m,n,NDAT,Hydro,mn1,mn2)
	  IF(KODE.EQ.7)CALL add(SS1,m,n,NDAT,Hydro,mn1,mn2)
	  IF(KODE.EQ.8)CALL split(SS1,m,n,NDAT,Hydro,mn1,mn2)
	  IF(KODE.EQ.9)CALL move(SS1,m,n,NDAT,Hydro,mn1,mn2)
	  !IF(KODE.EQ.10)CALL hydrog(SS1,m,n,NDAT,Hydro,mn1,mn2)   
	  !IF(KODE.EQ.11)CALL pdmc(SS2,m1,n1,NDAT,m2)                     
	  !IF(KODE.EQ.12)CALL uhcn(SS1,m,n,PP,m1,n1,NDAT,mn,Hydro1,mn1,mn2)
	  
      RETURN
      END SUBROUTINE Processes
	  
!END PROGRAM CUENCA
