C ----------------------------------------------------------------------------
!     PROGRAM 18  -  Based on Hromadka book pag 222
C ----------------------------------------------------------------------------
      SUBROUTINE SPLIT(m,n,mn1,mn2)   ! ARGU = NDAT (9.25.17)
C ----------------------------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C THIS SUBROUTINE SPLITS STREAM "A" INTO STREAM "A" AND STREAM "B"           C
C VARIABLES:                                                                 C
C NA:  Stream "A" number. This stream is the one to be modeled               C				
C NB:  Stream "B" number [0 for moving the excess flow from stream "A"       C
C      to a permanent storage; 1 for moving excess flow from                 C
C 	   stream "A" to stream "B"].								           C
C PB:  Percentage (decimal) of stream to be diverted	                       C					
C TIME1:  Time for Beginning of results (hrs)			                       C					
C TIME2:  Time for End of results (hrs)                                      C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC       
C ----------------------------------------------------------------------------
C   DECLARE VARIABLES
C ---------------------------------------------------------------------------- 
      IMPLICIT DOUBLE PRECISION (a-h, o-z)
      DIMENSION A(600),B(600)
      !COMMON/BLK1/SS(600,10),SS1(600,10),Hydro(600,3)
      COMMON/NUT/NUT
      COMMON/NDAT/NDAT
!     EXPORT Hydrograph, NDATe (hours) StreamA(CFS)
C ----------------------------------------------------------------------------
!      SS=SS1
C ----------------------------------------------------------------------------
C   INITIALIZE VARIABLES
C ----------------------------------------------------------------------------
      TIME=0.d0
C ----------------------------------------------------------------------------
C   READ INPUT DATA
C ----------------------------------------------------------------------------
      READ(NDAT,*)NA,NB,PB,TIME1,TIME2 !(9.25.17)
      NOUT1=INT(TIME1*12.d0+.01d0)
      NOUT2=INT(TIME2*12.d0+.01d0)
C ----------------------------------------------------------------------------
      WRITE(NUT,901)NA,NB
901   FORMAT(/,10X,'MODEL STREAM SPLITFLOW WHERE A CONSTANT PROPORTION'
     C ,/,10X,'OF STREAM',I2,' IS ADDED TO STREAM',I2,' :',//)
C ----------------------------------------------------------------------------
C   GRAPHICS
C ----------------------------------------------------------------------------
      PA=1.d0-PB
C ----------------------------------------------------------------------------
      WRITE(NUT,921)NB,NA,PB,NA
921   FORMAT(                              
     C 20X,'    INFLOW               INFLOW',/,
     C 20X,'  (STREAM:',I2,')          (STREAM:',I2,')' ,/,
     C 27X,'|',19X,'|',/,27X,'|',19X,'|',/,
     C 27X,'| (',F4.3,')(STREAM:',I2,') |')
      WRITE(NUT,923)NB,PA,NA,PB,NA
923   FORMAT (                         
     C 27X,'|<------------------* < = splitFLOW Model',/,
     C 27X,'|',19X,'|',/,27X,'|',19X,'|',/,
     C 27X,'|',19X,'|',/,
     C 27X,'V',19X,'V',/,24X,'STREAM:',I2,12X,'(',F6.3,')(STREAM:',I2,
     C')',/,20X,'+ (',F6.3,')(STREAM:',I2,')',//) 
      WRITE(NUT,903)NA,NB,PA,NA,PB,NB
903   FORMAT(11x,'STREAM NUMBER:',I2,' IS SPLIT TOWARDS STREAM:',I2,/,
     C 11X,'WHERE',F6.2,' (DECIMAL PERCENT) REMAINS IN STREAM: ',I2,/,
     C 11X,'AND ',F6.2,'  (DECIMAL PERCENT) IS ADDED TO STREAM:',I2)
      WRITE(NUT,905)NB,NA,NB,NA
905   FORMAT(//,22X,'STREAM SPLITFLOW MODELING RESULTS:',//,
     C 11X,' MODEL    INFLOW      INFLOW      OUTFLOW     OUTFLOW',/,
     C 11X,' TIME    STREAM', I2,3 (4X,  'STREAM',I2),/,
     C 11X,' (HRS)     (CFS)       (CFS)      (CFS)        (CFS)')
C ----------------------------------------------------------------------------
C READ IN STREAM DATA
C ----------------------------------------------------------------------------
      CALL MREAD(NA,A)
      CALL MREAD(NB,B)
C ----------------------------------------------------------------------------
C   MODEL SPLITFLOW
C ----------------------------------------------------------------------------
      NUMBER=INT(A(600))
      IF(PB.EQ.0.)GO TO 1000
      DO 100 I=1,NUMBER
      X=PB*A(I)
      AIN=A(I)
      BIN=B(I)
      B(I)=B(I)+X
      A(I)=A(I)-X
      TIME=TIME+.08333d0
      IF(I.LT.NOUT1.OR.I.GT.NOUT2)GO TO 100
C ----------------------------------------------------------------------------
      WRITE(NUT,906)TIME,BIN,AIN,B(I),A(I)
906   FORMAT(10X,F7.3,3X,4(F8.1,4X))
100   CONTINUE
1000  CONTINUE
      NUMB=INT(B(600))
      IF(NUMBER.GT.NUMB)NUMB=NUMBER
      B(600)=NUMB
      CALL MWRITE(NA,A)
      DO 1100 I=1,600
1100  A(I)=B(I)
      CALL MWRITE(NB,B)
C ----------------------------------------------------------------------------
C   HYDROGRAPH TO EXPORT
C ----------------------------------------------------------------------------
!      Hydro(:,2)=A*(0.3048d0**3)!To obtain in m^3/S
!      Hydro(600,2)=0
!      DO 715 I=1,600
!      WRITE(*,*) A(I)
!715   CONTINUE
!      Hydro(:,3)=B*(0.3048d0**3)!To obtain in m^3/S
!      Hydro(600,3)=0
!      DO 716 I=1,NUMB
!      DO 716 I=1,mn1
!      IF(I==1) THEN 
!      Hydro(I,1)=0.083333d0     
!      Hydro(I,2)=A(I)
!      Hydro(I,3)=B(I)
!      ELSE
!      J=I-1
!      Hydro(I,1)=Hydro(J,1)+0.083333d0
!      Hydro(I,2)=A(I)
!      Hydro(I,3)=B(I)
!      END IF
!      WRITE(*,*) Hydro(I,1), Hydro(I,2), Hydro(I,3)
!716   CONTINUE 
         
!      SS1=SS
      RETURN
      END SUBROUTINE SPLIT
