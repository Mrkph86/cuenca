C    PROGRAM 13 - Based on Hromadka book pag 164
C  ----------------------------------------------------------------------
!      SUBROUTINE   SUBSB(TIMLAG,PERCNT,KODE1,NUMBER,NUT)
      SUBROUTINE   SUBSB(TIMLAG,PERCNT,KODE1,NUMBER)
C -----------------------------------------------------------------------
C   THIS SUBROUTINE DETERMINES THE PERCENTAGES OF DISCHARGE
C   FACTORS FOR THE VARIOUS ZONE CLASSIFICATIONS.
C   RFAT VMNT
C -----------------------------------------------------------------------
C   DECLARE VARIABLES
C -----------------------------------------------------------------------
	IMPLICIT DOUBLE PRECISION (a-h, o-z)
      DIMENSION PERCNT(150),VAL(31,2),FOOT(31,2),VMNT(33,2),SCS(33,2)
      DIMENSION DESERT(33,2)
      DATA VAL/0.,15.0,25.,35.0,50.,65.0,75.,100.,115.,125.,
     C 140.0,150.0,165.0,175.0,200.0,225.0,250.,275.,300.,325.,350.,
     C 375.,400.,450.,500.,550.,600.,650.,700.,750.,99999.,0.0,2.6,
     C 5.0,8.6,15.5,25.0,32.0,50.0,57.9,62.0,66.8,69.5,72.6,74.3,
     C 78.0,81.0,83.5,85.7,87.5,89.0,90.5,91.6,92.7,94.3,95.8,
     C 96.9,97.8,98.5,99.0,99.5,100. /
      DATA FOOT/0.,15.0,25.,35.0,50.,60.0,75.,85.0,90.0,95.0,
     C 100.0,110.0,125.0,140.0,150.0,175.0,200.0,225.0,250.0,
     C 0275.0,300.0,325.0,350.0,375.0,400.0,450.0,500.0,550.,0,
     C 600.0,650.0,99999.,0.0,1.9,3.8,6.0,10.3,14.0,21.7,29.0,
     C 34.2,43.3,50.0,56.9,63.8,69.0,71.9,77.8,82.4,86.0,89.0,
     C 91.4,93.4,95.0,96.2,97.2,97.9,98.5,99.0,99.3,99.7,
     C 99 9 100  /
      DATA VMNT/0.,15.0,25.,35.0,40.0,50.,65.0,75.0,90.0,
     C 100.0,115.0,125.0,140.0,150.0,175.0,200.0,225.0,250.0,
     C 275.0,300.0,325.0,350.0,375.0,400.0,450.0,500.0,
     C 550.0,600.0,650.0,700.0,750.0,800.0,99999.0,0.0,
     C 3.3,6.7,10.6,13.4,21.0,33.0,39.3,46.3,50.0,54.2,
     C 56.7,59.8,61.8,65.8,69.2,72.2,74.8,77.0,79.0,80.7,
     C 82.2,83.5,84.8,86.9,88.9,90.5,92.0,93.3,94.5,95.5,
     C 96.4,100./
      DATA DESERT/0.,12.5,25.0,37.5,50.0,62.5,75.0,87.5,100.,
     C 112.5,125.,137.5,150.,162.5,175.,187.5,200.,225.,250.,275.,
     C 300.,325.,350.,375.,400.,450.,500.,550.,600.,700.,800.,1000.,
     C 9999.,0.,1.1,3.2,6.3,10.5,18.5,31.3,42.0,50.0,56.5,61.3,65.2,
     C 68.5,71.5,74.0,76.2,78.3,81.6,84.3,86.7,88.7,90.2,91.6,92.8,
     C 93.9,95.6,96.9,97.8,98.3,99.5,99.9,99.99,100./
      DATA SCS/0.,8.6,17.2,25.9,34.5,43.1,51.7,60.3,69.,77.6,
     C 86.2,94.8,103.4,112.1,120.7,129.3,137.9,146.5,155.2,
     C 163.8,172 4,189.6,206.9,224 1,241.4,258.6,275.8,293.1,
     C 310.3,327 6,344.8,387.9,999.,0.,.1,.6,1.2,3.5,6.5,10.7,
     C 16.3,22.8,30.,37.5,45.,52.2,58.9,65.,70.,75.1,79.,82.2,
     C 84.9,87.1,90.8,93.4,95.3,96 7,97.7,98.4,98.9,99.3,99.5,99.7,
     C 99.9,100./
C ----------------------------------------------------------------------
c      KODE1=1:  VALLEY
c      KODE1=2:  FOOTHILL
c      KODE1=3:  MOUNTAIN
c      KODE1=4:  DESERT
c      KODE1=5:  NOT DIRECTLY USED. LINEAR INTERPOLATION OF 1-4
c      KODE1=6:  SCS
c      TIMLAG=LAG
c      TIME=INCREMEMTS OF LAG
c      NUMBER=INDEX OF PERCNT VECTOR
C ----------------------------------------------------------------------
      ANEW=0.D0
      AOLD=0.D0
      K=1
      NUMBER=1
      TIME=TIMLAG
10    K=K+1
      IF(NUMBER.GE.151)GO TO 1000
      N=K-1
C
      GO TO(100,200,300,350,355,355),KODE1
C
100   CONTINUE
C -----------------------------------------------------------------------
C   INTEGRATE "S" GRAPH IN ORDER TO DETERMINE UH(I)
C -----------------------------------------------------------------------

      TEMP=0.5*(VAL(K,2)+VAL(N,2))*
     C(VAL(K,1)-VAL(N,1))
      ANEW=ANEW+TEMP
      IF(TIME.GT.VAL(K,1))GO TO 10
      Y=VAL(K,2)
      X=VAL(N,2)
      DEL=(TIME-VAL(N,1))/(VAL(K,1)-VAL(N,1))
      B=VAL(K,1)-VAL(N,1)
      GO TO 400
200   CONTINUE
      TEMP=0.5*(FOOT(K,2)+FOOT(N,2))*
     C(FOOT(K,1)-FOOT(N,1))
      ANEW=ANEW+TEMP
      IF(TIME.GT.FOOT(K,1))GO TO 10
C
      Y=FOOT(K,2)
      X=FOOT(N,2)
      DEL=(TIME-FOOT(N,1))/(FOOT(K,1)-FOOT(N,1))
      B=FOOT(K,1)-FOOT(N,1)
      GO TO 400
300   CONTINUE
      TEMP=0.5*(VMNT(K,2)+VMNT(N,2))*
     C(VMNT(K,1)-VMNT(N,1))
      ANEW=ANEW+TEMP
      IF(TIME.GT.VMNT(K,1))GO TO 10
      Y=VMNT(K,2)
      X=VMNT(N,2)
      DEL=(TIME-VMNT(N,1))/(VMNT(K,1)-VMNT(N,1))
      B=VMNT(K,1)-VMNT(N,1)
      GO TO 400
350   CONTINUE
      TEMP=0.5*(DESERT(K,2)+DESERT(N,2))*
     C(DESERT(K,1)-DESERT(N,1))
      ANEW=ANEW+TEMP
      IF(TIME.GT.DESERT(K,1))GO TO 10
      Y=DESERT(K,2)
      X=DESERT(N,2)
      DEL=(TIME-DESERT(N,1))/(DESERT(K,1)-DESERT(N,1))
      B=DESERT(K,1)-DESERT(N,1)
      GO TO 400
C  ----------------------------------------------------------------------
C  SCS METHOD
C  ----------------------------------------------------------------------
355   TEMP=0.5*(SCS(K,2)+SCS(N,2))*(SCS(K,1)-SCS(N,1))
      ANEW=ANEW+TEMP
C
      IF(TIME.GT.SCS(K,1))GO TO 10
      Y=SCS(K,2)
      X=SCS(N,2)
      DEL=(TIME-SCS(N,1))/(SCS(K,1)-SCS(N,1))
      B=SCS(K,1)-SCS(N,1)
C
400   CONTINUE
      DEL=DEL*(Y-X)
      XX=X+DEL
C  ----------------------------------------------------------------------
C   ADJUST INTEGRATION FOR INTERPOLATION
C  ----------------------------------------------------------------------
      DELA=0.5*(Y+XX)*(1.-DEL/(Y-X))*B
      ANEW=ANEW-DELA
      PERCNT(NUMBER)=(ANEW-AOLD)/TIMLAG
      NUMBER=NUMBER+1
      AOLD=ANEW
      ANEW=ANEW-.5*(X+XX)*DEL/(Y-X)*B
      TIME=TIME+TIMLAG
      K=K-1
      IF(NUMBER.EQ.2)GO TO 10
      DELX=PERCNT(NUMBER-1)-PERCNT(NUMBER-2)
      IF(DELX.LE..51)GO TO 1000
      GO TO 10
1000  CONTINUE
      NUMBER=NUMBER-1
C ------------------------------------------------------------------------  
      IF(NUMBER.GE.150) WRITE(NUT,1001)NUMBER
1001  FORMAT(8X,'UNIT - HYDROGRAPH TERMINATED AFTER',I4,' INTERVALS')
C  ----------------------------------------------------------------------
C    FINISH-OFF HYDROGRAPH
C  ----------------------------------------------------------------------
      IF(NUMBER.GE.150)GO TO 1250
      NNUM=150-NUMBER
      XNUM=NNUM
      REM=100.-PERCNT(NUMBER)
      REM1=REM/XNUM
      DELX=.5
      IF(REM1.LT.DELX)GO TO 1150
      DO 1140 K=1,NNUM
      KTI=NUMBER+K
1140  PERCNT(KTI)=PERCNT(KTI-1)+REM1
      GO TO 1200
1150  XNUM=REM/DELX+1.
      NNUM=XNUM
      DO 1160 K=1,NNUM
      KTI=NUMBER+K
1160  PERCNT(KTI-1)=PERCNT(KTI-2)+DELX
      NNUM=NNUM-1
1200  NUMBER=NUMBER+NNUM
      IF(PERCNT(NUMBER).GE.100.)PERCNT(NUMBER)=100.
      IF(NUMBER.GE.100)GO TO 1250
      IF(PERCNT(NUMBER).GE.100.)GO TO 1250
      NUMBER=NUMBER+1
      PERCNT(NUMBER)=100.
1250  CONTINUE
C
      RETURN
      END SUBROUTINE SUBSB
