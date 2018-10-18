C     PROGRAM 14 - Based on Hromadka book pag 167
C ------------------------------------------------------------------------------------------
      SUBROUTINE OASB (NUT,KTYPE,H,INTERV,XMAX,UNIT,SUM,TIME1,TIME2)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C INTEGER LETM,BLANK,DOT,CROSS,DASH,LINE(41)                             C
C To compile with gfortran is necesary to declare these variables as     C
C CHARACTER instead of INTEGER                                           C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      IMPLICIT DOUBLE PRECISION (a-h, o-z)
      DIMENSION H(440)
	CHARACTER*1 LETM,BLANK,DOT,CROSS,DASH,LINE(41) !this line is different from the book
      DATA LETM,BLANK,DOT,CROSS,DASH,LINE/'V',' ','.','Q','I',41*' '/
C ------------------------------------------------------------------------------------------
c rmc 5050  FORMAT (1X,A1)
      WRITE(NUT,101)
      WRITE(NUT,130)
      WRITE(NUT,101)
      WRITE(NUT,10)
10    FORMAT(/,32X,' 24 - HOUR STORM',/,
     C 32X,'RUNOFF HYDROGRAPH',/)
c rmc 100   CONTINUE
      WRITE(NUT,101)
      WRITE(NUT,130)
101   FORMAT(1X,76('*'))
102   FORMAT(1X,76('-'))
130   FORMAT(1X,76('='))   
      STEP=5.d0*60.d0/43560.d0 
      WRITE(NUT,103)
103   FORMAT(/,19X, 'HYDROGRAPH IN FIVE-MINUTE INTERVALS (CFS)',/)
      WRITE(NUT,130)
      F=40.d0/XMAX
      FMASS=40.d0/SUM
      T0=0.0d0
      T1=XMAX/4.d0
      T2=2.d0*T1
      T3=3.d0*T1
      WRITE(NUT,105)T0,T1,T2,T3,XMAX
105   FORMAT(3X,'TIME(HRS)  VOLUME(AF)  Q(CFS)  ',F3.0,1X,4F10.1)
      WRITE(NUT,130)
      XMASS=0.d0
C ------------------------------------------------------------------------------------------
      GO TO(141,142,143,146,147,148),KTYPE 
141   KNUM=1
      GO TO 144
142   KNUM=2
      GO TO 144 
143   KNUM=3
      GO TO 144
146   KNUM=4
      GO TO 144
147   KNUM=6    
      GO TO 144
148   KNUM=12
144   TIME=0
C -------------------------------------------------------------------------------------------
C     Output Graph Loop
C -------------------------------------------------------------------------------------------
      DO 200 I=1, INTERV
      TEST=H(I)*F
c	print*,'H(I),F,TEST=',H(I),F,TEST
      XMASS=XMASS+H(I)*STEP
      LINE(1)=DOT
      LINE(11)=DOT
      LINE(21)=DOT
      LINE(31)=DOT
      LINE(41)=DOT
      J=TEST+1
      JMASS=XMASS*FMASS+1
c	print*,'J,JMASS=',J,JMASS
      LINE(JMASS)=LETM
      LINE(J)=CROSS
      IF(KNUM.EQ.1) GO TO 400
c rmc 320   DO 350 K=1, KNUM
      DO 350 K=1, KNUM
      TIME=TIME+.083333d0 
      IF(K.EQ.1) GO TO 349
      XMASS=XMASS+H(I)*STEP
      JMASS=XMASS*FMASS+1
      LINE(JMASS)=LETM
      LINE(J)=CROSS
349   IF(TIME.GE.TIME1.AND.TIME.LE.TIME2) WRITE(NUT,210)TIME,XMASS,
     C H(I),LINE
350   LINE(JMASS)=BLANK
      GO TO 215
400   CONTINUE
      TIME=TIME+.083333d0
      IF(TIME.GE.TIME1.AND.TIME.LE.TIME2) WRITE(NUT,210)TIME,XMASS,
     C H(I),LINE
210   FORMAT(3X,F7.3,F12.4,1X,F9.2,2X,41A1) 
215   LINE(J)=BLANK
      LINE(JMASS)=BLANK
200   CONTINUE
C -------------------------------------------------------------------------------------------
      WRITE(NUT,130)
!      WRITE(NUT,989)
989   FORMAT(/)
C -------------------------------------------------------------------------------------------
      RETURN
      END SUBROUTINE OASB
