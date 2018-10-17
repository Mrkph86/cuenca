C     PROGRAM 14 - Based on Hromadka book pag 167
c -----------------------------------------------------------
      SUBROUTINE OASB (NUT,KTYPE,H,INTERV,XMAX,UNIT,SUM,TIME1,TIME2)
c -----------------------------------------------------------
! INTEGER LETM,BLANK,DOT,CROSS,DASH,LINE(41) 
! To compile with gfortran is necesary to declare these variables as
! CHARACTER instead of INTEGER
c -----------------------------------------------------------
      INTEGER LETM,BLANK,DOT,CROSS,DASH,LINE(41)
	  !CHARACTER*1 LETM,BLANK,DOT,CROSS,DASH,LINE(41) !this line is dIFferent from the book
      DIMENSION H(440)
c rmc       COMMON/NUT/NUT
      DATA LETM,BLANK,DOT,CROSS,DASH,LINE/'V',' ','.','Q','I',41*' '/
c
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
      STEP=5.*60./43560. 
      WRITE(NUT,103)
103   FORMAT(/,19X, 'HYDROGRAPH IN FIVE-MINUTE INTERVALS (CFS)',/)
      WRITE(NUT,130)
      F=40./XMAX
      FMASS=40./SUM
      T0=0.0
      T1=XMAX/4.
      T2=2.*T1
      T3=3.*T1
      WRITE(NUT,105)T0,T1,T2,T3,XMAX
105   FORMAT(3X,'TIME(HRS)  VOLUME(AF)  Q(CFS)  ',F3.0,1X,4F10.1)
      WRITE(NUT,130)
      XMASS=0.
c
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
C -----------------------------------------------------------------
C     Output Graph Loop
C -----------------------------------------------------------------
      DO 200 I=1, INTERV
      TEST=H(I)*F
      XMASS=XMASS+H(I)*STEP
      LINE(1)=DOT
      LINE(11)=DOT
      LINE(21)=DOT
      LINE(31)=DOT
      LINE(41)=DOT
      J=TEST+1
      JMASS=XMASS*FMASS+1
      LINE(JMASS)=LETM
      LINE(J)=CROSS
      IF(KNUM.EQ.1) GO TO 400
c rmc 320   DO 350 K=1, KNUM
      DO 350 K=1, KNUM
      TIME=TIME+.083333 
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
      TIME=TIME+.083333
      IF(TIME.GE.TIME1.AND.TIME.LE.TIME2) WRITE(NUT,210)TIME,XMASS,
     C H(I),LINE
210   FORMAT(3X,F7.3,F12.4,1X,F9.2,2X,41A1) 
215   LINE(J)=BLANK
      LINE(JMASS)=BLANK
200   CONTINUE
!Now We are not going to WRITE the Flood.ans----------------------------
      WRITE(NUT,130)
!      WRITE(NUT,989)
989   FORMAT(/)
!--END----!Now We are not going to WRITE the Flood.ans--------------------
      RETURN
      END SUBROUTINE OASB
