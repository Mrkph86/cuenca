! SUBROUTINES of nine processes considerer in (Hromadka et al., 1983)
! Each of one is CALLed from java, and at this moment the SS array is 
! RETURNed (UF, 9/25/2017- Marco Pazmino-Hernandez modIFied) 
C  ----------------------------------------------------------------------     
! Program 12 -  Based on Hromadka book pag 157
C  ----------------------------------------------------------------------
      SUBROUTINE unith (NUT,NDAT)   ! (1.26.18 - following the book code)
      !SUBROUTINE unith(SS1,m,n,PP,m1,n1,NDAT,Hydro,mn1,mn2) ! ARGU = NDAT (9.25.17)
!      SUBROUTINE UNITH(S1, m,n, ARGU, mn)
!------------------------------------------------------------------------
      ! REAL(8),DIMENSION(m,n) :: SS1
      ! INTEGER,VALUE :: m
      ! INTEGER,VALUE :: n
      ! INTEGER,VALUE :: mn  ! (9.27.17) mn 
      ! REAL(8),DIMENSION(mn1,mn2) :: Hydro
      ! INTEGER,VALUE :: mn1  
      ! INTEGER,VALUE :: mn2
      ! REAL(8),DIMENSION(m1,n1) :: PP
      ! INTEGER,VALUE :: m1
      ! INTEGER,VALUE :: n1
      !Hydro RETURNs the hydrograph, Time(Hours) and Discharge(CFS)
!------------------------------------------------------------------------
      
!      COMMON/BLK1/SS(600,10)
      DIMENSION H(440)
      DIMENSION UH(150),PERCNT(150),R(288)
      DIMENSION DYR(48)
      DATA DYR/0.,20.,40.,60.,80.,100.,125.,150.,100.,89.4,82.3,78.,
     C 74.8,72.4,70.2,68.5,100.,94.8,91.2,88.4,86.3,84.5,82.7,81.3,
     C 100.,96.2,93.8,91.8,90.3,89.,87.8,86.7,100.,97.4,95.8,94.8,
     C 93.9,93.2,92.8,92.3,100.,82.,72.5,66.6,63.,60.8,59.,57.5/
      DATA UH/150*0./
      DATA PERCNT/150*0./
      DATA R/288*0./
      DATA H/440*0./
      
C Setting H to zero
C FUNCTION
      RR(A,B,T)=EXP((A*ALOG(T))+B)
C..READ DATA INPUT
C...OPO...NA was added to make references to the number of stream to be simmulated
      READ(NDAT,*)NA,KTYPE,XL,XLCA,HH,XN,AREA,VSL,KODE1,BASCON,SLP,
     C  R5,R30,R1,R3,R6,R24,KSOIL,PV,PF,PM,PD,
     C  FX5,FX30,FX1,FX3,FX6,FX24,DAOPT,TIME1,TIME2

c 02/01/18      READ(5,*)NA,KTYPE,XL,XLCA,HH,XN,AREA,VSL,KODE1,BASCON,SLP,                                !"5" should be NDAT
c 02/01/18 M.P. - the program breaks in this point IF I elimiate the initial varialbles
c 02/01/18      READ(NDAT,*)KTYPE,XL,XLCA,HH,XN,AREA,VSL,KODE1,BASCON,SLP,  
c 02/01/18 R5,R30,R1,R3,R6,R24,SS,KSTORM,KSOIL,PV,PF,PM,PD,NUT,
c 02/01/18 M.P. -Added SS,KSTORM and NUT
c 02/01/18     C R5,R30,R1,R3,R6,R24,SS,KSTORM,KSOIL,PV,PF,PM,PD,NUT !Added SS,KSTORM c02/01/18and NUT
c 02/01/18     C FX5,FX30,FX1,FX3,FX6,FX24,DAOPT,TIME1,TIME2
!      SS=SS1 ! M.P. - the program breaks in this point 
      AX=AREA
      XLX=XL
      XLCAX=XLCA
C OPO This line was added to clear the hydrograph H
C before the next hydrograph in another node.
      DO 15 I=1,440 
      H(I)=0
15    CONTINUE 
c rmc FX=1.
      IF(DAOPT.EQ.2)GO TO 183
      
      AREA=AREA/640.
      FX1=.651
      FX3=.78
      FX6=.831
      FX24=.91
      IF(AREA.GE.350.)GO TO 180
      DO 100 I=1,13
      IF(AREA.LE.DYR(I))GO TO 110
100   CONTINUE
C
110   I=I-1
      DX=DYR(I+1)-DYR(I)
      FACT=AREA-DYR(I)
      FX1=(DYR(I+8)-FACT*(DYR(I+8)-DYR(I+9))/DX)/100.
      FX3=(DYR(I+16)-FACT*(DYR(I+16)-DYR (I+17))/DX)/100.
      FX6=(DYR(I+24)-FACT*(DYR(I+24)-DYR(I+25))/DX)/100.
      FX24=(DYR(I+32)-FACT*(DYR(I+32)-DYR(I+33))/DX)/100.
      FX30=(DYR(I+40)-FACT*(DYR(I+40)-DYR(I+41))/DX)/100.
180   CONTINUE
      AREA=AREA*640.
183   CONTINUE
!Now We are not going to WRITE the Flood.ans----------------------------
      WRITE(NUT,5000)
      WRITE(NUT,181)
181   FORMAT(/,28X,'UNIT-HYDROGRAPH ANALYSIS',/)
      WRITE(NUT,5000)
6666  FORMAT(/,11X,'USER SPECIFIED PRECIPITATION DEPTH-AREA',
     C  ' REDUCTION FACTORS:')                              
6665  FORMAT(/,11X,'PRECIPITATION DEPTH-AREA REDUCTION FACTORS:')
6664  FORMAT(14X,'5 - MINUTE FACTOR = ',F12.3,/,
     C 13X,'30 - MINUTE FACTOR = ',F12.3,/,
     C 14X,'1 -  HOUR  FACTOR = ',F12.3,/,14X,'3 -  HOUR  FACTOR = ' ,
     C F12.3,/,14X,'6 -  HOUR  FACTOR = ',F12.3,/,13X,'24 -  HOUR  FACTO
     CR = ',F12.3,/)
C ----------------------------------------------------------------------
C  DETERMINE HYDROGRAPH FACTORS
C ----------------------------------------------------------------------
      XL=XL/5280.
      XLCA=XLCA/5280.
      S=HH/XL
      XLAGX=1.2*(XL*XLCA/S**0.5)**0.38
      XLAG=20.0*XN*XLAGX
C -----------------------------------------------------------------------
C  DESIGNATE UNIT INTERVALS
C  UNIT in minutes
C ----------------------------------------------------------------------
      IF(KTYPE.EQ.1)UNIT=5.
      IF(KTYPE.EQ.2)UNIT=10.
      IF(KTYPE.EQ.3)UNIT=15.
      IF(KTYPE.EQ.4)UNIT=20.
      IF(KTYPE.EQ.5)UNIT=30.
      IF(KTYPE.EQ.6)UNIT=60.
C
      TIMLAG=100.*UNIT/60./XLAG
      SQMI=AREA*43560./5280./5280.
      BASFLO=BASCON*SQMI
      XK=645.*SQMI*60./UNIT
      XK=XK/100.
C ----------------------------------------------------------------------
C  RETURN XL AND XLCA TO UNITS OF FEET
C ----------------------------------------------------------------------
      XL=XL*5280.
      XLCA=XLCA*5280.
5000  FORMAT(1X,76('*'))
5010  FORMAT(1X,76('='))
!Now We are not going to WRITE the Flood.ans----------------------------
      WRITE(NUT,5010)
      WRITE(NUT,5001)XLX,XLCAX,HH,XN,AX,XLAG,UNIT,TIMLAG,
     C BASFLO,VSL,SLP                   
5001  FORMAT(11X,'WATERCOURSE LENGTH = ',F37.3,' FEET',/,
     C 11X,'LENGTH FROM CONCENTRATION POINT TO CENTROID = ',
     C F12.3,' FEET',/,11X,'ELEVATION VARIATION ALONG
     C WATERCOURSE = ',F18.3,' FEET',/,11X,'MANNINGS
     C FRICTION FACTOR ALONG WATERCOURSE = ',F13.3,/,
     C 11X,'WATERSHED AREA = ',F41.3,' ACRES',
     C /,11X,'WATERCOURSE "LAG" TIME = ',F33.3,' HOURS',/,
     C 11X,'UNIT HYDROGRAPH TIME UNIT = ',F30.3,' MINUTES',/,
     C 11X,'UNIT INTERVAL PERCENTAGE OF LAG-TIME = ',F19.3,/,
     C 11X,'HYDROGRAPH BASEFLOW = ',F36.3,' CFS',/,11X,
     C 'UNIFORM MEAN SOIL-LOSS(INCH/HOUR) = ',F22.3,/,
     C 11X,'LOW SOIL-LOSS RATE PERCENT(DECIMAL) = ',F20.3)
!Now We are not going to WRITE the Flood.ans----------------------------
      IF(KODE1.EQ.1)WRITE(NUT,8200)
      IF(KODE1.EQ.2)WRITE(NUT,8202)
      IF(KODE1.EQ.3)WRITE(NUT,8204)
      IF(KODE1.EQ.4)WRITE(NUT,8205)
      IF(KODE1.EQ.5)WRITE(NUT,82041)PV,PF,PM,PD
      IF(KODE1.EQ.6)WRITE(NUT,82042)
82041 FORMAT(11X,'VALLEY "S"-CURVE PERCENTAGE(DECIMAL NOTATION) = ',
     C F6.3,/,11X,'FOOTHILL "S"-CURVE PERCENTAGE(DECIMAL NOTATION) = '
     C ,F6.3,/,11X,'MOUNTAIN "S"-CURVE PERCENTAGE(DECIMAL NOTATION) = '
     C ,F6.3,/,11X,'DESERT "S"-CURVE PERCENTAGE(DECIMAL NOTATION) = ',
     C F6.3,/)                             
8205  FORMAT(11X,'DESERT S-GRAPH SELECTED',/)
8200  FORMAT(11X,'VALLEY S-GRAPH SELECTED',/)
8202  FORMAT(11X,'FOOTHILL S-GRAPH SELECTED',/)
8204  FORMAT(11X,'MOUNTAIN S-GRAPH SELECTED',/)
82042 FORMAT(10X,'U.S. SOIL CONSERVATION SERVICE S-GRAPH SELECTED',/)
!Now We are not going to WRITE the Floof.ans----------------------------
      WRITE(NUT,8209)R5,R30
8209  FORMAT(11X,'SPECIFIED PEAK  5-MINUTES RAINFALL(INCH) = ',F15.2,/,
     C 11X,'SPECIFIED PEAK 30-MINUTES RAINFALL(INCH) = ',F15.2)
!Now We are not going to WRITE the Floof.ans----------------------------
      WRITE(NUT,8206)R1,R3,R6,R24,KTYPE
8206  FORMAT(11X,'SPECIFIED PEAK  1-HOUR RAINFALL(INCH) = ',F18.2,/,
     C 11X,'SPECIFIED PEAK  3-HOUR RAINFALL(INCH) = ',F18.2,/,
     C 11X,'SPECIFIED PEAK  6-HOUR RAINFALL(INCH) = ',F18.2,/,
     C 11X,'SPECIFIED PEAK 24-HOUR RAINFALL(INCH) = ',F18.2,//,
     C 11X,'HYDROGRAPH MODEL #',11X,' SPECIFIED*')
!Now We are not going to WRITE the Floof.ans----------------------------
      IF(DAOPT.EQ.1)WRITE(NUT,6665)
      IF(DAOPT.EQ.2)WRITE(NUT,6666)
      WRITE(NUT,6664)FX5,FX30,FX1,FX3,FX6,FX24
      WRITE(NUT,5000)
C ----------------------------------------------------------------------
C  SOIL INFILTRATION EFFECTIVE LOSS RATE, XR
C ----------------------------------------------------------------------
      XR=VSL*UNIT/60.
C ----------------------------------------------------------------------
C  DETERMINATION OF UNIT HYDROGRAPH ORDINATES
C ----------------------------------------------------------------------
      IF(KODE1.LT.5 .OR. KODE1.EQ.6)GO TO 94
C ----------------------------------------------------------------------
C  LINEAR WEIGHTING OF S-CURVES
C ----------------------------------------------------------------------
      NLAG=0
      DO 98 KLAG=1,4
      PLAG=PV
      IF(KLAG.EQ.2)PLAG=PF
      IF(KLAG.EQ.3)PLAG=PM
      IF(KLAG.EQ.4)PLAG=PD
      IF(KLAG.EQ.0)GO TO 98
      CALL SUBSB(TIMLAG,PERCNT,KLAG,NUMBER)
      IF(NUMBER.GT.NLAG)NLAG=NUMBER
      DO 99 I=1,150
      IF (PERCNT(I).EQ.0.)PERCNT(I)=100.
      H(I)=H(I)+PERCNT(I)*PLAG
99    PERCNT(I)=0.
98    CONTINUE
C ---------------------------------------------------------------------
C  UNIT HYDROGRAPH DETERMINATION
C ----------------------------------------------------------------------
      NUMBER=NLAG
      DO 97 I=1,150
      IF(I.LE.NLAG)PERCNT(I)=H(I)
97    H(I)=0
      GO TO 96
94    CALL SUBSB(TIMLAG,PERCNT,KODE1,NUMBER)
!94    CALL SUBSB(TIMLAG,PERCNT,KODE1,NUMBER,NUT)
C
96    SUM=0.
!Now We are not going to WRITE the Floof.ans----------------------------
      IF(NUMBER.GE.150)WRITE(NUT,8207)
8207  FORMAT(4X,'UNIT HYDROGRAPH TERMINATED AFTER 150 UNIT INTERVALS',/)
      IF(NUMBER.GT.150)NUMBER=150
C
      DO 8208 I=1,NUMBER
      UH(I)=(PERCNT(I)-SUM)*XK
      SUM=PERCNT(I)
      IF(UH(I).LT.0.)UH(I)=0.
8208  CONTINUE
!Now We are not going to WRITE the Floof.ans----------------------------
      WRITE(NUT,5010)
      WRITE(NUT,5002)
      WRITE(NUT,5010)
      WRITE(NUT,5004)
      WRITE(NUT,5010)
      WRITE(NUT,5005)(I,PERCNT(I),UH(I),I=1,NUMBER)
      WRITE(NUT,5010)
5002  FORMAT(/,26X,'UNIT HYDROGRAPH DETERMINATION',/)
5003  FORMAT(1X,76('-'))
5004  FORMAT(6X,'INTERVAL',12X,'"S" GRAPH',12X,'UNIT HYDROGRAPH',/,
     C 7X,'NUMBER',12X,'MEAN VALUES',12X,'ORDINATES(CFS)')
5005  FORMAT(8X I3,15X,F7.3,13X,F10.3)

C 24-HOUR STORM RAINFALL PATTERN
      R5A=R5*FX5
      R30A=R30*FX30
      R1A=R1*FX1
      R3A=R3*FX3
      R6A=R6*FX6
      R24A=R24*FX24
C
      A=(ALOG(R30A)-ALOG(R5A))/(ALOG(.5)-ALOG(.0833))
      B=ALOG(R5A)-A*ALOG(.0833)
      R(193)=RR(A,B,.0833)
      R(194)=RR(A,B,.1667)-RR(A,B,.0833)
      R(195)=RR(A,B,.25)-RR(A,B,.1667)
      R(192)=RR(A,B,.3333)-RR(A,B,.25)
      R(196)=RR(A,B,.41667)-RR(A,B,.3333)
      R(191)=RR(A,B,.5)-RR(A,B,.41667)
      A=(ALOG(R1A)-ALOG(R30A))/(ALOG(1.)-ALOG(. 5))
      B=ALOG(R30A)-A*ALOG(.5)
      R(197)=RR(A,B,.5833)-RR(A,B,.5)
      R(190)=RR(A,B,.6667)-RR(A,B,.5833)
      R(198)=RR(A,B,.75)-RR(A,B,.6667)
      R(189)=RR(A,B,.8333)-RR(A,B,.75)
      R(188)=RR(A,B,.9167)-RR(A,B,.8333)
      R(187)=RR(A,B,1.)-RR(A,B,.9167)
C  REMAINING PART OF PEAK 3-HOUR STORM
      A=(ALOG(R3A)-ALOG(R1A))/(ALOG(3.)-ALOG(1.))
      B=ALOG(R1A)-A*ALOG(1.)
      RRSAVE=R1A
C
      DO 1001 J=1,12
      XJ=J
      DT=XJ*.1667
      T=1.+DT
      RRNEW=RR(A,B,T)
      DR=(RRNEW-RRSAVE)/2.
      R(J+198)=DR
      IR=187-J
      R(IR)=DR
1001  RRSAVE=RRNEW
C
C  REMAINING PART OF PEAK 6-HOUR STORM
      A=(ALOG(R6A)-ALOG(R3A))/(ALOG(6.)-ALOG(3.))
      B=ALOG(R3A)-A*ALOG(3.)
      RRSAVE=R3A
C
      DO 1010 J=1,18
      XJ=J
      DT=XJ*.1667
      T=3.+DT
      RRNEW=RR(A,B,T)
      DR=(RRNEW-RRSAVE)/2.
      R(J+210)=DR
      IR=175-J
      R(IR)=DR
1010  RRSAVE=RRNEW
C
C  REMAINING PART OF PEAK 24-HOUR STORM
      A=(ALOG(R24A)-ALOG(R6A))/(ALOG(24.)-ALOG(6.))
      B=ALOG(R6A)-A*ALOG(6.)
      RRSAVE=R6A
C
      DO 1020 J=1,60
      XJ=J
      DT=XJ*.1667
      T=6.+DT
      RRNEW=RR(A,B,T)
      DR=(RRNEW-RRSAVE)/2.
      R(J+228)=DR
      IR=157-J
      R(IR)=DR
1020  RRSAVE=RRNEW
C
      DO 1030 J=1,96
      XJ=J
      DT=XJ*.08333
      T=16.+DT
      RRNEW=RR(A,B,T)
      DR=RRNEW-RRSAVE
      IR=97-J
      R(IR)=DR
1030  RRSAVE=RRNEW       
C ----------------------------------------------------------------------
C  ADJUST R-ARRAY FOR LARGER UNIT INTERVALS
C ----------------------------------------------------------------------
C  NI = NUMBER OF STORM HYDROGRAPH INTERVALS
      NI=288
      IF(KTYPE.EQ.1)GO TO 1050
      K=KTYPE
      IF(KTYPE.EQ.5)K=6
      IF(KTYPE.EQ.6)K=12
      NI=288/K
      DO 1040 I=1,NI
      TEMP=0.
      II=(I-1)*K
      DO 1035 J=1,K
      IR=II+J
1035  TEMP=TEMP+R(IR)
1040  R(I)=TEMP
1050  CONTINUE
C     ADJUST FOR CONSTANT SOIL LOSS
      XTOTAL=0.
c rmc   TEMPS=UNIT/60.
      XRA=XR
!Now We are not going to WRITE the Floof.ans----------------------------  
      IF(KSOIL.EQ.1)WRITE(NUT,5000)
      IF(KSOIL.EQ.1)WRITE(NUT,5020)
      IF(KSOIL.EQ.1)WRITE(NUT,5003)
5020  FORMAT(11X,'UNIT',14X,'UNIT',12X,'UNIT',14X,'EFFECTIVE',/,
     C 10X,'PERIOD',11X,'RAINFALL',7X,'SOIL-LOSS',12X,'RAINFALL',/,
     C  9X,'(NUMBER)',10X,'(INCHES)',8X,'(INCHES)',12X,'(INCHES)')
      DO 300 I=1,NI
      XLOSS=R(I)*SLP
      IF(XLOSS.GT.XRA) XLOSS=XRA
      TEMP=R(I)
      R(I)=R(I)-XLOSS
      XTOTAL=XTOTAL+XLOSS
!Now We are not going to WRITE the Floof.ans----------------------------        
      IF(KSOIL.EQ.1)WRITE(NUT,5021)I,TEMP,XLOSS,R(I)
5021  FORMAT(11X,I3,12X,F7.4,10X,F7.4,13X,F7.4)
300   CONTINUE
!      DO 14 I=1,288
!      R(I)=PP(I,2)/25.4 !Change of unit, from mm to inch
!14    CONTINUE     
!      NI=PP(500,1)
C ----------------------------------------------------------------------
C DETERMINE STORM RUNOFF HYDROGRAPH
C ----------------------------------------------------------------------
      INTERV=NUMBER+NI-1
!Now We are not going to WRITE the Floof.ans----------------------------        
      IF(INTERV.GT.440)WRITE(NUT,432)
      IF(INTERV.GT.439)Then 
      INTERV=439
      WRITE(*,*) 'RUNOFF HYDROGRAPH TERMINATED AFTER 440 UNIT'
      WRITE(*,*) 'INTERVALS. SUGGEST USING A LARGER UNIT INTERVAL'
      END IF
432   FORMAT(4X,'RUNOFF HYDROGRAPH TERMINATED AFTER 440 UNIT',/,
     C 4X,'INTERVALS. SUGGEST USING A LARGER UNIT INTERVAL.')
        
      DO 600 I=1,INTERV
      M=I
      N=1+M
      DO 500 J=1,M
      K=N-J
      IF(J.GT.NI)GO TO 500
      IF(K.GT.NUMBER) GO TO 500
      H(M)=H(M)+R(J)*UH(K)
  500 CONTINUE
      H(M)=H(M)+BASFLO
  600 CONTINUE
C ----------------------------------------------------------------------
C COMPUTE SUMMED HYDROGRAPH
C ----------------------------------------------------------------------
      SUM=0.0
      XMAX=0.
      DO 700 I=1,INTERV
      IF(H(I).LT.0.)H(I)=0.
      IF(H(I).GT.XMAX)XMAX=H(I)
      SUM=SUM + H(I)
 700  CONTINUE
      SUM=SUM*UNIT*60./43560.
      XTOTAL=XTOTAL/12.*AREA
!Now We are not going to WRITE the Flood.ans----------------------------  
	WRITE(NUT,5003)
	WRITE(NUT,6008)XTOTAL,SUM
6008	FORMAT(6X,'TOTAL SOIL-LOSS VOLUME(ACRE-FEET) = ',F31.4,/,
     C 6X,'TOTAL STORM RUNOFF VOLUME(ACRE-FEET) = ',F28.4)
	IF(XMAX.LT.100.)GO TO 8000
	I=XMAX/100.
	II=I+1
	XMAX=II
	XMAX=XMAX*100.
	GO TO 8100
8000	I=XMAX/10.
	II=10*(I+1)
	XMAX=II
8100  CONTINUE
!Now We are not going to WRITE the Floof.ans----------------------------
	WRITE(NUT,5003)

C Pass hydrograph to STREAM flow MATRIX SS
      CALL ADDHY(UNIT,INTERV,NA,H)
      
C Print results and estimate mass balances         
      CALL OASB(NUT,KTYPE,H,INTERV,XMAX,UNIT,SUM,TIME1,TIME2)
!        CALL ADDHY(UNIT,INTERV,NA,H)
C ------------------------------------------------------------------------
C Hydrograph to export
C ------------------------------------------------------------------------
!Now is a main subroutine
!	  RETURN 
!      Hydro=H
!      WRITE (*,*) INTERV
!      WRITE (*,*) "Hydrograph UNITH"
!     DO 715 I=1,440
!      Hydro(I,2)=H(I)/(0.3048**3) !To obtain hydro in m^3/s
	  !Hydro(I,2)=H(I) !hydro in CFS
!      TIMEOUT=TIMEOUT+.083333
!      WRITE(*,*) TIMEOUT
!      WRITE(*,*) Hydro(I)
!715   CONTINUE     
!      DO 716 I=1,mn1
!          IF(I==1) THEN 
!              Hydro(I,1)=0.083333
!          ELSE
!              J=I-1
!              Hydro(I,1)=Hydro(J,1)+0.083333
!          END IF
!          WRITE(*,*) Hydro(I,1), Hydro(I,2)
!716   CONTINUE          
!      WRITE(*,*) Hydro(1,1), Hydro(1,2)
!      SS1=SS ! Just to update its value
10000  CONTINUE 
       RETURN 
       END SUBROUTINE unith
	  
C      RMC - 1.30.17 - creating the FUNCTION	  
	 !FUNCTION RR(A,B,T)
	 !RR=EXP((A*ALOG(T))+B)
       !RETURN
       !END
      
