! SUBROUTINES of nine processes considerer in (Hromadka et al., 1983)
! Each of one is called from java, and at this moment the SS array is 
! returned (UF, 9/25/2017- Marco Pazmino-Hernandez modIFied) 
C  ----------------------------------------------------------------------     
C Program 12 -  Based on Hromadka book pag 157
C  ----------------------------------------------------------------------
      SUBROUTINE unith(SS1,m,n,PP,m1,n1,NUT,NDAT,RAN,ZN1,ZN2,KODE,
     & Hydro,mn1,mn2) ! ARGU = NDAT (8.29.18)
C ------------------------------------------------------------------------
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C NA: 	 Stream "A" number. This stream is the one to be modeled					   C		
C KTYPE:   Select 24-hr storm unit-interval model number							       C	
C XL: 	 Piper length - the length of the  longest watercourse (FEET) 				   C				
C XLCA:    The length along the longest watershed watercourse measured from              C
C 		 the point of concentration upstream to a point opposite the                   C
C 		 centroid of the watershed area								                   C
C HH: 	 The difference in elevation between the most report point in the              C
C 		 watershed and the point of concentration (FEET)							   C	
C XN: 	 Basin Factor (Manning's Friction Factor) [ 0.008 - 0.999]				       C				
C AREA: 									                                               C
C VSL: 	 Lost rate (inch/hour)			                                               C					
C KODE1:   Unit-Hydograph "S" graph options: 1. Valley zone, 2. Foothill Zone,           C
C 		 3. Mountain Zone, 4. Desert Zone, 5. Combination of option 1 to 4	           C							
C BASCON:  BASEFLOW (CFS/square-mile)						                   		       C
C SLP: 	 Low lost rate percentage (decimal notation)			                       C				
C R5: 	 5 Min [inches]	- Watershed	area-averaged point rainfalls	                   C			
C R30:    30 min							                                               C
C R1: 	 1 Hour			                                                 			   C	
C R3: 	 3 Hour			                                                     		   C		
C R6: 	 6 Hour			                                                      		   C		
C R24: 	24 Hour		                                	                               C			
C SS* 			                              			                               C			
C KSTORM*			                                                            	       C					
C KSOIL: Efective rainfall information display options	                		       C					
C PV: 	 Percentage (decimal notation) of watershed specified with Valley "S" curve    C							
C PF: 	 Percentage (decimal notation) of watershed specified with Foothill "S" curve  C								
C PM: 	 Percentage (decimal notation) of watershed specified with Mountain "S" curve  C								
C PD: 	 Desert "S" curve percentage				                     			   C
C NUT* 								                                                   C
C FX5:	 5 Min - Depth-Area Adjustrent Factor		                                   C				
C FX30:	30 min						                                            	   C
C FX1:	 1 Hour						                                                   C	
C FX3:	 3 Hour					                                                       C		
C FX6:	 6 Hour					                                                       C		
C FX24:   24 Hour		                                                                   C					
C DAOPT: User-specified depht-area factor				                                   C				
C TIME1: Time for Beginning of results (hrs)			                                   C					
C TIME2: Time for End of results (hrs)                                                   C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C ------------------------------------------------------------------------
C   DECLARE VARIABLES
C ------------------------------------------------------------------------    
      IMPLICIT DOUBLE PRECISION (a-h, o-z)
      COMMON/BLK1/SS(600,10),SS1(600,10),Hydro(600,3)! TEST 2.21.18
      !DIMENSION SS(600,10),SS1(600,10),Hydro(600,3)
!      DIMENSION A(600) ! A and B are just a number in untih -  A and B should not have dimension 9.5.18 
!      DIMENSION B(600) ! added 9.4.2018
!      COMMON/BLK1/SS
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
      REAL RR,A,B,T,R5A,R30A,R1A,R3A,R6A,R24A !9.5.18 (A,B,T) = (1,1,1)
!      Hydro RETURNs the hydrograph, Time(Hours) and Discharge(CFS) 
C -----------------------------------------------------------------------      
C Setting H to zero
!C -----------------------------------------------------------------------     
!C RR - FUNCTION !9.4.18     
!C -----------------------------------------------------------------------      
!      REAL FUNCTION RR(A,B,T)
!      REAL A,B,T
!      RR(A,B,T)=(EXP((A*ALOG(T))+B))    
!      RETURN
!      END FUNCTION RR  
C -----------------------------------------------------------------------      
C FUNCTION
C -----------------------------------------------------------------------     
!      RR(T)=(EXP((A*ALOG(T))+B)) ! Book
!      RR(A,B,T)=(EXP((A*ALOG(T))+B))  
C -----------------------------------------------------------------------      
C..READ DATA INPUT
C...OPO...NA was added to make references to the number of stream to be simulated
      !! READ(NDAT,*)NA,KTYPE,XL,XLCA,HH,XN,AREA,VSL,KODE1,BASCON,SLP,
      !!C  R5,R30,R1,R3,R6,R24,KSOIL,PV,PF,PM,PD,
      !!C  FX5,FX30,FX1,FX3,FX6,FX24,DAOPT,TIME1,TIME2
      READ(NDAT,*)NA,KTYPE,XL,XLCA,HH,XN,AREA,VSL,KODE1,BASCON,SLP, ! TEST 2.16.18
     C  KSOIL,PV,PF,PM,PD,DAOPT,TIME1,TIME2
      READ(RAN,*) ZN1R, ZN2R, KODER
      IF(ZN1R.EQ.ZN1.AND.ZN2R.EQ.ZN2.AND.KODER.EQ.KODE) THEN ! TEST 2.16.18
      READ(RAN,*)R5,R30,R1,R3,R6,R24,FX5,FX30,FX1,FX3,FX6,FX24
      END IF
C ------------------------------------------------------------------------      
c 02/01/18      READ(5,*)NA,KTYPE,XL,XLCA,HH,XN,AREA,VSL,KODE1,BASCON,SLP,                                !"5" should be NDAT
c 02/01/18 M.P. - the program breaks in this point IF I elimiate the initial varialbles
c 02/01/18      READ(NDAT,*)KTYPE,XL,XLCA,HH,XN,AREA,VSL,KODE1,BASCON,SLP,  
c 02/01/18 R5,R30,R1,R3,R6,R24,SS,KSTORM,KSOIL,PV,PF,PM,PD,NUT,
c 02/01/18 M.P. -Added SS,KSTORM and NUT
c 02/01/18     C R5,R30,R1,R3,R6,R24,SS,KSTORM,KSOIL,PV,PF,PM,PD,NUT !Added SS,KSTORM c02/01/18and NUT
c 02/01/18     C FX5,FX30,FX1,FX3,FX6,FX24,DAOPT,TIME1,TIME2
C ------------------------------------------------------------------------      
      SS=SS1 ! M.P. ---------------  
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
C ------------------------------------------------------------------------
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
C  DESIGNATE UNIT INTERVALS [UNIT in minutes] 
C -----------------------------------------------------------------------
      IF(KTYPE.EQ.1)UNIT=5.
      IF(KTYPE.EQ.2)UNIT=10.
      IF(KTYPE.EQ.3)UNIT=15.
      IF(KTYPE.EQ.4)UNIT=20.
      IF(KTYPE.EQ.5)UNIT=30.
      IF(KTYPE.EQ.6)UNIT=60.
C ----------------------------------------------------------------------
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
C ------------------------------------------------------------------------ 
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
C ------------------------------------------------------------------------ 
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
C ------------------------------------------------------------------------ 
      WRITE(NUT,8209)R5,R30
8209  FORMAT(11X,'SPECIFIED PEAK  5-MINUTES RAINFALL(INCH) = ',F15.2,/,
     C 11X,'SPECIFIED PEAK 30-MINUTES RAINFALL(INCH) = ',F15.2)
C ------------------------------------------------------------------------
      WRITE(NUT,8206)R1,R3,R6,R24,KTYPE
8206  FORMAT(11X,'SPECIFIED PEAK  1-HOUR RAINFALL(INCH) = ',F18.2,/,
     C 11X,'SPECIFIED PEAK  3-HOUR RAINFALL(INCH) = ',F18.2,/,
     C 11X,'SPECIFIED PEAK  6-HOUR RAINFALL(INCH) = ',F18.2,/,
     C 11X,'SPECIFIED PEAK 24-HOUR RAINFALL(INCH) = ',F18.2,//,
     C 11X,'HYDROGRAPH MODEL #',11X,' SPECIFIED*')
C --------------------------------------------------------------------------
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
99    PERCNT(I)=0.D0
98    CONTINUE
C ---------------------------------------------------------------------
C  UNIT HYDROGRAPH DETERMINATION
C ----------------------------------------------------------------------
      NUMBER=NLAG
      DO 97 I=1,150
      IF(I.LE.NLAG)PERCNT(I)=H(I)
97    H(I)=0.D0
      GO TO 96
94    CALL SUBSB(TIMLAG,PERCNT,KODE1,NUMBER)
!94    CALL SUBSB(TIMLAG,PERCNT,KODE1,NUMBER,NUT)
96    SUM=0.D0
C ------------------------------------------------------------------------
      IF(NUMBER.GE.150)WRITE(NUT,8207)
8207  FORMAT(4X,'UNIT HYDROGRAPH TERMINATED AFTER 150 UNIT INTERVALS',/)
      IF(NUMBER.GT.150)NUMBER=150
C
      DO 8208 I=1,NUMBER
      UH(I)=(PERCNT(I)-SUM)*XK
      SUM=PERCNT(I)
      IF(UH(I).LT.0.)UH(I)=0.
8208  CONTINUE
C ------------------------------------------------------------------------
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
C ------------------------------------------------------------------------
C 24-HOUR STORM RAINFALL PATTERN
C ------------------------------------------------------------------------
      R5A=R5*FX5
      R30A=R30*FX30
      R1A=R1*FX1
      R3A=R3*FX3
      R6A=R6*FX6
      R24A=R24*FX24
C ------------------------------------------------------------------------
      A=((ALOG(R30A))-(ALOG(R5A))/(ALOG(.5))-(ALOG(.0833)))
      B=((ALOG(R5A))-(A*ALOG(.0833)))
! Different from the book pag 160    
      R(193)=RR(A,B,.0833)
      R(194)=RR(A,B,.1667)-RR(A,B,.0833)
      R(195)=RR(A,B,.25)-RR(A,B,.1667)
      R(192)=RR(A,B,.3333)-RR(A,B,.25)
      R(196)=RR(A,B,.41667)-RR(A,B,.3333)
      R(191)=RR(A,B,.5)-RR(A,B,.41667)
C ------------------------------------------------------------------------   
      A=((ALOG(R1A)-ALOG(R30A))/(ALOG(1.)-ALOG(.5)))
      B=((ALOG(R30A))-(A*ALOG(.5)))
      R(197)=RR(A,B,.5833)-RR(A,B,.5)
      R(190)=RR(A,B,.6667)-RR(A,B,.5833)
      R(198)=RR(A,B,.75)-RR(A,B,.6667)
      R(189)=RR(A,B,.8333)-RR(A,B,.75)
      R(188)=RR(A,B,.9167)-RR(A,B,.8333)
      R(187)=RR(A,B,1.)-RR(A,B,.9167)
C ------------------------------------------------------------------------      
C  REMAINING PART OF PEAK 3-HOUR STORM
C ------------------------------------------------------------------------      
      A=((ALOG(R3A))-(ALOG(R1A))/(ALOG(3.)-ALOG(1.)))
      B=(ALOG(R1A))-(A*ALOG(1.))
      RRSAVE=R1A
C ------------------------------------------------------------------------
      DO 1001 J=1,12
      XJ=J
      DT=XJ*.1667
      T=1.+DT !first time that T is mentioned
      RRNEW=RR(A,B,T)
      DR=(RRNEW-RRSAVE)/2.
      R(J+198)=DR
      IR=187-J
      R(IR)=DR
1001  RRSAVE=RRNEW
C ------------------------------------------------------------------------
C  REMAINING PART OF PEAK 6-HOUR STORM
C ------------------------------------------------------------------------
      A=(ALOG(R6A)-ALOG(R3A))/(ALOG(6.)-ALOG(3.))
      B=(ALOG(R3A)-(A*ALOG(3.)))
      RRSAVE=R3A
C ------------------------------------------------------------------------
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
C ------------------------------------------------------------------------
C  REMAINING PART OF PEAK 24-HOUR STORM
C ------------------------------------------------------------------------
      A=(ALOG(R24A)-ALOG(R6A))/(ALOG(24.)-ALOG(6.))
      B=ALOG(R6A)-A*ALOG(6.)
      RRSAVE=R6A
C ------------------------------------------------------------------------
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
C ------------------------------------------------------------------------
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
C ----------------------------------------------------------------------
      NI=288
      IF(KTYPE.EQ.1)GO TO 1050
      K=KTYPE
      IF(KTYPE.EQ.5)K=6
      IF(KTYPE.EQ.6)K=12
      NI=288/K
      DO 1040 I=1,NI
      TEMP=0.D0
      II=(I-1)*K
      DO 1035 J=1,K
      IR=II+J
1035  TEMP=TEMP+R(IR)
1040  R(I)=TEMP
1050  CONTINUE
C     ADJUST FOR CONSTANT SOIL LOSS
      XTOTAL=0.D0
c rmc   TEMPS=UNIT/60.
      XRA=XR
C ------------------------------------------------------------------------  
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
C ------------------------------------------------------------------------        
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
C ------------------------------------------------------------------------        
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
      XMAX=0.D0
      DO 700 I=1,INTERV
      IF(H(I).LT.0.)H(I)=0.D0
      IF(H(I).GT.XMAX)XMAX=H(I)
      SUM=SUM + H(I)
 700  CONTINUE
      SUM=SUM*UNIT*60./43560.
      XTOTAL=XTOTAL/12.*AREA
C ----------------------------------------------------------------------  
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
C ------------------------------------------------------------------------
	WRITE(NUT,5003)
C ------------------------------------------------------------------------
C Pass hydrograph to STREAM flow MATRIX SS
C ------------------------------------------------------------------------
      CALL ADDHY(UNIT,INTERV,NA,H)
C ------------------------------------------------------------------------     
C Print results and estimate mass balances
C ------------------------------------------------------------------------
      CALL OASB(NUT,KTYPE,H,INTERV,XMAX,UNIT,SUM,TIME1,TIME2)
!        CALL ADDHY(UNIT,INTERV,NA,H)
C ------------------------------------------------------------------------
C HYDROGRAPH TO EXPORT
C ------------------------------------------------------------------------
!	 RETURN 
!      Hydro=H
!      WRITE (*,*) INTERV
!      WRITE (*,*) "Hydrograph UNITH"
!      DO 715 I=1,440
!      Hydro(I,2)=H(I)/(0.3048**3) !To obtain hydro in m^3/s
!	 Hydro(I,2)=H(I) !hydro in CFS
!      TIMEOUT=TIMEOUT+.083333
!      WRITE(*,*) TIMEOUT
!      WRITE(*,*) Hydro(I)
!715   CONTINUE     
!      DO 716 I=1,mn1
!      IF(I==1) THEN 
!      Hydro(I,1)=0.083333
!      ELSE
!      J=I-1
!      Hydro(I,1)=Hydro(J,1)+0.083333
!      END IF
!      WRITE(*,*) Hydro(I,1), Hydro(I,2)
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
C -----------------------------------------------------------------------       
C RR - FUNCTION !9.4.18     
C -----------------------------------------------------------------------      
      REAL FUNCTION RR(A,B,T)
      REAL A,B,T
      RR= EXP((A*ALOG(T))+B)    
      RETURN
      END FUNCTION RR  