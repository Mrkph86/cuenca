C ------------------------------------------------------------------
C //////////////////////////////////////////////////////////////////
C /                          SCS METHOD                            /
C //////////////////////////////////////////////////////////////////
C ------------------------------------------------------------------
      SUBROUTINE uhcn (SS1,m,n,PP,m1,n1,NDAT,Hydro1,mn1,mn2) !ARGU = (NUT,NDAT) (2.1.18) 
C --------------------------------------------------------------
c  version 3.0.1, Last ModIFied: See ModIFications below
C  WRITTEN FOR: ASAE'99 Toronto paper, March 8, 2002 
C  Written by: R. Munoz-Carpena (rmc)   &   J. E. Parsons, BAE (jep)
C     University of Florida                 BAE, NC State University
C     Gainesville, FL 32611                 Raleigh, NC 27695-7625(USA)
C     e-mail: carpena@ufl.edu      
C --------------------------------------------------------------
C Program to create input files for VFSmod, based on NRCS-TR55 and Haan
C et al, 1996, with additional work DOne on coefficients for unit peak
C flow calculation.
c
c    Date      Modification                                 Initials
c   -------    ------------------------------                ------ 
c   2/17/99    Check for 0.1<Ia/P<0.5                           rmc
c   2/18/99    Added hyetograph output for 6 h storm            jep
c   2/18/99    ModIFy File Inputs for Erosion                   jep
c   2/20/99    Roughed in MUSLE                                 jep
c   3/01/99    Checked erosion parameters and units             rmc
c   3/02/99    Additional work on Musle - units close           jep
c   3/03/99    Added hyetographs for storm types I & IA         rmc
c   3/05/99    output irs file for VFSMOD                       jep
c   3/06/99    Input/Output files as in VFSMOD                  jep
c   3/10/99    Checked Input/Output files as in VFSMOD          rmc
c   3/10/99    Cleanup - created hydrograph.f for 
c              hydrograph subroutines, created io.f for
c              input and output related processing              jep
c   3/28/99    Erosion part: fixes in I30 calculation 
c              after Chow and checked for consistency in
c              units, clean up; Hydro: added delay time         rmc
c   8/27/99    Added option to select dIFferent methods 
c              for applying MUSLE, default is Foster, 
c              2=Williams, 3=GLEAMS
c   10/01/99   Fixed array so that storm duration (D)        
c              can now be up to 24h                             rmc
c   10/26/99   implemented the project file concept as in vfsm  jep
c    3/09/00   Version changed to 0.9, general program cleanup  rmc
c   16/06/00   Version changed to 1.0, erosion output organized rmc
c   16/03/02   Version changed to 1.06 to couple with VFSMOD, 
c              author affiliation changed                       rmc
c    4/18/03   Fixed K - computed IF we enter -1, other use     jep
c              entered value, also fixed dp output FORMAT
c    4/19/03   dp now being read in                             jep
c    4/20/03   Runoff calculation for low CN revised            rmc
c    5/01/03   Added chacked for small runoff case to switch    rmc
c              to Williams sediment calculation that includes
c              runoff.
c   11/10/03   Reordered Erosion ieroty 1=Williams, 2=Gleams
c                3=Foster to coincide with changes in Shell     jep
c   11/13/03   Fixed coef. on Type Ia - did not add new
c                hyet curves
c   01/10/05   Added changes suggested by U. of Guelph group    rmc 
c              v2.4.1
c   09/15/11   Rewritten hydrograph calculation using convolution
c              of excess rain steps, v3.0.0                     rmc
c   02/15/12   Added user table for 24-h hyetograph, v3.0.1     rmc
C --------------------------------------------------------------
c Compiling for Win32 and Unix environments:
c       1. The i/o for these operating systems is dIFferent.
c       2. Change the comments in the finput.f program to reflect
c          your operating system.   3/9/00
C --------------------------------------------------------------
c COMMON/hydgph:
c       rot(208), runoff time (units)
c       roq(208), runoff rate (m3/s)
c       u(208,2), unit hydrograph
c COMMON/rain/:
c       rfix, maximum rain intensity (mm/h)
c       rti(200), rainfall time (hrs)
c       rfi(200), rainfall intensity (mm/h)
c       rcum(100,2), cumm rainfall (mm)
c       ref(100), excess rainfall intensity (mm/h)
c       ncum: number of steps IF user hyetograph is read
c other:
c       nref  = number of excess hyetograph steps
c       mref  = number of unit hydrograph steps
c       nhyet = number of hyetograph steps
c       vol(m3), volro (mm) = runoff volume 
C -----------------------------------------------------------------------
C   DECLARE VARIABLES
C -----------------------------------------------------------------------
	IMPLICIT DOUBLE PRECISION (a-h, o-z)
!	COMMON/BLK1/SS(600,10),SS1(600,10),Hydro(600,3)
      DIMENSION SS(600,10),SS1(600,10),Hydro(600,3)
      COMMON/BLK1/SS
	COMMON/hydgph/u(5000,2),qh(5000,3)
	COMMON/rain/rfix,rti(5000),rfi(5000),rcum(5000,2),ref(5000,2),ncum
	CHARACTER*20 soilty
	CHARACTER*75 LISFIL(6)
C      DIMENSION PP(m1,n1)
C ------------------------------------------------------------------
C  Get inputs and open files
C ------------------------------------------------------------------
!	CALL  FINPUT(LISFIL)
!	CALL getinp(P,CN,Area,jstype,D,pL,Y,ek,cfact,pfact,soilty,
!     1            ieroty,dp,om)
	SS=SS1
	Hydro=Hydro1
      READ(NDAT,*)NA,P,CN,Area,jstype,D,pL,Y,Dstep,ek,cfact,pfact,dp,    ! (9.25.17)
     C ieroty                                                       ! (9.25.17)
	!!Dstep=time step (h) MAC 04/10/12
	!!Check in hydrograph definition Def=k*tc
	!!USLE FACTORS
C ------------------------------------------------------------------	
C   ieroty = select method to estimate storm erosion
C            Selections:
C          0 or not present = Foster's method for R-factor
C          1 = Using Williams R-factor
C          2 = Using R-factor from GLEAMS with daily rainfall
C	om = 2.0d0
c     om = % organic matter, read IF ek <0
	!rcum(1,1) !storm definition (rcum)
C - For user defined case, read "tmid" first and THEN 24-h P/P24 curve
C - "tmid" is stored in first position of the rcum(i,j) array
C ------------------------------------------------------------------
C  Calculate runoff volume by SCS method
C ------------------------------------------------------------------
	CALL runoff(P,CN,xIa,Q) !OK
	volro=Q
    !WRITE(*,*)Q
      IF (Q>0) THEN
C ------------------------------------------------------------------
C  Calculate concentration time by SCS method
C ------------------------------------------------------------------
	CALL calctc(pL,CN,Y,tc) !OK
C ------------------------------------------------------------------
C  Calculate peak flow and time by SCS-TR55 method
C ------------------------------------------------------------------
	CALL q_peak(Area,Q,xIa,P,tc,jstype,qp,tp) !OK
C ------------------------------------------------------------------
C  Output hydrology results
C ------------------------------------------------------------------
!MAC 04/10/12 We DO not PRINT any result
!	CALL results(P,CN,Q,Area,tc,xIa,jstype,D,pL,Y,qp,tp,qdepth,
!     1             ieroty)
C ------------------------------------------------------------------
c  Calculate SCS-unit hydrograph
C ------------------------------------------------------------------
	CALL unit_hyd(Q,Area,qp,tp,D,tc,qp5,tp5,mref)
C ------------------------------------------------------------------
c  Calculate storm hyetograph from SCS storm type
C ------------------------------------------------------------------
 	CALL hyetgh(jstype,P,D,volro,qdepth,vol,qp,Area,xIa,rtpeak,er,er1,
     C            erCoolm,ti,nref,tc,a1,b1,bigE,raimax30,nhyet)
C ------------------------------------------------------------------
c  Calculate storm hydrograph
C ------------------------------------------------------------------
      CALL tab_hyd(Q,Area,mref,nref,qp,nhyd,Dstep,NA)
      ELSE
      DO 63 iii=1,600
        Hydro(iii,2)=0
        Hydro(iii,1)=iii*Dstep
	!WRITE(*,*) Hydro(iii,1), Hydro(iii,2)
63    CONTINUE
      END IF
!      SS1=SS    !9.4.2018
!	Hydro1=Hydro ! why again bak to hydro1 to hydro 9.4.2018
C ------------------------------------------------------------------
C DO the modIFied usle to get erosion stuff
C ------------------------------------------------------------------
!MAC 04/10/12
!      CALL musle(er,er1,erCoolm,ek,Y,pl,cfact,pfact,Area,vol,tc,P,D,soilty,
!     C           dp,sconc,sconc1,sconc2,om,a1,b1,bigE,raimax30,qp)
C ------------------------------------------------------------------
C WRITE vfsmod compatible input files
C ------------------------------------------------------------------
C MAC 04/10/12
!      CALL vfsout(dp,ieroty,sconc,sconc1,sconc2,Area,pL,qp,tp,tc,D,ti,
!     C           nhyet,nhyd)

	!close(1) !MAC 04/10/12
	!close(2) !MAC 04/10/12

	!stop !MAC 04/10/12
	END SUBROUTINE uhcn
