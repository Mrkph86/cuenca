
C --------------------------------------------------------------
	SUBROUTINE q_peak(Area,Q,xIa,P,tc,j,qp,tp)
C --------------------------------------------------------------
C	SCS-TR55 peak flow calculation
C --------------------------------------------------------------
C      version 3.0.1, Last ModIFied: See ModIFications below
C      WRITTEN FOR: ASAE'99 Toronto paper, March 8, 2002 
C      Written by: R. Munoz-Carpena (rmc)   &   J. E. Parsons, BAE (jep)
C                  University of Florida        BAE, NC State University
C                  Gainesville, FL 32611        Raleigh, NC 27695-7625(USA)
C                  e-mail: carpena@ufl.edu      
C --------------------------------------------------------------
      
      IMPLICIT DOUBLE PRECISION (a-h, o-z)
      DIMENSION ci(3,4,5)

	DATA ci/68.0317,-82.907,11.1619,144.547,-130.64,-55.230,-11.312,
     C 16.6125,-43.015,-11.505,-64.177,65.9007,-74.693,105.222,-26.314,
     C -136.68,134.907,47.9565,12.1681,-16.337,50.4334,14.2182,85.7116,
     C -85.806,24.9255,-42.167,16.1126,41.8526,-45.773,-13.503,-6.5688,
     C 6.4981,-19.740,-7.8919,-38.206,39.0036,-3.9797,6.7479,-2.9776,
     C -6.2829,6.585,2.1954,1.0577,-1.1784,3.2996,1.3836,6.7419,-6.8946,
     C 2.5222,-0.8657,0.0456,2.3645,-0.6384,-0.2644,2.5021,-0.5476,
     C -0.3427,2.4007,-0.8899,0.2078/

c	DO 10 j=1,4
c	DO 10 i=1,3
c	WRITE(2,100) (ci(i,j,k),k=1,5)
c10	CONTINUE

C ---rmc 04/20/03 - Fix for Q=0
	IF(Q.le.0) THEN
	qp=0.d0
	tp=qp
      RETURN
	END IF
C ---rmc 04/20/03 - END of fix for Q=0
	xIaP=xIa/P
C --- TR55 stablishes that IF Ia/P is outside the range (0.1<Ia/P<0.5),
C ----use the limiting values  
	IF(xIa/P.gt.0.5d0)xIaP=0.5d0
	IF(xIa/P.lt.0.1d0)xIaP=0.1d0

C -- Import Ia/P and storm type I,IA,II,III (j=1,4) ------------
	C0=ci(1,j,1)*xIaP**4+ci(1,j,2)*xIaP**3+ci(1,j,3)*xIaP**2+
     C     ci(1,j,4)*xIaP+ci(1,j,5)
	C1=ci(2,j,1)*xIaP**4+ci(2,j,2)*xIaP**3+ci(2,j,3)*xIaP**2+
     C     ci(2,j,4)*xIaP+ci(2,j,5)
	C2=ci(3,j,1)*xIaP**4+ci(3,j,2)*xIaP**3+ci(3,j,3)*xIaP**2+
     C     ci(3,j,4)*xIaP+ci(3,j,5)

C --- Unit q peak, qp (m3/s) -----------------------------------
	qu=4.3046d0*10.d0**(C0+C1*dlog10(tc)+C2*(dlog10(tc))**2-6.d0)
	Fp=1.d0
	qp=qu*Area*Q*Fp

C -- Unit hydrograph time to peak (min) ------------------------
	tp=0.127481d0*Q*Area/qp/60.d0
100	FORMAT(5f9.3)
	RETURN
	END SUBROUTINE q_peak
