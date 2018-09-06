C --------------------------------------------------------------
	SUBROUTINE calctc(pL,CN,Y,tc)
C --------------------------------------------------------------
C SCS concentration time calculation
C --------------------------------------------------------------
C      version 3.0.1, Last ModIFied: See ModIFications below
C      WRITTEN FOR: ASAE'99 Toronto paper, March 8, 2002 
C      Written by: R. Munoz-Carpena (rmc)   &   J. E. Parsons, BAE (jep)
C                  University of Florida        BAE, NC State University
C                  Gainesville, FL 32611        Raleigh, NC 27695-7625(USA)
C                  e-mail: carpena@ufl.edu      
C --------------------------------------------------------------
	IMPLICIT DOUBLE PRECISION (a-h, o-z)

	tc=pL**0.8d0*(1000.d0/CN-9.d0)**0.7d0/(4407.d0*dsqrt(Y))
	RETURN
	END SUBROUTINE calctc
