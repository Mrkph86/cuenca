C --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
C   Program:      
C --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
       FUNCTION SCStorm(Jstype,ptime)
c !----------------------------------------------------
c ! SCS design storm type equation using generalized coefficients
c ! (Munoz-Carpena and Parsons,2004) for 24 hour storms,
c !     P(t)       t-b (    d    ) ^g
c !     ---- = a + ----(---------)
c !     P24         c  (e|t-b|+f )
c !----------------------------------------------------
       IMPLICIT DOUBLE PRECISION (a-h,o-z)
      
       COMMON/rain/rfix,rti(5000),rfi(5000),rcum(5000,2),ref(5000,2),ncum
       DIMENSION cff(4,7)
       DATA cff/0.4511d0,0.3919d0,0.495d0,0.5d0,9.995d0,7.96d0,11.8d0,       
     C   12.d0,1d0,1.d0,0.56d0,24.d0,-0.1617d0,0.843d0,10.6d0,24.04d0,
     C   -3.0163d0,120.39d0,130.d0,2.d0,0.013d0,0.3567d0,0.525d0,
     C   0.04d0,0.5853d0,0.4228d0,0.75d0,0.75d0/

c      DO 10 i=1,4
c      WRITE(*,100)(cff(i,j), j=1,7)
c10    CONTINUE
 
       IF(Jstype.le.4) THEN
       cffa=cff(Jstype,1)
       cffb=cff(Jstype,2)
       cffc=cff(Jstype,3)
       cffd=cff(Jstype,4)
       cffe=cff(Jstype,5)
       cfff=cff(Jstype,6)
       cffg=cff(Jstype,7)
       bigT= ptime-cffb             
       denom=cffe*dabs(bigT)+cfff
       IF(Jstype.eq.1.and.denom.ge.0.d0) THEN
       SCStorm=0.5129d0
       ELSE
       SCStorm=cffa+(bigT/cffc)*(cffd/denom)**cffg
       END IF
       ELSE
       DO 15 i=2,ncum
       t1=rcum(i,1)
       rcum1=rcum(i,2)
       t2=rcum(i+1,1)
       rcum2=rcum(i+1,2)
       IF(ptime.gt.t1.and.ptime.le.t2) THEN                  
       SCStorm=(ptime-t1)/(t2-t1)*(rcum2-
     C  rcum1)+rcum1
       END IF
15     CONTINUE
       END IF
     
c100   FORMAT(7f9.4)
  
       RETURN
       END FUNCTION SCStorm
C --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	 SUBROUTINE runoff(P,CN,xIa,Q)
C --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
C SCS runoff calculation
C --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
C      version 3.0.1, Last ModIFied: See ModIFications below
C      WRITTEN FOR: ASAE'99 Toronto paper, March 8, 2002 
C      Written by: R. Munoz-Carpena (rmc)   &   J. E. Parsons, BAE (jep)
C                  University of Florida        BAE, NC State University
C                  Gainesville, FL 32611        Raleigh, NC 27695-7625(USA)
C                  e-mail: carpena@ufl.edu      
C --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
	   IMPLICIT DOUBLE PRECISION (a-h, o-z)
	   S=25400.d0/CN-254.d0
	   xIa=0.2d0*S
	   IF(P.gt.xIa)THEN
	   Q=(P-xIa)**2/(P+0.8d0*S)
	   ELSE
	   Q=.0d0 
	   END IF

	   RETURN
	   END SUBROUTINE runoff
