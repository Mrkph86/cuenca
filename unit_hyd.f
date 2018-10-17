C---------------------------------------------------------------
	SUBROUTINE unit_hyd(Q,A,qp,tp,D,tc,qp5,tp5,mref)
C---------------------------------------------------------------
C Unit NRCS hydrograph using Haan's equation (k=3.77)
C---------------------------------------------------------------
C      version 3.0.1, Last ModIFied: See ModIFications below
C      WRITTEN FOR: ASAE'99 Toronto paper, March 8, 2002 
C      Written by: R. Munoz-Carpena (rmc)   &   J. E. Parsons, BAE (jep)
C                  University of Florida        BAE, NC State University
C                  Gainesville, FL 32611        Raleigh, NC 27695-7625(USA)
C                  e-mail: carpena@ufl.edu      
C---------------------------------------------------------------
	IMPLICIT DOUBLE PRECISION (a-h, o-z)
	COMMON/hydgph/u(5000,2),qh(5000,3)
      ck=3.77d0
      t5=0.d0 !MAC 10/04/12
c----Initialize u vector
      DO 5 i=1,208
      DO 5 j=1,2
      u(i,j)=0.d0
5     CONTINUE

c----rmc 09/30/11 obsolete from uh v2.x, scaling unit hydrograph now
c----uh v3.x hydrograph by convolution from synthetic excess hyetograph 
c
c----rmc 04/20/03 - Fix for Q=0
c	IF(Q.le.0) THEN
c	    ttotal=D
c	  ELSE
c	    ttotal=0.6375d0*Q*A/qp/60.d0
c	END IF
c----rmc 04/20/03 - END of fix for Q=0
c	dt=ttotal/50.d0
c	DO 10 i=0,51
c          t=i*dt
c	    IF(Q.le.0) THENc
c		    qi=0.d0
c		  ELSE
c                qi=qp*(t/tp*dexp(1-t/tp))**ck
c	    END IF
c	    rot(i+1)=t
c	    roq(i+1)=qi
c          qdepth=qi*360.d0/A
c          WRITE(2,100)t,qi,qdepth
c10	CONTINUE
c------ END of uh v2.x calculations----------------

c----rmc 09/15/11- New total hydrograph calculation from unit hydrograph
c----unit hidrograph values. Def:duration(h),qp5(m3/s),tp5(h):peak t,q
c----a)Estimate time step for dimesionless unit hydrograph as Def<>1/3.tp
c----since tp=0.6tc+Def/2 --> IF Def<>1/3tp --> Def<>0.24tc
      Def=0.24d0*tc !MAC 04/10/12 Time step defined as 5 min
      !WRITE(2,205)Def*60.d0 !MAC 04/10/12
205	FORMAT(' SCS ',f4.1,'-min unit hydrograph',/,
     C ' time (h)    q(m3/s)   q(mm/h)',/,30('-'))
      tp5=0.6d0*tc+0.5d0*Def
      qp5=0.127481d0*A/(tp5*60.d0)
c--- SCS triangular hydrograph---
c     ttotal5=2.67d0*tp5
c----SCS aDIMENSIONal unit hydrograph
      ttotal5=5.d0*tp5
c--------------------------------
      dt5=Def
      i=0
      cqdepth5=0.d0
      !WRITE(*,*)"Hydro", Q,dt5
      !WRITE(*,*)t5, ttotal5
      DO while (t5.le.ttotal5)
      t5=i*dt5
      IF(Q.le.0) THEN
      qi5=0.d0
      ELSE
      qi5=qp5*(t5/tp5*dexp(1-t5/tp5))**ck
      END IF
      u(i+1,1)=t5
      u(i+1,2)=qi5
      qdepth5=qi5*360.d0/A !Explanation, to obtain q in (mm/h)
      cqdepth5=qdepth5+cqdepth5
      !WRITE(*,'(3f10.4)')(u(i+1,j),j=1,2),qdepth5 !MAC 04/10/12
      i=i+1
      END DO
c----rmc - mref, number of unit hydrograph steps needed in convolution
      mref=i
c----rmc - check unit hydrograph volumen <> 1
      unitq=cqdepth5*dt5
!      IF(dabs(1.d0-unitq).le.0.05d0) THEN
!      !      !WRITE(2,120)'PASSED unit hydrograph check- V(mm)=',unitq!MAC 04/10/12
!			WRITE(*,*)'PASSED unit hydrograph check- V(mm)=',unitq
!         ELSE
!      !      !WRITE(2,120)'FAILED unit hydrograph check- V(mm)=',unitq!MAC 04/10/12
!			WRITE(*,*)'FAILED unit hydrograph check- V(mm)=',unitq
!      END IF

c100	FORMAT(f9.2,2f10.4)
!120   FORMAT(/,A36,f6.2)
	RETURN
	END SUBROUTINE unit_hyd
