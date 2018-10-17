C  PROGRAM 19 - Based on Hromadka book 232 pag
C  --------------------------------------------------------------------------
      SUBROUTINE TRAPV(Q,B,Z,E1,E2,XL,RN,V)     
C ---------------------------------------------------------------------------
C DECLARE VARIABLES
C ---------------------------------------------------------------------------      
      IMPLICIT DOUBLE PRECISION (a-h, o-z)
C ---------------------------------------------------------------------------
!     FN(QQ,BB,DD,ZZ,SS)=1.-QQ*RN*(BB+2.*DD*SQRT(ZZ*ZZ+1.))**.66667/
!     C (1.486*((BB+ZZ*DD)*DD)**1.6667*SQRT(SS))
	S=(E1-E2)/XL
      IF(B.LE.0.)B=.0001
      DMAX=1000.
      YMAX=DMAX
      YMIN=0.
      DO 440 I=1,17
      DN=.5*(YMIN+YMAX)
      !F=FN(Q,B,DN,Z,S) !line in the book
      F=1.-Q*RN*(B+2.*DN*SQRT(Z*Z+1.))**.66667/
     c (1.486*((B+Z*DN)*DN)**1.6667*SQRT(S))
      IF(F)420,450,430
420   YMIN=DN
      GO TO 440
430   YMAX=DN
440   CONTINUE
450   CONTINUE
      TW=B+2.*Z*DN
      AREA=.5*(B+TW)*DN
      V=Q/AREA
C ---------------------------------------------------------------------------
      RETURN
      END SUBROUTINE TRAPV
