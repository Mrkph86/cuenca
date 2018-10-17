      
!  Part of Program 15 - Based on Hromadka book pag 197
C  ----------------------------------------------------------------------
      SUBROUTINE ADDHY(UNIT,INTERV,NA,H)
C  ----------------------------------------------------------------------
C     *USED FOR FLOOD ROUTING SYSTEM ONLY*
C     TRANSFORMS SUBROUTINE UNITH FOR USE WITH THE FLOOD SYSTEM
C     JUST INSERT CALL TO ADDHY AT END OF SUBROUTINE UNITH
C     IMMEDIATELY PRECEEDING CALL TO OABS
C     ADD RUNOFF HYDROGRAPH TO A STREAM
C  ----------------------------------------------------------------------
      DIMENSION H(440)
      DIMENSION AA(600)
C
      CALL MREAD(NA,AA)
      NUMX=UNIT/5.+.01
      IF(NUMX-2)751,752,753
751   DO 750 I=1,INTERV
      AA(I)=AA(I)+H(I)
750   CONTINUE
      GO TO 760
752   DO 755 I=1,INTERV
	J=2*I
	K=J-1
	AA(K)=AA(K)+H(I)
	AA(J)=AA(J)+H(I)
755   CONTINUE
      GO TO 760
753   DO 756 I=1,INTERV
      L=3*I
	K=L-1
	J=L-2
	AA(L)=AA(L)+H(I)
	AA(K)=AA(K)+H(I)
	AA(J)=AA(J)+H(I)
756   CONTINUE
760   AA(600)=INTERV*NUMX
      CALL MWRITE(NA,AA)
C
      RETURN
      END SUBROUTINE ADDHY      
