C PROGRAM 17 ! Based on Hromadka book pag 217
C-----------------------------------------------------
      SUBROUTINE SEE (X,D1,D2,I1,I2,NUT,Y,NB,TIME)
!      SUBROUTINE SEE (X,D1,D2,I1,I2,Y,NB,TIME)
C-----------------------------------------------------
      DIMENSION Y(20)
      DO 100 K=2,NB
      IF(X.LT.Y(K))GO TO 200
100   CONTINUE
      TI=TIME+.0833333
!Now We are not going to WRITE the Flood.ans---------------------------- 
      WRITE(NUT,101)TI
101   FORMAT(10X,F7.3,5X,
     C  '*BASIN CAPACITY EXCEEDED: BASIN DATA IS EXTRAPOLATED*')
      K=NB
200   I1=K-1
      I2=K
      D1=Y(I1)
      D2=Y(I2)
c rmc 1000    CONTINUE
      RETURN
      END SUBROUTINE SEE
