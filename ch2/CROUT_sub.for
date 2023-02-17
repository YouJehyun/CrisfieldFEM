      SUBROUTINE CROUT(AK,D,N,IWRIT,IWR)
C
C     INPUTS AK(N,N); OUTPUTS UPPER TRIANGLE IN AK AND DIAG
C     PIVOTS IN D(N)
C
      DOUBLE PRECISION AK(N,N),D(N),A
      INTEGER N,I,J,IWR,IWRIT
C
      D(1) = AK(1,1)
      DO 1 J=2,N
        DO 2 I=1,J-1
          A = AK(I,J)
          IF (I.EQ.1) GO TO 2
          DO 3 L=1,I-1
            A=A-AK(L,J)*AK(L,I)
    3     CONTINUE
          AK(I,J) = A
    2   CONTINUE
        DO 4 I=1,J-1
          AK(I,J) = AK(I,J)/AK(I,I)
    4   CONTINUE   
        DO 5 L=1,J-1
          AK(J,J) = AK(J,J) - AK(L,J)*AK(L,J)*AK(L,J)
    5   CONTINUE
        D(J) = AK(J,J)
    1   CONTINUE
C     
      IF (IWRIT.NE.0) THEN
        WRITE (IWR,1000)
 1000   FORMAT (/,1X,'FACTORISED MATRIX IS',/)
        DO 10 I=1,N
          WRITE (IWR,67) (AK(I,J), J=1,N)
   67     FORMAT (1X,7G12.5)
   10   CONTINUE
        WRITE (IWR,1001)
 1001   FORMAT (/,1X,'DIAG.PIVOTS ARE',/)
        WRITE (IWR,67) (D(I), I=1,N)
      ENDIF
C
      RETURN
      END