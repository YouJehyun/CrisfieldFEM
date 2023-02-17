      SUBROUTINE ELEMENT(FI,AKT,AN,X,Z,P,E,ARA,AL,IWRIT,IWR,IMOD,
     1                   IDUM,ADUM1,ADUM2)
C     ARGUMENTS IN LINE ABOVE AND ARRAY X NOT USED FOR SHALLOW TRUSS
C
C
C     FOR SHALLOW TRUSS ELEMENT
C     IMOD = 1 COMPUTES INT.LD.VECT.FI
C     IMOD = 2 COMPUTES TAN.STIFF.AKT
C     IMOD = 3 COMPUTES BOTH
C
C     AN = INPUT (TOTAL FORCE IN BAR)
C     Z = INPUT (Z COORD VECTOR)
C     P = INPUT (TOTAL DISP.VECTOR)
C     AL = INPUT (LENGTH OF ELEMENT)
C     EA = INPUT (YOUNGS MODULUS)
C     ARA = INPUT (AREA OF ELEMENT)
C
C     IF IWRIT.NE.0(NOT EQUAL TO 0) WRITES OUT FI AND/OR AKT ON CHANNEL IWR
C
      DOUBLE PRECISION AKT(4,4),FI(4),Z(2),P(4),X(2),EA,E,ARA,EAL,AL,
     1                 Z21,W21,BET,AN,ANL,ADUM1,ADUM2
      INTEGER I,J,IDUM,IMOD,IWR,IWRIT
      
C
      EA = E*ARA
      EAL = EA/AL
      Z21 = Z(2) - Z(1)
      W21 = P(4) - P(3)
      BET = (Z21 + W21)/AL
C
      IF (IMOD.NE.2) THEN
C     COMPUTES INT.FORCE.VECT (SEE 2.17)
        FI(1) = -1.D0
        FI(2) = -1.D0
        FI(3) = -BET
        FI(4) = BET
        DO 1 I=1,4
          FI(I) = AN*FI(I)
    1   CONTINUE
        IF (IWRIT.NE.0) THEN
          WRITE (IWR,1000) (FI(I),I=1,4)
 1000     FORMAT(/,1X,'INT.FORCE VECT.FOR TRUSS EL IS',1X,4G13.5,/)
        ENDIF
C
      ENDIF
C
      IF(IMOD.NE.1) THEN
C     COMPUTES TAN STIFF.MATRIX(UPPER STRIANGLE) (SEE 2.23)
        AKT(1,1) = 1.D0
        AKT(1,2) = -1.D0
        AKT(1,3) = BET
        AKT(1,4) = -BET
        AKT(2,2) = 1.D0
        AKT(2,3) = -BET
        AKT(2,4) = BET
        AKT(3,3) = BET*BET
        AKT(3,4) = -AKT(3,3)
        AKT(4,4) = BET*BET
        DO 12 I=1,4
          DO 13 J=1,4
            AKT(I,J) = EAL*AKT(I,J)
   13     CONTINUE
   12   CONTINUE
C
C           NOW ADD GEOM. OR INIT STRESS MATRIX (SEE 2.23)
C
        ANL = AN/AL
        AKT(3,3) = AKT(3,3) + ANL
        AKT(3,4) = AKT(3,4) - ANL
        AKT(4,4) = AKT(3,3) + ANL
        IF (IWRIT.NE.0) THEN
          WRITE (IWR,1001)
 1001     FORMAT (/, 1X, 'TAN.STIFF.MATRIX FOR TRUSS EL. IS', /)
          DO 14 I = 1,4
            WRITE (IWR,67) (AKT(I,J),J=1,4)
   67       FORMAT (1X,7G13.5)
   14     CONTINUE
        ENDIF
C
      ENDIF           
C
      RETURN
      END