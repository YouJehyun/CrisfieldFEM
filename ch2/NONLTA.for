      PROGRAM NONLTA
C
C     PERFORMS NON-LINEAR INCREMENTAL SOLUTION FOR TRUSS
C     NV = NUMBER OF ARIABLES(4 OR 5)
C     QFI = FIXED LOAD VECTOR
C     IBC = BOUNDARY CONDITION COUNTER(0=FREE, 1=FIXED)
C     Z = Z COORDS OF NODES
C     QINC = INCREMENTAL LOAD VECTOR
C     PT = TOTAL DISPLACEMENT VECTOR
C     AKTS = STRUCT. TANGENT STIFFNESS MATRIX
C     AKTE = ELEMENT TANGENT STIFFNESS MATRIX
C     FI(NOT USED HERE) = INTERNAL FORCES
C     D = DIAGONAL PIVOTS FROM LDL(TRAN) FACTORISATION
C     ID14S = VAR. NOS. (1-4) AT WHICH LIN EARTHED SPRINGS
C     AK14S = EQUIVALENT LINEAR SPRING STIFFNESSES
C     AK15 = LINEAR SPRING STIFFNESS BETWEEN VARBLS. 1 AND 5 (IF NV=5)
C
      DOUBLE PRECISION QFI(5),Z(2),QINC(5),PT(5),AKTE(4,4),FI(5),D(5),
     1                 AK14S(4),AKTS(25),X(2),POISS,E,ARA,AL,ANIT,AK15,
     2                 AN,ALN,ARN
      INTEGER IBC(5),ID14S(4),IRE,IWR,I,NV,NDSP,ITYEL,N

C     ARRAY X ABOVE NOT USED FOR SHALLOW TRUSS
C
      IRE = 5
      IWR = 6
      OPEN(UNIT=5, FILE='nonltainput.txt')
      OPEN(UNIT=6, FILE='nonltaresult.txt')
C
      CALL INPUT(E,ARA,AL,QFI,X,Z,ANIT,IBC,IRE,IWR,AK14S,ID14S,NDSP,
     1          NV,AK15,
     2          POISS,ITYEL)
C     ARGUMENTS IN LINE ABOVE NOT USED FOR SHALLOW TRUSS
C     BELOW RELEVANT TO DEEP TRUSS BUT LEAVE FOR SHALLOW TRUSS
      ALN = AL
      ARN = ARA
C 
      READ(IRE,*) FACI,NINC,IWRIT
      WRITE(IWR,1000) FACI,NINC,IWRIT
 1000 FORMAT(/,1X,'INCREMENT OF LOAD FACTOR =',G13.5,/,
     1      1X,'NO. OF INCREMENT (NINC)=',G13.5,/,
     2      1X,'WRITE CONTROL (IWRIT)',G13.5,/,
     3      3X,'(0=LIMITED ; 1=FULL)',/)
C
      AN = ANIT
      FACT = 0.D0
      DO 5 I=1,NV
        PT(I) = 0.D0
    5 CONTINUE
C
C
      DO 100 INC=1,NINC
        FACT = FACT + FACI
        WRITE(IWR,1001) INC,FACT
 1001   FORMAT(/,1X,'INCREMENT = ',G13.5,3X,'LOAD FACTOR = ',G13.5,/)
        DO 10 I=1,NV
          QINC(I) = FACI*QFI(I)
   10   CONTINUE
C
C     BELOW FORMS ELEMENT TANGENT STIFFNESS MATRIX AKT
        CALL ELEMENT(FI,AKTE,AN,X,Z,PT,E,ARA,AL,IWRIT,IWR,2,
     1               ITYEL,ALN,ARN)
C     ARGUMENTS IN LINE ABOVE NOT USED FOR SHALLOW TRUSS
C
        CALL ELSTRUC(AKTE,AKTS,NV,AK15,ID14S,AK14S,NDSP,FI,PT,
     1              2,IWRIT,IWR)
C     ABOVE PUTS EL.STIFF AKTE IN STRUC STIFF AKTS AND
C     ADDS EFFECTS OF VARIOUS LINEAR SPRINGS
C
        CALL BCON(AKTS,IBC,N,QINC,IWRIT,IWR)
C     ABOVE APPLIES B.CONDITIONS
        CALL CROUT(AKTS,D,NV,IWRIT,IWR)
C     ABOVE FORMS LDL(TRAN) FACTORISATION INTO AKT AND D
        CALL SOLVCR(AKTS,D,QINC,NV,IWRIT,IWR)
C     ABOVE SOLVES EQNS. AND GETS INC. DISPS IN QIN
C
        DO 20 I=1,NV
          PT(I) = PT(I) + QINC(I)
   20   CONTINUE
C     ABOVE UPDATES TOTAL DISPS.
C
        WRITE (6,1002) (PT(I), I=1,NV)
 1002   FORMAT(/,1X,'TOTAL DISPS. ARE',1X,5G15.5,/)
C
C     BELOW FORMS TOTAL FORCE IN BAR
        CALL FORCE(AN,ANIT,E,ARA,AL,X,Z,PT,IWRIT,IWR,
     1             ITYEL,ARN,ALN,POISS)
C     ABOVE ARGUMENTS NOT USED FOR SHALLOW TRUSS
  100 CONTINUE
C
      STOP 'NONLTA'
      END

C-----------------------------------------------------------------------

      SUBROUTINE INPUT(E,ARA,AL,QFI,X,Z,ANIT,IBC,IRE,IWR,AK14S,ID14S,
     1                NDSP,NV,AK15,
     2                ADUM1,IDUM)
C     ARGUMENTS IN LINE ABOVE AND ARRAY X NOT USED FOR SHALLOW TRUSS
C
C     READS INPUT FOR TRUSS ELEMENT
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DIMENSION X(2),Z(2),QFI(NV),IBC(NV),AK14S(4),ID14S(4)
C
      READ(IRE,*) NV,EA,AL,ANIT
      E = EA
      ARA = 1.D0
      WRITE(IWR,1000) NV,EA,AL,ANIT
 1000 FORMAT(/,1X,'NV=NO. OF VARIABLES=',G13.5,/,
     1      1X,'EA=',G13.5,/,
     2      1X,'AL=EL.LENGTH=',G13.5,/,
     3      1X,'ANIT=INIT.FORCE=',G13.5,/)
      IF (NV.NE.4.AND.NV.NE.5) STOP 'INPUT 1000'
      READ(IRE,*) Z(1),Z(2)
      WRITE(IWR,1001) Z(1),Z(2)
 1001 FORMAT(/,1X,'Z COORDINATE OF NODE 1=',G13.5,/,
     1      1X,'Z COORDINATE OF NODE 2=',G13.5,/)
      READ(IRE,*) (QFI(I),I=1,NV)
      WRITE(IWR,1002) (QFI(I),I=1,NV)
 1002 FORMAT(/,1X,'FIXED LOAD OR DISP.VECTOR, QFI=',/,1X,5G13.5,/)
      WRITE(IWR,1008)
 1008 FORMAT(/,1X,'IF IBC(I)-SEE BELOW-=0, VARIABLE=A LOAD',/,
     2      1X,'IF IBC(I)-SEE BELOW-=-1, VARIABLE=A DISP.',/)
      READ(IRE,*) (IBC(I),I=1,NV)
      WRITE(IWR,1003) (IBC(I),I=1,NV)
 1003 FORMAT(/,1X,'BOUND.COND.COUNTER, IBC',/,
     1      1X,'(0 : FREE, 1: REST TO ZERO, -1 : REST TO NON-ZERO)',/,
     2      1X,5G13.5,/)
      READ(IRE,*) NDSP
      IF (NDSP.NE.0) THEN
        READ(IRE,*) (ID14S(I),I=1,NDSP)
        READ(IRE,*) (AK14S(I),I=1,NDSP)
        DO 40 I=1,NDSP
          WRITE(IWR,1004) AK14S(I), ID14S(I)
 1004     FORMAT(/,1X,'LINEAR SPRING OF STIFFNESS',G13.5,/,
     1          1X,'ADDED AT VARIABLE NO.',G13.5,/)
   40   CONTINUE
      ENDIF
C
      IF (NV.EQ.5) THEN
        READ(IRE,*) AK15
        WRITE(IWR,1005) AK15
 1005   FORMAT(/,1X,'LINEAR SPRING BETWEEN VARBLS. 1 AND 5 OF STIFF ',
     1        G13.5,/)
      ENDIF
C
      RETURN
      END
      
C-----------------------------------------------------------------------

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
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION AKT(4,4),FI(4),Z(2),P(4),X(2)
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
        FI(2) = 1.D0 ! TYPO IN BOOK
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
C     COMPUTES TAN STIFF.MATRIX (SEE 2.23)
        AKT(1,1) = 1.D0
        AKT(1,2) = -1.D0
        AKT(1,3) = BET
        AKT(1,4) = -BET
        AKT(2,1) = AKT(1,2)
        AKT(2,2) = 1.D0
        AKT(2,3) = -BET
        AKT(2,4) = BET
        AKT(3,1) = AKT(1,3)
        AKT(3,2) = AKT(2,3)
        AKT(3,3) = BET*BET
        AKT(3,4) = -AKT(3,3)
        AKT(4,1) = AKT(1,4)
        AKT(4,2) = AKT(2,4)
        AKT(4,3) = AKT(3,4)
        AKT(4,4) = BET*BET
        DO 12 I=1,4
          DO 13 J=1,4
            AKT(I,J) = EAL*AKT(I,J)
   13     CONTINUE
   12   CONTINUE
C
C     NOW ADD GEOM. OR INIT STRESS MATRIX (SEE 2.23)
C
        ANL = AN/AL
        AKT(3,3) = AKT(3,3) + ANL
        AKT(3,4) = AKT(3,4) - ANL
        AKT(4,4) = AKT(4,4) + ANL
        AKT(4,3) = AKT(3,4)
        IF (IWRIT.NE.0) THEN
          WRITE (IWR,1001)
 1001     FORMAT (/, 1X, 'TAN.STIFF.MATRIX FOR TRUSS ELEMENT IS', /)
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

C-----------------------------------------------------------------------

      SUBROUTINE ELSTRUC(AKTE,AKTS,NV,AK15,ID14S,AK14S,NDSP,FI,PT,
     1                    IMOD,IWRIT,IWR)
C
C     FOR IMOD=2 OR 3
C     PUTS EL-STIFF MATRIX AKTE(4,4) INTO STRUCT.STIFF AKTS(NV,NV)
C     IF NV = 5, ALSO ADDS IN LINEAR SPRING AK15 BETWEEN VARBLS.1&5
C     ALSO ADDS IN NDSP EARTHED LINEAR SPRINGS FOR VARBLS.1-4
C     USING PROPERTIES IN AK14S(4) AND DEGS.OF F.IN IDSPS(4)
C     THROUGHOUT ONLY WORKS WITH UPPER TRIANGLE
C     FOR IMOD=1 OR 3
C     MODIFIES INTERNAL FORCE VECT., FI TO INCLUDE EFFECTS FROM
C     VARIOUS LINEAR SPRINGS USING TOTAL DISPS., PT.
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION AKTE(4,4),AKTS(NV,NV),ID14S(4),AK14S(4)
      DIMENSION FI(NV),PT(NV)
C
      IF (IMOD.NE.2) THEN
C     MODIFY FORCES
        IF (NDSP.NE.0) THEN ! TYPO IN BOOK
C     FOR EARTHED SPRINGS
          DO 40 I=1,NDSP
            IDS = ID14S(I)
            ! INT. FORCE += X   <=>   EXT. FORCE -= X
            FI(IDS) = FI(IDS) + AK14S(I)*PT(IDS)
   40     CONTINUE
        ENDIF
C
        IF (NV.EQ.5) THEN
C     MODIFY FOR SPRING BTWEEN VARBLS. 1 AND 5
          ! INT. FORCE += X   <=>   EXT. FORCE -= X
          FI(1) = FI(1) + AK15*(PT(1)-PT(5))
          FI(5) = AK15*(-PT(1)+PT(5))
        ENDIF
C
        IF (IWRIT.NE.0) WRITE (IWR,1002) FI
 1002   FORMAT(/,1X,'STR.INT.FORCE VECT IS',1X,5G13.5,/)
C
      ENDIF
C
      IF (IMOD.NE.1) THEN
C     WORK ON STIFFNESS MATRIX; CLEAR STRUCT.STIFFNESS MATRIX
        DO 10 I=1,NV
          DO 11 J=1,NV
            AKTS(I,J) = 0.D0
   11     CONTINUE
   10   CONTINUE
C
C     INSERT EL.STIFFNESS MATRIX
        DO 20 I=1,4
          DO 21 J=1,4
            AKTS(I,J) = AKTE(I,J)
   21     CONTINUE
   20   CONTINUE
C
C     SPRING BETWEEN VARBLS.1&5
        IF (NV.EQ.5) THEN
          AKTS(1,1) = AKTS(1,1) + AK15
          AKTS(1,5) = AKTS(1,5) - AK15
          AKTS(5,5) = AKTS(5,5) + AK15
          AKTS(5,1) = AKTS(1,5)
        ENDIF
C
C       EARTHED SPRINGS FOR VARBLS.1-4
        IF (NDSP.NE.0) THEN
          DO 30 I=1,NDSP
            IDS = ID14S(I)
            AKTS(IDS,IDS) = AKTS(IDS,IDS) + AK14S(I)
   30     CONTINUE
        ENDIF
C
        IF (IWRIT.NE.0) THEN
          WRITE (IWR,1001)
 1001     FORMAT(/,1X,'FULL STRUCT.TAN.STIFF. IS',/)
          DO 50 I=1,NV
            WRITE (IWR,67) (AKTS(I,J), J=1,NV)
   50     CONTINUE ! DEPRECATED CODING STYLE IN BOOK
   67     FORMAT(1X,7G13.5)
        ENDIF
C
      ENDIF
C
C
      RETURN
      END

C-----------------------------------------------------------------------

      SUBROUTINE BCON(AK,IBC,N,F,IWRIT,IWR)
C     APPLIES BOUNDARY CONDITIONS TO MATRIX AK AS WELL AS
C     ALTERING 'LOAD VECTOR', F FOR PRESCRIBED DISPLACEMENTS.
C     BY SETTING DIAG = 1. AND ROW AND COL TO ZERO IN REST.
C     USES COUNTER IBC WHICH IS 0 IF FREE, 1 IF REST. TO ZERO,
C     -1 IF REST. TO NON-ZERO VALUE.
C     ON ENTRY F HAS LOADS FOR FREE ARIABLES AND DISPLACEMENTS FOR
C     REST. (POSSIBLY ZERO) VARIABLES
C     ON EXIT THE LATTER ARE UNCHANGED BUT LOADS ARE ALTERED
C
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      DIMENSION IBC(N) ! TYPO IN BOOK
      DIMENSION AK(N,N),F(N)
C
C     ! SIMPLER VERSION OF CODE (COMPARED TO TEXTBOOK).
C
C     ! FIRST, F WILL BE MODIFIED.
      IPRS = 0
      DO 40 I=1,N
        II = IBC(I)
        DO 50 J=I+1,N ! TYPO IN BOOK
          JJ = IBC(J)
          ! Q_F(I) -= K_FP(I,J)*P_P(J) ~ EINSTIEN NOTATION
          ! EACH Q_F(I) SHOULD BE SUBTRACTED BY K_FP(I,J)*P_P(J)
          ! FOR EVERY J, BUT WE ARE LOOKING FOR ONLY UPPER TRIANGLE.
          ! SO, IF K_FP, Q_F(I) -= K_FP(I,J)*P_P(J), AND 
          !     IF K_PF, Q_F(J) -= K_PF(I,J)*P_P(I)
          IF (II.EQ.0.AND.JJ.NE.0) THEN
            ! K_FP
            IPRS = 1
            F(I) = F(I) - AK(I,J)*F(J)
          ENDIF
          IF (II.NE.0.AND.JJ.EQ.0) THEN
            ! K_PF
            F(J) = F(J) - AK(I,J)*F(I)
          ENDIF 
   50   CONTINUE
   40 CONTINUE
C
C     ! NEXT, AK WILL BE MODIFIED.
      DO 10 I=1,N
        II = IBC(I)
        DO 20 J=I,N
          JJ = IBC(J)
          IF (II.NE.0.AND.JJ.NE.0) THEN
            ! K_PP
            IF (I.EQ.J) THEN
              AK(I,J) = 1.D0
            ELSE
              AK(I,J) = 0.D0
              AK(J,I) = 0.D0
            ENDIF 
          ENDIF
          IF ((II.EQ.0.AND.JJ.NE.0) .OR. (II.NE.0.AND.JJ.EQ.0)) THEN
            ! K_FP, K_PF
            AK(I,J) = 0.D0
            AK(J,I) = 0.D0
          ENDIF
   20   CONTINUE
   10 CONTINUE
C
      IF (IWRIT.NE.0) THEN
        WRITE (IWR,1000)
 1000   FORMAT(/,1X,'STIFF.MAT.AFTER B.CONDS. IS'/)
        DO 30 I=1,N
          WRITE(IWR,67) (AK(I,J),J=1,N)
   67     FORMAT(1X,7G12.5)
   30   CONTINUE
        IF (IPRS.EQ.1) WRITE(IWR,1001) F
 1001   FORMAT(/,1X,'MODIFIED LOAD VECTOR AFTER B CONDS IS',
     1          1X,5G13.4,/)
      ENDIF
C
      RETURN
      END

C-----------------------------------------------------------------------

      SUBROUTINE CROUT(AK,D,N,IWRIT,IWR)
C
C     INPUTS AK(N,N); OUTPUTS UPPER TRIANGLE IN AK AND DIAG
C     PIVOTS IN D(N)
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION AK(N,N),D(N),AL(N,N),AU(N,N)
C     ! REWROTE WHOLE CODE
C     ! REFERENCE:
C       "NUMERICAL RECIPES IN FORTRAN",WILLIAM H. PRESS ET AL, CH 2.3
C     ! AK IS ALWAYS POS. DEFINITE, SO NO NEED TO CONSIDER PERMUTATION
      DO 1 I=1,N
        AL(I,I) = 1.D0
    1 CONTINUE
      DO 2 J=1,N
        DO 3 I=1,J ! EQ 2.3.12
          AU(I,J)=AK(I,J)
          DO 4 K=1,I-1
            AU(I,J) = AU(I,J) - AL(I,K)*AU(K,J)
    4     CONTINUE
    3   CONTINUE
        DO 5 I=J+1,N ! EQ 2.3.13
          AL(I,J) = AK(I,J)/AU(J,J)
          DO 6 K=1,J-1
            AL(I,J) = AL(I,J) - AL(I,K)*AU(K,J)/AU(J,J)
    6     CONTINUE
    5   CONTINUE
    2 CONTINUE
      DO 7 I=1,N
        DO 8 J=1,N
          AK(I,J) = AL(I,J)
    8   CONTINUE
    7 CONTINUE
      DO 9 I=1,N
        D(I) = AU(I,I)
    9 CONTINUE
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

C-----------------------------------------------------------------------

      SUBROUTINE SOLVCR(AK,D,Q,N,IWRIT,IWR)
C
C     APPLIES FORWARD AND BACK CROUT SUBS ON Q
C
      DOUBLE PRECISION AK(N,N),D(N),Q(N)
      INTEGER N,I,J,L,IWRIT,IWR
C
C     FORWARD SUBS
      DO 1 J=2,N
        DO 2 L=1,J-1
          Q(J) = Q(J) - AK(L,J)*Q(L)
    2   CONTINUE
    1 CONTINUE
      IF (IWRIT.NE.0) THEN
        WRITE (IWR,1000) (Q(I), I=1,N)
 1000   FORMAT(/,1X,'DISP.INCS AFTER FORWARD SUBS.ARE',1X,7G15.5,/)
      ENDIF
C
C     BACK SUBS.
      DO 3 I=1,N
        Q(I) = Q(I)/D(I)
    3 CONTINUE
C
      DO 4 JJ=2,N
        J = N + 2 - JJ
        DO 5 L=1,J-1
          Q(L) = Q(L) - AK(L,J)*Q(J)
    5   CONTINUE
    4 CONTINUE
C
      IF (IWRIT.NE.0) THEN
        WRITE (IWR,1001) (Q(I), I=1,N)
 1001   FORMAT(/,1X,'DISP INCS.AFTER BACKWARD SUBS.ARE',1X,7G15.5,/)
      ENDIF
C
      RETURN
      END

C-----------------------------------------------------------------------

      SUBROUTINE FORCE(AN,ANIT,E,ARA,AL,X,Z,P,IWRIT,IWR,
     1                 ITUM,ADUM1,ADUM2,ADUM3)
C     ARGUMENTS IN LINE ABOVE AND ARRAY X NOT USED FOR SHALLOW TRUSS
C
C     COMPUTES INTERNAL.FORCE IN A SHALLOW TRUSS ELEMENT
C     USING (2.7) AND (2.8)
      IMPLICIT DOUBLE PRECISION (A-H, P-Z)
      DIMENSION Z(2),P(4),X(2)
C
      EA = E*ARA
      EAL = EA/AL
      U21 = P(2) - P(1)
      W21 = P(4) - P(3)
      Z21 = Z(2) - Z(1)
      AN = U21 + (Z21*W21/AL) + 0.5D0*(W21*W21/AL) ! TYPO IN BOOK
      AN = EAL*AN + ANIT ! ANIT IS 0 IN CHAPTER 2
      IF (IWRIT.NE.0) WRITE (IWR,1000) AN
 1000 FORMAT(/,1X,'AXIAL FORCE AN = ',G13.5/)
      RETURN
      END