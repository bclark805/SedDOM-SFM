        !COMPILER-GENERATED INTERFACE MODULE: Tue Aug  1 12:58:25 2017
        MODULE BRACKET__genmod
          INTERFACE 
            SUBROUTINE BRACKET(TMAP,STIME,L1,L2,FACT,BACT,IERR)
              USE MOD_TYPES, ONLY :                                     &
     &          BC
              TYPE (BC), INTENT(IN) :: TMAP
              REAL(KIND=8), INTENT(IN) :: STIME
              INTEGER(KIND=4), INTENT(OUT) :: L1
              INTEGER(KIND=4), INTENT(OUT) :: L2
              REAL(KIND=8), INTENT(OUT) :: FACT
              REAL(KIND=8), INTENT(OUT) :: BACT
              INTEGER(KIND=4), INTENT(OUT) :: IERR
            END SUBROUTINE BRACKET
          END INTERFACE 
        END MODULE BRACKET__genmod
