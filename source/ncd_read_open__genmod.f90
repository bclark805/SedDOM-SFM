        !COMPILER-GENERATED INTERFACE MODULE: Tue Aug  1 12:58:31 2017
        MODULE NCD_READ_OPEN__genmod
          INTERFACE 
            SUBROUTINE NCD_READ_OPEN(INFILE,UL,VL,WTSL,UARD_OBCNL,      &
     &XFLUX_OBCL,DTFAL,KHL,ELL,TL,SL,HO)
              USE MOD_LIMS, ONLY :                                      &
     &          MTLOC,                                                  &
     &          NTLOC,                                                  &
     &          KBM1,                                                   &
     &          KB
              USE MOD_SIZES, ONLY :                                     &
     &          NOBTY
              CHARACTER(LEN=1024), INTENT(IN) :: INFILE
              REAL(KIND=8), INTENT(OUT) :: UL(0:NTLOC,KB)
              REAL(KIND=8), INTENT(OUT) :: VL(0:NTLOC,KB)
              REAL(KIND=8), INTENT(OUT) :: WTSL(0:MTLOC,KB)
              REAL(KIND=8), INTENT(OUT) :: UARD_OBCNL(0:NOBTY+1)
              REAL(KIND=8), INTENT(OUT) :: XFLUX_OBCL(0:NOBTY,KBM1)
              REAL(KIND=8), INTENT(OUT) :: DTFAL(0:MTLOC)
              REAL(KIND=8), INTENT(OUT) :: KHL(0:MTLOC,KB)
              REAL(KIND=8), INTENT(OUT) :: ELL(0:MTLOC)
              REAL(KIND=8), INTENT(OUT) :: TL(0:MTLOC,KBM1)
              REAL(KIND=8), INTENT(OUT) :: SL(0:MTLOC,KBM1)
              INTEGER(KIND=4), INTENT(IN) :: HO
            END SUBROUTINE NCD_READ_OPEN
          END INTERFACE 
        END MODULE NCD_READ_OPEN__genmod
