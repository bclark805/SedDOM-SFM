        !COMPILER-GENERATED INTERFACE MODULE: Tue Aug  1 12:58:39 2017
        MODULE VDIF_WQM__genmod
          INTERFACE 
            SUBROUTINE VDIF_WQM(F)
              USE MOD_LIMS, ONLY :                                      &
     &          MTLOC,                                                  &
     &          KBM1,                                                   &
     &          MLOC,                                                   &
     &          KB,                                                     &
     &          KBM2
              REAL(KIND=8) :: F(0:MTLOC,KBM1,44)
            END SUBROUTINE VDIF_WQM
          END INTERFACE 
        END MODULE VDIF_WQM__genmod
