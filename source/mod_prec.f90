!===============================================================================!
! DEFINE FLOATING POINT PRECISION USING KIND                                    !
!===============================================================================!
MODULE MOD_PREC
   IMPLICIT NONE




!--Single Precision Coding------------------------------------------------------!







!--Double Precision Coding------------------------------------------------------!
   INTEGER, PARAMETER :: SP = SELECTED_REAL_KIND(12,300)






  
   INTEGER, PARAMETER :: DP     = SELECTED_REAL_KIND(12,300)



   
! KURT GLAESEMANN SEPT 22 2009 - USE CORRECT PRECISION WHEN READ NETCDF
  INTEGER, PARAMETER :: CDF_PREC = 4

! KURT GLAESEMANN 14 April 2015 - USE CORRECT PRECISION WHEN USING MPI ON CDF DATA




END MODULE MOD_PREC


