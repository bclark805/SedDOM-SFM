MODULE WC_DOM

! This module will be used to calculate the WATER COLUMN Dissolved Organic Matter kinetics
! all DOM kinetics will be moved from mod_kin where allowable to this portion 
! this allows a dynamic simulation involving light reactions with
! the CDOM pool.  In the Old code, there was refractory and labile DOM in the water column.
! We are taking those pools and recoding to be in conjunction with the sediment model to have 3 G classes
! based on lability.
!
!
!
! B Clark August 2015
! Horn Point Labs, UMCES
!
!
!
!
!
USE MOD_SIZES, ONLY : MGL,  &  !global node and element indices
					  NGL    

USE MOD_LIMS, ONLY :  NTLOC, &              !!TOTAL OF LOCAL INTERNAL + HALO ELEMENTS
                      MTLOC, &             !!TOTAL OF LOCAL INTERNAL + HALO NODES
		              MLOC,  &			   
					  KBM1,	 &
					  MYID,  &
					  NPROCS
USE MOD_HYDROVARS, ONLY: D, DZ








        
					
USE MOD_CONTROL, ONLY: MSR, SERIAL

USE MOD_PREC, ONLY : SP

USE MOD_SED_DOM_EXCHANGE, ONLY:   SED_DOM_FLAG,         &! Sed_dom logical switch
                                JWCDOC1,JWCDOC2,JWCDOC3, &
					            JWNCDOC1,JWNCDOC2,JWNCDOC3,  &
					            JWCDON1,JWCDON2,JWCDON3,  &
					            JWNCDON1,JWNCDON2,JWNCDON3,  &
					            JWCDOP1, JWCDOP2, JWCDOP3,  &
					            JWNCDOP1,JWNCDOP2,JWNCDOP3  

!USE MOD_SED_DOM ONLY: SED_DOM,  &! Sed_dom logical switch
IMPLICIT NONE
SAVE
INTEGER  :: I,K

LOGICAL :: PHOTODEG = .FALSE.  ! switch to turn on a simple photodegradation carbon sink 

CHARACTER(len=:),allocatable :: SANT

REAL(SP)  :: SALTC !TEMP SALT VARIABLE FOR CALCULATING DOC COAGULATION
REAL(SP) :: KDOC1_in, KDOC2_in, KDOC3_in, &   ! remineralizaton coefficients
			KDON1_in, KDON2_in, KDON3_in, &   ! read in from input file
			KDOP1_in, KDOP2_in, KDOP3_in
			
REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: KDOC1, KDOC2, KDOC3,   &  ! REMINERALIZATION RATES AFTER TEMPERATURE AFFECTS AND NUTRIENT LIMITAIOTN ARE TAKEN INTO ACCOUNT
										 KDON1, KDON2, KDON3,   &  ! calcualted in mod_kin 
										 KDOP1, KDOP2, KDOP3
			
!REAL(SP) :: DENIT_FCDOC, DENIT_FCDON, DENIT_FCDOP  ! FRACTIONATIONS OF DENITRIFICATION CONSUMPTION FOR COLORED DOC, DON AND DOP

REAL(SP) KHR1,KHR2,KHR3  ! Half saturation constants for Algal DOC production relating to oxygen availability (g O2/m^3), read from input file

            !G1     !G2		!G3
REAL(SP) :: FCD11, FCD12, FCD13, &    ! Algal 1 CDOC production fraction into each of the three colored G classes, read from input file
			FCD21, FCD22, FCD23, &    ! ALGAL 2 CDOC
			FNCD11, FNCD12, FNCD13, &    ! Algal 1 NCDOC production fraction into each of the three colored G classes, read from input file
			FNCD21, FNCD22, FNCD23, &    ! ALGAL 2 NCDOC
		!	FCD31, FCD32, FCD33, &    ! ALGAL 3 CDOC
			FCND11, FCND12, FCND13, &    ! ALGAL 1 CDON 
			FCND21, FCND22, FCND23, &    ! ALGAL 2 CDON
			FNCND11, FNCND12, FNCND13, &    ! ALGAL 1 NCDON 
			FNCND21, FNCND22, FNCND23, & !    ! ALGAL 2 NCDON
		!	FND31, FND32, FND33, &    ! ALGAL 3 CDON
			FCPD11, FCPD12, FCPD13, &    ! ALGAL 1 CDOP
			FCPD21, FCPD22, FCPD23, &!    ! ALGAL 2 CDOP
			FNCPD11, FNCPD12, FNCPD13, &    ! ALGAL 1 NCDOP
			FNCPD21, FNCPD22, FNCPD23!, &    ! ALGAL 2 NCDOP			
		!	FPD31, FPD32, FPD33       ! ALGAL 3 CDOP
			
 REAL(SP) :: FCD1 ,&!  DOC production fraction by algae 1          
             FCD2    ! DOC production fraction by algae 2
			!FCD3   
			 
 REAL(SP):: FPD1,     &  !DOP production fraction by algae 1
            FPD2!,    & !DOP production fraction by algae 2
           !FPD3
REAL(SP)::  FND1,    &  !DON production fraction by algae 1
            FND2  !,  & !DON production fraction by algae 2
		   !FND3


REAL(SP) :: FHDRC1, FHDRC2, FHDRC3, & !FRACTION OF TOTAL HYDROLYZED POC THAT IS COLORED IN G1,2,3
            FHDRNC1, FHDRNC2, FHDRNC3, & !FRACTION OF TOTAL HYDROLYZED POC THAT IS NonCOLORED IN G1,2,3
			FHDRCN1, FHDRCN2, FHDRCN3, & !FRACTION OF TOTAL HYDROLYZED PON THAT IS COLORED IN G1,2,3
			FHDRNCN1, FHDRNCN2, FHDRNCN3, & !FRACTION OF TOTAL HYDROLYZED PON THAT IS NonCOLORED IN G1,2,3
			FHDRCP1, FHDRCP2, FHDRCP3, &   !FRACTION OF TOTAL HYDROLYZED POP THAT IS COLORED IN G1,2,3	
			FHDRNCP1, FHDRNCP2, FHDRNCP3    !FRACTION OF TOTAL HYDROLYZED POP THAT IS NON-COLORED IN G1,2,3	
			
REAL(SP) :: FDOC1SZ,FDOC2SZ,FDOC3SZ,  &    ! SMALL ZOOPLANKTON TOTAL DOC FRACTIONATION [0,1]  ! READ FROM INPUT
			FDOC1LZ,FDOC2LZ,FDOC3LZ,  &    ! LARGE ZOOPLANKTON TOTAL DOC FRACTIONATION [0,1]
			
			FDON1SZ,FDON2SZ,FDON3SZ,  &    ! SMALL ZOOPLANKTON TOTAL DON FRACTIONATION [0,1]
			FDON1LZ,FDON2LZ,FDON3LZ,  &    ! LARGE ZOOPLANKTON TOTAL DON FRACTIONATION [0,1]
			
			FDOP1SZ,FDOP2SZ,FDOP3SZ,  &    ! SMALL ZOOPLANKTON TOTAL DOP FRACTIONATION [0,1]
			FDOP1LZ,FDOP2LZ,FDOP3LZ        ! LARGE ZOOPLANKTON TOTAL DOP FRACTIONATION [0,1]		
			
REAL(SP) :: FCDOC1SZ,FCDOC2SZ,FCDOC3SZ,  &    ! SMALL ZOOPLANKTON DOC COLORED FRACTIONATION [0,1]  ! READ FROM INPUT
			FCDOC1LZ,FCDOC2LZ,FCDOC3LZ,  &    ! LARGE ZOOPLANKTON DOC COLORED FRACTIONATION [0,1]
			
			FCDON1SZ,FCDON2SZ,FCDON3SZ,  &    ! SMALL ZOOPLANKTON DON COLORED FRACTIONATION [0,1]
			FCDON1LZ,FCDON2LZ,FCDON3LZ,  &    ! LARGE ZOOPLANKTON DON COLORED FRACTIONATION [0,1]
			
			FCDOP1SZ,FCDOP2SZ,FCDOP3SZ,  &    ! SMALL ZOOPLANKTON DOP COLORED FRACTIONATION [0,1]
			FCDOP1LZ,FCDOP2LZ,FCDOP3LZ        ! LARGE ZOOPLANKTON DOP COLORED FRACTIONATION [0,1]
			
REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: COAGC, COAGN, COAGP  ! COAGULATION OF REFRACTORY DOM THAT IS PASSED BACK TO MOD_KIN FOR POC FLUXES
REAL(SP)  :: COAG	
REAL(SP) :: DOM1, DOM2!, !DOM3   ! TEMP VARS TO FIND THE ALGAL GROUP 1 AND 2 DOM PRODUCTION

REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: DOCSZ1, DOCSZ2, DOCSZ3,&  !SMALL ZOOPLANKOTN G1,G2 and G3 DOC POOLS	 g C/m^3	
										 DOCLZ1, DOCLZ2,DOCLZ3, &  !LARGE ZOOPLANKTON G1 AND G2 and G3 DOC POOLS	 g C/m^3	
										 DONSZ1, DONSZ2,DONSZ3, &  !SMALL ZOOPLANKOTN G1 AND G2 and G3 DON POOLS	 g C/m^3	
										 DONLZ1, DONLZ2, DONLZ3,&  !LARGE ZOOPLANKTON G1 AND G2 and G3  DON POOLS	 g C/m^3
										 DOPSZ1, DOPSZ2,DOPSZ3, &  !SMALL ZOOPLANKOTN G1 AND G2 and G3 DOP POOLS	 g C/m^3	
										 DOPLZ1, DOPLZ2,  DOPLZ3  !LARGE ZOOPLANKTON G1 AND G2 and G3 DOP POOLS	 g C/m^3
!!!! Define all the variables for dependencies of this mod;

REAL(SP), ALLOCATABLE :: CRRATE_SZ(:,:), NRRATE_SZ(:,:), PRRATE_SZ(:,:),&   ! moved from subroutine zooplankton in mod kin to make an array to be passed between subroutines with dependencies
						 CRRATE_LZ(:,:), NRRATE_LZ(:,:), PRRATE_LZ(:,:)     !large zooplankton   g / m^3 / sec

REAL(SP), ALLOCATABLE :: FRD1(:,:), FRD2(:,:)!,    &        ! Carbon fraction of zooplankton consumption of DOC
						 !FRN1SZ(:,:), FRN2SZ(:,:), FRN1LZ(:,:), FRN2LZ(:,:), &    ! fraction of zooplankton consumption of DON, currently not used
						 !FRP1SZ(:,:),FRP2SZ(:,:), FRP1LZ(:,:), FRP2LZ(:,:)        !fraction of zooplankton consumption of organic phosphorus 

REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: MNLDOC,MNLDON,MNLDOP ! AMOUNT OF DOC THAT IS REMINERALIZED, PASSED TO OXYGEN SUBROUTINE TO FIND THE OXYGEN UPTAKE VIA BACTERIAL RESPIRAATION
REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: WC_NCDOC1,WC_NCDOC2,WC_NCDOC3,   &    ! Water Column Non-COlored Dissolved organic Carbon g C/m^3
								         WC_CDOC1,WC_CDOC2,WC_CDOC3,   &       ! Water column colored dissolved organic carbon g C/m^3
										 WC_NCDON1,WC_NCDON2,WC_NCDON3,   &    ! Water column non-colored dissolved organic nitrogen g N/m^3
										 WC_CDON1,WC_CDON2,WC_CDON3,   &       ! Water column colored dissolved organic nitrogen g N/m^3
										 WC_NCDOP1,WC_NCDOP2,WC_NCDOP3,   &    ! Water column colored dissolved organic phosphorus g P/m^3
										 WC_CDOP1,WC_CDOP2,WC_CDOP3            ! Water column colored dissolved organic phosphorus g P/m^3''

REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: CP1C, CP2C, CP3C	! Carbon algal basal matabolism coefficient, calculated in wqm_kin and passed back to solve DOC here									 
REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: NP1, NP2, NP3   ! ALGAL 1 AND 2 PRODUCTION OF DON	! CALCULATED IN MOD KIN
REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: PP1, PP2, PP3  ! ALGAL PRODUCTION OF DOP  ! CALCULATED IN MOD KIN
										 
REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: DOCBM1,DOCBM2!,DOCBM3										 
										 
REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: DTWNCDOC1,DTWNCDOC2,DTWNCDOC3,  &   ! DT ORGANICS, ADD INTO TOTAL POOL AND TIME INTEGRATE
							             DTWCDOC1,DTWCDOC2,DTWCDOC3,     &
										 DTWNCDON1,DTWNCDON2,DTWNCDON3,  &
										 DTWCDON1,DTWCDON2,DTWCDON3,	  &
										 DTWNCDOP1,DTWNCDOP2,DTWNCDOP3,  &
										 DTWCDOP1,DTWCDOP2,DTWCDOP3

REAL(SP), ALLOCATABLE, DIMENSION(:,:) :: ALGCAR 										 
										 
REAL(SP)  :: MNLDOM1, MNLDOM2, MNLDOM3       ! mineralization rates for DOM, positive is a DOC loss g /m^3/day



   REAL(SP), ALLOCATABLE :: ICDOM_abs(:,:,:,:) ,& ! The absorbance of cdom 1,2,3 for each wavelength, passed back from the OWQ mod (MTLOC,KBM1,NWAVEL,3)  
							TOTAL_CDOM_ABS(:,:,:), &! The total absorption spectra for the 3 classes of CDOM summed (MTLOC,KBM1,NWAVEL)					! m^-1
							N_photons(:,:,:,:)   !Loss in photon flux (mols Quanta/m^2/s) absorbed by each CDOM at each wavelength (MTLOC, KBM1, NWAVEL, 3)
   REAL(SP), ALLOCATABLE :: APQY32(:) ,  &! Apparent quantum yields for CDOM 1 2 and 3 (NWAVEL,3) ( mg C / mols Quanta ), constant
							APQY31(:) ,  & ! ALL READ IN FROM INPUT FILE wcdom_apqy.npt
							APQY30(:) ,  &
							APQY3N(:) ,  &
							
							APQY21(:) ,  &
							APQY20(:) ,  &
							APQY2N(:) ,  &
							
							APQY10(:) ,  &
							APQY1N(:)  
  REAL(SP) :: DPD31,DPD32,DPD30,DPD3N,DPD21,DPD20,DPD2N,DPD10,DPD1N   ! our temp vars that integrate the spectral photodegradatin of carbon						
  REAl(SP) :: FPDC31,FPDC32,FPDC33,    & ! fraction of carbon photodegraded from CDOC3 --> NCDOC1,2,3 (unitless)		
			  FPDC21,FPDC22,FPDC23,     & !same as above but from CDOC2
			 
			  FPDC11,FPDC12,FPDC13 	 !SAME AS ABOVE BY FROM CDOC1		
			  
			  
 REAL(SP),ALLOCATABLE ::  PHOTOCHEM_DOC_ACCUM(:,:,:),   &
						  PHOTOCHEM_DOC_ACCUM_GL(:,:,:)   ! B Clark add variable to accumulate DOC fluxes over time for output

 
			 INTEGER  :: NWAVELx=431 , j
			 
			 
!allocate all the variables
CONTAINS

SUBROUTINE WC_DOM_ALLOCATE

 !USE MOD_SIZES, ONLY : MGL,  &  !global node and element indices
!					  NGL    

 ! USE MOD_LIMS, ONLY :  NTLOC, &              !!TOTAL OF LOCAL INTERNAL + HALO ELEMENTS
 !                     MTLOC, &             !!TOTAL OF LOCAL INTERNAL + HALO NODES
!		              MLOC,  &			   
!					  KBM1,	 &
!					  MYID,  &
!					  NPROCS

  !KBM1 = 1
  !MTLOC = 1
    ALLOCATE(KDOC1(MTLOC,KBM1)) ; KDOC1 = 0.0
	ALLOCATE(KDOC2(MTLOC,KBM1)) ; KDOC2 = 0.0
	ALLOCATE(KDOC3(MTLOC,KBM1)) ; KDOC3 = 0.0
	ALLOCATE(KDON1(MTLOC,KBM1)) ; KDON1 = 0.0
	ALLOCATE(KDON2(MTLOC,KBM1)) ; KDON2 = 0.0
	ALLOCATE(KDON3(MTLOC,KBM1)) ; KDON3 = 0.0
	ALLOCATE(KDOP1(MTLOC,KBM1)) ; KDOP1 = 0.0
	ALLOCATE(KDOP2(MTLOC,KBM1)) ; KDOP2 = 0.0
	ALLOCATE(KDOP3(MTLOC,KBM1)) ; KDOP3 = 0.0
	
	
    ALLOCATE(DOCSZ1(MTLOC,KBM1)); DOCSZ1 = 0.0
	ALLOCATE(DOCLZ1(MTLOC,KBM1)); DOCLZ1 = 0.0
	ALLOCATE(DOCSZ2(MTLOC,KBM1)); DOCSZ2 = 0.0
	ALLOCATE(DOCLZ2(MTLOC,KBM1)); DOCLZ2 = 0.0
	ALLOCATE(DOCSZ3(MTLOC,KBM1)); DOCSZ3 = 0.0
	ALLOCATE(DOCLZ3(MTLOC,KBM1)); DOCLZ3 = 0.0
	
	ALLOCATE(DONSZ1(MTLOC,KBM1)); DONSZ1 = 0.0
	ALLOCATE(DONLZ1(MTLOC,KBM1)); DONLZ1 = 0.0
	ALLOCATE(DONSZ2(MTLOC,KBM1)); DONSZ2 = 0.0
	ALLOCATE(DONLZ2(MTLOC,KBM1)); DONLZ2 = 0.0
	ALLOCATE(DONSZ3(MTLOC,KBM1)); DONSZ3 = 0.0
	ALLOCATE(DONLZ3(MTLOC,KBM1)); DONLZ3 = 0.0
	
	ALLOCATE(DOPSZ1(MTLOC,KBM1)); DOPSZ1 = 0.0
	ALLOCATE(DOPLZ1(MTLOC,KBM1)); DOPLZ1 = 0.0
	ALLOCATE(DOPSZ2(MTLOC,KBM1)); DOPSZ2 = 0.0
	ALLOCATE(DOPLZ2(MTLOC,KBM1)); DOPLZ2 = 0.0
	ALLOCATE(DOPSZ3(MTLOC,KBM1)); DOPSZ3 = 0.0
	ALLOCATE(DOPLZ3(MTLOC,KBM1)); DOPLZ3 = 0.0
	
    ALLOCATE(CRRATE_SZ(MTLOC,KBM1)) ;   CRRATE_SZ = 0.0! small zoos 
	ALLOCATE(NRRATE_SZ(MTLOC,KBM1))	;	NRRATE_SZ = 0.0
	ALLOCATE(PRRATE_SZ(MTLOC,KBM1)) ;   PRRATE_SZ = 0.0
	ALLOCATE(CRRATE_LZ(MTLOC,KBM1)); 	CRRATE_LZ = 0.0	! large zoos
	ALLOCATE(NRRATE_LZ(MTLOC,KBM1));    NRRATE_LZ = 0.0
	ALLOCATE(PRRATE_LZ(MTLOC,KBM1))	;   PRRATE_LZ = 0.0
	
    ALLOCATE(FRD1(MTLOC,KBM1)) ; FRD1 = 0.0  ! DOC CONSUMED BY ZOOPLANKTON
	ALLOCATE(FRD2(MTLOC,KBM1)) ;  FRD2 = 0.0

	!CURRENTLY NOT USED, SMALL ZOOPLANKTON DON'T CONSUME NITROGEN AND PHOSPHORUS IN THE MODEL, WHICH IS WEIRD
	
	!ALLOCATE(FRN1SZ(MTLOC,KBM1))        ! DON CONSUMED BY ZOOPLANKTON
	!ALLOCATE(FRN2SZ(MTLOC,KBM1))

	!ALLOCATE(FRP1SZ(MTLOC,KBM1)) ! DOP CONSUMED BY ZOOPLANKTON
	!ALLOCATE(FRP2SZ(MTLOC,KBM1))
	ALLOCATE(WC_NCDOC1(MTLOC,KBM1)); WC_NCDOC1 = 0.0 

	ALLOCATE(WC_NCDOC2(MTLOC,KBM1)); WC_NCDOC2 = 0.0 
	ALLOCATE(WC_NCDOC3(MTLOC,KBM1)); WC_NCDOC3 = 0.0 
	ALLOCATE(WC_CDOC1(MTLOC,KBM1)); WC_CDOC1 = 0.0 
	ALLOCATE(WC_CDOC2(MTLOC,KBM1)); WC_CDOC2 = 0.0 
	ALLOCATE(WC_CDOC3(MTLOC,KBM1)); WC_CDOC3 = 0.0 
	
	ALLOCATE(WC_NCDON1(MTLOC,KBM1)); WC_NCDON1 = 0.0
	ALLOCATE(WC_NCDON2(MTLOC,KBM1)); WC_NCDON2 = 0.0
	ALLOCATE(WC_NCDON3(MTLOC,KBM1)); WC_NCDON3 = 0.0
	ALLOCATE(WC_CDON1(MTLOC,KBM1)); WC_CDON1 = 0.0
	ALLOCATE(WC_CDON2(MTLOC,KBM1)); WC_CDON2 = 0.0
	ALLOCATE(WC_CDON3(MTLOC,KBM1)); WC_CDON3 = 0.0
	
	ALLOCATE(WC_NCDOP1(MTLOC,KBM1)); WC_NCDOP1 = 0.0
	ALLOCATE(WC_NCDOP2(MTLOC,KBM1)); WC_NCDOP2 = 0.0
	ALLOCATE(WC_NCDOP3(MTLOC,KBM1)); WC_NCDOP3 = 0.0
	ALLOCATE(WC_CDOP1(MTLOC,KBM1)); WC_CDOP1 = 0.0
	ALLOCATE(WC_CDOP2(MTLOC,KBM1)); WC_CDOP2 = 0.0
	ALLOCATE(WC_CDOP3(MTLOC,KBM1)); WC_CDOP3 = 0.0
	
	ALLOCATE(DTWNCDOC1(MTLOC,KBM1)); DTWNCDOC1 = 0.0
	ALLOCATE(DTWNCDOC2(MTLOC,KBM1)); DTWNCDOC2 = 0.0
	ALLOCATE(DTWNCDOC3(MTLOC,KBM1)); DTWNCDOC3 = 0.0
	ALLOCATE(DTWCDOC1(MTLOC,KBM1)); DTWCDOC1 = 0.0
	ALLOCATE(DTWCDOC2(MTLOC,KBM1)); DTWCDOC2 = 0.0
	ALLOCATE(DTWCDOC3(MTLOC,KBM1)); DTWCDOC3 = 0.0
	
	ALLOCATE(DTWNCDON1(MTLOC,KBM1)); DTWNCDON1 = 0.0
	ALLOCATE(DTWNCDON2(MTLOC,KBM1)); DTWNCDON2 = 0.0
	ALLOCATE(DTWNCDON3(MTLOC,KBM1)); DTWNCDON3 = 0.0
	ALLOCATE(DTWCDON1(MTLOC,KBM1)); DTWCDON1 = 0.0
	ALLOCATE(DTWCDON2(MTLOC,KBM1)); DTWCDON2 = 0.0
	ALLOCATE(DTWCDON3(MTLOC,KBM1)); DTWCDON3 = 0.0
	
	ALLOCATE(DTWNCDOP1(MTLOC,KBM1)); DTWNCDOP1 = 0.0
	ALLOCATE(DTWNCDOP2(MTLOC,KBM1)); DTWNCDOP2 = 0.0
	ALLOCATE(DTWNCDOP3(MTLOC,KBM1)); DTWNCDOP3 = 0.0
	ALLOCATE(DTWCDOP1(MTLOC,KBM1)); DTWCDOP1 = 0.0
	ALLOCATE(DTWCDOP2(MTLOC,KBM1)); DTWCDOP2 = 0.0
	ALLOCATE(DTWCDOP3(MTLOC,KBM1)); DTWCDOP3 = 0.0
	
	ALLOCATE(CP1C(MTLOC,KBM1)); CP1C = 0.0
	ALLOCATE(CP2C(MTLOC,KBM1)); CP2C = 0.0
	ALLOCATE(CP3C(MTLOC,KBM1)); CP3C = 0.0

	ALLOCATE(NP1(MTLOC,KBM1)); NP1 =0.0
	ALLOCATE(NP2(MTLOC,KBM1)); NP2 =0.0
	ALLOCATE(NP3(MTLOC,KBM1)); NP3 =0.0
	
	ALLOCATE(PP1(MTLOC,KBM1)); PP1 =0.0
	ALLOCATE(PP2(MTLOC,KBM1)); PP2 =0.0
	ALLOCATE(PP3(MTLOC,KBM1)); PP3 =0.0

    ALLOCATE(MNLDOC(MTLOC,KBM1)); MNLDOC=0.0
	ALLOCATE(MNLDON(MTLOC,KBM1)); MNLDON=0.0
	ALLOCATE(MNLDOP(MTLOC,KBM1)); MNLDOP=0.0
	
	
	ALLOCATE(COAGC(MTLOC,KBM1)); COAGC = 0.0
	ALLOCATE(COAGN(MTLOC,KBM1)); COAGN = 0.0
	ALLOCATE(COAGP(MTLOC,KBM1)); COAGP = 0.0
	
	ALLOCATE(ALGCAR(MTLOC,KBM1)); ALGCAR = 0.0
	ALLOCATE(DOCBM1(MTLOC,KBM1)); DOCBM1 = 0.0
	ALLOCATE(DOCBM2(MTLOC,KBM1)); DOCBM2 = 0.0
	

!	write(*,*)'NWAVELx = ' , NWAVELx
    ALLOCATE(ICDOM_abs(MTLOC,KBM1,NWAVELx,3)); ICDOM_abs = 0.0    ! total light Absorbed due to cdom
	ALLOCATE(N_photons(MTLOC,KBM1,NWAVELx,3)); N_photons = 0.0    ! total light Absorbed due to cdom
	ALLOCATE(TOTAL_CDOM_ABS(MTLOC,KBM1,NWAVELx));TOTAL_CDOM_ABS = 0.0 
	ALLOCATE(APQY32(NWAVELx)); APQY32 = 0.0
	ALLOCATE(APQY31(NWAVELx)); APQY31 = 0.0
	ALLOCATE(APQY30(NWAVELx)); APQY30 = 0.0
	ALLOCATE(APQY3N(NWAVELx)); APQY3N = 0.0
	
	ALLOCATE(APQY21(NWAVELx)); APQY21 = 0.0
	ALLOCATE(APQY20(NWAVELx)); APQY20 = 0.0
	ALLOCATE(APQY2N(NWAVELx)); APQY2N = 0.0	
	
	ALLOCATE(APQY10(NWAVELx)); APQY10 = 0.0
	ALLOCATE(APQY1N(NWAVELx)); APQY1N = 0.0
	
	
	ALLOCATE(PHOTOCHEM_DOC_ACCUM(MTLOC,KBM1,7))  ; PHOTOCHEM_DOC_ACCUM=0.0;
	ALLOCATE(PHOTOCHEM_DOC_ACCUM_GL(MGL,KBM1,7))  ; PHOTOCHEM_DOC_ACCUM_GL=0.0;
	

	write(*,*)'Allocated everything'
	
END SUBROUTINE WC_DOM_ALLOCATE

! Deallocate all the variables
SUBROUTINE WC_DOM_DEALLOCATE

    IF(ALLOCATED(KDOC1)) DEALLOCATE(KDOC1)
	IF(ALLOCATED(KDOC2)) DEALLOCATE(KDOC2)
	IF(ALLOCATED(KDOC3)) DEALLOCATE(KDOC3)
	
    IF(ALLOCATED(DOCSZ1)) DEALLOCATE(DOCSZ1)
	IF(ALLOCATED(DOCLZ1)) DEALLOCATE(DOCLZ1)
	IF(ALLOCATED(DOCSZ2)) DEALLOCATE(DOCSZ2)
	IF(ALLOCATED(DOCLZ2)) DEALLOCATE(DOCLZ2)
	IF(ALLOCATED(DOCSZ3)) DEALLOCATE(DOCSZ3)
	IF(ALLOCATED(DOCLZ3)) DEALLOCATE(DOCLZ3)
	
	IF(ALLOCATED(DONSZ1)) DEALLOCATE(DONSZ1)
	IF(ALLOCATED(DONLZ1)) DEALLOCATE(DONLZ1)
	IF(ALLOCATED(DONSZ2)) DEALLOCATE(DONSZ2)
	IF(ALLOCATED(DONLZ2)) DEALLOCATE(DONLZ2)
    IF(ALLOCATED(DONSZ3)) DEALLOCATE(DONSZ3)
	IF(ALLOCATED(DONLZ3)) DEALLOCATE(DONLZ3)
	
	IF(ALLOCATED(DOPSZ1)) DEALLOCATE(DOPSZ1)
	IF(ALLOCATED(DOPLZ1)) DEALLOCATE(DOPLZ1)
	IF(ALLOCATED(DOPSZ2)) DEALLOCATE(DOPSZ2)
	IF(ALLOCATED(DOPLZ2)) DEALLOCATE(DOPLZ2)
	IF(ALLOCATED(DOPSZ3)) DEALLOCATE(DOPSZ3)
	IF(ALLOCATED(DOPLZ3)) DEALLOCATE(DOPLZ3)
	
    IF(ALLOCATED(CRRATE_SZ)) DEALLOCATE(CRRATE_SZ) ! small zoos 
    IF(ALLOCATED(NRRATE_SZ)) DEALLOCATE(NRRATE_SZ) 
    IF(ALLOCATED(PRRATE_SZ)) DEALLOCATE(PRRATE_SZ) 
    IF(ALLOCATED(CRRATE_LZ)) DEALLOCATE(CRRATE_LZ) 
    IF(ALLOCATED(NRRATE_LZ)) DEALLOCATE(NRRATE_LZ) 
    IF(ALLOCATED(PRRATE_LZ)) DEALLOCATE(PRRATE_LZ) 
	
	IF(ALLOCATED(FRD1)) DEALLOCATE(FRD1)   ! DOC CONSUMED BY ZOOPLANKTON
	IF(ALLOCATED(FRD2)) DEALLOCATE(FRD2) 

	IF(ALLOCATED(WC_NCDOC1)) DEALLOCATE(WC_NCDOC1)
	IF(ALLOCATED(WC_NCDOC2)) DEALLOCATE(WC_NCDOC2)
	IF(ALLOCATED(WC_NCDOC3)) DEALLOCATE(WC_NCDOC3)
	IF(ALLOCATED(WC_CDOC1)) DEALLOCATE(WC_CDOC1)
	IF(ALLOCATED(WC_CDOC2)) DEALLOCATE(WC_CDOC2)
	IF(ALLOCATED(WC_CDOC3)) DEALLOCATE(WC_CDOC3)
					
	IF(ALLOCATED(WC_NCDON1)) DEALLOCATE(WC_NCDON1)
	IF(ALLOCATED(WC_NCDON2)) DEALLOCATE(WC_NCDON2)
	IF(ALLOCATED(WC_NCDON3)) DEALLOCATE(WC_NCDON3)
	IF(ALLOCATED(WC_CDON1)) DEALLOCATE(WC_CDON1)
	IF(ALLOCATED(WC_CDON2)) DEALLOCATE(WC_CDON2)
	IF(ALLOCATED(WC_CDON3)) DEALLOCATE(WC_CDON3)
	
	IF(ALLOCATED(WC_NCDOP1)) DEALLOCATE(WC_NCDOP1)
	IF(ALLOCATED(WC_NCDOP2)) DEALLOCATE(WC_NCDOP2)
	IF(ALLOCATED(WC_NCDOP3)) DEALLOCATE(WC_NCDOP3)
	IF(ALLOCATED(WC_CDOP1)) DEALLOCATE(WC_CDOP1)
	IF(ALLOCATED(WC_CDOP2)) DEALLOCATE(WC_CDOP2)
	IF(ALLOCATED(WC_CDOP3)) DEALLOCATE(WC_CDOP3)
	
	IF(ALLOCATED(DTWNCDOC1)) DEALLOCATE(DTWNCDOC1)
	IF(ALLOCATED(DTWNCDOC2)) DEALLOCATE(DTWNCDOC2)
	IF(ALLOCATED(DTWNCDOC3)) DEALLOCATE(DTWNCDOC3)
	IF(ALLOCATED(DTWCDOC1)) DEALLOCATE(DTWCDOC1)
	IF(ALLOCATED(DTWCDOC2)) DEALLOCATE(DTWCDOC2)
	IF(ALLOCATED(DTWCDOC3)) DEALLOCATE(DTWCDOC3)
	
	IF(ALLOCATED(DTWNCDON1)) DEALLOCATE(DTWNCDON1)
	IF(ALLOCATED(DTWNCDON2)) DEALLOCATE(DTWNCDON2)
	IF(ALLOCATED(DTWNCDON3)) DEALLOCATE(DTWNCDON3)
	IF(ALLOCATED(DTWCDON1)) DEALLOCATE(DTWCDON1)
	IF(ALLOCATED(DTWCDON2)) DEALLOCATE(DTWCDON2)
	IF(ALLOCATED(DTWCDON3)) DEALLOCATE(DTWCDON3)
	
	IF(ALLOCATED(DTWNCDOP1)) DEALLOCATE(DTWNCDOP1)
	IF(ALLOCATED(DTWNCDOP2)) DEALLOCATE(DTWNCDOP2)
	IF(ALLOCATED(DTWNCDOP3)) DEALLOCATE(DTWNCDOP3)
	IF(ALLOCATED(DTWCDOP1)) DEALLOCATE(DTWCDOP1)
	IF(ALLOCATED(DTWCDOP2)) DEALLOCATE(DTWCDOP2)
	IF(ALLOCATED(DTWCDOP3)) DEALLOCATE(DTWCDOP3)

	IF(ALLOCATED(CP1C)) DEALLOCATE(CP1C)
    IF(ALLOCATED(CP2C)) DEALLOCATE(CP2C)
    IF(ALLOCATED(CP3C)) DEALLOCATE(CP3C)	
	
	IF (ALLOCATED(NP1)) DEALLOCATE(NP1)
	IF (ALLOCATED(NP2)) DEALLOCATE(NP2)
	IF (ALLOCATED(NP3)) DEALLOCATE(NP3)
	
	IF (ALLOCATED(PP1)) DEALLOCATE(PP1)
	IF (ALLOCATED(PP2)) DEALLOCATE(PP2)
	IF (ALLOCATED(PP3)) DEALLOCATE(PP3)
	
    IF(ALLOCATED(MNLDOC)) DEALLOCATE(MNLDOC)
	IF(ALLOCATED(MNLDON)) DEALLOCATE(MNLDON)
	IF(ALLOCATED(MNLDOP)) DEALLOCATE(MNLDOP)
	
	
	IF(ALLOCATED(COAGC)) DEALLOCATE(COAGC)
	IF(ALLOCATED(COAGN)) DEALLOCATE(COAGN)
	IF(ALLOCATED(COAGP)) DEALLOCATE(COAGP)
	
	IF(ALLOCATED(ALGCAR)) DEALLOCATE(ALGCAR)
    IF(ALLOCATED(DOCBM1)) DEALLOCATE(DOCBM1)
	IF(ALLOCATED(DOCBM2)) DEALLOCATE(DOCBM2)
	

	
	IF(ALLOCATED(ICDOM_abs)) DEALLOCATE(ICDOM_abs)
	IF(ALLOCATED(N_photons)) DEALLOCATE(N_photons)
	IF(ALLOCATED(TOTAL_CDOM_ABS)) DEALLOCATE(TOTAL_CDOM_ABS)
	
	IF(ALLOCATED(APQY32)) DEALLOCATE(APQY32)
	IF(ALLOCATED(APQY31)) DEALLOCATE(APQY31)
	IF(ALLOCATED(APQY30)) DEALLOCATE(APQY30)
	IF(ALLOCATED(APQY3N)) DEALLOCATE(APQY3N)
	
    IF(ALLOCATED(APQY21)) DEALLOCATE(APQY21)
	IF(ALLOCATED(APQY20)) DEALLOCATE(APQY20)
	IF(ALLOCATED(APQY2N)) DEALLOCATE(APQY2N)
	
	IF(ALLOCATED(APQY10)) DEALLOCATE(APQY10)
	IF(ALLOCATED(APQY1N)) DEALLOCATE(APQY1N)	
	
	IF(ALLOCATED(PHOTOCHEM_DOC_ACCUM)) DEALLOCATE(PHOTOCHEM_DOC_ACCUM)
	IF(ALLOCATED(PHOTOCHEM_DOC_ACCUM_GL)) DEALLOCATE(PHOTOCHEM_DOC_ACCUM_GL)

	
END SUBROUTINE WC_DOM_DEALLOCATE

SUBROUTINE WC_DOM_INPUT
! read all the parameters from an input file

 CHARACTER(LEN=120) :: WCDOM_FILEin,APQY_filein
 real(SP) :: sanity!, summed_up, tol
 INTEGER :: ingood
 
  WCDOM_FILEin = 'wcdom_control.npt'
   
!IF(SERIAL) THEN

	 OPEN(unit=3456,file = WCDOM_Filein, iostat=ingood)
	   if (ingood /= 0) THEN
	     write(*,*) 'Problem opening the DOM control file'
	     write(*,*) ' error code is ',ingood
	     write(*,*) 'Exiting'
	    stop
	   ELSE
         write(*,*)'opened the dom parameterization file'
	   ENDIF
!DOC remineralization	 
        !READ(3456,*)
        !READ(3456,*)
        !READ(3456,*)
        !READ(3456,*

   
!Photodegradation
	READ(3456,221)PHOTODEG   
	write(*,*)'Photodeg = ',PHOTODEG
	READ(3456,444)KDOC1_in,KDOC2_in,KDOC3_in
	write(*,*)'KDOC1 = ',KDOC1_in
    READ(3456,444)KDON1_in,KDON2_in,KDON3_in
	READ(3456,444)KDOP1_in,KDOP2_in,KDOP3_in
! Denitrification (last two are currently unused)	
    !READ(3456,444)DENIT_FCDOC,DENIT_FCDON,DENIT_FCDOP
!Algae DOM
	READ(3456,444)KHR1,KHR2,KHR3
	READ(3456,444)FCD11,FCD12,FCD13
	READ(3456,444)FCD21,FCD22,FCD23
	READ(3456,444)FNCD11,FNCD12,FNCD13
	READ(3456,444)FNCD21,FNCD22,FNCD23
	READ(3456,444)FCND11,FCND12,FCND13
	READ(3456,444)FCND21,FCND22,FCND23
	READ(3456,444)FNCND11,FNCND12,FNCND13
	READ(3456,444)FNCND21,FNCND22,FNCND23
	READ(3456,444)FCPD11,FCPD12,FCPD13
	READ(3456,444)FCPD21,FCPD22,FCPD23
	READ(3456,444)FNCPD11,FNCPD12,FNCPD13
	READ(3456,444)FNCPD21,FNCPD22,FNCPD23
		
	READ(3456,555)FCD1,FCD2
	READ(3456,555)FND1,FND2
	READ(3456,555)FPD1,FPD2
	
!HYDROLYSIS
    READ(3456,444)FHDRC1,FHDRC2,FHDRC3
	READ(3456,444)FHDRNC1,FHDRNC2,FHDRNC3
    READ(3456,444)FHDRCN1,FHDRCN2,FHDRCN3
	READ(3456,444)FHDRNCN1,FHDRNCN2,FHDRNCN3
    READ(3456,444)FHDRCP1,FHDRCP2,FHDRCP3
	READ(3456,444)FHDRNCP1,FHDRNCP2,FHDRNCP3

!ZOOPLANKTON	
	READ(3456,444)FDOC1SZ,FDOC2SZ,FDOC3SZ
	READ(3456,444)FDOC1LZ,FDOC2LZ,FDOC3LZ
	READ(3456,444)FCDOC1SZ,FCDOC2SZ,FCDOC3SZ
	READ(3456,444)FCDOC1LZ,FCDOC2LZ,FCDOC3LZ
	READ(3456,444)FDON1SZ,FDON2SZ,FDON3SZ
	READ(3456,444)FDON1LZ,FDON2LZ,FDON3LZ
	READ(3456,444)FCDON1SZ,FCDON2SZ,FCDON3SZ
	READ(3456,444)FCDON1LZ,FCDON2LZ,FCDON3LZ
	READ(3456,444)FDOP1SZ,FDOP2SZ,FDOP3SZ
	READ(3456,444)FDOP1LZ,FDOP2LZ,FDOP3LZ
	READ(3456,444)FCDOP1SZ,FCDOP2SZ,FCDOP3SZ
	READ(3456,445)FCDOP1LZ,FCDOP2LZ,FCDOP3LZ

	

	
IF(PHOTODEG) THEN
	
	APQY_filein = 'wcdom_apqy.npt'
	write(*,*)'Opening photodegradation file'
	  OPEN(unit=3457,file = APQY_filein, iostat=ingood)
	   if (ingood /= 0) THEN
	       write(*,*) 'Problem opening the Apparent Quantum Yield control file'
	       write(*,*) ' error code is ',ingood
	       write(*,*) 'Exiting'
	      stop
	   ELSE
            write(*,*)'opened the APQY spectral dependency file'
	   endif
	   
	  READ(3457,443)FPDC31,FPDC32,FPDC33   ! read in the photodegrdation fractions from COlored to non-colored
	  READ(3457,444)FPDC21,FPDC22,FPDC23   ! read in the photodegrdation fractions from COlored to non-colored
	  READ(3457,444)FPDC11,FPDC12,FPDC13   ! read in the photodegrdation fractions from COlored to non-colored
	  
	 sanity = FPDC31+FPDC32+FPDC33 
	sant='CDOC3 photodegradation fractions'
	    call sanity_check(sant,sanity,1.0,0.011)
	 
	 sanity = FPDC21+FPDC22+FPDC23 
	sant='CDOC2 photodegradation fractions'
	    call sanity_check(sant,sanity,1.0,0.011)
		
	 sanity = FPDC11+FPDC12+FPDC13 
	  sant='CDOC1 photodegradation fractions'
	 call sanity_check(sant,sanity,1.0,0.011)
	!	
	 !read(3457,*) ! skip a couple lines
	 !read(3457,*)
	 
	 write(*,*)'NWAVELx = ',NWAVELx
	
	 DO J =1, NWAVELx  ! read in the spectral quantum yields
	 
	    read(3457,*) APQY32(J),APQY31(J),APQY30(J),APQY3N(J),APQY21(J),APQY20(J),APQY2N(J),APQY10(J),APQY1N(J)
	   
     END DO
	 
	 
	 		  ! B Clark lets bump up the DOM3 APQY to get a better value
		  
		  APQY32 = APQY32  * 100
		  APQY31 = APQY31  * 100
		  APQY3N = APQY3N  * 100
		  APQY21 = APQY21  * 100
		  APQY2N = APQY2N  * 100
		  APQY1N = APQY1N  * 100
		  
		  APQY30 = APQY30 *  100
		  APQY20 = APQY20 *  100
		  APQY10 = APQY10 *  100
	 
	   write(*,*)'photodegradation is on, read APQY file'
	   
ENDIF

	close(3457)
		

	
	
	


       

	
	write(*,*)'KDOC1_in,KDOC2_in,KDOC3_in'
	write(*,445)KDOC1_in,KDOC2_in,KDOC3_in
	
    write(*,*)'KDON1_in,KDON2_in,KDON3_in'
	 write(*,445)KDON1_in,KDON2_in,KDON3_in
	 
	write(*,*)'KDOP1_in,KDOP2_in,KDOP3_in'
	write(*,445)KDOP1_in,KDOP2_in,KDOP3_in
	
! Denitrification (last two are currently unused)	
    !READ(3456,444)DENIT_FCDOC,DENIT_FCDON,DENIT_FCDOP
!Algae DOM
	write(*,*)'KHR1,KHR2,KHR3'
	write(*,445)KHR1,KHR2,KHR3
	
	write(*,*)'FCD11,FCD12,FCD13'
	write(*,445)FCD11,FCD12,FCD13
	
    Write(*,*)'FCD21,FCD22,FCD23'
	write(*,445)FCD21,FCD22,FCD23
	
	write(*,*)'FNCD11,FNCD12,FNCD13'
	write(*,445)FNCD11,FNCD12,FNCD13
	
	write(*,*)'FNCD21,FNCD22,FNCD23'
	write(*,445)FNCD21,FNCD22,FNCD23
	
	write(*,*)'FCND11,FCND12,FCND13'
	write(*,445)FCND11,FCND12,FCND13
	
	write(*,*)'FCND21,FCND22,FCND23'
	write(*,445)FCND21,FCND22,FCND23
	
	write(*,*)'FNCND11,FNCND12,FNCND13'
	write(*,445)FNCND11,FNCND12,FNCND13
	
	write(*,*)'FNCND21,FNCND22,FNCND23'
	write(*,445)FNCND21,FNCND22,FNCND23
	
	write(*,*)'FCPD11,FCPD12,FCPD13'
	write(*,445)FCPD11,FCPD12,FCPD13
	
	write(*,*)'FCPD21,FCPD22,FCPD23'
	write(*,445)FCPD21,FCPD22,FCPD23
	
	write(*,*)'FNCPD11,FNCPD12,FNCPD13'
	write(*,445)FNCPD11,FNCPD12,FNCPD13
	
	write(*,*)'FNCPD21,FNCPD22,FNCPD23'
	write(*,445)FNCPD21,FNCPD22,FNCPD23
	
		
	write(*,*)'FCD1,FCD2'
	write(*,445)FCD1,FCD2
	
	write(*,*)'FND1,FND2'
	write(*,445)FND1,FND2
	
	write(*,*)'FPD1,FPD2'
	write(*,445)FPD1,FPD2
	
!HYDROLYSIS
    write(*,*)'FHDRC1,FHDRC2,FHDRC3'
	write(*,445)FHDRC1,FHDRC2,FHDRC3
	
	write(*,*)'FHDRNC1,FHDRNC2,FHDRNC3'
	write(*,445)FHDRNC1,FHDRNC2,FHDRNC3
	
    write(*,*)'FHDRCN1,FHDRCN2,FHDRCN3'
	write(*,445)FHDRCN1,FHDRCN2,FHDRCN3
	
	write(*,*)'FHDRNCN1,FHDRNCN2,FHDRNCN3'
	write(*,445)FHDRNCN1,FHDRNCN2,FHDRNCN3
	
    write(*,*)'FHDRCP1,FHDRCP2,FHDRCP3'
	write(*,445)FHDRCP1,FHDRCP2,FHDRCP3
	
	write(*,*)'FHDRNCP1,FHDRNCP2,FHDRNCP3'
	write(*,445)FHDRNCP1,FHDRNCP2,FHDRNCP3

!ZOOPLANKTON	
	write(*,*)'FDOC1SZ   FDOC2SZ  FDOC3SZ'
	write(*,445) FDOC1SZ,FDOC2SZ,FDOC3SZ
	
	Write(*,*)'FDOC1LZ  FDOC2LZ   FDOC3LZ'
	Write(*,445)FDOC1LZ , FDOC2LZ ,  FDOC3LZ
	
	write(*,*)'FCDOC1SZ  FCDOC2SZ    FCDOC3SZ'
	write(*,445) FCDOC1SZ,FCDOC2SZ,FCDOC3SZ
	
	Write(*,*)'FCDOC1SZ,  FCDOC2SZ,  FCDOC3SZ'
	write(*,445)FCDOC1SZ,FCDOC2SZ,FCDOC3SZ
	
	Write(*,*)'FCDOC1LZ , FCDOC2LZ,  FCDOC3LZ'
	write(*,445)FCDOC1LZ,FCDOC2LZ,FCDOC3LZ
	
	Write(*,*)' FDON1SZ, FDON2SZ, FDON3SZ'
	write(*,445)FDON1SZ,FDON2SZ,FDON3SZ
	
	Write(*,*)'FDON1LZ  ,FDON2LZ,  FDON3LZ'
	write(*,445)FDON1LZ,FDON2LZ,FDON3LZ
	
	Write(*,*)'  FCDON1SZ,  FCDON2SZ,  FCDON3SZ'
	write(*,445)FCDON1SZ,FCDON2SZ,FCDON3SZ
	
	Write(*,*)'FCDON1LZ,FCDON2LZ,FCDON3LZ'
	write(*,445)FCDON1LZ,FCDON2LZ,FCDON3LZ
	
	Write(*,*)'FDOP1SZ,FDOP2SZ,FDOP3SZ'
	write(*,445)FDOP1SZ,FDOP2SZ,FDOP3SZ
	
	Write(*,*)'FDOP1LZ,FDOP2LZ,FDOP3LZ'
	write(*,445)FDOP1LZ,FDOP2LZ,FDOP3LZ
	
	Write(*,*)'FCDOP1SZ,FCDOP2SZ,FCDOP3SZ'
	write(*,445)FCDOP1SZ,FCDOP2SZ,FCDOP3SZ
	
	Write(*,*)'FCDOP1LZ,FCDOP2LZ,FCDOP3LZ'
	write(*,445)FCDOP1LZ,FCDOP2LZ,FCDOP3LZ


        
!ENDIF


 ! summed_up = 1.0;tol = 0.011
   !!! sanity checks see function sanity_check
	sanity = FCD11+FCD12+FCD13+FNCD11+FNCD12+FNCD13
	!write(*,*)'sanity = ',sanity
	sant='Algae 1 DOC'
	
	call sanity_check(sant,sanity,1.0,0.011)
  !  write(*,*)'sanity = ',sanity
	sanity = FCD21+FCD22+FCD23+FNCD21+FNCD22+FNCD23
	sant='Algae 2 DOC'
	call sanity_check(sant,sanity,1.0,0.011)

	Sanity = FCND11+FCND12+FCND13+FNCND11+FNCND12+FNCND13
	sant='Algae 1 DON'
	call sanity_check(sant,sanity,1.0,0.011)

	Sanity = FCND21+FCND22+FCND23+FNCND21+FNCND22+FNCND23
	sant='Algae 2 DON'
	call sanity_check(sant,sanity,1.0,0.011)
	
	Sanity = FCPD11+FCPD12+FCPD13+FNCPD11+FNCPD12+FNCPD13
	sant='Algae 1 DOP'
	call sanity_check(sant,sanity,1.0,0.011)

	Sanity = FCPD21+FCPD22+FCPD23+FNCPD21+FNCPD22+FNCPD23
	sant='Algae 2 DOP'
	call sanity_check(sant,sanity,1.0,0.011)

	Sanity = FHDRC1+FHDRC2+FHDRC3+FHDRNC1+FHDRNC2+FHDRNC3
	sant='DOC hydrolysis'
	call sanity_check(sant,sanity,1.0,0.011)

	Sanity = FHDRCN1+FHDRCN2+FHDRCN3+FHDRNCN1+FHDRNCN2+FHDRNCN3
	sant='DON hydrolysis'
	call sanity_check(sant,sanity,1.0,0.011)

	Sanity = FHDRCP1+FHDRCP2+FHDRCP3+FHDRNCP1+FHDRNCP2+FHDRNCP3
	sant='DOP hydrolysis'
	call sanity_check(sant,sanity,1.0,0.011)

	Sanity = FDOC1SZ+FDOC2SZ+FDOC3SZ
	sant='Microzooplankton DOC'
	call sanity_check(sant,sanity,1.0,0.011)
	
	Sanity = FDOC1LZ+FDOC2LZ+FDOC3LZ
	sant='Mesozooplankton DOC'
	call sanity_check(sant,sanity,1.0,0.011)

	Sanity = FDON1SZ+FDON2SZ+FDON3SZ
	sant='Microzooplankton DON'
	call sanity_check(sant,sanity,1.0,0.011)
	
	Sanity = FDON1LZ+FDON2LZ+FDON3LZ
	sant='Mesozooplankton DON'
	call sanity_check(sant,sanity,1.0,0.011)

	Sanity = FDOP1SZ+FDOP2SZ+FDOP3SZ
	sant='Microzooplankton DOP'
	call sanity_check(sant,sanity,1.0,0.011)
	
	Sanity = FDOP1LZ+FDOP2LZ+FDOP3LZ
	sant='Mesozooplankton DOP'
	call sanity_check(sant,sanity,1.0,0.011)
	
 221 FORMAT(5/,L4,3/)	
 443 FORMAT(5/,3(F8.4),3/)
 444 FORMAT(3(F8.4),3/)
 445 FORMAT(3(F8.4))
 446 FORMAT(3(F10.6),/)
 555 FORMAT(2(F8.4),3/)

  close(3456) 
  
 END SUBROUTINE WC_DOM_INPUT
 
SUBROUTINE ZOOP_DOM
! calculate the zooplankton contribution to DOM

USE MOD_ZOOP, ONLY: CTSZ,   & ! carbon zoop grazing threshold, gC/m^3
					RSZ, SZ,  &
					PRASZ , &
					 DOCLZ, DOCSZ,  &  ! all the zooplankton productions of DOM, into labile and semi-labile portions
					 DONLZ, DONSZ,  &
					 DOPLZ, DOPSZ,  &
					 DOC1ASZ,DOC2ASZ!,  &
					 !DOC2SZ, DOC2LZ,  &
					 !DON2SZ, DON2LZ,  &
                     !DOP2SZ, DOP2LZ,
!========*=======*=========*========*=======*=======*=======*
!					ZOOPLANKTON DOC							!
!========*=======*=========*========*=======*=======*=======*

   DO K=1,KBM1
     DO I=1,MLOC

        DOCSZ1(I,K)  = -FRD1(I,K)*RSZ(I,K)*SZ(I,K)+CRRATE_SZ(I,K)*FDOC1SZ  ! DOC_flag   amount of DOC1 respired by SZ (microbial loop) and amount of DOC1 created by small SZ
        DOCSZ2(I,K)  = -FRD2(I,K)*RSZ(I,K)*SZ(I,K)+CRRATE_SZ(I,K)*FDOC2SZ  ! DOC_flag   amount of DOC2 respired by SZ and amount of ROC2 created by small SZ
	    DOCSZ3(I,K) =   CRRATE_SZ(I,K) * FDOC3SZ  ! SMALL ZOOPLANKTON ONLY PRODUCE REFRAC DOC, IT IS NOT PASSED UP FOOD CHANGE VIA MICROBIAL LOOP
	   
	   DOCSZ(I,K) = DOCSZ1(I,K)+DOCSZ2(I,K)+DOCSZ3(I,K)
	   
        DOCLZ1(I,K)  = CRRATE_LZ(I,K)*FDOC1LZ   ! DOC_flag            ! large zooplankton only create DOC1 AND DOC 2
        DOCLZ2(I,K)  = CRRATE_LZ(I,K)*FDOC2LZ   ! DOC_flag
	    DOCLZ3(I,K)  =  CRRATE_LZ(I,K)*FDOC3LZ 
	   
	   DOCLZ(I,K) = DOCLZ1(I,K)+DOCLZ2(I,K) + DOCLZ3(I,K)

!========*=======*=========*========*=======*=======*=======*
!					ZOOPLANKTON DON							!
!========*=======*=========*========*=======*=======*=======*

		 DONSZ1(I,K) = NRRATE_SZ(I,K)*FDON1SZ   ! DON_flag  ! Zooplankton Effect
		 DONSZ2(I,K) = NRRATE_SZ(I,K)*FDON2SZ   ! DON_flag	
		 DONSZ3(I,K) = NRRATE_SZ(I,K)*FDON3SZ
	 
		DONSZ(I,K)=DONSZ1(I,K) + DONSZ2(I,K) + DONSZ3(I,K)! took above and compressed into one equation, if we want zooplankton consumption of don and dop, need to change this
		
		 DONLZ1(I,K) = NRRATE_LZ(I,K)*FDON1LZ     ! DON_flag		|	
	     DONLZ2(I,K) = NRRATE_LZ(I,K)*FDON2LZ     ! DON_flag		V
		 DONLZ3(I,K) = NRRATE_LZ(I,K)*FDON3LZ 
		
		DONLZ(I,K)=DONLZ1(I,K) + DONLZ2(I,K)+ DONLZ3(I,K) 

!========*=======*=========*========*=======*=======*=======*
!					ZOOPLANKTON DOP							!
!========*=======*=========*========*=======*=======*=======*

        DOPSZ1(I,K) = PRRATE_SZ(I,K)*FDOP1SZ       ! DOP_flag   ! ZOOPLANKTON ONLY CAN LOSE DISSOLVED PHOSPHORUS, NO CONSUMPTION
        DOPSZ2(I,K) = PRRATE_SZ(I,K)*FDOP2SZ       ! DOP_flag
		DOPSZ3(I,K) = PRRATE_SZ(I,K)*FDOP3SZ
		
	    DOPSZ(I,K) = DOPSZ1(I,K)   +  DOPSZ2(I,K) +DOPSZ3(I,K) !
	 
        DOPLZ1(I,K) = PRRATE_LZ(I,K)*FDOP1LZ    ! DOP_flag
        DOPLZ2(I,K) = PRRATE_LZ(I,K)*FDOP2LZ    ! DOP_flag
		DOPLZ3(I,K) = PRRATE_LZ(I,K)*FDOP3LZ
	 
	   DOPLZ(I,K) = DOPLZ1(I,K)   +  DOPLZ2(I,K)  + DOPLZ3(I,K)
	 
	 ENDDO
   ENDDO
  END SUBROUTINE ZOOP_DOM

SUBROUTINE DOC!(DTWCDOC1,DTWCDOC2,DTWCDOC3,DTWNCDOC1,DTWNCDOC2,DTWNCDOC3)

USE MOD_WQM, ONLY : DOXG, FTMNL, KRCOAG, KHCOAG, SALT, RATOX,FCDP, &!
					FDOP,FCLP1,FCLP2,FCLP3,FCRP1,FCRP2,FCRP3,ALGDOC, DENIT, &
					PR1, PR2, HDRLPOC,HDRRPOC,KHODOC,KDCALG, DLT


USE MOD_HYDROVARS, ONLY : ART1  ! area of node control volume					
					
					
!USE WC_DOM
! write(*,*)'Carbon Loop'
!========*=======*=========*========*=======*=======*=======*
!					   ALGAE DOC							!
!========*=======*=========*========*=======*=======*=======*
!Calculate algale contribution to DOC pools
   DO K=1,KBM1
     DO I=1,MLOC
	!   write(*,*)'DTWCDOC1  before = ',DTWCDOC1(I,K)
	   !  write(*,*)'DOXG = ', DOXG
		 
	       DOCBM1(I,K)  = KHR1/(KHR1+DOXG(I,K))    ! DOC_flag    ! hyperbolic saturation for DOC excretion from algae
           DOCBM2(I,K)  = KHR2/(KHR2+DOXG(I,K))    ! DOC_flag

	      DOM1 = FCD1*CP1C(I,K) +     &  	 ! eq 3-18                             ! mg C/m^3/day ! fraction of basal metabolism exuded as DOC
			     (1.-FCD1-FCLP1-FCRP1)*DOCBM1(I,K) * CP1C(I,K)  &                       !DOC exudation as a function of Oxygen concentration.  As oxygen decreases (DOCBM1 becomes larger) the DOC exudation increases
			     + (FCDP+FDOP*DOCBM1(I,K)) * PR1(I,K) !+   &                       ! DOC exudation due to algal predation, increases with increasing oxygen stress
		       !  FCRDP*PR1(I,K)            !  FCRD1*CP1C(I,K) 				   ! refractory carbon production from algae 1
			    ! changed the above so that all carbon productoin is based on one fractionation term, FCDP and FCD1
			   
          DOM2 = FCD2*CP2C(I,K)+ &                                             ! Same as above but for algae 2    mg C/m^3/day
     			 (1.-FCD2-FCLP2-FCRP2) * DOCBM2(I,K) * CP2C(I,K) &                      ! All FC coefficients are currently set to zero.  Therefore there is only oxygen limitation for the DOC excretion    
	 		     + (FCDP+FDOP*DOCBM2(I,K)) * PR2(I,K)!  + &  
		         !FCRD2*CP2C(I,K)+FCRDP*PR2(I,K)                               ! refractory carbon production from algae 2


		! algae 3 below, needs work
		 !DOC3 = FCD1*CP1C(I,K)+ &  	                              ! mg C/m^3/day ! fraction of basal metabolism exuded as DOC
		!	   (1.-FCD1-FCRD1-FCLP1-FCRP1)*DOCBM1(I,K)*CP1C(I,K)  &  !DOC exudation as a function of Oxygen concentration.  As oxygen decreases (DOCBM1 becomes larger) the DOC exudation increases
		!	   + (FCLDP+FDOP*DOCBM1(I,K))*PR1(I,K) +   &           ! DOC exudation due to algal production, increases with decreasing oxygen stress
		 !      FCRD1*CP1C(I,K)+FCRDP*PR1(I,K)      				! refractory carbon production from algae 3
			   
			   
	!	  write(*,*)'DOM from algae =',DOM1,DOM2
	      ALGDOC(I,K)  = DOM1+DOM2   !+ DOC3   ! ALL DOC PRODUCE BY ALGAE 

!!Calculate water column DOC

	   SALTC       = MAX(0., SALT(I,K)) 
	   
	   KDOC1(I,K)   = KDOC1_in+KDCALG(I,K)*ALGCAR(I,K)   !As of now, remineralization rates will be the same for both colored and non-colored DOM
	   
	   KDOC2(I,K)   = KDOC2_in+KDCALG(I,K)*ALGCAR(I,K)
	   
	   KDOC3(I,K)   = KDOC3_in+KDCALG(I,K)*ALGCAR(I,K)
	   
	!	write(*,*)'FTMNl in WCDOC = ',FTMNL
!mineralization of non colored doc	 
	   MNLDOM1 = KDOC1(I,K)*FTMNL(I,K)&                       ! temperature dependent remineralization rate     ! mg C/m^3/day
					   *DOXG(I,K)/(KHODOC+DOXG(I,K))  &                ! dissolved oxygen limitation       
					   *WC_NCDOC1(I,K)        ! DOC_flag  ! mineralization rate of labile DOC (positive is a DOC loss)

	   
       MNLDOM2 = KDOC2(I,K)*FTMNL(I,K)*DOXG(I,K)/ &
					   (KHODOC+DOXG(I,K))&                                ! aggregate colored and non-colored DOM into the same formula for calculating the remineralization 
					   *WC_NCDOC2(I,K)							           ! DOC_flag  ! Same as above but for refractory DOC
															            ! need to split these two remineralzation formulations into 3 pools.  Oxygen dependence is ok	  												                         !because the final formulation for DTDOC incudles terms for denitrification
	   MNLDOM3 = KDOC3(I,K)*FTMNL(I,K)*DOXG(I,K)/&
					   (KHODOC+DOXG(I,K))&                                 !mg C/m^3/day positive is loss  
					    *WC_NCDOC3(I,K)	
	
	  MNLDOC(I,K) = MNLDOM1+MNLDOM2+MNLDOM3  ! POSITIVE IS A LOSS OF DISSOLVED CARBON
	
! NON COLORED DOC					
       DTWNCDOC1(I,K) = (DOM1*FNCD11 + DOM2*FNCD21   -  MNLDOM1 -    &               ! fraction of algae1 non colored doc production  + fraction of algae2 non colored doc prodction - mineralization of noncolored DOC1
						DENIT(I,K)*WC_NCDOC1(I,K)/(WC_CDOC1(I,K)+WC_NCDOC1(I,K)+ 1E-30)    &      ! total DENIT is based on CDOC1 and NCDOC1, therefore fraction that is COLORED is the ratio of NCDOC1: total DOC1 						 
						+ (HDRLPOC(I,K)+HDRRPOC(I,K))*FHDRNC1+ 			 &                 ! + fraction of labile and refractory poc that is hydrolyzed to ncdoc1
						DOCSZ1(I,K)*(1-FCDOC1SZ)+DOCLZ1(I,K)*(1-FCDOC1LZ))/86400.        !+ the small and large zooplankton non colored doc 1 produced all time integrated 

	   
	!   write(*,*)'DTWNCDOC1 after algae = ',DTWNCDOC1
	   
       DTWNCDOC2(I,K) = (DOM1 * FNCD12 + DOM2* FNCD22 -  MNLDOM2 + & ! DOC_flag  ! same as above but with out hydrolysis
						+(HDRLPOC(I,K) + HDRRPOC(I,K))*FHDRNC2   +   &
						DOCSZ2(I,K)*(1-FCDOC2SZ)+DOCLZ2(I,K)*(1-FCDOC2LZ))/86400.     	 ! DOC_flag  ! MAKE COAG ACT ON REFRACTORY AND SEMI-LABILE DOC
	   
	   COAG        = 0.5*(1.+TANH(SALTC-KHCOAG))*KRCOAG(I,K)*WC_NCDOC3(I,K) ! MOVED HERE AND NOW ONLY REFRACTORY DOC COAGULATES
	   
	   COAGC(I,K) = COAG  ! set up to fill an array with the coagulation flux from both colored and non colored

	   DTWNCDOC3(I,K) = (DOM1*FNCD13 + DOM2*FNCD23 -MNLDOM3    &
						+(HDRLPOC(I,K) + HDRRPOC(I,K))*FHDRNC3 + 	&						
					     DOCSZ3(I,K)*(1-FCDOC3SZ) + DOCLZ3(I,K)*(1-FCDOC3LZ)-COAG)/86400
					
	   MNLDOM1= KDOC1(I,K)*FTMNL(I,K)&                   ! temperature dependent remineralization rate     ! mg C/m^3/day
					  *DOXG(I,K)/(KHODOC+DOXG(I,K))   &          ! dissolved oxygen limitation       
					  *WC_CDOC1(I,K)           		               ! DOC_flag  ! mineralization rate of labile DOC (positive is a DOC loss)

       MNLDOM2= KDOC2(I,K)*FTMNL(I,K)*DOXG(I,K)/&
					  (KHODOC+DOXG(I,K))&                                ! aggregate colored and non-colored DOM into the same formula for calculating the remineralization 
					  *WC_CDOC2(I,K)							     ! DOC_flag  ! Same as above but for refractory DOC
															                         ! need to split these two remineralzation formulations into 3 pools.  Oxygen dependence is ok	  												                         !because the final formulation for DTDOC incudles terms for denitrification
	   MNLDOM3= KDOC3(I,K)*FTMNL(I,K)*DOXG(I,K)/&
					  (KHODOC+DOXG(I,K))&                                 !mg C/m^3/day positive is loss  
					  * WC_CDOC3(I,K)							

	 MNLDOC(I,K) = MNLDOC(I,K)+MNLDOM1+MNLDOM2+MNLDOM3 ! GET THE TOTAL REMINERALIZED DOC FOR THIS TCE TO PASS TO OXYGEN EQUATIONS		  
					  
 ! COLORED DOC			
	 
	 

       DTWCDOC1(I,K) = (DOM1*FCD11+DOM2*FCD21 -  MNLDOM1 -  &           ! ALGAL PRODUCTION OF CDOM 1 AND 2 MINUS REMINERALIZATION
					    DENIT(I,K)*WC_CDOC1(I,K)/(WC_CDOC1(I,K)+WC_NCDOC1(I,K)+ 1E-30)   + &      !Denitrification is calcualted with total DOC1, therefore fractin of Denit that is colored is CDOC1: total DOC1						 
						(HDRLPOC(I,K)+HDRRPOC(I,K))*FHDRC1 		+	         &    ! with the hydrolosis terms from POC and the Zooplankton contributions
						DOCSZ1(I,K)*FCDOC1SZ+DOCLZ1(I,K)*FCDOC1LZ)/86400.        !ZOOPLANKTON CONTRIBUTION
    ! B Clark Debug
	   
!	   write(*,*)'Hydrolysis = ',(HDRLPOC(I,K)+HDRRPOC(I,K))*FHDRC1 	
	
!	   write(*,*)'DTWCDOC1 after algae = ',DTWCDOC1
	   
	   
	   
       DTWCDOC2(I,K) = (DOM1*FCD21+DOM2*FCD22 - MNLDOM2   +      &       			                       ! DOC_flag  ! same as above but with out hydrolysis
					   (HDRLPOC(I,K)+HDRRPOC(I,K))  *  FHDRC2 + 	 &
						DOCSZ2(I,K)*FCDOC2SZ+DOCLZ2(I,K)*FCDOC2LZ   &
					    )/86400.     										               ! DOC_flag  ! MAKE COAG ACT ON REFRACTORY AND SEMI-LABILE DOC
	   
	   COAG        = 0.5*(1.+TANH(SALTC-KHCOAG))*KRCOAG(I,K)*WC_CDOC3(I,K) ! MOVED HERE AND NOW ONLY REFRACTORY DOC COAGULATES

	   COAGC(I,K) = COAGC(I,K) + COAG  ! THIS GOES BACK TO BE ADDED INTO THE RPOC FLUX IN MOD_KIN 
!	   write(*,*)'Coag = ',COAG
	   DTWCDOC3(I,K) = (DOM1*FCD13+DOM2*FCD23 - MNLDOM3 +     & !ALGAL PRODUCTION AND MINERALIZATON OF REFRAC CDOC, 
					   (HDRLPOC(I,K)+HDRRPOC(I,K))*FHDRC3    +    &  ! HYDROLYSIS OF POC TO DOC3    
						DOCSZ3(I,K)*FCDOC3SZ+DOCLZ3(I,K)*FCDOC3LZ-COAG)/86400      ! DOC3SZ*FCDOC3SZ + DOC3LZ*FCDOC3LZ  ! REMOVED ZOOPLANKTON CONTRIBUTION TO DOM3
   	!  write(*,*)'DTWCDOC1  after = ',DTWCDOC1(I,K)
	   
	   ENDDO
   ENDDO 
	   

!!!!! MORE options available to have included DOC fluxes into model

  IF(SED_DOM_FLAG) THEN    ! DOC_flag  ! UPDATE DTDOC WITH A SEDIMENT FLUX 

    DO I=1,MLOC  ! DOC_flag
	
	 DTWNCDOC1(I,KBM1) = DTWNCDOC1(I,KBM1)+JWNCDOC1(I)/(D(I)*DZ(KBM1))/86400.     
	 DTWNCDOC2(I,KBM1) = DTWNCDOC2(I,KBM1)+JWNCDOC2(I)/(D(I)*DZ(KBM1))/86400. 
	 DTWNCDOC3(I,KBM1) = DTWNCDOC3(I,KBM1)+JWNCDOC3(I)/(D(I)*DZ(KBM1))/86400. 
	 
     DTWCDOC1(I,KBM1) = DTWCDOC1(I,KBM1)+JWCDOC1(I)/(D(I)*DZ(KBM1))/86400.     
	 DTWCDOC2(I,KBM1) = DTWCDOC2(I,KBM1)+JWCDOC2(I)/(D(I)*DZ(KBM1))/86400. 
	 DTWCDOC3(I,KBM1) = DTWCDOC3(I,KBM1)+JWCDOC3(I)/(D(I)*DZ(KBM1))/86400. 
	 
	 !! ADD in JDOM flux accumulation here
 

    ENDDO   
	! DOC_flag
   ENDIF    ! DOC_flag
   


   
   
   				!DO I = 1,MLOC
				!DO K = 1,KBM1
				!	write(*,*)'WCDOC1 outside = ',WC_CDOC1(I,K)
				!	write(*,*)'WCDOC2 outside = ',WC_CDOC2(I,K)
				!	write(*,*)'WCDOC3 outside = ',WC_CDOC3(I,K)
				!	
				!  ENDDO
			    ! ENDDO
   
   
   

  IF(PHOTODEG) THEN ! Now have a simple formulation for photodegradation, if desirable
						   ! The absorbance is currenlty based on the sum of the colored DOM
		
       
  
				! so the absorbance due to one is the fraction of the total pool of CDOM
    
!write(*,*)'photodeg is on'
     DO I = 1, MTLOC
	   DO K = 1, KBM1
                      ! We can integrate it over the entire spectra and get a wavelength dependent CDOMx loss to photodegradation and then sum that up to get the total loss f
					  ! from each pooll
	  !    DO J = 1,NWAVEL   ! Loop over all wavelengths to get the integrated spectral apparent quantum yield for each waveband therefore total loss of DOM
		  DPD32 = 0.
		  DPD31 = 0.
		  DPD30 = 0.
		  DPD3N = 0.
		  DPD21 = 0.
		  DPD20 = 0.
		  DPD2N = 0.
		  DPD10 = 0.
		  DPD1N = 0.
		  
		  
		  
!write(*,*)'N_photons in WCDOM = ',N_photons(:,:,51,:)
		  
	        DO J = 1,NWAVELx! integrate our changed carbon

				  DPD32 = DPD32 + APQY32(J) * N_photons(I,K,J,3)  ! DP32 = mg C/mols Quanta * mols Quanta/m^2/s = mg C /m^2/s   ---> still need to depth integrate
			      DPD31 = DPD31 + APQY31(J) * N_photons(I,K,J,3)
				  DPD30 = DPD30 + APQY30(J) * N_photons(I,K,J,3)
				  DPD3N = DPD3N + APQY3N(J) * N_photons(I,K,J,3)
				  
				  DPD21 = DPD21 + APQY21(J) * N_photons(I,K,J,2)
				  DPD20 = DPD20 + APQY20(J) * N_photons(I,K,J,2) 
				  DPD2N = DPD2N + APQY2N(J) * N_photons(I,K,J,2)
				  
				  DPD10 = DPD10 + APQY10(J) * N_photons(I,K,J,1)
				  DPD1N = DPD1N + APQY1N(J) * N_photons(I,K,J,1)
				  		  
				!!  write(*,*)'integrating APQY '
				!  write(*,*)'DPD32 = ',DPD32
				!  write(*,*)'DPD31 = ',DPD31
				!  write(*,*)'DPD30 = ',DPD30
				!  write(*,*)'DPD3N = ',DPD3N
				!  write(*,*)'DPD21 = ',DPD21
				!  write(*,*)'DPD20 = ',DPD20
				!  write(*,*)'DPD2N = ',DPD2N
				!  write(*,*)'DPD10 = ',DPD10
				!  write(*,*)'DPD1N = ',DPD1n
				! 
			
			ENDDO
			
			
			

        
																	
			!get the instaneous flux if not accumulating over time and space														
			PHOTOCHEM_DOC_ACCUM(I,K,1) = - DPD32  &	
															        - DPD31  &
																    - DPD30 &
																	- DPD3N


			! Accumulate total photodegradation into CDOM2 from CDOM3
			PHOTOCHEM_DOC_ACCUM(I,K,2) = DPD32
			
			! Accumulate total photodegradation out of CDOM2
			PHOTOCHEM_DOC_ACCUM(I,K,3) =  - DPD20      &
																	- DPD21    &
																	- DPD2N 	
			
	
			! accumualte total CDOM photodegradtion from CDOM3 and CDOM2 into CDOM1
			PHOTOCHEM_DOC_ACCUM(I,K,4) =  + DPD31       &
																	+ DPD21

			
			
			PHOTOCHEM_DOC_ACCUM(I,K,5) = - DPD10      &
																	- DPD1N 
			
			!CDOM lost to NCDOM
			PHOTOCHEM_DOC_ACCUM(I,K,6) = 	DPD3N   &
																	+DPD2N   &
																	+DPD1N
																	
			PHOTOCHEM_DOC_ACCUM(I,K,7) =  +	DPD30   &
																	+DPD20   &
																	+DPD10
		! convert from mg C m^-2 s^-1		to mg C m^-2 d^-1 for output
																	
		    PHOTOCHEM_DOC_ACCUM(I,K,:)= PHOTOCHEM_DOC_ACCUM(I,K,:)/86400
																	
																	

																	
																	

																	
			!
			!
			
			
			
		!	write(*,*)'Depth = ',D(I)
	!	if(DPD32 > 0.1) THEN
		
!	      write(*,*) ' DPD32 = ', DPD32
		  
	!    endif
	!a		write(*,*)'DTW before = ',DTWCDOC3(I,K)
!			
!	   
	   	      DTWCDOC3(I,K) = DTWCDOC3(I,K) - DPD32 /(D(I)*DZ(K)) & !SUM(APQY31 * N_photons(I,K,:,3)) &   !integrated loss to CDOC1
											- DPD31 /(D(I)*DZ(K)) & !SUM(APQY32 * N_photons(I,K,:,3)) &    ! loss to CDOC2
											- DPD30 /(D(I)*DZ(K)) & !SUM(APQY30 * N_photons(I,K,:,3))   &   ! loss to DIC		
											- DPD3N /(D(I)*DZ(K))   !SUM(APQY3N * N_photons(I,K,:,3))    ! loss to NCDOC  ! Don't know if this pathway exists, really, because there is inherent color loss in the loss to the less colored pools...
	   
											
	
										!	write(*,*)DPD30 /(D(I)*DZ(I)) 
										!	write(*,*)DPD21 /(D(I)*DZ(I)) 
										!	write(*,*)DPD32 /(D(I)*DZ(I)) 
			!	write(*,*)'DTW after = ',DTWCDOC3(I,K)
	   
			  DTWCDOC2(I,K) = DTWCDOC2(I,K) - DPD21  /(D(I)*DZ(K)) &  !SUM(APQY21 * N_photons(I,K,:,2) )  &!
										    - DPD20  /(D(I)*DZ(K)) &  !SUM(APQY20 * N_photons(I,K,:,2) )   &!
										    - DPD2N  /(D(I)*DZ(K)) &  !SUM(APQY2N * N_photons(I,K,:,2) )       &      ! loss to NCDOC
										    + DPD32  /(D(I)*DZ(K))    !SUM(APQY32 * N_photons(I,K,:,3))    ! gain from CDOM 3
										 
			  DTWCDOC1(I,K) = DTWCDOC1(I,K) + DPD31  /(D(I)*DZ(K)) &  !SUM(APQY31 * N_photons(I,K,:,3)) &!
										    + DPD21  /(D(I)*DZ(K)) &  !SUM(APQY21 * N_photons(I,K,:,2))	       &		!
										    - DPD10  /(D(I)*DZ(K)) &  !SUM(APQY10 * N_photons(I,K,:,1))         &!
										    - DPD1N  /(D(I)*DZ(K))   ! SUM(APQY1N * N_photons(I,K,:,1))            ! loss to NCDOC
											
			! all losses to the non-colored pools get fractionated after their transformations
											
! THE IMPACT of PD on transformations from colored ---> non-colored

	!NCDOC1											
			DTWNCDOC1(I,K) = DTWNCDOC1(I,K) + DPD3N * FPDC31 &  !SUM(APQY3N * N_photons(I,K,:,3)) * FPDC31     &   ! non colored PD transformation from 3 to 1 (HMW colored to non-colored biolabile)
											+ DPD2N * FPDC21 &  !SUM(APQY2N * N_photons(I,K,:,2))* FPDC21	& ! same as above but 2 to 1 
											+ DPD1N * FPDC11    !SUM(APQY1N * N_photons(I,K,:,1)) * FPDC11   ! from LMW colored to bio-labile
	!NCDOC2										
			DTWNCDOC2(I,K) = DTWNCDOC2(I,K) + DPD3N * FPDC32  &  ! SUM(APQY3N * N_photons(I,K,:,3)) * FPDC32     &   ! non colored PD transformation from 3 to 1 (HMW colored to non-colored semi-labile)
											+ DPD2N * FPDC22 &  !    SUM(APQY2N * N_photons(I,K,:,2))* FPDC22	& ! same as above but 2 to 1 
											+ DPD1N * FPDC12    !SUM(APQY1N * N_photons(I,K,:,1)) * FPDC12   ! from LMW colored to bio-labile											
	
	!NCDOC3
			DTWNCDOC3(I,K) = DTWNCDOC3(I,K) + DPD3N * FPDC33 &  !SUM(APQY3N * N_photons(I,K,:,3)) * FPDC33     &   ! non colored PD transformation from 3 to 1 (HMW colored to non-colored refractory)
											+ DPD2N * FPDC23 &  !SUM(APQY2N * N_photons(I,K,:,2))* FPDC23	& ! same as above but 2 to 1 
											+ DPD1N * FPDC13    !SUM(APQY1N * N_photons(I,K,:,1)) * FPDC13   ! from LMW colored to bio-labile											
!											
											
	   		! get the total spectral absorption
			
			!TOTAL_CDOM_ABS = SUM(ICDOM_abs,4)
			
			!!! add in all of our favorite photochemistry output variables here...

							
		!	END DO							
		END DO
	END DO	
!	write(*,*)'DTWCDOC1 in mod wc = ',DTWCDOC1
  END IF

  
   ! the below still needs to be converted for use with SAV module
  ! 
  !IF (SAV_CALC) THEN    ! DOC_flag
  !  DO I=1,NSAVCELL     ! DOC_flag
  !    B=SAVCELL(I)      ! DOC_flag
  !    DTLDOC(B,KBM1)  = DTLDOC(B,KBM1)+(LDOCSAVW(B)+LDOCEPIW(B))/(D(B)*DZ(KBM1))/86400.     ! DOC_flag
  !    DTRDOC(B,KBM1)  = DTRDOC(B,KBM1)+(RDOCSAVW(B)+RDOCEPIW(B))/(D(B)*DZ(KBM1))/86400.     ! DOC_flag
!	ENDDO
  !END IF
  
  !IF(SAV_LOADS) THEN   ! DOC_flag
  !   DO I=1,NSAVCELL    ! DOC_flag
  !      B=SAVCELL(I)    ! DOC_flag
  !      DTLDOC(B,KBM1)  = DTLDOC(B,KBM1)+LDOCSAVW(B)/(D(B)*DZ(KBM1))/86400.      ! DOC_flag
  !      DTRDOC(B,KBM1)  = DTRDOC(B,KBM1)+RDOCSAVW(B)/(D(B)*DZ(KBM1))/86400.      ! DOC_flag
  !      DTDO(I,K) = (DOR1+DOR2-DOP1-DOP2-DDOC(I,K)      ! DOC_flag
!	   ENDDO
  !ENDIF

END SUBROUTINE DOC

SUBROUTINE DON!(DTWDON1,DTWCDON2,DTWCDON3,DTWNCDON1,DTWNCDON2,DTWNCDON3)
!========*=======*=========*========*=======*=======*=======*
!					    ALGAE DON							!
!========*=======*=========*========*=======*=======*=======*   
USE MOD_WQM, ONLY : DOXG, FTMNL, KRCOAG, KHCOAG, SALT, RATOX, &!
				    ANC1,ANC2,FNDP,ALGDON, PR1, PR2, HDRLPON, HDRRPON  !
!write(*,*)'CNitrogen Loop'
   DO K=1,KBM1
     DO I=1,MLOC
	 
        DOM1    = FND1*NP1(I,K)+FNDP*PR1(I,K)*ANC1    ! algal 1 labile don production
		         ! FNRD1*NP1(I,K)+FNRDP*PR1(I,K)*ANC1      ! algal 1 refrac don production
        DOM2     = FND2*NP2(I,K)+FNDP*PR2(I,K)*ANC2    ! algae 2 labile don production
				!   FNRD2*NP2(I,K)+FNRDP*PR2(I,K)*ANC2       ! algae 2 refractory don production
		!DOM3     = FNLD2*NP3(I,K)+FNLDP*PR3(I,K)*ANC2 + &   ! algae 3 labile don production
		!		   FNRD2*NP3(I,K)+FNRDP*PR3(I,K)*ANC2       ! algae 3 refractory don production		  
				   
      !  DOM2     = FNRD1*NP1+FNRDP*PR1(I,K)*ANC1        ! DON_flag		 |
      !  RDON2      = FNRD2*NP2+FNRDP*PR2(I,K)*ANC2        ! DON_flag		 V

        ALGDON(I,K) = DOM1+DOM2          ! DON_flag   ! total algae DON contribtuion
		
   ! REMINERALIZATION OF DON	
	   MNLDOM1 = RATOX(I,K)*KDON1(I,K)*FTMNL(I,K)*WC_NCDON1(I,K)    ! ! NON COLORED DON1 REMINERALIZATION 1/DAY
       MNLDOM2 = RATOX(I,K)*KDON2(I,K)*FTMNL(I,K)*WC_NCDON2(I,K)    ! NON COLORED DON1 REMINERALIZATION 1/DAY
	   MNLDOM3 = RATOX(I,K)*KDON3(I,K)*FTMNL(I,K)*WC_NCDON3(I,K) 	
	   MNLDON(I,K) = MNLDOM1+MNLDOM2+MNLDOM3
	   
   !COAGULATION OF NCDON3  
	    COAG        = 0.5*(1.+TANH(SALTC-KHCOAG))*KRCOAG(I,K)*WC_NCDON3(I,K) ! MOVED HERE AND NOW ONLY REFRACTORY DOC COAGULATES
	    COAGN(I,K) = COAG  ! set up to fill an array with the coagulation fflux from both colored and non colored
	!	write(*,*)'Coag = ',COAG
! NON-COLORED DON
        DTWNCDON1(I,K) = (DOM1*FNCND11+DOM2*FNCND21 -MNLDOM1+  &  ! ALGAL 1 AND 2 PRODUCTION OF DON - THE MINERALIZATION OF NCDON1
						 (HDRLPON(I,K) + HDRRPON(I,K))*FHDRNCN1  + &   ! THE HYDROLYSIS OF PON TO NCDON1
						 DONSZ1(I,K)*(1-FCDON1SZ)+DONLZ1(I,K)*(1-FCDON1LZ))/86400.      !ZOOPLANKTON CONTRIBUTION   
		
		DTWNCDON2(I,K) = (DOM1*FNCND12+DOM2*FNCND22-MNLDOM2+  &  ! ALGAL 1 AND 2 PRODUCTION OF DON - THE MINERALIZATION OF NCDON1
						 (HDRLPON(I,K) + HDRRPON(I,K))*FHDRNCN2  +         &     ! THE HYDROLYSIS OF PON TO NCDON2
						 DONSZ2(I,K)*(1-FCDON2SZ)+DONLZ2(I,K)*(1-FCDON2LZ))/86400.      !ZOOPLANKTON CONTRIBUTION      
		
		DTWNCDON3(I,K) = (DOM1*FNCND13+DOM2*FNCND23 -MNLDOM3  +  &  ! ALGAL 1 AND 2 PRODUCTION OF DON - THE MINERALIZATION OF NCDON1
						 (HDRLPON(I,K) + HDRRPON(I,K))*FHDRNCN3  +         &     ! THE HYDROLYSIS OF PON TO NCDON3
						 DONSZ3(I,K)*(1-FCDON3SZ)+DONLZ3(I,K)*(1-FCDON3LZ) -COAG)/86400.          !ZOOPLANKTON CONTRIBUTION     !DONSZ(I,K)*(1-FCDON3SZ)+DONLZ(I,K)*(1-FCDON3LZ)
		
! REMINERALIZATION OF DON	
		
	   MNLDOM1 = RATOX(I,K)*KDON1(I,K)*FTMNL(I,K)*WC_CDON1(I,K)    ! COLORED DON3 REMIN RATE 1/DAY
       MNLDOM2 = RATOX(I,K)*KDON2(I,K)*FTMNL(I,K)*WC_CDON2(I,K)    ! COLORED DON3 REMIN RATE 1/DAY
	   MNLDOM3 = RATOX(I,K)*KDON3(I,K)*FTMNL(I,K)*WC_CDON3(I,K) 	! COLORED DON3 REMIN RATE 1/DAY
       MNLDON(I,K) = MNLDON(I,K) + MNLDOM1 + MNLDOM2 + MNLDOM3 
	   
! COAGULATION OF CDON3	   
	   COAG        = 0.5*(1.+TANH(SALTC-KHCOAG))*KRCOAG(I,K)*WC_CDON3(I,K) ! MOVED HERE AND NOW ONLY REFRACTORY DOC COAGULATES
	   COAGN(I,K) =  COAGN(I,K)+COAG  ! set up to fill an array with the coagulation fflux from both colored and non colored
	!   write(*,*)'Coag = ',COAG
!COLORED DON
        DTWCDON1(I,K) = (DOM1*FCND11+DOM2*FCND21 -MNLDOM1+  &  ! ALGAL 1 AND 2 PRODUCTION OF DON - THE MINERALIZATION OF NCDON1
						 (HDRLPON(I,K) + HDRRPON(I,K))*FHDRCN1 +      &     ! THE HYDROLYSIS OF PON TO NCDON1
						 DONSZ1(I,K)*FCDON1SZ+DONLZ1(I,K)*FCDON1LZ)/86400.      !ZOOPLANKTON CONTRIBUTION   
		
		DTWCDON2(I,K) = (DOM1*FCND12+DOM2*FCND22 -MNLDOM2+  &  ! ALGAL 1 AND 2 PRODUCTION OF DON - THE MINERALIZATION OF NCDON1
						 (HDRLPON(I,K) + HDRRPON(I,K))*FHDRCN2  +       &     ! THE HYDROLYSIS OF PON TO NCDON1
						 DONSZ2(I,K)*FCDON2SZ+DONLZ2(I,K)*FCDON2LZ)/86400.      !ZOOPLANKTON CONTRIBUTION      
		
		DTWCDON3(I,K) = (DOM1*FCND13+DOM2*FCND23 -MNLDOM3    +  &  ! ALGAL 1 AND 2 PRODUCTION OF DON - THE MINERALIZATION OF NCDON1
						 (HDRLPON(I,K) + HDRRPON(I,K))*FHDRCN3 +         &     ! THE HYDROLYSIS OF PON TO NCDON1
						  DONSZ3(I,K)*FCDON3SZ+DONLZ3(I,K)*FCDON3LZ - COAG)/86400.      !ZOOPLANKTON CONTRIBUTION    	 !DONSZ(I,K)*FCDON3SZ+DONLZ(I,K)*FCDON3LZ
		
		
   ENDDO  ! MLOC LOOP
  ENDDO   ! KBMI LOOP
 !!!!END OF ALGAE DON
  

  !IF(ATMOS_LOADS) THEN ! Kurt Glaesemann 13 April 2015       ! DON_flag
  !  DO I=1,MLOC  											  ! DON_flag   ! atmospheric flux
  !     DTLDON(I,1)= DTLDON(I,1)+PRECIP*ATMLDON/(D(I)*DZ(1))     ! DON_flag
  !     DTRDON(I,1)= DTRDON(I,1)+PRECIP*ATMRDON/(D(I)*DZ(1))      ! DON_flag
!	ENDDO
  ! ENDIf
  !******* SAV and epiphytes
   !IF (SAV_CALC) THEN
   !  DO I=1,NSAVCELL
   !    B=SAVCELL(I)
   !    DTNH4(B,KBM1)   = DTNH4(B,KBM1) +( NH4SAVW(B)+ NH4EPIW(B))/(D(B)*DZ(KBM1))/86400.
   !    DTNO3(B,KBM1)   = DTNO3(B,KBM1) +( NO3SAVW(B)+ NO3EPIW(B))/(D(B)*DZ(KBM1))/86400.
   !    DTLDON(B,KBM1)  = DTLDON(B,KBM1)+(LDONSAVW(B)+LDONEPIW(B))/(D(B)*DZ(KBM1))/86400.
   !    DTRDON(B,KBM1)  = DTRDON(B,KBM1)+(RDONSAVW(B)+RDONEPIW(B))/(D(B)*DZ(KBM1))/86400.
   !    DTLPON(B,KBM1)  = DTLPON(B,KBM1)+(LPONSAVW(B)+LPONEPIW(B))/(D(B)*DZ(KBM1))/86400.
   !    DTRPON(B,KBM1)  = DTRPON(B,KBM1)+(RPONSAVW(B)+RPONEPIW(B))/(D(B)*DZ(KBM1))/86400./
   !  ENDDO
   !ENDIF

!******* Atmospheric Loads
 
   !IF(ATMOS_LOADS) THEN ! Kurt Glaesemann 13 April 2015       ! DON_flag
   ! DO I=1,MLOC  											  ! DON_flag
   !     DTLDON(I,1)= DTLDON(I,1)+PRECIP*ATMLDON/(D(I)*DZ(1))     ! DON_flag
   !     DTRDON(I,1)= DTRDON(I,1)+PRECIP*ATMRDON/(D(I)*DZ(1))      ! DON_flag
   ! ENDDO
   !ENDIF

IF(SED_DOM_FLAG) THEN ! B CLARK SEDIMENT DOM AUG 2015
!    write(*,*) 'DTWNCDON1 before = ',DTWNCDON1(5856,KBM1)
    DO I = 1,MLOC					! DON_flag    ! sediment flux	
	  DTWNCDON1(I,KBM1) = DTWNCDON1(I,KBM1)+JWNCDON1(I)/(D(I)*DZ(KBM1))/86400.
	  DTWNCDON2(I,KBM1) = DTWNCDON2(I,KBM1)+JWNCDON2(I)/(D(I)*DZ(KBM1))/86400.  
	  DTWNCDON3(I,KBM1) = DTWNCDON3(I,KBM1)+JWNCDON3(I)/(D(I)*DZ(KBM1))/86400.  
	  
	  DTWCDON1(I,KBM1) = DTWCDON1(I,KBM1)+JWCDON1(I)/(D(I)*DZ(KBM1))/86400.  
	  DTWCDON2(I,KBM1) = DTWCDON2(I,KBM1)+JWCDON2(I)/(D(I)*DZ(KBM1))/86400.  
	  DTWCDON3(I,KBM1) = DTWCDON3(I,KBM1)+JWCDON3(I)/(D(I)*DZ(KBM1))/86400.  
  	 !DTLDON(I,KBM1) = DTLDON(I,KBM1)+J_LDON_SHARE(I)/(D(I)*DZ(KBM1))/86400.         ! DON_flag
	 !DTRDON(I,KBM1) = DTRDON(I,KBM1)+J_RDON_SHARE(I)/(D(I)*DZ(KBM1))/86400.          ! DON_flag

        ENDDO  ! DON_flag

!    write(*,*) 'DT after = ',DTWNCDON1(5856,KBM1)
!    write(*,*)'FLUX =',JWNCDON1(5867)
   ENDIF   ! DON_flag

!   
!#if defined(1)   
!  IF(PHOTODEG) THEN ! Now have a simple formulation for photodegradation, if desirable
!						   ! The absorbance is currenlty based on the sum of the colored DOM
!		
!       
!  
!				! so the absorbance due to one is the fraction of the total pool of CDOM
!    
!!write(*,*)'photodeg is on'
!     DO I = 1, MTLOC
!	   DO K = 1, KBM1
!                      ! We can integrate it over the entire spectra and get a wavelength dependent CDOMx loss to photodegradation and then sum that up to get the total loss f
!					  ! from each pooll
!	  !    DO J = 1,NWAVEL   ! Loop over all wavelengths to get the integrated spectral apparent quantum yield for each waveband therefore total loss of DOM
!		  DPD32 = 0.
!		  DPD31 = 0.
!		  DPD30 = 0.
!		  DPD3N = 0.
!		  DPD21 = 0.
!		  DPD20 = 0.
!		  DPD2N = 0.
!		  DPD10 = 0.
!		  DPD1N = 0.
!		  
!!write(*,*)'N_photons in WCDOM = ',N_photons(:,:,51,:)
!		  
!	        DO J = 1,NWAVELx! integrate our changed carbon

!				  DPD32 = DPD32 + APQY32(J) * N_photons(I,K,J,3)  ! DP32 = mg C/mols Quanta * mols Quanta/m^2/s = mg C /m^2/s   ---> still need to depth integrate
!			      DPD31 = DPD31 + APQY31(J) * N_photons(I,K,J,3)
!				  DPD30 = DPD30 + APQY30(J) * N_photons(I,K,J,3)   ! Nitrogen that goes to ammonium
!				  DPD3N = DPD3N + APQY3N(J) * N_photons(I,K,J,3)
!				  
!				  DPD21 = DPD21 + APQY21(J) * N_photons(I,K,J,2)
!				  DPD20 = DPD20 + APQY20(J) * N_photons(I,K,J,2) ! nitrogen that goes to ammonium
!				  DPD2N = DPD2N + APQY2N(J) * N_photons(I,K,J,2)
!				  
!				  DPD10 = DPD10 + APQY10(J) * N_photons(I,K,J,1)
!				  DPD1N = DPD1N + APQY1N(J) * N_photons(I,K,J,1)
!				  		  
!				!  write(*,*)'integrating APQY '
!			!	  write(*,*)'DPD32 = ',DPD32
!			
!			ENDDO
!			
!			  ! need to convert the carbon to nitrogen, just use the marsh N to C ratio assuming that most of the photoreactive stuff comes from the wetland
!			
!			
!			DPD32 = DPD32*M_NtoC
!			DPD31 = DPD31*M_NtoC
!			DPD30 = DPD30*M_NtoC
!			DPD3N = DPD3N*M_NtoC
!			
!			DPD21 = DPD21*M_NtoC
!			DPD21 = DPD21*M_NtoC
!			
!			
!		!	! ADD IN HERE A VARIABLE TO ACCUMULATE ALL OF THE PHOTOCHEMICAL FLUXES
!		!	
!		!	
!		!! Accumulate total photodegradation out of CDOM3	
!		!	PHOTOCHEM_DOC_ACCUM(I,K,1) = PHOTOCHEM_DOC_ACCUM(I,K,1) - DPD32  &	
!		!													        - DPD31  &
!		!														    - DPD30 &
!		!															- DPD3N


!		!	! Accumulate total photodegradation into CDOM2 from CDOM3
!		!	PHOTOCHEM_DOC_ACCUM(I,K,2) = PHOTOCHEM_DOC_ACCUM(I,K,2) + DPD32
!		!	
!		!	! Accumulate total photodegradation out of CDOM2
!		!	PHOTOCHEM_DOC_ACCUM(I,K,3) = PHOTOCHEM_DOC_ACCUM(I,K,3) - DPD20      &
!		!															- DPD21    &
!		!															- DPD2N 	
!		!	
!	
!		!	! accumualte total CDOM photodegradtion from CDOM3 and CDOM2 into CDOM1
!		!	PHOTOCHEM_DOC_ACCUM(I,K,4) = PHOTOCHEM_DOC_ACCUM(I,K,4) + DPD31       &
!		!															+ DPD21

!		!	
!		!	
!		!	PHOTOCHEM_DOC_ACCUM(I,K,5) = PHOTOCHEM_DOC_ACCUM(I,K,3) - DPD10      &
!		!															- DPD1N 
!		!	
!		!	!CDOM lost to NCDOM
!		!	PHOTOCHEM_DOC_ACCUM(I,K,6) = PHOTOCHEM_DOC_ACCUM(I,K,6) +	DPD3N   &
!		!															+DPD2N   &
!		!															+DPD1N
!		!												

!		!	!
!			!
!			
!			
!			
!		!	write(*,*)'Depth = ',D(I)
!	!	if(DPD32 > 0.1) THEN
!		
!!	      write(*,*) ' DPD32 = ', DPD32
!		  
!	!    endif
!	!a		write(*,*)'DTW before = ',DTWCDOC3(I,K)
!!			
!!	   
!	   	      DTWCDON3(I,K) = DTWCDON3(I,K) - DPD32 /(D(I)*DZ(K)) & !SUM(APQY31 * N_photons(I,K,:,3)) &   !integrated loss to CDOC1
!											- DPD31 /(D(I)*DZ(K)) & !SUM(APQY32 * N_photons(I,K,:,3)) &    ! loss to CDOC2
!											- DPD30 /(D(I)*DZ(K)) & !SUM(APQY30 * N_photons(I,K,:,3))   &   ! loss to DIC		
!											- DPD3N /(D(I)*DZ(K))   !SUM(APQY3N * N_photons(I,K,:,3))    ! loss to NCDOC  ! Don't know if this pathway exists, really, because there is inherent color loss in the loss to the less colored pools...
!	   
!											
!	
!										!	write(*,*)DPD30 /(D(I)*DZ(I)) 
!										!	write(*,*)DPD21 /(D(I)*DZ(I)) 
!										!	write(*,*)DPD32 /(D(I)*DZ(I)) 
!			!	write(*,*)'DTW after = ',DTWCDOC3(I,K)
!	   
!			  DTWCDON2(I,K) = DTWCDOCN(I,K) - DPD21  /(D(I)*DZ(K)) &  !SUM(APQY21 * N_photons(I,K,:,2) )  &!
!										    - DPD20  /(D(I)*DZ(K)) &  !SUM(APQY20 * N_photons(I,K,:,2) )   &!
!										    - DPD2N  /(D(I)*DZ(K)) &  !SUM(APQY2N * N_photons(I,K,:,2) )       &      ! loss to NCDOC
!										    + DPD32  /(D(I)*DZ(K))    !SUM(APQY32 * N_photons(I,K,:,3))    ! gain from CDOM 3
!										 
!			  DTWCDON1(I,K) = DTWCDON1(I,K) + DPD31  /(D(I)*DZ(K)) &  !SUM(APQY31 * N_photons(I,K,:,3)) &!
!										    + DPD21  /(D(I)*DZ(K)) &  !SUM(APQY21 * N_photons(I,K,:,2))	       &		!
!										    - DPD10  /(D(I)*DZ(K)) &  !SUM(APQY10 * N_photons(I,K,:,1))         &!
!										    - DPD1N  /(D(I)*DZ(K))   ! SUM(APQY1N * N_photons(I,K,:,1))            ! loss to NCDOC
!											
!			! all losses to the non-colored pools get fractionated after their transformations
!											
!! THE IMPACT of PD on transformations from colored ---> non-colored

!	!NCDOC1											
!			DTWNCDOC1(I,K) = DTWNCDON1(I,K) + DPD3N * FPDC31 &  !SUM(APQY3N * N_photons(I,K,:,3)) * FPDC31     &   ! non colored PD transformation from 3 to 1 (HMW colored to non-colored biolabile)
!											+ DPD2N * FPDC21 &  !SUM(APQY2N * N_photons(I,K,:,2))* FPDC21	& ! same as above but 2 to 1 
!											+ DPD1N * FPDC11    !SUM(APQY1N * N_photons(I,K,:,1)) * FPDC11   ! from LMW colored to bio-labile
!	!NCDOC2										
!			DTWNCDOC2(I,K) = DTWNCDON2(I,K) + DPD3N * FPDC32  &  ! SUM(APQY3N * N_photons(I,K,:,3)) * FPDC32     &   ! non colored PD transformation from 3 to 1 (HMW colored to non-colored semi-labile)
!											+ DPD2N * FPDC22 &  !    SUM(APQY2N * N_photons(I,K,:,2))* FPDC22	& ! same as above but 2 to 1 
!											+ DPD1N * FPDC12    !SUM(APQY1N * N_photons(I,K,:,1)) * FPDC12   ! from LMW colored to bio-labile											
!	
!	!NCDOC3
!			DTWNCDOC3(I,K) = DTWNCDON3(I,K) + DPD3N * FPDC33 &  !SUM(APQY3N * N_photons(I,K,:,3)) * FPDC33     &   ! non colored PD transformation from 3 to 1 (HMW colored to non-colored refractory)
!											+ DPD2N * FPDC23 &  !SUM(APQY2N * N_photons(I,K,:,2))* FPDC23	& ! same as above but 2 to 1 
!											+ DPD1N * FPDC13    !SUM(APQY1N * N_photons(I,K,:,1)) * FPDC13   ! from LMW colored to bio-labile											
!!											
!											
!	   		! get the total spectral absorption
!			
!			!TOTAL_CDOM_ABS = SUM(ICDOM_abs,4)
!			
!			!!! add in all of our favorite photochemistry output variables here...

!							
!		!	END DO							
!		END DO
!	END DO	
!!	write(*,*)'DTWCDOC1 in mod wc = ',DTWCDOC1
!  END IF
!#endif 
!   
   
   
   
   
END SUBROUTINE DON

 SUBROUTINE DOP!(DTWCDOP1,DTWCDOP2,DTWCDOP3,DTWNCDOP1,DTWNCDOP2,DTWNCDOP3) 
 !calculate DOP kinetics
 USE MOD_WQM, ONLY : DOXG, FTMNL, KRCOAG, KHCOAG, SALT, RATOX, &!
				     Q1,Q2,FPDP,ALGDOP, PR1, PR2, HDRLPOP, HDRRPOP   !
!========*=======*=========*========*=======*=======*=======*
!					   ALGAE DOP							!
!========*=======*=========*========*=======*=======*=======*
!write(*,*)'Phosphorus Loop'
DO I  = 1,MLOC
   DO K = 1,KBM1
	DOM1 =  FPD1*PP1(I,K)+FPDP*PR1(I,K)*Q1(I,K) !+    ! ALGAE 1 DOP CREATION  ! 
	        !FPRD1*PP1(I,K)+FPRDP*PR1(I,K)*Q1(I,K)       ! ALGAE 1 RDOP CREATION
			
	DOM2 =  FPD2*PP2(I,K)+FPDP*PR2(I,K)*Q2(I,K)! + &    ! ALGAE 2 LDOP CREATION 
	    !    FPRD2*PP2(I,K)+FPRDP*PR2(I,K)*Q2(I,K)       ! ALGAE 2 RDOP CREATION
	
    ALGDOP(I,K) = DOM1 + DOM2
	
! REMINERALIZATION OF DOP
	   MNLDOM1 = RATOX(I,K)*KDOP1(I,K)*FTMNL(I,K)*WC_NCDOP1(I,K)    ! ! NON COLORED DON1 REMINERALIZATION 1/DAY
       MNLDOM2 = RATOX(I,K)*KDOP2(I,K)*FTMNL(I,K)*WC_NCDOP2(I,K)    ! NON COLORED DON1 REMINERALIZATION 1/DAY
	   MNLDOM3 = RATOX(I,K)*KDOP3(I,K)*FTMNL(I,K)*WC_NCDOP3(I,K)
	   MNLDOP(I,K) = MNLDOM1 + MNLDOM2 + MNLDOM3
	   
	   COAG        = 0.5*(1.+TANH(SALTC-KHCOAG))*KRCOAG(I,K)*WC_NCDOP3(I,K) ! MOVED HERE AND NOW ONLY REFRACTORY DOP COAGULATES   
       COAGP(I,K)=COAG
	!   write(*,*) 'Coag = ',COAG
	   
! NON COLORED DOP   
	   DTWNCDOP1(I,K) = (DOM1*FNCPD11 + DOM2*FNCPD21 - MNLDOM1 &  ! ALGAL 1 AND 2 DOP PRODUCTION - REMINERALIZATOIN
						+(HDRLPOP(I,K)+HDRRPOP(I,K))*FHDRNCP1  &     !+ POP HYDROLYSIS 
                        +DOPSZ1(I,K)*(1-FCDOP1SZ)+DOPLZ1(I,K)*(1-FCDOP1LZ))/86400.   !+ ZOOPLANKTON PRODUCTION
	   
	   DTWNCDOP2(I,K) = (DOM1*FNCPD12+DOM2*FNCPD22 - MNLDOM2  & ! ALGAL 1 AND 2 DOP PRODUCTION - REMINERALIZATOIN
						+(HDRLPOP(I,K)+HDRRPOP(I,K))*FHDRNCP2  &     !+ POP HYDROLYSIS 
                        +DOPSZ2(I,K)*(1-FCDOP2SZ)+DOPLZ2(I,K)*(1-FCDOP2LZ))/86400.   !+ ZOOPLANKTON PRODUCTION

	   DTWNCDOP3(I,K) = (DOM1*FNCPD13+DOM2*FNCPD23 - MNLDOM3  &   ! ALGAL 1 AND 2 DOP PRODUCTION - REMINERALIZATOIN
						+(HDRLPOP(I,K)+HDRRPOP(I,K))*FHDRNCP3  &     !+ POP HYDROLYSIS 
                         +DOPSZ3(I,K)*(1-FCDOP3SZ)+DOPLZ3(I,K)*(1-FCDOP3LZ )- COAG)/86400.   !+ ZOOPLANKTON PRODUCTION    ! +DOPSZ(I,K)*(1-FCDOP3SZ)+DOPLZ(I,K)*(1-FCDOP3LZ) )
	   
	   MNLDOM1 = RATOX(I,K)*KDOP1(I,K)*FTMNL(I,K)*WC_CDOP1(I,K)    ! ! NON COLORED DON1 REMINERALIZATION 1/DAY
       MNLDOM2 = RATOX(I,K)*KDOP2(I,K)*FTMNL(I,K)*WC_CDOP2(I,K)    ! NON COLORED DON1 REMINERALIZATION 1/DAY
	   MNLDOM3 = RATOX(I,K)*KDOP3(I,K)*FTMNL(I,K)*WC_CDOP3(I,K) 	
           MNLDOP(I,K) = MNLDOP(I,K) + MNLDOM1 + MNLDOM2 + MNLDOM3
	   
	   COAG        = 0.5*(1.+TANH(SALTC-KHCOAG))*KRCOAG(I,K)*WC_CDOP3(I,K) ! MOVED HERE AND NOW ONLY REFRACTORY DOC COAGULATES 
	   COAGP(I,K)=COAGP(I,K)+COAG
!!!  COLORED DOP 
	!   write(*,*)'Coag = ',COAG
	   DTWCDOP1(I,K) = (DOM1*FCPD11+DOM2*FCPD21 -MNLDOM1  &  ! ALGAL 1 AND 2 DOP PRODUCTION - REMINERALIZATOIN
						+(HDRLPOP(I,K)+HDRRPOP(I,K))*FHDRCP1  &     !+ POP HYDROLYSIS 
                        +DOPSZ1(I,K)*FCDOP1SZ+DOPLZ1(I,K)*FCDOP1LZ)/86400.   !+ ZOOPLANKTON PRODUCTION
	   
	   DTWCDOP2(I,K) = (DOM1*FCPD12+DOM2*FCPD22 -MNLDOM2  & ! ALGAL 1 AND 2 DOP PRODUCTION - REMINERALIZATOIN
						+(HDRLPOP(I,K)+HDRRPOP(I,K))*FHDRCP2  &     !+ POP HYDROLYSIS 
                        +DOPSZ2(I,K)*FCDOP2SZ+DOPLZ2(I,K)*FCDOP2LZ)/86400.   !+ ZOOPLANKTON PRODUCTION

	   DTWCDOP3(I,K) = (DOM1*FCPD13+DOM2*FCPD23 - MNLDOM3  &  ! ALGAL 1 AND 2 DOP PRODUCTION - REMINERALIZATOIN
						+ (HDRLPOP(I,K)+HDRRPOP(I,K))*FHDRCP3  &     !+ POP HYDROLYSIS 
                       + DOPSZ3(I,K)*FCDOP3SZ+DOPLZ3(I,K)*FCDOP3LZ - COAG ) /86400.   !+ ZOOPLANKTON PRODUCTION !!+DOPSZ(I,K)*FCDOP3SZ+DOPLZ(I,K)*FCDOP3LZ)
	ENDDO ! MLOC
   ENDDO  !KBM1
!!! END ALGAE DOP	

  IF(SED_DOM_FLAG)  THEN   ! DOP_flag

!	 write(*,*)'NCDOP1 before = ',DTWNCDOP1(5856,KBM1) 
!        write(*,*) ' JWNCDOP1 = ',JWNCDOP1(5856)
   DO I = 1,MLOC     ! DOP_flag

      DTWNCDOP1(I,KBM1) = DTWNCDOP1(I,KBM1)+JWNCDOP1(I)/(D(I)*DZ(KBM1))/86400.
	  DTWNCDOP2(I,KBM1) = DTWNCDOP2(I,KBM1)+JWNCDOP2(I)/(D(I)*DZ(KBM1))/86400.  
	  DTWNCDOP3(I,KBM1) = DTWNCDOP3(I,KBM1)+JWNCDOP3(I)/(D(I)*DZ(KBM1))/86400.  
	  
	  DTWCDOP1(I,KBM1) = DTWCDOP1(I,KBM1)+JWCDOP1(I)/(D(I)*DZ(KBM1))/86400.  
	  DTWCDOP2(I,KBM1) = DTWCDOP2(I,KBM1)+JWCDOP2(I)/(D(I)*DZ(KBM1))/86400.  
	  DTWCDOP3(I,KBM1) = DTWCDOP3(I,KBM1)+JWCDOP3(I)/(D(I)*DZ(KBM1))/86400.  
   ENDDO   ! DOP_flag
!	 write(*,*)'NCDOP1 = ',DTWNCDOP1(5856,KBM1) 
  ENDIF     ! DOP_flag



END SUBROUTINE DOP 


!!!!=============================================================!!!!
!						     FUNCTIONS
!!!!=============================================================!!!!

SUBROUTINE SANITY_CHECK(in_string,in_vars,total_sum,tolerance)

use mod_prec, only: SP

use mod_control, only: MSR
!in_string = pass in a string that defines what is being checked
!in_vars = then pass in all the vars summed up
!total_sum = and the total that you want the vars to add up to
IMPLICIT none

CHARACTER(LEN=:),allocatable, intent(in) :: in_string

 
REAL(SP), INTENT(IN) :: in_vars  ! in
 Real :: diffs, total_sum,tolerance 
 diffs = in_vars - total_sum


       
    IF (abs(diffs) > tolerance) THEN
      write(*,*)'difference between vars is --->,',diffs
      write(*,*) in_string,'Does not equal the required total'
      write(*,*) Total_Sum
      write(*,*) 'Currently the sum of the parameters is',in_vars
      write(*,*) 'Check input file, stopping program'
      stop 
	END iF


       
END SUBROUTINE SANITY_CHECK

END MODULE




