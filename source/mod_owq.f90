
MODULE MOD_OWQ
      
!********************************************************************************
!**                             OWQ Model Setup (Ocean Water Color)            **
!********************************************************************************
 USE MOD_WQM, ONLY :			&		
                B1,             &!
                B2,             &!
                B3,             &!
                CHLCMN1,        &!
                CHLCMN2,        &!
                CHLCMN3,        &!
                I0,             &!
                JDAY,           &!
               ! LDOC,           &!
                LPOC,           &!
              !  RDOC,           &!
                RPOC,           &!
                SREFLECT,       &!
                SSI,   &!,            
				SPVARKE,PRINTKE

   !USE MOD_LIMS, ONLY: MLOC
   USE MOD_HYDROVARS, ONLY:     &
   		!GRAV	&		!
		!,PI	&		!
		!,PI2	&		!
		!,ZERO	&		!
		!,ONE_THIRD	&	!
		!,NVG	&		!
		!,XG	&		!GLOBAL X-COORD AT NODE 
		!,YG	&		!GLOBAL X-COORD AT NODE 
		!,HG	&		!GLOBAL DEPTH AT NODE 
		!,XCG	&		!GLOBAL X-COORD AT FACE CENTER 
		!,YCG	&		!GLOBAL X-COORD AT FACE CENTER 
		!,VXMIN	&		!
		!,VYMIN	&		!
		!,VXMAX	&		!
		!,VYMAX	&		!
		!,XC	&		!X-COORD AT FACE CENTER 
		!,YC	&		!Y-COORD AT FACE CENTER
		!,VX	&		!X-COORD AT GRID POINT
		!,VY	&		!Y-COORD AT GRID POINT
		!,ART	&		!AREA OF ELEMENT
		!,ART1	&		!AREA OF NODE-BASE CONTROl VOLUME
		!,ART2	&		!AREA OF ELEMENTS AROUND NODE
		!,NV	&		!NODE NUMBERING FOR ELEMENTS
		!,NBE	&		!INDICES OF ELMNT NEIGHBORS
		!,NTVE	&		!
		!,NTSN	&		!
		!,ISONB	&		!NODE MARKER = 0,1,2 
		!,ISBC	&		!
		!,ISBCE	&		!
		!,IEC	&		!
		!,IENODE &		!
		!,NBSN	&		!
		!,NIEC	&		!
		!,NTRG	&		!
		!,NBVE	&		!
		!,NBVT	&		!
		!,LISBCE_1	&	!LIST OF ELEMENTS WITH ISBCE=1
		!,LISBCE_2	&	!LIST OF ELEMENTS WITH ISBCE=2
		!,LISBCE_3	&	!LIST OF ELEMENTS WITH ISBCE=3
		!,DLTXC	&		!
		!,DLTYC	&		!
		!,DLTXYC	&	!
		!,DLTXE	&		!
		!,DLTYE	&		!
		!,DLTXYE	&	!
		!,SITAC	&		!
		!,SITAE	&		!
		!,XIJC	&		!
		!,YIJC	&		!
		!,XIJE	&		!
		!,YIJE	&		!
		!,EPOR	&		!ELEMENT FLUX POROSITY (=0. IF ISBCE = 2)
		!,IBCGEO	&	!LOCAL GEOSTROPHIC FRICTION CORRECTION NODES

		!,Z	&			!SIGMA COORDINATE VALUE 
		!,ZZ	&		!INTRA LEVEL SIGMA VALUE
		  DZ	&		!DELTA-SIGMA VALUE
		!,DZZ	&		!DELTA OF INTRA LEVEL SIGMA 
		!,H1	&		!BATHYMETRIC DEPTH 
		!,H	&			!BATHYMETRIC DEPTH 
		 ,D	!&			!CURRENT DEPTH 
		!,DT	&		!DEPTH AT PREVIOUS TIME STEP
		!,DT1	&		!DEPTH AT PREVIOUS TIME STEP
		!,EL	&		!CURRENT SURFACE ELEVATION
		!,ET	&		!SURFACE ELEVATION AT PREVIOUS TIME STEP
		!,DTFA	&		!ADJUSTED DEPTH FOR MASS CONSERVATION
		!,UU	&		!X-VELOCITY
		!,VV	&		!Y-VELOCITY
		!,UUT	&		!X-VELOCITY FROM PREVIOUS TIMESTEP
		!,VVT	&		!Y-VELOCITY FROM PREVIOUS TIMESTEP
		!,WWT	&		!Z-VELOCITY FROM PREVIOUS TIMESTEP
		!,WTST	&		!Vertical velocity in sigma from PREVIOUS TIMESTEP
		!,UARD_OBCNT	&!tykim
		!,XFLUX_OBCT	&!tykim
		!,DTFAT	&		!tykim
		!,TT_T	&		!tykim
		!,SALTT	&		!tykim
		!,WTS	&		!VERTICAL VELOCITY IN SIGMA SYSTEM
		!,UARD_OBCN	&	! tykim 
		!,XFLUX_OBC	&	! tykim 
		!,WTTS	&		!VERTICAL VELOCITY IN SIGMA SYSTEM 
		!,KH	&		!TURBULENT DIFFUSIVITY
		!,A1U	&		!
		!,A2U	&		!
		!,AWX	&		!
		!,AWY	&		!
		!,AW0	&		!
		!,VISCOFH	&	!
		!,UNC1	&		!
		!,VNC1	&		!
		!,WNC1	&		!
		!,WTSNC1	&		!
		!,UARD_OBCNNC1	&	!
		!,XFLUX_OBCNC1	&	!
		!,DTFANC1	&		!
		!,KHNC1	&		!
		!,TNC1	&		!
		!,SNC1	&		!
		!,ELNC1	&		!
		!,UNC2	&		!
		!,VNC2	&		!
		!,WNC2	&		!
		!,WTSNC2	&	!
		!,UARD_OBCNNC2	&!
		!,XFLUX_OBCNC2	&!
		!,DTFANC2	&	!
		!,KHNC2	&		!
		!,TNC2	&		!
		!,SNC2	&		!
		!,ELNC2	&		!
		!,num_hyd_ints	&!number of records in each hydrodynamics netcdf file
		!,TIME_MAP	&	!
		!,THOUR1	&	!SIMULATION TIME AT END OF CURRENT EXTERNAL STEP (IEXT) IN HOURS
		!,THOUR	&		!
		!,NCFILE_DIR	&!
		!,NCFILE_PREFIX	&!
		!,NCFILE_SUFFIX	&!
		!,NCFILE_NUMBER	&!
		!,FORMAT_STR	&!
		!,hydro_dir, 	&	! directory name where hydrodynamics results (netcdf) files are stored
		!,hydro_prefix, &	! prefix of file name, e.g. 'psm_'
		!,hydro_suffix	&	! suffix of filename, e.g. '.nc'
		!,hydro_filenumwidth, &	! number of digits in filename following hydro_prefix, e.g. 4 for psm_0002.nc
		!,hydro_filenumstart, &	! starting number of the file name in the digital part of the file name, e.g. 185 for psm_0185.nc
		!,hydro_Nrec	&		! number of records in each of hydrodynamics file
		!,hydro_dlt	&			! time step in hydrodynamics file (in seconds), e.g. 100 for 100sec
		!,t_his_start	&		!
		!,t_his_end	&			!
		!,t_his_dlt	&			!starting time, ending time, and interval of history outputs (days)
		!,Nstation	&			!
		!,NstationNum_GL	&	!maximum number of station is NstationMax!
		!,t_stn_start	&		!
		!,t_stn_end	&			!
		!,t_stn_dlt	&			!starting time, ending time, and interval of station outputs (days)
		!,STNFN	&				!file name for station output
		!,HISFN	&				!file name for history output
		!,HISFN_PREFIX	&		!prefix of history output file
		!,HISFN_EXT	&			!extention name of history output file
		!,HISFN_FINAL	&		! 
		!,HISFN_SPLIT_BYLEVEL	&!True or False for splitting history output in files level by level (default is .FALSE.)
		!,hydro_netcdf	&		!
		!,wqm_history	&		!
		!,wqm_stations	&		!
		!,IFNC	&				!file number index for hydrodynamics netcdf files, set to hydro_filenumstart initially for cold start, set otherwise 
		!,NTRECNC	&			!time record index for a particular hydrodynamics netcdf file, reset to 1 upon opening new file. 
		!,NTHYDRO				!overall time record index for all netcdf files, increment by 1 each time a hydrodynamics record is read


        USE MOD_LIMS, ONLY: MTLOC, 		&
						    KBM1,		&
							MLOC
							
		USE MOD_SIZES, ONLY: MGL
		USE MOD_FILEINFO, ONLY: KEI
		
		USE MOD_CONTROL, ONLY: PAR,	&
								SERIAL		






							
	    USE MOD_PREC, ONLY: SP										
		
		USE MOD_ZOOP, ONLY : 	SZ,		&
								LZ
		
								

        USE WC_DOM, only:   WC_CDOC1,WC_CDOC2,WC_CDOC3,  & !get our colored DOC from mod_wc_dom B Clark Sep 2015		
							TOTAL_CDOM_ABS,   &
						   ICDOM_abs, PHOTODEG, NWAVELx,   &
						   N_photons
						   
						   
		USE IFPORT    ! recommended by intel when using random number generator
						   




						   
IMPLICIT NONE		


		REAL(SP) :: 	INITKE, 			&
					INTKE,				&
					NXKEI
							
		REAL(SP) :: 	LAT, 				&	
					LON
 

		
			REAL(SP) ::	PHISTAR676,			&
					    BNSTARNTU,			&
					    BNSTARCHL,      	&
					   PSTARINT,			&
					   PSTAR440,			&
					   PSTARCOLOR,			&
					   SCDOM,				&
					   EZEROINT,    		&
					   TCHL2CHL,			&
					   DOFFSET,			&
					   DEPTHKE		



        INTEGER :: NWAVEL

        REAL(SP),ALLOCATABLE,DIMENSION(:,:) :: 	COLOR, 	&
												TURB, &
												TOTCHL
												
		REAL(SP) :: TSS, TOTPOC										


	!	REAL(SP), ALLOCATABLE ::! COLORS(:,:,:), &   ! THE COLORS FOR THE Kd calculation, 1 for each CDOM COLORS(I,K,1:3)
			!					 AG440(:,:,:),  & ! the absorbance at 440, wil have one for each CDOM (I,K,1:3)
							!	 AP440(:,:,:)!,  &		!Turbidity absorbance curve
		
		REAL(SP), ALLOCATABLE :: COLORS(:,:),   &   !THE COLORS FOR THE Kd calculation, 1 for each CDOM COLORS(I,K)	
								 AP440(:,:)			! the absorbance at 440, wil have one for each CDOM (I,K)
								 
		REAL(SP), ALLOCATABLE ::  APHI676(:,:), B440(:,:)
		REAL(SP) ::  ICDOM_bot      ! POC   					
		
		REAL(SP), ALLOCATABLE :: IAVG_pd(:,:,:)   ! avg light in photoedgradation (MTLOC,KBM1,NWAVEL)
	    REAL(SP),ALLOCATABLE ::  ABSCDOM(:,:,:,:) !,   & ! absorbance due to cdom at each wavelevnth (MTLOC,KBM1,NWAVEL,3)
							  

	   
	   INTEGER ::  PAR_MARK  ! where visible light begins in our wave spectra
	   
	   REAL(SP), ALLOCATABLE :: I0_lambda(:)     ! spectrally dependnet surface irradiance (Calcualte from SWRAD and broken down into spectra)
	   REAL(SP), ALLOCATABLE :: KDLAMB_out(:,:,:)  ! our spectral dependency of the total Kd (m^-1) (MTLOC,KBM1,NWAVEL)
	   REAL(SP),ALLOCATABLE  :: GLAMB(:,:)   ! The absorbance curves for our CDOM 1,2,3 at each wavelenght. read from the input file kei  ! should make these allocatable
	   REAL(SP):: GSTAR440		! absorbance of our CDOM 1,2,3 at 440 nm (reference wavelength)






	  
												
        REAL(SP),SAVE :: 	OPZ(9), 	&
						G1(9),		&
						G2(9)
						
	     REAL(SP),ALLOCATABLE, DIMENSION(:) ::  ALAMB, 			&   ! B Clark: Before these vars were just assigned an arbitiraty dimension to fill, now they will be allocated to the size of the wavelength 
																	! array  
								
								PLAMB,			&
								PHILAMB,        &
                                BLAMB,			&
								WAVEL,			&
								EZERO, EZ	
								
		
						
									
        REAL(SP),DIMENSION(10) :: GMUZERO
		
		REAL(SP), ALLOCATABLE::	IAVG(:,:),	& ! Average irradiance for each cell at layer k (MTLOC,KBM1)
											
								KESS(:,:), &  ! Total attenuation coefficient from OWQ model (m^-1) (MTLOC,KBM1)
								IATBOT_out(:)  !Irradiance reaching the sediments (MTLOC)
												
		 INTEGER:: I,J,K,IDOM,Iscat
! from subroutine LIGHT ATN		 
   REAL(SP)    :: ITHTAA

   REAL(SP),PARAMETER :: TWOPI = 6.2832
!vjp 9/19/2005 added declarations
   REAL(SP) :: DOY, HOUR, SOLDATE, DECL, TAU, CTHTAA, THTAW, AVREFCOS
   REAL(SP) ::TOTDOC, IATTOP, OPTDEPTH 



    REAL(SP), ALLOCATABLE :: IATBOT(:)   ! need to make left over irradiance!allocatble if using the photodegradation
	
!	INTEGER :: CDOM_counter ,  &! the number of spectra we will have for our CDOM array, need to allocate array after reading in the number at the start of each input file
			   
!				 myCDOM_ID    ! the random number counter for our CDOM abs spectra
	
!	REAL(SP):: myRAND			 
				 
!	REAL(SP),ALLOCATABLE :: CDOM1_ABSin(:,:),CDOM2_ABSin(:,:), CDOM3_ABSin(:,:)   ! our forced absorption spectra read in on input
	



   
   REAL(SP) :: CDOM_optdepth ! Added by B Clark for photodegradation  Nov 2015
    
		 
! from subroutine photochemistry  
   REAL(SP) :: PLR,DEPTH,REFSCAT,EZINT

   REAL(SP) :: ABCHL, ABNAPM, ABTOT, SCAT, KDLAMB	 
	
      REAL(SP) SUMI

CONTAINS

!=========================================================================
!
!=========================================================================
   
   SUBROUTINE OWQ_READ
       !USE MOD_LIMS, ONLY: MLOC
 
  !     CHARACTER(120) :: CDOM_ASTAR_FIN,CDOM_ASTAR_FOUT  ! file for reading in the CDOM specfic absorption spectra arrays
	   
	   REAL(SP), ALLOCATABLE :: RTMP2(:,:),RTMP21(:,:)
	   
       INTEGER :: I, K, CDOM_wavel, iCDOM
       write(*,*)'Reading the Light Field definition file'
       READ(KEI,1032)
       READ(KEI,1038) INTKE, INITKE
	   


	 ! 
	 !READ(KEI,10371) GSTAR440(1),GSTAR440(2), GSTAR440(3), PHISTAR676,BNSTARNTU,BNSTARCHL,PSTARINT,   &
     !                      PSTAR440,PSTARCOLOR, SCDOM 
	   
	   	   
       READ(KEI,1037) GSTAR440,PHISTAR676,BNSTARNTU,BNSTARCHL,PSTARINT,   &   ! dont need a spectral GSTAR440 if we have the CDOM normalized absorption curves
                      PSTAR440,PSTARCOLOR,SCDOM  ! just read in the same as before
					  
		   
	   ! OPEN OUR CDOM SPECFIC SPECTRA FILE FOR CDOM1
	 !  CDOM_ASTAR_FIN = 'CDOM1_specABS.dat'
	!   CDOM_ASTAR_FOUT = 'CDOM1_out.dat'
	 !  OPEN(unit=954,file=CDOM_ASTAR_FIN)
	 !  OPEN(UNIT=955,file=CDOM_ASTAR_FOUT)
	   
	!   READ(954,*)CDOM_counter, CDOM_wavel   ! first number of file is number of columns, second is number of row (wavelengths)
	!  write(*,*)'CDOM counter = ',CDOM_counter
	!   write(*,*)'CDOM wavelenghts = ',CDOM_wavel
	   








					  
					  
       READ(KEI,1036) LAT, LON, NWAVEL, TCHL2CHL, DOFFSET, DEPTHKE  ! can just modify the input file so it reads in more wavelengths, pretty easy!!!
	   write(*,*) 'NWAVEL = ',NWAVEL
	CALL OWQ_ALLOC   ! move the OWQ alloc here so that we can make our variables the cocrect size (NWAVEL)
	  ! write(*,*)'Gstart = ',GSTAR440
! INITIALIZE KEI INTERVAL, KESS

       NXKEI = INTKE/24.
       !DO K=1,KBM1
	   !DO I=1,MLOC
			KESS = INITKE  ! B Clark dont know why this was looping when it is spatially constant value...
	   !ENDDO  
       !ENDDO

! CONVERT FROM DECIMAL DEGREES TO RADIANS
       LAT = 6.2832*LAT/360.
       LON = 6.2832*LON/360.

       READ(KEI,1032)
	   
	   	   

	   !IF(NWAVEL /= CDOM_wavel)THEN
	   
	!		write(*,*)' Number of CDOM specfic absorption wavelenghts'
	!		write(*,*)' Is not equal to the number of wavelentghs in KEI file'
	!		write(*,*)' Stopping Program'
	!		STOP
	!		
	   !END IF
	   
	 ! CDOM2_ABSin(:,:), CDOM3_ABSin(:,:) 
	   
	!   ALLOCATE(CDOM1_ABSin(CDOM_wavel,CDOM_counter))  ! allocate the variable to the number of wavelengths x the number of spectra
	  
	DO I =1 , NWAVEL   
!	    write(*,*)
		
	   READ(KEI,*) WAVEL(I),ALAMB(I),GLAMB(I,1),GLAMB(I,2),GLAMB(I,3),PHILAMB(I),PLAMB(I),      &
                    BLAMB(I),EZERO(I)
					
		!			
		!DO iCDOM = 1,CDOM_counter
		
		!    read(954,398(11.5F))CDOM1_ABSin(I,:)
		!	write(*,*)' i, cdom counter',I,iCDOM
		!	write(*,*)'CDOM1_abs = ',CDOM1_ABSin(I,iCDOM)
		!	
		
		!END DO
		!		
					
	!		READ(954,*) (CDOM1_ABSin(I,J),J=1,CDOM_counter)
			!read(954,*)				
	END DO   
	
	  !  read(954,*)CDOM1_ABSin
			!write(*,*)' i, cdom counter',I,iCDOM
		!	write(*,*)'CDOM1_abs = ',CDOM1_ABSin

!	write(*,*) (CDOM1_ABSin(I,J),J=1,CDOM_counter)
!	close(954)






       READ (KEI,1082) SPVARKE, PRINTKE
       IF (SPVARKE == 'CONSTANT') THEN
			READ (KEI,1087) COLOR(1,1),TURB(1,1)
			DO K=1,KBM1
			DO I=1,MTLOC
				COLOR(I,K)   = COLOR(1,1)
				TURB(I,K)    = TURB(1,1)
			ENDDO  
			ENDDO
       ELSE
			ALLOCATE(RTMP2(MGL,KBM1));    RTMP2 = 0.0
			ALLOCATE(RTMP21(MGL,KBM1));   RTMP21 = 0.0
			DO K=1,KBM1
				READ (KEI,1087) (COLOR(I,K),TURB(I,K),I=1,MGL)
			ENDDO  
	 
			IF(SERIAL)THEN
				COLOR = RTMP2
				TURB  = RTMP21
			ENDIF

			DEALLOCATE(RTMP2,RTMP21)
       ENDIF

	   

	 ! find where our visible light begins for when we want to integrate the PAR curve for the light dependcy of algae growth

	   DO I = 1,NWAVEL
	   
			IF (WAVEL(I) == 400)THEN
			
			PAR_MARK = I
			
			  EXIT
			
	        ENDIF 
	   ENDDO
	   
	!   20 continue
	   

! INTEGRATE AVERAGE QUANTUM SPECTRUM OVER VISIBLE WAVELENGTHS

       SUMI=0
       DO I=2,NWAVEL-1
			SUMI=SUMI+EZERO(I)
       ENDDO
       EZEROINT = 2.5*(EZERO(1)+EZERO(NWAVEL))+5.*SUMI
1032 FORMAT(/)
1036 FORMAT(//8X,2F8.1,I8,3F8.1)

1037 FORMAT(//8X,8F8.1)
10371 FORMAT(//8X,10F8.1)  ! B Clark for new input file

1038 FORMAT(//8X,8F8.1)
1082 FORMAT(//8X,2A8//)
1087 FORMAT(8X,4F8.0)

   END SUBROUTINE OWQ_READ
   
!=======================================================================
!			Allocate vars
!=======================================================================
   
 SUBROUTINE OWQ_ALLOC

      ALLOCATE(COLOR(MTLOC, KBM1)); COLOR=0.0
	  ALLOCATE(TURB(MTLOC,KBM1));   TURB  = 0.0
	  ALLOCATE(IAVG(MTLOC,KBM1));   IAVG   = 0.0	!WLong moved this from mod_wqm.F
	
	  ALLOCATE(KESS(MTLOC,KBM1));   KESS   = 0.0	!WLong moved this from mod_wqm.F
	 
	  ALLOCATE(TOTCHL(MTLOC,KBM1)); TOTCHL = 0.0	! B Clark April 2016, made these allocatble (includine below)
	  ALLOCATE(IATBOT_out(MTLOC));IATBOT_out = 0.0;
	  
      ALLOCATE(ALAMB(NWAVEL)) 			
	  ALLOCATE(PLAMB(NWAVEL))
	  ALLOCATE(PHILAMB(NWAVEL))
	  ALLOCATE(BLAMB(NWAVEL))
      ALLOCATE(WAVEL(NWAVEL))	
      ALLOCATE(EZERO(NWAVEL))
	  ALLOCATE(EZ(NWAVEL))

                               	   

	  
	  ALLOCATE(ABSCDOM(MTLOC,KBM1,NWAVEL,3));ABSCDOM =0.0
	  ALLOCATE(COLORS(MTLOC,KBM1));COLORS = 0.0

	  ALLOCATE(AP440(MTLOC,KBM1)); AP440 = 0.0
	  
	  ALLOCATE(APHI676(MTLOC,KBM1)); APHI676 = 0.0
	  ALLOCATE(B440(MTLOC,KBM1)); B440 = 0.0
	  ALLOCATE(KDLAMB_out(MTLOC,KBM1,NWAVEL)); KDLAMB_out = 0.0
	  ALLOCATE(IAVG_pd(MTLOC,KBM1,NWAVEL)); IAVG_pd = 0.0
	  
	  ALLOCATE(I0_lambda(NWAVEL)); I0_lambda=0.0
	  
	  	  ALLOCATE(GLAMB(NWAVEL,3))
          ALLOCATE(IATBOT(NWAVEL))

   END SUBROUTINE OWQ_ALLOC

!=======================================================================
!		 	Deallocate Vars
!=======================================================================
   
   SUBROUTINE OWQ_DEALLOC
        IF(ALLOCATED(COLOR)) 	DEALLOCATE (COLOR)
        IF(ALLOCATED(TURB)) 	DEALLOCATE (TURB)
		IF(ALLOCATED(IAVG)) 	DEALLOCATE (IAVG)	!WLong moved this to here from wqm_main.F
		
		IF(ALLOCATED(KESS)) DEALLOCATE (KESS)  		!WLong moved this to here from wqm_main.F
		
		IF(ALLOCATED(TOTCHL)) DEALLOCATE (TOTCHL)   ! B Clark see above
        IF(ALLOCATED(IATBOT_out)) DEALLOCATE(IATBOT_out)
        IF(ALLOCATED(ALAMB)) DEALLOCATE(ALAMB)
        IF(ALLOCATED(PLAMB)) DEALLOCATE(PLAMB)		
        IF(ALLOCATED(PHILAMB)) DEALLOCATE(PHILAMB)	
	    IF(ALLOCATED(BLAMB)) DEALLOCATE(BLAMB)
		IF(ALLOCATED(WAVEL)) DEALLOCATE(WAVEL)
		IF(ALLOCATED(EZERO)) DEALLOCATE(EZERO)
		IF(ALLOCATED(EZ)) DEALLOCATE(EZ)		
		
		

		 IF(ALLOCATED(COLORS)) DEALLOCATE(COLORS)
		 IF(ALLOCATED(ABSCDOM)) DEALLOCATE(ABSCDOM)
	!	 IF(ALLOCATED(AG440)) DEALLOCATE(AG440)
		 IF(ALLOCATED(AP440)) DEALLOCATE(AP440)
		 IF(ALLOCATED(APHI676)) DEALLOCATE(APHI676)
		 IF(ALLOCATED(B440)) DEALLOCATE(B440)
		 IF(ALLOCATED(KDLAMB_Out)) DEALLOCATE(KDLAMB_out)
		 IF(ALLOCATED(IAVG_pd)) DEALLOCATE(IAVG_pd)
		 IF(ALLOCATED(I0_lambda)) DEALLOCATE(I0_lambda)
		 IF(ALLOCATED(GLAMB)) DEALLOCATE(GLAMB)
	         IF(ALLOCATED(IATBOT)) DEALLOCATE(IATBOT)	
			 
		!	 
	     !IF(allocated(CDOM1_ABSin)) DEALLOCATE(CDOM1_ABSin)
		 !IF(allocated(CDOM2_ABSin)) DEALLOCATE(CDOM2_ABSin)
		 !IF(allocated(CDOM3_ABSin)) DEALLOCATE(CDOM3_ABSin)
		 
		 

		
   END SUBROUTINE OWQ_DEALLOC

	!FUNCTION   OWQ(COLORS,CHL,TURBS,DEPTH,AVREFCOS)
	!SUBROUTINE LGHT_ATTN()			!calculate light attenuation
    !SUBROUTINE OWQ_ALLOC()
    !SUBROUTINE OWQ_DEALLOC()
	!SUBROUTINE OWQ_READ()
   

!=========================================================================
!
!MAKE OWQ the subroutine for calculating the abs due to all cdom parts and then
!find the irradiaton left over, while still finding the total diffuse attenuation coefficient
!
!
!  When do we integrate our function over wavelenght?
!  in the wc_dom subroutine!!! 
!

!=========================================================================

! Added in to remove the older function that was clunky
   


 SUBROUTINE PHOTOCHEMISTRY  ! This is modified form the OWQ function to have vector operations on the whole matrices and solve for the CDOM1,2,3 absorbance

							! B Clark, April 2016
   IMPLICIT NONE



   B440   = TURB(:,:) * BNSTARNTU + TOTCHL(:,:) * BNSTARCHL 
   
   APHI676 = PHISTAR676 * TOTCHL(:,:)
   
  !    DO M = 1,3
	  
		!AG440(:,:,M)   = GSTAR440(M)*COLORS(:,:,M)
		
		AP440(:,:)   = PSTARINT*(TURB(:,:)**PSTAR440)*(COLORS(:,:)**PSTARCOLOR)  ! still need colors for the ap440 calculation, but why?
		
	!  END DO
   

! COMPUTE ABSORPTION AND SCATTTERING FOR EACH WAVELENGTH
	  
DO I=1,MLOC
 DO K = 1,KBM1
 
 
!!#if defined(SPECTRAL_DISTRIBUTION) 
!   ! generate a random number for indexing our CDOM specific array
!      
!   myRAND = RAND()
!   
!!   write(*,*)' IRAND = ',myRAND 
!   
!   myRAND  = myRAND  * CDOM_counter
!   
!   myCDOM_ID = FLOOR(myRAND ) + 1
!   
! !  write(*,*)'myCDOM_ID = ',myCDOM_ID
!   
!  !GLAMB(:,1) = abs(sum(CDOM1_ABSin,2)/CDOM_counter)
!  GLAMB(:,1) = abs(CDOM1_ABSIN(:,myCDOM_ID))
!!#endif   
  ! write(955,*) GLAMB = GLAMB(:,1)
   
 !  write(*,*)'GLAMB =',GLAMB(:,1)

   DO J=1,NWAVEL
   
   !write(*,*) 'GLAMB 1=',GLAMB(J,1)
   !write(*,*) 'GLAMB 2=',GLAMB(J,2)
   !write(*,*) 'GLAMB 3=',GLAMB(J,3)
   
!	write(*,*)'GLAMB =',GLAMB(J,1)
	 
	 ABSCDOM(I,K,J,1) = GLAMB(J,1)*WC_CDOC1(I,K) !  ! get all 3 cdoms and add em up, want to keep the array for calculating the irradiance lost due to each CDOM
  
	 ABSCDOM(I,K,J,2) = GLAMB(J,2)*WC_CDOC2(I,K)
	 
 !	 write(*,*)'WCDOC3 in mod OWQ = ',WC_CDOC3(I,K)
!	 write(*,*)'WCDOC2 in mod OWQ = ',WC_CDOC2(I,K)
!	 write(*,*)'WCDOC1 in mod OWQ = ',WC_CDOC1(I,K)
	 
	 ABSCDOM(I,K,J,3) = GLAMB(J,3)*WC_CDOC3(I,K)
	 
	! Write(*,*)'ABs ffrom WCDOC3 =',ABSCDOM(I,K,51,3)
	 !do m = 1,3   ! can not have 0 absorbance, it messes up the optdepth calculation 
	 !  if (ABSCDOM(I,K,J,m) .EQ. 0.0) THEN
	 !      ABSCDOM(I,K,J,m) = 1E-20
	 !  endif
     ! enddo   
     ABCHL  = PHILAMB(J)*APHI676(I,K)       ! the total CHL
	 
     ABNAPM = PLAMB(J) * AP440(I,K)  ! need to figure out the AP440 part, what is going on here?
	 
     ABTOT  = ALAMB(J) + ABCHL + ABNAPM +SUM(ABSCDOM(I,K,J,:))  ! get the total absorbance with all 3 cdoms
	 !write(*,*)'ALAMB =',ALAMB(J)
	 !write(*,*) ' ABCHL = ',ABCHL
	 !write(*,*)'ABNAPM = ',ABNAPM
	! write(*,*)'ABSCDOM = ',SUM(ABSCDOM(I,K,J,:))
     SCAT  = BLAMB(J)*B440(I,K)
	 
	 ! B Clark put all this in th eloop so now the subrotuine is onl y called once and it calculates everything
 ! FIRST ESTIMATE ATTENUATION BASED ON DEPTH TO 1% LIGHT PENETRATION
      KDLAMB = SQRT (ABTOT * ABTOT + GMUZERO(2) * ABTOT * SCAT )   &
                 /AVREFCOS

! PERCENT LIGHT REMAINING
      PLR = EXP ( -KDLAMB * DEPTH )

! USE LOOKUP TABLE TO REFINE ESTIMATE OF SCATTERING FUNCTION BASED ON 
! PERCENT LIGHT REMAINING

     REFSCAT = GMUZERO(9)
     DO Iscat=1,8
       IF (PLR >= OPZ(Iscat) .AND. PLR < OPZ(Iscat+1)) THEN
         REFSCAT = GMUZERO(Iscat)
         GO TO 1
       ENDIF
     ENDDO
       
1    CONTINUE
        
! RE-EVALUATE ATTENUATION BASED ON REFINED SCATERING FUNCTION
     KDLAMB_out(I,K,J) = SQRT (ABTOT * ABTOT + REFSCAT * ABTOT * SCAT )/AVREFCOS 

! COMPUTE REMAINING SPECTRAL IRRADIANCE

     EZ(J) = I0*EZERO(J) * EXP( -KDLAMB_out(I,K,J) * DEPTH )
	 
!	 write(*,*)'EZERO = ',EZERO(J)
!	 WRITE(*,*)'EZ = ',EZ(J)

  ENDDO  ! NWAVEL loop

! INTEGRATE PAR REMAINING AT THE REFERENCE DEPTH  
! B Clark here do we need to stick to the visible wavelengths or should we keep it integrating over the entier Lambdas including UV?
   
  
  ! B Clark commented, now integrating a differnt way , after calculating total spectra underwater
!   SUMI=0   
!   DO J=2,NWAVEL-1
!     SUMI=SUMI+EZ(J)
!   ENDDO
!   
!   EZINT=2.5*(EZ(1)+EZ(NWAVEL))+5.*SUMI

!! COMPUTE DIFFUSE ATTENUATION COEFFICIENT for all wavelengths
!   KESS(I,K) = -ALOG(EZINT/EZEROINT)/DEPTH
!   
  ENDDO
ENDDO
!write(*,*)' now onto the new CDOM absorption parts'
! Still need to get a formula to convert SWRAD to UV-VIS energy
!write(*,*)'abscdom  = ' , ABSCDOM

I0_lambda = I0 * EZERO   ! This gives the spectral distribution of our shortwave radiation, before the model took par,
                         !we are using shortwave radiation now

 DO I=1,MLOC
 
   DO J = 1,NWAVEL    ! This will need to be spectrally dependent to get the photobleaching correct

     IATTOP = I0_lambda(J)*(1.-SREFLECT/100.)  ! get the surface irradiance
	  
	! IATTOP = 100
!	 write(*,*)'I0_lambda = ',I0_lambda(j)
	! write(*,*)'KDLAMB = ',KDLAMB_out(I,1,J)
!RGL added print below for irradiance at top of wc       
!WL     IF(MSR.and.I.eq.MLOC) WRITE(*,*)'IATTOP', IATTOP  
	 
     OPTDEPTH = KDLAMB_out(I,1,J)*D(I)*DZ(1) !  optical depth for the surface cell from photochem subroutine

     IATBOT(J) = IATTOP*EXP(-OPTDEPTH)    ! ! irradiance left at the bottom of the cell, this gets carried to the next cell

     IAVG_pd(I,1,J) = (IATTOP-IATBOT(J))/OPTDEPTH ! average light level in the cell, assumption is the cell is vertically well mixed
	 
  ! write(*,*)'IAVG_pd = ',IAVG_pd(I,1,J)

       DO Idom= 1,3
		
      !    write(*,*)'ABSCDOM = ' , ABSCDOM(I,1,J,M)

          CDOM_optdepth = ABSCDOM(I,1,J,Idom)*D(I)*DZ(1)  ! Simple photodegradation update B Clark April 2016, we want to use each wavelenght 
	 	    
		!  write(*,*)'CDOM_optdepth=',CDOM_optdepth							   ! so that we get a spectral quantum yield before integrating the total DOC that gets photodegraded
          ICDOM_bot  = IATTOP*EXP(-CDOM_optdepth)    ! This calculates the light remaining after absorption by CDOM1
	!	write(*,*)'ICDOM_bot = ',ICDOM_bot
		
          ICDOM_abs(I,1,J,Idom) = IATTOP-ICDOM_bot
		  
	!	write(*,*)'ICDOM_abs = ',ICDOM_abs(I,1,J,Idom) 
        END DO

		
	 END DO 
	 
	 		!	write(*,*)'ICDOM_abs = ',ICDOM_ABS(:,:,51,:)
	 
! get our PAR light remaining from 400:700 nm
  ! still need to convert the energy flux to number of photons so we can use in P vs I curve (Einstens/m^2)
	!https://www.berthold.com/en/bio/how-do-i-convert-irradiance-photon-flux
!	   IAVG(I,K) = 0.0	 
	 
	!watts/m^2 * lambda(nm) * [Energy / photon / lambda conversion * 86400 s / day[] / avogradros # = moles Photons/m^2/day
	 
	 !  The number of photons Np can be calculated by (Note: the nm value for λ is used):
	   ! h = Planck's constant, c = speed of light in vacuum
	 
     !  Np = E/Ep= E•((λ•10-9)/h•c) = E [W/m2]•λ•10-9[m]•/ (1.988•10-25) [J/s•m/s]
  !       =  E•λ•5.03•1015 [1/(m2•s)] 
		
	 
	IAVG(I,1) = 0.0	 ! set to zero
	
	   DO J = PAR_MARK,NWAVEL
	     
	     IAVG(I,1) = IAVG(I,1) + IAVG_pd(I,1,J) * (WAVEL(J)  &
						*4.3459E20) / 6.0221409E+23      

	   END DO  ! K
	   
!	   write(*,*) 'IAVG in OWQ = ',IAVG(I,1)
!          write(*,*)'IATBOTOM = ',IATBOT

!      if (cdom_optdepth .NE. 0) THEN
!
!          write(*,*)'NGID = ',NGID(I)
!	  write(*,*)'CDOM optical depth  = ',CDOM_optdepth
!          write(*,*)'TOTAL CDOM ABS = ',TOTALCDOM_ABS(I,1) 
!        ENDIF


    DO K=2,KBM1 ! loop through the rest of the grid vertically
		
	       DO J = 1,NWAVEL
            IATTOP = IATBOT(J)
            OPTDEPTH = KDLAMB_out(I,K,J)*D(I)*DZ(K)   ! now need to make this spectrally depenedent so that we get the spectral irradiance left over 
			
            IATBOT(J) = IATTOP*EXP(-OPTDEPTH)
			
            IAVG_pd(I,K,J) = (IATTOP-IATBOT(J))/OPTDEPTH 
         !      write(*,*)'IAt top = ',IATTOP
        !       write(*,*) 'I at bottom =',IATBOT(J)

         !      write(*,*)'OPTDEPTH = ',OPTDEPTH
		
              DO Idom = 1,3
		   
               CDOM_optdepth = ABSCDOM(I,K,J,Idom)*D(I)*DZ(1)  ! Simple photodegradation update B Clark April 2016, we want to use each wavelenght 
												   ! so that we get a spectral quantum yield before integrating the total DOC that gets photodegraded
               ICDOM_bot     = IATTOP*EXP(-CDOM_optdepth)    ! This calculates the light remaining after absorption by CDOM1
			   
               ICDOM_abs(I,K,J,Idom) = IATTOP-ICDOM_bot ! W / m^2 / day
               
   	          END DO !J
			  
            END DO  ! K
			

	!  write(*,*)'got bottom'
		
			IAVG(I,K) = 0.0	 ! set to zero
! same as above but for rest of the layers
	    DO J = PAR_MARK,NWAVEL
	  
	       ! convert now, wavelength dependent
	       IAVG(I,K) = IAVG(I,K) + IAVG_pd(I,K,J) * (WAVEL(J)  &   ! Now is in moles Photons/m^2/day (E/m^2/day) for use in the PAR algae calculation
						*4.3459E20)/6.0221409E+23             	
       END DO
	  

	!  write(*,*)'integrated'
!	  if (ICDOM_abs(I,K,1,3) .NE. 0.) THEN
	  
!	  	write(*,*)'ICDOM_abs = ',ICDOM_abs(I,K,1,3)  ! B Clark debug
!	  endif	
	  
     END DO  ! K
	   
	!write(*,*)'IAVG in OWQ = ',IAVG(I,3)  
	 
	END DO  ! I

  ! still need to convert the energy flux to number of photons so we can use directly with our quantum yield calculation (mols C / mols Photon)
	!https://www.berthold.com/en/bio/how-do-i-convert-irradiance-photon-flux
	!write(*,*)'Wave j = ',WAVEL
	

   DO J = 1,NWAVEL	
   
	   N_photons(:,:,j,:)  = (ICDOM_abs(:,:,j,:)*WAVEL(J)*5.03E15 ) / 6.0221409E+23    ! mol Photons/m^2/s, still need to convert to mols Photons for use with our AQY in mod_wc_dom
	
   END	DO
 ! write(*,*)'N_photons = ',n_photons(1,1,51,:)
!  write(*,*)
  ! write(*,*)'made it to end of photochem subroutine'
	
END SUBROUTINE PHOTOCHEMISTRY
   

   
SUBROUTINE LGHT_ATTN
   

!vjp  10/11/04 initialize arrays OPZ, G1, and G2

   OPZ(1:9) = (/ 0.0, 0.01, 0.025, 0.033, 0.05, 0.1, 0.2, 0.286, 0.4 /)
   G1(1:9) =  (/ 0.425, 0.425, 0.364, 0.3383, 0.2996, 0.2175,           &
                 0.1232, 0.073167, 0.01546 /)
   G2(1:9) =  (/ 0.19, 0.19, 0.1396, 0.1187, 0.0875, 0.02288,           &
                -0.048, -0.08367, -0.12386 /)
                
! CONVERT MODEL JULIAN DAY TO DAY AND HOUR

   DOY = AMOD(JDAY,365.)
   HOUR = 24.*(DOY-AINT(DOY))
   DOY = AINT(DOY)+DOFFSET

! COMPUTE COSIN OF IN-WATER SOLAR ANGLE

! SOLAR DATE
   SOLDATE =  TWOPI*DOY/365.
  ! SOLDATE = 5
 !  write(*,*)'SOLDATE = ',SOLDATE

! DECLINATION
   DECL = TWOPI*(0.39637-(22.9133*COS(SOLDATE))+(4.02543*SIN(SOLDATE))-  &
         (0.3872*COS(2.*SOLDATE))+(0.052*SIN(2*SOLDATE)))/360.

! TIME OF DAY
   TAU = TWOPI*HOUR/24.
   
   
   ! B Clark fix the tau to be at Noon
!   TAU = TWOPI/2.
 !  write(*,*)'ALERT!!!!! TAU is FIXED TO HAVE THE SUN AT NOON!!!!!!'
   
   
   
   
   
   
!write(*,*)'Tau =',TAU
! COSIN OF THETA IN AIR
   CTHTAA=((SIN(LAT))*(SIN(DECL)))-((COS(LAT))*(COS(DECL))*(COS(TAU)))

! INCIDENCE OF THETA IN AIR
   ITHTAA = ACOS(CTHTAA)

! THETA IN WATER
! Wen Long, here 1.33 is the water refraction index of light
   THTAW=ASIN(SIN(ITHTAA)/1.33)

! REFLECTANCE
   IF (ITHTAA > TWOPI/4.) THEN
     SREFLECT = 100.
   ELSE
     SREFLECT = EXP(0.6148+ITHTAA**3)
   ENDIF

! SCATTERING FUNCTION
   AVREFCOS = COS(THTAW)
   GMUZERO(1) = 0.198
   DO I=2,9
     GMUZERO(I) = AVREFCOS*G1(I)-G2(I)
   ENDDO

! EVALUATE ATTENUATION FOR EACH SURFACE CELL

  DO K = 1, KBM1 ! B Clark added in vertical distribution of differential KD values Sep 2015
   DO I=1,MLOC
   
     TOTCHL(I,K) = (B1(I,k)*CHLCMN1 + B2(I,k)*CHLCMN2 + B3(I,k)*CHLCMN3)*TCHL2CHL

! EMPIRICAL FUNCTIONS FOR COLOR AND TURBIDITY

     TOTPOC   = B1(I,k)+B2(I,k)+B3(I,k)+SZ(I,k)+LZ(I,k)+LPOC(I,k)+RPOC(I,k)
     TSS    = MAX(SSI(I,k) + 2.5*TOTPOC, 1.0) 
     TURB(I,k)  = MIN(0.4*TSS,12.)
	 
	END DO 
  END DO
	

	 
	 ! Add in all the code here for each different DOC
  ! still need these "colors" formuilations for the TSS scaling. dont quite understand this
   DO K = 1, KBM1 ! B Clark added in vertical distribution of differential KD values Sep 2015
    DO I=1,MLOC	
!	
	   COLORS(I,K)= MAX(12.2*EXP(0.14*min(35.,WC_CDOC1(I,K)+WC_CDOC2(I,K)+WC_CDOC3(I,K))), 10.)  ! Calciualte our three colors the same as before, empirical function, might want to tweak a little bit
  !  
 	 END DO
   END DO
  !! 
   
 ! write(*,*)'Called lit ATN, no onto PHOTOCHEMISTRY' 
CALL PHOTOCHEMISTRY 



     
   
   RETURN
   END SUBROUTINE LGHT_ATTN

   
END MODULE MOD_OWQ

