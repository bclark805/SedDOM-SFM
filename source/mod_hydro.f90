MODULE MOD_HYDRO

        CONTAINS
        !subroutine HYDRO ()

        !WLong: This should be combined with mod_hydrovars.F

        !************************************************************************
        !**                  S U B R O U T I N E   H Y D R O                   **
        !************************************************************************

            SUBROUTINE HYDRO ()
            USE MOD_PREC, ONLY: SP
            USE MOD_HYDROVARS, ONLY:  &
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
                        !,DZ	&		!DELTA-SIGMA VALUE
                        !,DZZ	&		!DELTA OF INTRA LEVEL SIGMA 
                        !,H1	&		!BATHYMETRIC DEPTH 
                        !,H	&			!BATHYMETRIC DEPTH 
                        !,D	&			!CURRENT DEPTH 
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
                          UNC2	&		!
                         ,VNC2	&		!
                        !,WNC2	&		!
                         ,WTSNC2	&	!
                         ,UARD_OBCNNC2	&!
                         ,XFLUX_OBCNC2	&!
                         ,DTFANC2	&	!
                         ,KHNC2	&		!
                         ,TNC2	&		!
                         ,SNC2	&		!
                         ,ELNC2	&		!
                         ,num_hyd_ints	&!number of records in each hydrodynamics netcdf file
                        !,TIME_MAP	&	!
                        !,THOUR1	&	!SIMULATION TIME AT END OF CURRENT EXTERNAL STEP (IEXT) IN HOURS
                        !,THOUR	&		!
                        !,NCFILE_DIR	&!
                        ,NCFILE_PREFIX	&!
                        ,NCFILE_SUFFIX	&!
                        ,NCFILE_NUMBER	&!
                        ,FORMAT_STR	&!
                        ,hydro_dir 	&	! directory name where hydrodynamics results (netcdf) files are stored
                        !,hydro_prefix  &	! prefix of file name, e.g. 'psm_'
                        !,hydro_suffix	&	! suffix of filename, e.g. '.nc'
                        ,hydro_filenumwidth &	! number of digits in filename following hydro_prefix, e.g. 4 for psm_0002.nc
                        ,hydro_filenumstart &	! starting number of the file name in the digital part of the file name, e.g. 185 for psm_0185.nc
                        ,hydro_Nrec	&		! number of records in each of hydrodynamics file
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
                        ,IFNC	&				!file number index for hydrodynamics netcdf files, set to hydro_filenumstart initially for cold start, set otherwise 
                        ,NTRECNC	&			!time record index for a particular hydrodynamics netcdf file, reset to 1 upon opening new file. 
                        ,NTHYDRO				!overall time record index for all netcdf files, increment by 1 each time a hydrodynamics record is read

                !Wen Long took MOD_CONTROL out of MOD_HYDROVARS and put the used variables here
            USE MOD_CONTROL, ONLY : 		&
                                !SERIAL  		&           !!TRUE IF SINGLE PROCESSOR
                                MSR        	&           !!TRUE IF MASTER PROCESSOR (MYID==1)
                                ,PAR        !	&           !!TRUE IF MULTIPROCESSOR RUN
                                !,CASENAME  	&   		!!LETTER ACRONYM SPECIFYING CASE IDENTITY (MAX 80 CHARS)
                                !,CASETITLE  	&  			!!CASE TITLE                                 
                                !,HMAX       	&  			!!GLOBAL MAXIMUM DEPTH
                                !,HMIN       	&  			!!GLOBAL MINIMUM DEPTH
                                !,UMOL       	&  			!!VERTICAL DIFFUSION COEFFICIENT
                                !,HORCON     	&  			!!HORIZONTAL DIFFUSION COEFFICIENT
                                !,DTI        	&  			!!internal time step
                                !,HORZMIX    	&   		!!CONTROLS HORIZONTAL DIFFUSION COEF CALC (constant/closure)
                                !,FILENUMBER	&			!!
                                !,PREFF			&			!!
                                !,INPDIR		&			!!
                                !,GEOAREA		&			!!
                                !,RIV_FILENUMBER	&			!!
                    !,INFLOW_TYPE   	&			!!SPECIFIED RIVER INFLOW TYPE (edge/node) 
                    !,POINT_ST_TYPE 	&			!!(calculated/specified)
                    !,PNT_SOURCE    	&			!!point_source
                    !,DAY				&
                                !,in_jday			

                USE MOD_LIMS, ONLY: MTLOC, KBM1
            USE MOD_SIZES, ONLY :        &	!
                                NCP!,            &  !
                                !NQFP,           &  !
                    !NHQP,           &  !
                                !NS1P,           &  !
                    !NS2P,           &  !
                    !NS3P,           &  !
                    !NBCP,           &  !
                    !NDP,            &  !
                    !NFLP,           &  !
                    !NOIP,           &  !
                    !NSSFP,			 &  !
                    !MGL,            &  !
                                !NGL!,            &  !
                                !OBCGL,          &  !
                                !NOBTY  
          
                
            IMPLICIT NONE



            SAVE
            INTEGER   F, SB, L, JCON, I, J, NC_ID2
            LOGICAL   END_OF_FILE
            REAL(SP)   NXDAY, TDUM
            REAL(SP)   MASS(0:MTLOC,KBM1,NCP)  !WLong: this is unncessary variable
            CHARACTER(LEN=1024) :: NCFILE

        !************************************************************************
        !**                              Inputs                                **
        !************************************************************************

        ! Wen Long moved these to the main program wam_main.F 
        !    UNC1  = UNC2
        !    VNC1  = VNC2
        !    WNC1  = WNC2
        !    WTSNC1 = WTSNC2
        !    UARD_OBCNNC1=UARD_OBCNNC2
        !    XFLUX_OBCNC1=XFLUX_OBCNC2
        !    DTFANC1=DTFANC2
        !    KHNC1 = KHNC2
        !    ELNC1 = ELNC2
        !    TNC1  = TNC2
        !    SNC1  = SNC2

        !**************RGL section begins below**********************************
        ! RGl need to change below harddwire if change number of tsteps/file netcdf
          num_hyd_ints = hydro_Nrec

        ! find indices for the next record to read 
        !         index of file:                        IFNC 
        !         time record index within the file:    NTRECNC
        !         overwall hydrodynamics record index:  NTHYDRO

          IF(NTRECNC.eq.num_hyd_ints) THEN
             NTRECNC = 0
             IFNC = IFNC +1       
          ENDIF
          NTRECNC=NTRECNC+1     !NTRECNC is time record # within one hydro file
          NTHYDRO=NTHYDRO+1     !NTHYDRO is overall record # of all hydro files that have been used
         
              IF(NTRECNC.eq.1)THEN  !open new file (IFNC) when trying to read the first record
                                    
                 NCFILE_NUMBER=''
                 WRITE(NCFILE_NUMBER(1:hydro_filenumwidth),TRIM(FORMAT_STR))(IFNC+hydro_filenumstart-1)
                 NCFILE=TRIM(hydro_dir)//TRIM(NCFILE_PREFIX)//TRIM(NCFILE_NUMBER)//TRIM(NCFILE_SUFFIX)
                 IF(MSR) WRITE(*,*)'NCFILE=',TRIM(NCFILE)

                                                           !        variable             varilable                         unit       dimensions (e.g. :
                                                           !        name in              meaning                                      siglay=10, siglev=11
                                                           !        NCFILE                                                            obc=11,obc2=11,node=9013
                                                           !                                                                          nele=13941 )
            !                                    ----------------------------------------------------------------------------------------------------------
                 CALL NCD_READ_OPEN(NCFILE,             &  !        --NetCDF file name 
                                      UNC2,             &  !        --'u'                 "Eastward Water Velocity"        m/s        (time,siglay,nele)
                                      VNC2,             &  !        --'v'                 "Northward Water Velocity"       m/s        (time,siglay,nele)
                                    WTSNC2,             &  !        --'wts'               "Upward Water Velocity at node"  m/s        (time,siglev,node)
                              UARD_OBCNNC2,             &  !        --'uard_obcn'         "UARD at OBC"                    m/s        (time,obc)
                              XFLUX_OBCNC2,             &  !        --'xflux_obc'         "Xflux at OBC"                   m/s***     (time,siglay,obc2)
                                   DTFANC2,             &  !        --'dtfa'              "FSH + Water Height"              m         (time,node)
                                     KHNC2,             &  !        --'kh'                "Turbulent Eddy Diffusivity"     m^2/s      (time,siglev,node)
                                     ELNC2,             &  !        --'zeta'              "Water Surface Elevation"         m         (time,node)
                                      TNC2,             &  !        --'temp'              "temperature"                    deg-C      (time,siglay,node)
                                      SNC2,             &  !        --'salinity'          "salinity"                        psu       (time,siglay,node)
                                   NTRECNC)                !        -- time record number corresponding to index of 'time' variable in NCFILE
            !                                    -----------------------------------------------------------------------------------------------------------
            !Wen Long                           !*** the unit of xflux_obc should be [m][m/s][concentration]  i.e. [m^2/s][concentration] rather than [m/s]!!
            !                                   !    where concentration could be for dye, deg-C for temperature, psu for salinity
            !                                   !    FVCOM is not clear on what unit xflux_obc has, but the calculation gives (velocity * depth * concentration)

              ELSE

                 CALL NCD_READ(NCFILE,UNC2,VNC2,WTSNC2,UARD_OBCNNC2,XFLUX_OBCNC2,DTFANC2,&
                      KHNC2,ELNC2,TNC2,SNC2,NTRECNC)

              ENDIF

            RETURN
            END SUBROUTINE HYDRO





                


END MODULE MOD_HYDRO

