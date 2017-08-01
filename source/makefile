# Makefile for testing the water column DOM module
# B Clark Oct 2, 2015
#
#
#-----------BEGIN MAKEFILE-------------------------------------------------
            SHELL         = /bin/bash
            DEF_FLAGS     = -P -C -traditional 
            EXEC          =./test_sed_dom
#==========================================================================
#  BEGIN USER DEFINITION SECTION
#==========================================================================
#        SELECT MODEL OPTIONS
#          SELECT FROM THE FOLLOWING OPTIONS BEFORE COMPILING CODE
#          SELECT/UNSELECT BY COMMENTING/UNCOMMENTING LINE (#)
#          CODE MUST BE CLEANED (with "make clean") AND
#          RECOMPILED IF NEW SET OF OPTIONS IS DESIRED
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
#        MULTI_PROCESSOR    INCLUDES PARALLELIZATION WITH MPI
#                           REQUIRES LINKING MPI LIBRARIES OR COMPILING
#                           WITH A PRELINKED SCRIPT (mpif90/mpf90/etc)
#                           DEFAULT: NO PARALLEL CAPABILITY
#                           UNCOMMENT TO INCLUDE MPI PARALLEL CAPABILITY
#--------------------------------------------------------------------------

#               FLAG_1 = -DMULTIPROCESSOR
#              PARLIB = -L/hosts/salmon01/data00/medm/lib -lmetis 
#	       PARLIB = -L/Users/bclark/Desktop/metis -lmetis

#--------------------------------------------------------------------------
#
#     TEST FLAG, change to run different test, see code for details
#              TEST1 = all terms set to 0
#              TEST2 = Algae
#
#
#--------------------------------------------------------------------------

#               FLAG_1 = -DTEST1
#              PARLIB = -L/hosts/salmon01/data00/medm/lib -lmetis 
#	       PARLIB = -L/Users/bclark/Desktop/metis -lmetis

               

		FLAG_3 = -DSEDIMENT_DOM

#--------------------------------------------------------------------------
#        NETCDF OUTPUT      DUMP OUTPUT INTO NETCDF FILES (yes/no)
#                           REQUIRES SYSTEM DEPENDENT NETCDF LIBRARIES
#                           COMPILED WITH SAME F90 COMPILER
#                           SET PATH TO LIBRARIES WITH IOLIBS      
#                           SET PATH TO INCLUDE FILES (netcdf.mod) WITH IOINCS
#                           DEFAULT: NO NETCDF OUTPUT
#                           UNCOMMENT TO INCLUDE NETCDF OUTPUT CAPABILITY
#--------------------------------------------------------------------------
           FLAG_2       =  -DNETCDF_IO
           IOLIBS       =  -L/usr/local/bin/netcdf-install/lib/  -lnetcdf
           IOINCS       =  -I/usr/local/bin/netcdf-install/include/
#--------------------------------------------------------------------------
#        SELECT COMPILER/PLATFORM SPECIFIC DEFINITIONS
#          SELECT FROM THE FOLLOWING PLATFORMS OR USE "OTHER" TO DEFINE
#          THE FOLLOWING VARIABLES:
#          CPP:  PATH TO C PREPROCESSOR 
#           FC:  PATH TO FORTRAN COMPILER (OR MPI COMPILE SCRIPT)
#          OPT:  COMPILER OPTIONS
#       MPILIB:  PATH TO MPI LIBRARIES (IF NOT LINKED THROUGH COMPILE SCRIPT)
#--------------------------------------------------------------------------

#--------------------------------------------------------------------------
#  Intel Compiler Definitions
#--------------------------------------------------------------------------
#        CPP      = /usr/bin/cpp
#        CPPFLAGS = $(DEF_FLAGS) -DINTEL 
#        FC       = /share/apps/intel/Compiler/11.0/069/bin/intel64/ifort      
#        DEBFLGS  = #-check all 
#        OPT      = #-O3 -xN -axN -tpp7
#        CLIB     = #-static-libcxa 
#--------------------------------------------------------------------------
#  Intel Compiler Definitions
#--------------------------------------------------------------------------
          CPP      = /usr/bin/cpp
          CPPFLAGS = $(DEF_FLAGS) -DINTEL -DDOUBLE_PRECISION
 #         FC       = /usr/local/mpich2/bin/mpif90
	   FC     = ifort  
         DEBFLGS  = -r8 -warn all -i4 -g -Wl -no_pie -nozero -O0  -check all -g -debug -debug-parameters -fp-stack-check -fpe0 -traceback -automatic  #-ftrapuv 
          OPT      = -heap-arrays  -i4  -Wl -no_pie -vec-report0 -r8 -xHost -no-prec-div  -no-prec-sqrt -assume nominus0 -assume noprotect_parens -assume norealloc_lhs  -shared-intel -warn unused -check uninit #  -p -g  -fno-inline-functions
          CLIB     = #-static-libcxa 
#--------------------------------------------------------------------------
#   Linux/Portland Group Definitions 
#--------------------------------------------------------------------------
#         CPP      = /usr/bin/cpp
#         CPPFLAGS = $(DEF_FLAGS) 
#         FC       = pgf90
#         DEBFLGS  = -Mbounds -g -Mprof=func
#         OPT      = #-fast  -Mvect=assoc,cachesize:512000,sse  
#--------------------------------------------------------------------------
#  gfortran defs 
#--------------------------------------------------------------------------
#         CPP      = /usr/bin/cpp 
#         CPPFLAGS = $(DEF_FLAGS)  -DGFORTRAN
#         FC       = gfortran  -O3 
#         DEBFLGS  = 
#         OPT      = 
#         CLIB     = 
#==========================================================================
#  END USER DEFINITION SECTION
#==========================================================================

         FFLAGS = $(DEBFLGS) $(OPT) 
         MDEPFLAGS = --cpp --fext=f90 --file=-
         RANLIB = ranlib

#--------------------------------------------------------------------------
#  CAT Preprocessing Flags
#--------------------------------------------------------------------------
           CPPARGS = $(CPPFLAGS) $(DEF_FLAGS) $(FLAG_2) $(FLAG_3) 
#--------------------------------------------------------------------------
#  Libraries           
#--------------------------------------------------------------------------

            LIBS  = $(CLIB)  $(PARLIB) $(IOLIBS) $(MPILIB)
#            LIBS  = -L../SPARSKIT2 -lskit $(PARLIB)
#             LIBS  = $(PARLIB) $(PETSC_LIB)
#           INCS  = $(IOINCS)$(PETSC_FC_INCLUDES)$(IOINCS)
           INCS  = $(IOINCS)

#--------------------------------------------------------------------------
#  Preprocessing and Compilation Directives
#--------------------------------------------------------------------------
.SUFFIXES:  .f90   

#.F.o:
#	$(CPP) $(CPPARGS) $(INCS) $*.F > $*.f90
	$(FC)  -c $(FFLAGS) $(INCS) $*.f90
#	\rm $*.f90

#--------------------------------------------------------------------------
#  FVCOM Source Code.
#--------------------------------------------------------------------------

MAIN  = mod_prec.f90	mod_types.f90	mod_utils.f90	mod_control.f90	\
	mod_lims.f90	mod_bcs.f90	mod_sizes.f90	mod_hydrovars.f90\
	mod_buffers.f90 	mod_fileinfo.f90	       \
	mod_wqm.f90	mod_zoop.f90	mod_wqminit.f90    \
	utilities.f90	mod_par.f90	mod_sed_sav_exchange_vars.f90	\
	bracket.f90	mod_sed_sf_exchange_vars.f90	mod_sf.f90	\
	mod_sed_df_exchange_vars.f90	mod_df.f90	    \
	mod_sed_dom_exchange.f90          mod_sed_dom.f90   mod_owq.f90	 \
	mod_sav.f90	mod_ba.f90	mod_sed.f90	mod_algal.f90	\
	mod_bcmap.f90	mod_obcs.f90	mod_ncd.f90	ncdio.f90	\
	mod_tge.f90	cell_area.f90	pdomdec.f90      mod_wc_dom.f90			\
	domdec.f90	genmap.f90	bcs_force.f90	wqm_inputs.f90	\
	mod_kin.f90	mod_hydro.f90		tvds.f90		\
		vdif_wqm.f90	vertvl.f90	viscofh.f90	\
	bcond_wqm.f90	fct_nut.f90	mod_filenames.f90			\
        test_sed_withDOM.f90  							\



 SRCS = $(MAIN)  

 OBJS = $(SRCS:.F=.o)

#--------------------------------------------------------------------------
#  Linking Directives               
#--------------------------------------------------------------------------

$(EXEC):	$(OBJS)
		$(FC) $(FFLAGS) $(LDFLAGS) -o $(EXEC) $(OBJS) $(LIBS)

#--------------------------------------------------------------------------
#  Target to create dependecies.
#--------------------------------------------------------------------------

depend:
		makedepf90  $(SRCS) > makedepends


#--------------------------------------------------------------------------
#  Tar Up Code                           
#--------------------------------------------------------------------------

tarfile:
	tar cvf fvcom.tar *.F  makefile exa_run.dat makedepends RELEASE_NOTES 

#--------------------------------------------------------------------------
#  Cleaning targets.
#--------------------------------------------------------------------------

clean:
		/bin/rm -f *.o *.mod  # *.f90

clobber:	clean
		/bin/rm -f .o #*.f90  wqm

#--------------------------------------------------------------------------
#  Common rules for all Makefiles - do not edit.
#--------------------------------------------------------------------------

emptyrule::

#--------------------------------------------------------------------------
#  Empty rules for directories that do not have SUBDIRS - do not edit.
#--------------------------------------------------------------------------

install::
	@echo "install in $(CURRENT_DIR) done"

install.man::
	@echo "install.man in $(CURRENT_DIR) done"

Makefiles::

includes::
include ./makedepends
# DO NOT DELETE
