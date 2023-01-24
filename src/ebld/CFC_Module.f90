! CFC_Module
! 
! This module contains the data for the CFC model. This replaces the 
! CFC_common.h file (TODO).
!
! Created by: Adam Wills
! Initial Creation Date: Jul 2018
! ********************************************************************
      Module CFC_Module
      IMPLICIT NONE
      SAVE !ensures values do not change between sucessive invocations
      ! Declare parameters
        Integer MGCL_CFC ! Number of categories in a CFClayers database
        Integer MGIT_CFC ! Number of items in a CFClayers database
        
      ! CFC layers header common blocks:
        real cfcver         ! version of the database (current version is 1.1)
        integer cfccats     ! number of categories
        integer cfcdbitems  ! number of items (overall) in database
        integer, ALLOCATABLE :: cfccatitems(:) ! number of items in each category(MGCL_CFC)

      ! CFC layers category commons
        character cfcdbdate*24 ! date stamp for the database
        character cfcdbdoc*248 ! documentation for the CFC layers database
        character(len=124),allocatable :: cfccatname(:) ! identity/name of category (MGCL_CFC)
        character(len=248),allocatable :: cfccatdoc(:) ! documentation for the category (MGCL_CFC)
        
      ! Strings associated with a CFC layer
        character(len=32),allocatable :: cfcname(:) ! name for CFC layer
        character(len=248),allocatable :: cfcdoc(:) ! documentation

      ! Data for each CFC layer:
        integer(KIND=2), ALLOCATABLE :: cfccatindex(:)  ! is pointer to index of the material category (MGIT_CFC)
        integer(KIND=2), ALLOCATABLE :: cfcitmindex(:)  ! is pointer to item index (MGIT_CFC)
        real, ALLOCATABLE :: cfcdbcon(:)        ! is the conductivity () (MGIT_CFC)
        real, ALLOCATABLE :: cfcdbden(:)        ! is the density () (MGIT_CFC)
        real, ALLOCATABLE :: cfcdbsht(:)        ! is the specific heat () (MGIT_CFC)
        real, ALLOCATABLE :: cfcdbthick (:)     ! default thickness (mm) for item (MGIT_CFC)

      ! Stores the CFC layers database item index for CFCs in the MLC
      ! database. Dimension statement should match MMLC in building.h
      ! If -1 then not a CFC
      ! If 0 then confused or missing layer
      ! If gt 0 then legitimate db index 
        integer, DIMENSION(300, 16) :: ITMCFCDB
        logical, DIMENSION(300, 16) :: CFC_layer_flipped
     
      ! Data structures associated with CFC layers (all will be allocated to MGIT_CFC)
        integer(KIND=1), ALLOCATABLE :: CFCshdtp(:)        ! CFC shade type (defined in CFC_common.h)
        real, ALLOCATABLE :: CFCsolreflout(:)      ! CFC fabric/material solar reflectance, outside
        real, ALLOCATABLE :: CFCsolreflin(:)       ! CFC fabric/material solar reflectance, inside
        real, ALLOCATABLE :: CFCsoltrandir(:)      ! CFC fabric/material solar direct transmittance
        real, ALLOCATABLE :: CFCsoltrantotout(:)   ! CFC fabric/material solar total transmittance, outside
        real, ALLOCATABLE :: CFCsoltrantotin(:)    ! CFC fabric/material solar total transmittance, inside
        real, ALLOCATABLE :: CFCemissout(:)        ! CFC fabric/material emissivity, outside
        real, ALLOCATABLE :: CFCemissin(:)         ! CFC fabric/material emissivity, inside
        real, ALLOCATABLE :: CFClwtran(:)          ! CFC fabric/material longwave transmittance      
        real, ALLOCATABLE :: CFCvisreflout(:)      ! CFC fabric/material visual reflectance, outside
        real, ALLOCATABLE :: CFCvisreflin(:)       ! CFC fabric/material visual reflectance, inside
        real, ALLOCATABLE :: CFCvistrandir(:)      ! CFC fabric/material visual direct transmittance
        real, ALLOCATABLE :: CFCvistrantotout(:)   ! CFC fabric/material visual total transmittance, outside
        real, ALLOCATABLE :: CFCvistrantotin(:)    ! CFC fabric/material visual total transmittance, inside
        real, ALLOCATABLE :: CFCdrpwidth(:)        ! CFC pleated drape width (mm)
        real, ALLOCATABLE :: CFCdrpspacing(:)      ! CFC pleated drape spacing (mm)
        real, ALLOCATABLE :: CFCwireemiss(:)       ! CFC insect screen wire emissivity
        real, ALLOCATABLE :: CFCwirediam(:)        ! CFC insect screen wire diameter (mm)
        real, ALLOCATABLE :: CFCwirespace(:)       ! CFC insect screen mesh spacing (mm)
        real, ALLOCATABLE :: CFCslattran(:)        ! CFC venetian blind slat beam-diffuse transmittance
        real, ALLOCATABLE :: CFCslatwidth(:)       ! CFC venetian blind slat width (mm)
        real, ALLOCATABLE :: CFCslatspacing(:)     ! CFC venetian blind slat spacing (mm)
        real, ALLOCATABLE :: CFCslatangle(:)       ! CFC venetian blind initial slat angle (deg, 0 =slats are open )
        character(len=4),allocatable :: CFCslatorient(:) ! CFC venetian blind slat orientation (HORZ or VERT)
        real, ALLOCATABLE :: CFCslatcrown(:)       ! CFC venetian blind slat crown (mm)
        real, ALLOCATABLE :: CFCslatwr(:)          ! CFC venetian blind slat width/radius of curvature ratio
        real, ALLOCATABLE :: CFCslatthk(:)         ! CFC venetian blind slat thickness (mm)
        integer(KIND=1), ALLOCATABLE :: CFCfillAir(:)      ! CFC fill gas % mole fraction, Air
        integer(KIND=1), ALLOCATABLE :: CFCfillAr(:)       ! CFC fill gas % mole fraction, argon
        integer(KIND=1), ALLOCATABLE :: CFCfillKr(:)       ! CFC fill gas % mole fraction, krypton
        integer(KIND=1), ALLOCATABLE :: CFCfillXe(:)       ! CFC fill gas % mole fraction, xenon
        integer(KIND=1), ALLOCATABLE :: CFCfillSF6(:)      ! CFC fill gas % mole fraction, SF6
        character(len=6), allocatable :: CFC_IGDB_ID(:) ! CFC IGDB ID for glazings from the IGDB database

      ! DEFINITION OF SUBROUTINES HERE
      CONTAINS
        subroutine CFC_DB_INITIALIZE()
            
            ! Allocate the arrays
            Allocate(cfccatitems(MGCL_CFC))
            Allocate(cfccatname(MGCL_CFC))
            Allocate(cfccatdoc(MGCL_CFC))
            Allocate(cfcname(MGIT_CFC))
            Allocate(cfcdoc(MGIT_CFC))
            Allocate(cfccatindex(MGIT_CFC))
            Allocate(cfcitmindex(MGIT_CFC))
            Allocate(cfcdbcon(MGIT_CFC))
            Allocate(cfcdbden(MGIT_CFC))
            Allocate(cfcdbsht(MGIT_CFC))
            Allocate(cfcdbthick(MGIT_CFC))
            Allocate(CFCshdtp(MGIT_CFC))  
            Allocate(CFCsolreflout(MGIT_CFC))   
            Allocate(CFCsolreflin(MGIT_CFC))  
            Allocate(CFCsoltrandir(MGIT_CFC))
            Allocate(CFCsoltrantotout(MGIT_CFC))
            Allocate(CFCsoltrantotin(MGIT_CFC)) 
            Allocate(CFCemissout(MGIT_CFC))     
            Allocate(CFCemissin(MGIT_CFC))      
            Allocate(CFClwtran(MGIT_CFC))  
            Allocate(CFCvisreflout(MGIT_CFC))   
            Allocate(CFCvisreflin(MGIT_CFC))  
            Allocate(CFCvistrandir(MGIT_CFC))
            Allocate(CFCvistrantotout(MGIT_CFC))
            Allocate(CFCvistrantotin(MGIT_CFC)) 
            Allocate(CFCdrpwidth(MGIT_CFC))  
            Allocate(CFCdrpspacing(MGIT_CFC))   
            Allocate(CFCwireemiss(MGIT_CFC))    
            Allocate(CFCwirediam(MGIT_CFC))   
            Allocate(CFCwirespace(MGIT_CFC))    
            Allocate(CFCslattran(MGIT_CFC))   
            Allocate(CFCslatwidth(MGIT_CFC)) 
            Allocate(CFCslatspacing(MGIT_CFC))  
            Allocate(CFCslatangle(MGIT_CFC))  
            Allocate(CFCslatorient(MGIT_CFC)) 
            Allocate(CFCslatcrown(MGIT_CFC))   
            Allocate(CFCslatwr(MGIT_CFC))     
            Allocate(CFCslatthk(MGIT_CFC))     
            Allocate(CFCfillAir(MGIT_CFC))      
            Allocate(CFCfillAr(MGIT_CFC))      
            Allocate(CFCfillKr(MGIT_CFC))      
            Allocate(CFCfillXe(MGIT_CFC))       
            Allocate(CFCfillSF6(MGIT_CFC))      
            Allocate(CFC_IGDB_ID(MGIT_CFC))

        end subroutine CFC_DB_INITIALIZE
        subroutine CFC_DB_DEALLO()
            Deallocate(cfccatitems)
            Deallocate(cfccatname)
            Deallocate(cfcname)
            Deallocate(cfcdoc)
            Deallocate(cfccatdoc)
            Deallocate(cfccatindex)
            Deallocate(cfcitmindex)
            Deallocate(cfcdbcon)
            Deallocate(cfcdbden)
            Deallocate(cfcdbsht)
            Deallocate(cfcdbthick)
            Deallocate(CFCshdtp)  
            Deallocate(CFCsolreflout)   
            Deallocate(CFCsolreflin)  
            Deallocate(CFCsoltrandir)
            Deallocate(CFCsoltrantotout)
            Deallocate(CFCsoltrantotin) 
            Deallocate(CFCemissout)     
            Deallocate(CFCemissin)      
            Deallocate(CFClwtran)  
            Deallocate(CFCvisreflout)   
            Deallocate(CFCvisreflin)  
            Deallocate(CFCvistrandir)
            Deallocate(CFCvistrantotout)
            Deallocate(CFCvistrantotin) 
            Deallocate(CFCdrpwidth)  
            Deallocate(CFCdrpspacing)   
            Deallocate(CFCwireemiss)    
            Deallocate(CFCwirediam)   
            Deallocate(CFCwirespace)    
            Deallocate(CFCslattran)   
            Deallocate(CFCslatwidth) 
            Deallocate(CFCslatspacing)  
            Deallocate(CFCslatangle)  
            Deallocate(CFCslatorient) 
            Deallocate(CFCslatcrown)   
            Deallocate(CFCslatwr)     
            Deallocate(CFCslatthk)     
            Deallocate(CFCfillAir)      
            Deallocate(CFCfillAr)      
            Deallocate(CFCfillKr)      
            Deallocate(CFCfillXe)       
            Deallocate(CFCfillSF6)      
            Deallocate(CFC_IGDB_ID)
        end subroutine CFC_DB_DEALLO

      end module CFC_Module       ! End of the dat containing module
