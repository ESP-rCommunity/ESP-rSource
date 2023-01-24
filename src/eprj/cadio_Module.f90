! Cadio_Module
! 
! This module contains the data for Wavefront object files.
      Module Cadio_Module
      SAVE               ! Ensures values do not change between sucessive invocations.
      Integer MOBJS      ! Array size for objects (nbobjs returned from howcomplexobj).
      Integer MOBJFACE   ! Array size for faces (ditto nbobjface).
      Integer MOBJVER    ! Array size for vertices (ditto nbobjver).
      Integer MOBJNVER   ! Array size for edges (ditto nbobjnver).

      integer iwnbvert,iwnbobj,iwnbface          ! Total nb of vertices, objects, faces.
      integer, ALLOCATABLE :: iwfacenver(:)      ! Wavefront face nb of vertices and list.            
      integer, ALLOCATABLE :: iwfacejvn(:,:)     ! Wavefront face nb of vertices and list.             

      CONTAINS

! Allocate the Wavefront object arrays.
      subroutine wave_obj_initialize()
            
      Allocate(iwfacenver(MOBJFACE))
      Allocate(iwfacejvn(MOBJFACE,MOBJNVER))
      end subroutine wave_obj_initialize

! Deallocate the Wavefront object arrays.
      subroutine wave_obj_deallocate()

      Deallocate(iwfacenver)
      Deallocate(iwfacejvn)
      end subroutine wave_obj_deallocate

      end module Cadio_Module       ! End of the dat containing module
