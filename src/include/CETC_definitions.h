C---------------------------------------------------------------------
C
C Created by: Alex Ferguson
C Initial Creation Date: Feb 27, 2004.
C Copyright CETC 2003
C
C This file is used to define miscellaneous named constants and
C other frequently used varibles used by various CETC routines
C
C--------------------------------------------------------------------


C---------------------------------------------------------------------
C     Named constants used to describe the nature of the electric load
C     that elec_net_load_calc (in elec_net_utilities.F) is asked
C     to characterize.
C---------------------------------------------------------------------
      INTEGER total_load, hvac_load, occupant_load, total_gen,
     &  store_data, cluster_load, balance, Offsite_gen
      PARAMETER (total_load              = 1)
      PARAMETER (hvac_load               = 2)
      PARAMETER (occupant_load           = 3)
      PARAMETER (total_gen               = 4)
      PARAMETER (cluster_load            = 5)
      PARAMETER (balance                 = 6)
      PARAMETER (Offsite_gen             = 7)
      PARAMETER (store_data              =-1)

C---------------------------------------------------------------------
C     Operations performend by electrical and plant domain time-row
C     transport facilities. See pmatrx.F &
C     enet_state_variable_manipulate.F
C---------------------------------------------------------------------
      integer iSaving           ! flag for storing present time-row data
      integer iWinding          ! flag for overwriting present with future
                                ! time-row data
      integer iRewinding        ! flag for restoring present and future
                                ! time-row data

      parameter (iSaving    = 1,
     &           iWinding   = 2,
     &           iRewinding = 3)

C----------------------------------------------------------------------
C     Flags for first phase flow, second phase flow, hydrogen flow
C     and temperature proprties
C----------------------------------------------------------------------
      integer iPropTemp         ! named constant for temperature properity
      integer iProp1stFlow      ! named constant for first phase flow properity
      integer iProp2ndFlow      ! named constant for second phase flow properity
      integer iPropH2Flow       ! named constant for hydrogen flow
      parameter ( iPropTemp    = 1,
     &            iProp1stFlow = 2,
     &            iProp2ndFlow = 3,
     &            iPropH2Flow  = 4 )

C----------------------------------------------------------------------
C     Flags controlling h3kreports post-processor routines
C----------------------------------------------------------------------
      integer iInitialize           ! Named constant indicating h3kreports
                                    ! modules should self-initialize
      integer iReport               ! Named constant indicating h3kreports
                                    ! modules should report data
      parameter ( iInitialize  = 1,
     &            iReport      = 2 )
