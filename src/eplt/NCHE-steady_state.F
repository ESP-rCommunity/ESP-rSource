C This file is part of the ESP-r system.
C Copyright Natural Resources Canada, Government
C of Canada 2004/2005. Please Contact Ian
C Beausoliel-Morrison for details concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C======================== NCHE.F =====================================
C
C This file contains code necessary to model a natural convection
C heat exchanger from experimental performance results.
C
C This calculation requires two major sections:
C   - the natural convection heat exchanger plant component model
C     which solves for the exit temperatures of the heat exchanger
C   - the natural convection flow rate generator component model
C     which calculates the flow rate based on the configuration of
C     the loop.
C
C   NCHE_coeff_gen: Routine called by the plant modelling domain to
C                   characterize the natural convection heat exchanger
C   NCHE_flow: Calculates the natural convection flow rate based on the
C              pressure drop correlation input by the user
C   NCHE_H3Kreports_module: Transports data to the h3kreports module
C   NCHE_static_template: Static template executed prior to time-step simulation.
C                         Collects NCHE model inputs and performs some
C                         error trapping
C
C REFERENCES:
C
C J. Purdy 2005, "Natural Convection Heat Exchanger and Natural
C Convection Flow-rate Generator Specification for SDHW System",
C CETC-O, Natural Resources Canada Internal Report.
C
C This report was updated in 2007 and several equations in this
C subroutine reference these new equations.
C J. Purdy 2007, "Heat Exchanger Specification for SDHW Project",
C CETC-O, Natural Resources Canada Internal Report.
C======================================================================

C-------------------- NCHE_coeff_gen ----------------------------------
C
C Date:      November 8, 2005
C Author:    Julia Purdy
C Copyright: Natural Resources Canada
C Updated:   November 6, 2007 (Alex Ferguson and Julia Purdy)
C
C ABSTRACT:
C This routine calculates the matrix coefficients for the natural convection
C heat exchanger component model.
C
C
C Inputs:
C
C  iComp_index        - component index number in plant network
C  ISTATS             - index of plant matrix being solved
C
C Outputs:
C  fCoefficients      - array (of 5) containing components matrix
C                       coefficients
C----------------------------------------------------------------------


      SUBROUTINE NCHE_coeff_gen ( iComp_index, fCoefficients,
     &                            ISTATS )
      IMPLICIT NONE

#include "plant.h"
#include "building.h"
#include "NCHE_coefficients.h"

C----------------------------------------------------------------------
C     Passed arguments
C----------------------------------------------------------------------
      INTEGER iComp_index     ! pointer to component in network
      INTEGER ISTATS          ! index of matrix to be solved, will generate
                              ! coefficients for energy balances (ISTATS=1),
                              ! 1st phase mass balances (ISTATS=2), or
                              ! 2nd phase mass balances (ISTATS=3).


      REAL fCoefficients(mpcoe)   ! array containing matrix coefficients
                                  ! calculated by the component.

C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------

C.....Network & control data
      COMMON/c12ps/npcdat,ipofs1,ipofs2
      INTEGER npcdat(mpcom,9)      ! miscellaneous plant data
      INTEGER ipofs1(mcoefg)       ! not used in current context
      INTEGER ipofs2(mcoefg,mpvar) ! not used in current context

C.....Trace & error reporting data (unit numbers)
      COMMON/tc/itc,icnt
      COMMON/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      INTEGER itc,icnt
      INTEGER itcf,itrace,izntrc,itu

C.....Plant interation data
      COMMON/piter/maxitp,perrel,pertmp,perflx,permfl,itrclp,
     &             icsv,csvi
      INTEGER maxitp            ! not used in current context
      INTEGER itrclp            ! not used in current context
      INTEGER icsv(mpnode,mpvar)! flag marking nodes for iteration
      REAL perrel               ! not used in current context
      REAL pertmp               ! not used in current context
      REAL perflx               ! not used in current context
      REAL permfl               ! not used in current context
      REAL csvi(mpnode,mpvar)   ! 'initial' values of state variables
                                ! at start of iteration

C.....Plant present and future state variables
      COMMON/pcval/csvf,csvp
      REAL csvf(mpnode,mpvar)   ! future time-row state variables
      REAL csvp(mpnode,mpvar)   ! present time-row state variables

C.....Plant component connection data
      COMMON/pcond/convar, icontp, icondx
      REAL    convar(mpcon,mconvr)        ! state varibles for connections
      INTEGER icontp(mpcon)               ! type of each connection
      INTEGER icondx(mpcom,mnodec,mpconc) ! pointer to connections for each
                                          ! component/node

      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON ! number of inter-component connections
      INTEGER IPC1  ! component number defining receiving component
      INTEGER IPN1  ! node number defining receiving component
                    ! ...(ie defining inlet node)
      INTEGER IPCT  ! plant component inter-connection type where:
                    !    1 = connected to identical environmental conditions
                    !        as the source component
                    !    2 = connected to constant and known environmental
                    !        conditions (defined by PCONSD(?,1) and PCONSD(?,2))
                    !    3 = connected to another component (as defined
                    !        by IPC2 and IPN2);
                    !    4 = connected to a building zone (as defined by
                    !        PCONSD(?,1)) or ambient conditions (PCONSD(?,1)=0)
      INTEGER IPC2  ! component number defining the sending component
      INTEGER IPN2  ! node number defining the sneding component
      REAL PCONDR   ! ratio of the mass flow rate through the current connection
      REAL PCONSD   ! Array containing a list of constant data items for the
                    ! ...current connection.


C.....Common storing status of plant initialization
      COMMON/plant_initialization/bInitialized
      LOGICAL bInitialized(mpcom)

C.....Simulation timestep
      COMMON/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
      INTEGER ihrp              ! not used in current context
      INTEGER ihrf              ! not used in current context
      INTEGER idyp              ! not used - present day #
      INTEGER idyf              ! not used in current context
      INTEGER idwp              ! not used in current context
      INTEGER idwf              ! not used in current context
      INTEGER nsinc             ! time step number
      INTEGER its,idynow        ! not used in current context

C      common/pltcon/bPltCon,LastTSTP
C      logical bPltCon           ! Boolean indicating whether plant domain has converged.
C      real LastTStp             ! not used

C----------------------------------------------------------------------
C     ESP-r functions
C----------------------------------------------------------------------

      REAL SHtFld       ! Standard ESP-r function - returns the specific heat
                        ! of air, water vapor, or water.


      REAL glycol_water_cp          ! Function for glycol-water specific heat
      REAL glycol_vol_fraction_find ! Function for finding volume fraction of
                                    ! ...glycol inside water-glycol mixture

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------

C.....Heat exchanger conditions
C....... Hot Side
      REAL fHX_hot_mass_flow    ! HX hot side flow rate (kg/s)
      REAL fHX_hot_temp_in      ! HX hot side inlet temperature (oC)
      REAL fHX_hot_fluid_cp     ! HX hot side specific heat
      REAL fHX_hot_heat_cap     ! HX hot side heat capacity (W/K)
C....... Cold side
      REAL fHX_cold_mass_flow   ! HX cold side flow rate (kg/s)
      REAL fHX_cold_temp_in     ! HX cold side inlet temperature (oC)
      REAL fHX_cold_heat_cap    ! HX cold side heat capacity (W/K)
      REAL fHX_cold_temp_out    ! HX cold side outlet temperature (oC)

C.....Heat exchanger future time-row conditions
C....... Cold side
      REAL fHX_cold_temp_out_P  ! HX cold side outlet temperature (oC)



C.....Modified heat exchanger performance indices
      REAL fHX_mod_eff          ! modified HX effectiveness
      REAL fHX_mod_cap_ratio    ! modified capacitance ratio
      REAL fHX_mod_cap_ratio_adj
      REAL fQ_HX                ! heat transfer rate


C.....Heat exchanger glycol-water solution fluid properties
      REAL fHX_glycol_mass_fraction   ! mass fraction of glycol in incoming flow  [%]
      REAL fHX_glycol_volume_fraction ! volume fraction of glycol in incoming flow

C.....Model Topology
C....... Nodal indices
      INTEGER iHX_Hot_Node_index  ! Index of heat exchanger component hot side
                                  !  node in plant network.
      INTEGER iHX_Cold_Node_index ! Index of heat exchanger component cold side
                                  !  node in plant network.
C....... Connection indices
      INTEGER iHX_hot_conn_index  ! Index number of HX component hot node
                                  !  in plant network
      INTEGER iHX_cold_conn_index ! Index number of HX component cold node
                                  !  in plant network

C.....Temperature iteration loop controls
      logical bSearchUnconverged  ! Flag for state of search

      real fHX_cold_temp_out_UB   ! Upper bound of bisection root search
      real fHX_cold_temp_out_LB   ! Lower bound of bisection root search
      real fHX_cold_temp_out_MD   ! Mid-point of bisection root search
      real fHX_cold_temp_out_comp ! Computed cold-side outlet temperature

      real fTempDifference        ! Difference between search loop bounds

      real fSaved_HX_cold_mass_flow(MPCOM) ! Variable storing computed
      save fSaved_HX_cold_mass_flow        !   cold-side flow rate.

C-----------------------------------------------------------------------
C     Named constants
C-----------------------------------------------------------------------
      INTEGER iTemp             ! named constant for temperature property
      INTEGER i1stFlow          ! named constant for first phase flow property
                                !  (water)
      INTEGER i2ndFlow          ! named constant for second phase flow property
                                !  (glycol)
      PARAMETER ( iTemp       = 1,
     &            i1stFlow    = 2,
     &            i2ndFlow    = 3 )

      INTEGER num_matrix_coeff  ! Number of Matrix coefficients

      INTEGER iInitialize       ! Named constant indicating h3kreports
                                !  modules should self-initialize
      INTEGER iReport           ! Named constant indicating h3kreports
                                !  modules should report data
      PARAMETER ( iInitialize = 1,
     &            iReport     = 2 )


C      LOGICAL close             ! Logical variable used to check how close a number is
C                                ! to zero

      logical bNoFlowHot, bNoFlowCold,  ! Flags for no-flow condition
     &        bTempsEqual, bNoNCHEFlow  !   on hot and cold sides.

      REAL small    ! used to ensure there are no divide by zero errors
      PARAMETER ( small = 1.0E-15 )

      INTEGER I
      integer i_temp_loop       ! Counter - temperature convergence loop



C--------------------------------------------------------------------------
C     Initialization
C--------------------------------------------------------------------------
C--------------------------------------------------------------------------
C Start trace if trace output requested.
C--------------------------------------------------------------------------
      IF( ITC.gt.0 .AND. NSINC.ge.ITC .AND. NSINC.le.ITCF .AND.
     &   ITRACE(37).ne.0 )THEN
        WRITE(ITU,*) ' Entering subroutine NCHE_coeff_gen '
      END IF
C------------------------------------------------------------------------
C     Determine component location in plant network topology, and
C     mark component node for iteration
C
C     -> Get node indicies, and mark nodes for iteration
C
C         NPCDAT(i,9):
C          row and column number defining location of component 'i'
C          sub-matrix template in the overall plant network matrix. It is
C          used to identify the location of the current component's nodes
C          within the global plant matrix.
C
C         ICSV(i,j)
C          flag indicating that node 'i' is marked for iteration for state
C          variable 'j'; j=1 for temperature, j=2 for 1st phase mass flow,
C          j=3 for 2nd phase mass flow.
C
C         CSVI(i,j)
C          initial value for judging whether iteration required. Same
C          indices as ICSV.
C
C         CSVF(i,j)
C          future time-row solution variable for plant. Same indices
C          as ICSV.
C----------------------------------------------------------------------
      iHX_Hot_Node_index  = NPCDAT ( iComp_index, 9 )
      iHX_Cold_Node_index = NPCDAT ( iComp_index, 9 ) + 1

C.....Iteration on HX hot side for temperature
      icsv( iHX_Hot_Node_index, 1 )  = 1
C.....Iteration on HX cold side for temperature and flow rate
C.......temperature
      icsv( iHX_Cold_Node_index, 1 ) = 1
C.......1st phase flow
      icsv( iHX_Cold_Node_index, 2 ) = 1
C........there is no 2nd phase flow (for the potential of glycol) on the cold
C........side of the natural convection heat exchanger.

C.....Transport future values to initial values for iteration.
      csvi( iHX_Hot_Node_index, ISTATS )  =
     &                         csvf( iHX_Hot_Node_index, ISTATS )
      csvi( iHX_Cold_Node_index, ISTATS ) =
     &                         csvf( iHX_Cold_Node_index, ISTATS )



C.....HX connection indices
C.....  -> icondx (i,j,k) holds the connection number for the k'th
C.....     connection to the j'th node of component i. It is used
C.....     as a pointer.
      iHX_hot_conn_index  = icondx ( iComp_index, 1, 1 ) ! connection to node 1 (hot)
      iHX_cold_conn_index = icondx ( iComp_index, 2, 1 ) ! connection to node 2 (cold)


C.....check to see if this is the first timestep, i.e., the variables need to
C.....be initialized
      check_initialized: if ( .not. bInitialized ( iComp_index ) ) then

C........Initialize NCHE engine module for H3K reports
         call NCHE_H3Kreports_module ( iComp_index,iInitialize )


C........Set flag to ensure initialization is not repeated
         bInitialized ( iComp_index ) = .true.

C........Initialize stored cold-side flow rate
         fSaved_HX_cold_mass_flow( iComp_index ) = 0.0

      endif check_initialized

C----------------------------------------------------------------------
C    Get inlet temperatures and flow rates to the heat exchanger from connections.
C
C    CONVAR(i,1) - temperature (oC) of sending node for connection `i'.
C    CONVAR(i,2) - mass flow rate (kg/s) of water at sending node for conn `i'.
C    CONVAR(i,3) - mass flow rate (kg/s) of glycol solution at sending node for conn `i'.
C    PCONDR(i)   - ratio of flow rate leaving sending node `i' that reaches
C                  receiving node.
C-------------------------------------------------------------------
C....... Hot Side
      fHX_hot_temp_in    =
     &        CONVAR(iHX_hot_conn_index,iTemp)              ! oC
C....... Total mass flow rate is sum of water mass flow and
C....... glycol mass flow
      fHX_hot_mass_flow  =
     &       PCONDR( iHX_hot_conn_index ) *
     &            CONVAR( iHX_hot_conn_index, i1stFlow )
     &     + PCONDR( iHX_hot_conn_index ) *
     &            CONVAR( iHX_hot_conn_index, i2ndFlow )   ! kg/s
C....... Cold Side
      fHX_cold_temp_in    =
     &       CONVAR( iHX_cold_conn_index, iTemp )    ! oC

      fHX_cold_mass_flow  =
     &       PCONDR( iHX_cold_conn_index ) *
     &            CONVAR( iHX_cold_conn_index, i1stFlow )

C....... The flow rate on the cold side is the natural convection flow
C....... rate and is calculated in the subroutine NCHE_flow

C----------------------------------------------------------------------
C     Get outlet temperatures to the heat exchanger
C     from connections.
C----------------------------------------------------------------------
C.....The present row value is the temperature calculated during
C.......the last time step.
      fHX_cold_temp_out_P = CSVP( iHX_Cold_Node_index, iTemp )   ! oC

C.....And get the most recent temperature.
      fHX_cold_temp_out   = CSVF( iHX_Cold_Node_index, iTemp )   ! oC

C------------------------------------------------------------------------
C Calculate properties of water-glycol solution - if applicable - or water
C at HX hot side.
C------------------------------------------------------------------------
C.....Set mass fraction of glycol in incoming flow to HX. This is a %
C.......for glycol-water mixture properties functions use % for volume fraction
      fHX_glycol_mass_fraction =
     &            100 * CONVAR( iHX_hot_conn_index,i2ndFlow ) /
     &            ( fHX_hot_mass_flow + small )

C.....Find volume fraction of glycol in mixture of incoming flow, based on mass
C.....fraction
      fHX_glycol_volume_fraction =
     &       glycol_vol_fraction_find( fHX_glycol_mass_fraction,
     &              CONVAR( iHX_hot_conn_index, iTemp ))

C.....Specifc heat of fluid inside hot side of HX at the future timestep
      fHX_hot_fluid_cp =
     &                glycol_water_cp( fHX_glycol_volume_fraction,
     &                                fHX_hot_temp_in )


C.....Calculate the future heat capacity of the hot-side fluid.
C.......Could be a glycol-water solution.
      fHX_hot_heat_cap = fHX_hot_mass_flow
     &                 * fHX_hot_fluid_cp            ! W/K

C.....Calculate the future heat capacity of the cold-side fluid:
C.....  specific heat capacity = Shtfld ( X , Temperature ) (J/kg oC)
C.....  where X=1 -> air, X=2 -> vapor, X=3 -> liq. water
      fHX_cold_heat_cap = fHX_cold_mass_flow
     &                * SHtFld ( 3, fHX_cold_temp_in ) ! W/K


C----------------------------------------------------------------------
C-----Check that there is flow through the heat exchanger.
C----------------------------------------------------------------------
      call ECLOSE ( fHX_hot_mass_flow,0.0,0.0001,bNoFlowHot  )
      call ECLOSE ( fHX_cold_mass_flow,0.0,0.0001,bNoFlowCold  )
      call ECLOSE ( fHX_cold_temp_out, fHX_cold_temp_in,
     &                                   0.0001,bTempsEqual)


      check_ISTATS: select case ( ISTATS )

        case ( iTemp )


C-----------------------------------------------------------------------
C         Check for flow through the hot side of the
C         heat exchanger --- NCHE will only induce flow if supplied
C         with hot water
C-----------------------------------------------------------------------
          no_hot_flow: if ( bNoFlowHot ) then
            fHX_cold_mass_flow = 0.0
          else

C-----------------------------------------------------------------------
C           The hot water supplied by the heat exchanger may induce
C           flow. We need to characterize both the mass flow and
C           heat transfer across the NCHE. But these two phenomena
C           are inter-related. We'll establish a bisection root search
C           to determine both the temperature and the flow.
C
C           An earlier version of this model relied on ESP-r's native
C           solver to iterate between the mass and temperature flow
C           domains. But the NCHE model is highly non-linear, and is
C           therefore prone to instablilty.
C-----------------------------------------------------------------------

C...........First, establish the temperature bounds. Outlet temperature
C...........*MUST* lie between cold-side and hot-side inlet temperatures.

            fHX_cold_temp_out_UB = fHX_hot_temp_in
            fHX_cold_temp_out_LB = fHX_cold_temp_in


C-----------------------------------------------------------------------
C           Start iteration loop:
C-----------------------------------------------------------------------

            bSearchUnconverged = .true.
            i_temp_loop = 0


C---------------------------------------------------------------------
C  If the initial temperatures between the hot and cold flows are
C  very close together, there won't be any flow generated
C---------------------------------------------------------------------
              fTempDifference =   fHX_cold_temp_out_UB
     &                          - fHX_cold_temp_out_LB

C              check_initial_temps: if ( fTempDifference < 2.0 ) then
C...............Temperatures agree to within two degrees. Exit loop
C                bSearchUnconverged = .false.
C              endif check_initial_temps

            temp_search_loop: do while ( bSearchUnconverged )
              i_temp_loop = i_temp_loop + 1

C-----------------------------------------------------------------------
C             Compute midpoint for search.
C-----------------------------------------------------------------------
              fHX_cold_temp_out_MD = 0.5 * ( fHX_cold_temp_out_UB
     &                 + fHX_cold_temp_out_LB )

C-----------------------------------------------------------------------
C             Now compute corresponding flow, using function NCHE_flow
C-----------------------------------------------------------------------
              call NCHE_flow( iComp_index,
     &                        fHX_cold_temp_in,
     &                        fHX_cold_temp_out_MD,
     &                        fHX_cold_mass_flow    )

C.............And check if result is zero.
              call ECLOSE ( fHX_cold_mass_flow,0.0,0.0001,bNoNCHEFlow )

C-----------------------------------------------------------------------
C             If there is flow through both sides of the heat
C             exchanger, characterize heat transfer.
C-----------------------------------------------------------------------
              no_cold_flow: if ( .not. bNoNCHEFlow ) then

C...............Calculate the future heat capacity of the hot-side fluid.
C.................Could be a glycol-water solution.
                fHX_hot_heat_cap = fHX_hot_mass_flow
     &                           * fHX_hot_fluid_cp            ! W/K

C...............Calculate the future heat capacity of the cold-side fluid:
C...............  specific heat capacity = Shtfld ( X , Temperature ) (J/kg oC)
C...............  where X=1 -> air, X=2 -> vapor, X=3 -> liq. water
                fHX_cold_heat_cap = fHX_cold_mass_flow
     &                          * SHtFld ( 3, fHX_cold_temp_in ) ! W/K

C-------------------------------------------------------------------------
C               The performance of natural convection heat exchangers
C               is based on modified performance indices.
C-------------------------------------------------------------------------
C...............Capacitance Ratio
C...............  (equation 4 Purdy, 2007)
                fHX_mod_cap_ratio = fHX_cold_heat_cap /
     &                           ( fHX_hot_heat_cap + small )

C...............But the correlation is apparently only
C...............valid when 0<=Cr<1.1 (See Figure 5 in Purdy.)
                fHX_mod_cap_ratio_adj = min( fHX_mod_cap_ratio, 1.1 )


C...............Effectiveness, based on experimental results
C...............(equation 30 Purdy, 2007)
                fHX_mod_eff = fHX_eff_CR_a ( iComp_index ) *
     &                        fHX_mod_cap_ratio_adj
     &                      + fHX_eff_CR_b ( iComp_index ) *
     &                        fHX_mod_cap_ratio_adj**2

C...............The heat transfer rate through the
C...............heat exchanger is determined from the
C...............modified performance indices and the
C...............inlet fluid temperatures (equation 31
C...............in Purdy, 2007).
                fQ_HX = fHX_mod_eff *
     &              fHX_hot_heat_cap
     &                * ( fHX_hot_temp_in - fHX_cold_temp_in )
C                fQ_HX = 500

C...............Now compute resulting outlet temperature:
                fHX_cold_temp_out_comp =
     &              fHX_cold_temp_in + fQ_HX / fHX_cold_heat_cap

              endif no_cold_flow


C---------------------------------------------------------------------
C             Check for convergence
C---------------------------------------------------------------------
              fTempDifference =   fHX_cold_temp_out_UB
     &                          - fHX_cold_temp_out_LB
              check_convergence: if ( fTempDifference < 0.05 ) then
C...............Temperatures agree to within a degree. Exit loop
                bSearchUnconverged = .false.

              else

C...............If the computed outlet temperature is higher than
C...............the mid-point temperature, or there is no flow rate,
C...............raise lower bound.

                update_bounds: if ( bNoNCHEFlow .or.
     &                               ( fHX_cold_temp_out_comp >
     &                                 fHX_cold_temp_out_MD
     &                                )
     &                             ) then


                  fHX_cold_temp_out_LB = fHX_cold_temp_out_MD

                elseif (  fHX_cold_temp_out_comp <=
     &                   fHX_cold_temp_out_MD ) then
                  fHX_cold_temp_out_UB = fHX_cold_temp_out_MD

                endif update_bounds

              endif check_convergence

            enddo temp_search_loop

          endif no_hot_flow

C-----------------------------------------------------------------------
C         Save computed mass flow rate for later use in istats = 2
C         coefficients.
C-----------------------------------------------------------------------
          fSaved_HX_cold_mass_flow(iComp_index) = fHX_cold_mass_flow
C         fSaved_HX_cold_mass_flow(iComp_index) = 0.038
C-----------------------------------------------------------------------
C         Now set temperature matrix coeffiients.
C-----------------------------------------------------------------------

          no_hot_or_cold_flow: if ( bNoFlowHot  .or.
     &                              bNoFlowCold .or.
     &                              bNoNCHEFlow      ) then

            fQ_HX = 0.0
            fHX_mod_eff = 0.0
            fHX_mod_cap_ratio = 0.0
            fHX_mod_cap_ratio_adj = 0.0

C-----------------------------------------------------------------------
C           For no-flow situation, heat transfer is zero. Set outlet
C           temperatures to inlet temperatures.
C-----------------------------------------------------------------------
C............assign ESP-r plant temperature solution matrix coefficients
C
C             |  1   -  -1  -   |  | T_{h_o} |     | 0 |
C             |  -   1  -   -1  |  | T_{c_o} |  =  | 0 |
C                                  | T_{h,i} |
C                                  | T_{c,i} |
C           where:
C
C             |  1   -   3   -  |  | T_{h_o} |     | 5 |
C             |  -   2   -   4  |  | T_{c_o} |  =  | 6 |
C                                  | T_{h,i} |
C                                  | T_{c,i} |

            fCoefficients( 1 ) =  1.0
            fCoefficients( 2 ) =  1.0
            fCoefficients( 3 ) = -1.0
            fCoefficients( 4 ) = -1.0
            fCoefficients( 5 ) =  0.0
            fCoefficients( 6 ) =  0.0

          else

C----------------------------------------------------------------------------
C           assign ESP-r plant temperature solution matrix coefficients
C----------------------------------------------------------------------------
C
C              ESP-r matrix has the following form:
C
C                 | a11   -  a13  -  |  | T_{h_o} |     | R1 |
C                 |  -   a22  -  a24 |  | T_{c_o} |  =  | R2 |
C                                       | T_{h,i} |
C                                       | T_{c,i} |
C
C                 |  1   -    3   -  |  | T_{h_o} |     | 5 |
C                 |  -   2    -   4  |  | T_{c_o} |  =  | 6 |
C                                       | T_{h,i} |
C                                       | T_{c,i} |
C
C----------------------------------------------------------------------------
C              First self-coupling coefficient (equation 15 in Purdy, 2007):
C
C                 a11 = - hot-side heat capacity
C----------------------------------------------------------------------------

            fCoefficients(1) = -1.0 * fHX_hot_heat_cap

C----------------------------------------------------------------------------
C              Second self-coupling coefficient (equation 16 in Purdy, 2007):
C
C                  a22 = cold-side heat capacity
C----------------------------------------------------------------------------

            fCoefficients(2) = fHX_cold_heat_cap

C----------------------------------------------------------------------------
C              First cross-coupling coefficient (equation 17 in Purdy, 2007):
C
C                  a13 = - hot-side heat capacity
C----------------------------------------------------------------------------

            fCoefficients(3) = fHX_hot_heat_cap

C----------------------------------------------------------------------------
C              Second cross coupling coefficient (equation 18 in Purdy, 2007):
C
C                 a24 = - cold-side heat capacity
C----------------------------------------------------------------------------

            fCoefficients(4) = -1.0 * fHX_cold_heat_cap

C----------------------------------------------------------------------------
C              Present-time & known coefficient 1 (equation 19 in Purdy, 2007):
C
C                 R1 = Future HX heat
C----------------------------------------------------------------------------

            fCoefficients(5) = fQ_HX

C----------------------------------------------------------------------------
C              Present-time & known coefficient 2 (equation 20 in Purdy, 2007):
C
C                 R2 =  Future HX heat
C
C----------------------------------------------------------------------------
            fCoefficients(6) = fQ_HX

          endif no_hot_or_cold_flow


C-----------------------------------------------------------------------
C       First-phase flow matrix solution
C-----------------------------------------------------------------------
        case ( i1stFlow )
C........ assign ESP-r plant flow solution matrix coefficients
C         'mass-flow' scenario:
C           |  1   -   -1  -   |  | M_{h_o} |     |    0       |
C           |  -   1   -   0   |  | M_{c_o} |  =  | fNCHE_flow |
C                                 | M_{h,i} |
C                                 | M_{c,i} |
C         where:
C           |  1   -    3   -  |  | M_{h_o} |     | 5 |
C           |  -   2    -   4  |  | M_{c_o} |  =  | 6 |
C                                 | M_{h,i} |
C                                 | M_{c,i} |

C---------Node 1: flows in and out are equal
          fCoefficients( 1 ) =  1.0
          fCoefficients( 3 ) = -1.0 * PCONDR( iHX_hot_conn_index )
C---------Node 2: flow in and flow out equal zero.
          fCoefficients( 2 ) = 1.0
          fCoefficients( 4 ) = 0.0
C---------RHS coefficients
          fCoefficients( 5 ) = 0.0
          fCoefficients( 6 ) = fSaved_HX_cold_mass_flow(iComp_index)

C-----------------------------------------------------------------------
C       2nd-phase (gylcol) solution. Glycol mixutures are supported
C       on the hot-side of the heat exchanger.
C-----------------------------------------------------------------------

        case ( i2ndFlow )
C........ assign ESP-r plant flow solution matrix coefficients
C         'mass-flow' scenario:
C           |  1   -   -1  -   |  | M_{h_o} |     |    0   |
C           |  -   1   -   -1  |  | M_{c_o} |  =  |    0   |
C                                 | M_{h,i} |
C                                 | M_{c,i} |
C         where:
C           |  1   -    3   -  |  | M_{h_o} |     | 5 |
C           |  -   2    -   4  |  | M_{c_o} |  =  | 6 |
C                                 | M_{h,i} |
C                                 | M_{c,i} |
C---------Node 1: flows in and our are equal
          fCoefficients( 1 ) =  1.0
          fCoefficients( 3 ) = -1.0 * PCONDR( iHX_hot_conn_index )
C---------Node 2: flow in and flow out equal zero.
          fCoefficients( 2 ) =  1.0
          fCoefficients( 4 ) = 0.0
C---------RHS coefficients
          fCoefficients( 5 ) = 0.0
          fCoefficients( 6 ) = 0.0

      end select check_ISTATS

C Check to ensure hot outlet temperature is not higher than inlet temperature


C----------------------------------------------------------------------
C     Save reportable data in commons for use at end of simulation
C     timestep.
C
C     -> Report instantaneous data describing system operation at
C        end of the timestep.
C----------------------------------------------------------------------
C.....Heat Exchanger modified effectiveness
      fReport_HX_effectiveness( iComp_index ) = fHX_mod_eff

C.....Heat Exchanger heat transfer rate
      fReport_HX_q ( iComp_index ) = fQ_HX

C.....Hot and cold stream flow rates
      fReport_HX_hot_mass_flow( iComp_index )  = fHX_hot_mass_flow  ! (kg/s)
      fReport_HX_cold_mass_flow( iComp_index ) = fHX_cold_mass_flow ! (kg/s)
C.....Inlet and outlet water temperatures
      fReport_HX_hot_temp_in( iComp_index )  = fHX_hot_temp_in      ! (oC)
      fReport_HX_hot_temp_out( iComp_index ) =
     &                         CSVF( iHX_Hot_Node_index, iTemp )    ! (oC)

      fReport_HX_cold_temp_in( iComp_index )  = fHX_cold_temp_in    ! (oC)
      fReport_HX_cold_temp_out( iComp_index ) =
     &                         CSVF( iHX_Cold_Node_index, iTemp )   ! (oC)


C---------------------------------------------------------------------------------
C Complete trace if trace output requested.
C---------------------------------------------------------------------------------

      IF(ITC.gt.0 .AND. NSINC.ge.ITC .AND. NSINC.le.ITCF .AND.
     &   ITRACE(37).ne.0) THEN
         WRITE(ITU,*) ' Component      ',iComp_index,':'
         WRITE(ITU,*) ' 2 node natural convection heat exchanger'
         WRITE(ITU,*) ' Matrix node(s) ',iHX_Hot_Node_index,
     &                                   iHX_Cold_Node_index
         WRITE(ITU,*) ' Connection(s)  ',iHX_hot_conn_index,
     &                                   iHX_cold_conn_index
         IF(ISTATS.EQ.1) THEN
             WRITE(ITU,*) ' RHS 1 values :'
             WRITE(ITU,*) ' fQ_HX =', fQ_HX
             WRITE(ITU,*) ' and it is made up of :'
             WRITE(ITU,*) ' fHX_mod_eff =', fHX_mod_eff
             WRITE(ITU,*) ' fHX_mod_cap_ratio =', fHX_mod_cap_ratio
             WRITE(ITU,*) ' fHX_hot_heat_cap =', fHX_hot_heat_cap
             WRITE(ITU,*) ' fHX_hot_mass_flow =', fHX_hot_mass_flow
             WRITE(ITU,*) ' fHX_hot_temp_in = ', fHX_hot_temp_in
             WRITE(ITU,*) ' fHX_cold_temp_in = ', fHX_cold_temp_in
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         num_matrix_coeff = 6
         WRITE(ITU,*)  (fCoefficients(I),I = 1,num_matrix_coeff)
c        IF(ITU.eq.IUOUT) THEN  ! trace output going to screen, not file
c          itemp_trace=(IPCOMP/4)*4
c          IF(itemp_trace.eq.IPCOMP .OR. IPCOMP.eq.NPCOMP) call epagew ! write 4 lines at a time.
c        END IF
        WRITE(ITU,*) ' Leaving subroutine NCHE_coeff_gen'
      END IF
C Trace output
      if (ITRACE(35) .NE. 0) THEN
      endif


      return

      END SUBROUTINE



C----------------------------- NCHE_flow --------------------------------------------
C

C Created by: Julia Purdy
C Initial Creation Date: November 22, 2005.
C Copyright CETC 2005
C
C This subroutine is used to evaluate the natural convection flow rate in the
C stratified storage tank-side loop of a solar domestic hot water (SDHW) system.
C
C This component requires information input by the user for the Natural Convection
C Heat Exchanger (NCHE) model - component 120 - specifically the heat exchanger
C Flow Rate vs Pressure Drop (DP) coeffiecients, c and d, where
C flow=c*DP^d.
C
C INPUTS
C    fHX_flow_P_c       ! Natural convection flow rate vs. pressure drop coefficient c
C    fHX_flow_P_d       ! Natural convection flow rate vs. pressure drop coefficient d
C    fTank_height       ! Height of storage tank (m)
C    fHX_height         ! Height of heat exchanger (m)
C    fHX_cold_temp_in   ! HX cold side inlet temperature (oC)
c    fHX_cold_temp_out  ! HX cold side outlet temperature (oC)
C
C OUTPUTS
C    fNCHE_flow         ! natural convection flow rate around loop
C
C Reference:
C J. Purdy 2005, "Natural Convection Heat Exchanger and Natural
C Convection Flow-rate Generator Specification for SDHW System",
C CETC-O, Natural Resources Canada Internal Report.
C------------------------------------------------------------------------------------

      SUBROUTINE NCHE_flow( iComp_index,
     &                      fHX_cold_temp_in,
     &                      fHX_cold_temp_out,
     &                      fNCHE_flow )
      IMPLICIT NONE

#include "plant.h"
#include "building.h"
#include "NCHE_coefficients.h"



C Passed Variables
      INTEGER iComp_index    ! pointer to component in network
      REAL fHX_cold_temp_in  ! HX cold side inlet temperature (oC)
      REAL fHX_cold_temp_out ! HX cold side outlet temperature (oC)

C Local Variables
      REAL fTank_temp_mean   ! mean stratified storage tank temperature
      REAL fRHO_tank_mean    ! density of fluid at mean storage tank temperature
      REAL fRHO_HX_cold_in   ! density of water at HX cold side inlet
      REAL fRHO_HX_cold_out  ! density of water at HX cold side outlet
      REAL fNCHE_DP          ! pressure drop due to shear around the natural
                             ! convection loop

      REAL g                 ! acceleration due gravity (m/s^2)
      INTEGER iFluid_type    ! fluid type in storage tank (water=2)

C Called Function
      REAL DENSIT            ! ESP-r function that returns the density (kg/m^3)
                             ! as function of fluid type (1=air, 2=water) and
                             ! fluid temperature (C)


C.....Trace & reporting data
      COMMON/tc/itc,icnt
      COMMON/trace/itcf,itrace(mtrace),izntrc(mcom),itu
C.....Trace/error reporting unit numbers
      INTEGER itc,icnt
      INTEGER itcf,itrace,izntrc,itu
C.....Simulation timestep
      COMMON/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
      INTEGER ihrp              ! not used in current context
      INTEGER ihrf              ! not used in current context
      INTEGER idyp              ! not used - present day #
      INTEGER idyf              ! not used in current context
      INTEGER idwp              ! not used in current context
      INTEGER idwf              ! not used in current context
      INTEGER nsinc             ! time step number
      INTEGER its,idynow        ! not used in current context

C Constants
      g = 9.81               ! acceleration due gravity (m/s^2)
      iFluid_type = 2        ! fluid type in cold-side of HX -> water


C The value of fTank_Height (Stratified tank height) is retrieved from the stratified tank model
C ADATA items in the NCHE static template (NCHE_static_template) and
C is stored in COMMON/NCHE_coefficients/.
C      fTank_height          ! Height of storage tank (m)


C The average tank temperature is retrieved from the stratified tank model for the present timestep.
C This value is stored in the common block defined in stratified_tank.h

       fTank_temp_mean =
     &          fHX_get_stratified_tank_avg_temp ( iStrat_tank_number )  ! Average tank temperature (oC)


C---------------------------------------------------------------------------------
C   Calculate the density of the storage tank
C---------------------------------------------------------------------------------
C..... DENSIT is a standard ESP-r subroutine that returns density (kg/m^3) as function of
C..... fluid type (1=air, 2=water) and fluid temperature (oC)

      fRHO_tank_mean = DENSIT( iFluid_type, fTank_temp_mean )


C---------------------------------------------------------------------------------
C   Calculate the densities of the fluid at the heat exchanger cold-side
C   inlet and outlet
C---------------------------------------------------------------------------------
C..... DENSIT is a standard ESP-r subroutine that returns density (kg/m^3) as
C..... function of fluid type (1=air, 2=water) and fluid temperature (oC)

      fRHO_HX_cold_in  = DENSIT( iFluid_type,fHX_cold_temp_in )
      fRHO_HX_cold_out = DENSIT( iFluid_type,fHX_cold_temp_out )


C---------------------------------------------------------------------------------
C   Pressure drop around the loop (equation 44, Purdy 2005)
C---------------------------------------------------------------------------------
        fNCHE_DP = g *
     &    (
     &      ( fRHO_tank_mean * fTank_height ) -
     &      ( fRHO_HX_cold_in * ( fHX_height (iComp_index) / 2 )) -
     &      ( fRHO_HX_cold_out *
     &        ( fTank_height - fHX_height (iComp_index) / 2 ))
     &     )

C..... Check for possible negative pressure drop around the loop.
      if (fNCHE_DP .LT. 0.0 ) fNCHE_DP = 0.0

C---------------------------------------------------------------------------------
C   Natural convection mass flow rate (equation 45, Purdy 2005)
C---------------------------------------------------------------------------------

      fNCHE_flow = fHX_flow_P_c ( iComp_index ) *
     &               fNCHE_DP**fHX_flow_P_d ( iComp_index )


      IF(ITC.gt.0 .AND. NSINC.ge.ITC .AND. NSINC.le.ITCF .AND.
     &   ITRACE(37).ne.0) THEN
         WRITE(ITU,*) ' NCHE_flow subroutine:'
         WRITE(ITU,*) ' fHX_flow_P_c (iComp_index) =',
     &                    fHX_flow_P_c (iComp_index)
         WRITE(ITU,*) ' fHX_flow_P_d (iComp_index) =',
     &                    fHX_flow_P_d (iComp_index)
         WRITE(ITU,*) ' fNCHE_DP =', fNCHE_DP
         WRITE(ITU,*) ' fNCHE_flow =', fNCHE_flow
         WRITE(ITU,*) ' leaving NCHE_flow subroutine:'
      end if


      return

      END SUBROUTINE


C--------------------- NCHE_H3Kreports_module -------------------------
C
C This procedure transports data from the NCHE (Natural Convection Heat Exchanger)
C model to the h3kreporting object.
C
C Inputs:
C   - iIndex:  pointer to component in plant network
C   - iStatus: flag indicating if reports should be initialized,
C              or if data can be transported to the h3kreports
C              module.
C Outputs:
C    None.
C
C-----------------------------------------------------------------------

      SUBROUTINE NCHE_H3Kreports_module ( iIndex, iStatus )
      use h3kmodule
      IMPLICIT NONE

#include "plant.h"
#include "CETC_definitions.h"
#include "NCHE_coefficients.h"

C.....Arguments
      INTEGER iIndex              ! Index of plant component
      INTEGER iStatus

C-----------------------------------------------------------------------
C     ESP-r commons necessary for reporting
C-----------------------------------------------------------------------
      COMMON/pcnam/pcname(mpcom)        ! Plant component names
      CHARACTER*15, pcname

C-----------------------------------------------------------------------
C     Local variables
C-----------------------------------------------------------------------
      INTEGER iNameLength(mpcom)  ! length of plant component name

      REAL fDay                   ! day of the year (0.0 - 365.0)

      LOGICAL bInitialized(mpcom) ! Flag for initializing parameters.
      SAVE bInitialized
      DATA bInitialized / mpcom * .false. /


      INTEGER lnblnk             ! function returning length of a string


C----------------------------------------------------------------------
C     References
C----------------------------------------------------------------------

      REAL fConvert_Current_TS_to_DAY ! Function converting the current
                                      ! timestep to a REAL # representing
                                      ! the current day & fraction thereof



C-----------------------------------------------------------------------
C     First call - determine length of string
C-----------------------------------------------------------------------
      if ( .not. bInitialized ( iIndex ) ) then

        ! Note: LNBLNK will be called once for each component
        iNameLength( iIndex ) = lnblnk ( pcname ( iIndex ) )
        bInitialized ( iIndex ) = .true.

      endif

C........Get current day
         fDay = fConvert_Current_TS_to_DAY()

C----------------------------------------------------------------------
C.....Heat exchanger effectiveness
C     Report value
      call AddToReport(rvPltHXchgEff%Identifier,
     &         fReport_HX_effectiveness( iIndex ),
     &         pcname(iIndex)(1:iNameLength(iIndex)))

C----------------------------------------------------------------------
C.....Heat Exchanger heat transfer rate
      call AddToReport(rvPltHXchgHTrans%Identifier,
     &      fReport_HX_q( iIndex ),
     &      pcname(iIndex)(1:iNameLength(iIndex)))

C---------------------------------------------------------------------
C.....Hot and cold stream flow rates (kg/s)
C---------------------------------------------------------------------
C.....Write a name: Hot water (forced flow) mass flow rate
      call AddToReport(rvPltHXchgHFlowRate%Identifier,
     &      fReport_HX_hot_mass_flow( iIndex ),
     &      pcname(iIndex)(1:iNameLength(iIndex)))

C---------------------------------------------------------------------
C.....Write a name: Cold water (natural convection) mass flow rate
      call AddToReport(rvPltHXchgCFlowRate%Identifier,
     &      fReport_HX_cold_mass_flow( iIndex ),
     &      pcname(iIndex)(1:iNameLength(iIndex)))

C---------------------------------------------------------------------
C.....Inlet and outlet fluid temperatures
C-------------------------------------------------------------------
C.....Write a name: Hot fluid inlet temperature
      call AddToReport(rvPltHXchgHTempIn%Identifier,
     &      fReport_HX_hot_temp_in( iIndex ),
     &      pcname(iIndex)(1:iNameLength(iIndex)))

C-------------------------------------------------------------------
C.....Write a name: Hot fluid outlet temperature
      call AddToReport(rvPltHXchgHTempOut%Identifier,
     &      fReport_HX_hot_temp_out( iIndex ),
     &      pcname(iIndex)(1:iNameLength(iIndex)))

C-----------------------------------------------------------------
C.....Write a name: Cold water inlet temperature
      call AddToReport(rvPltHXchgCTempIn%Identifier,
     &      fReport_HX_cold_temp_in( iIndex ),
     &      pcname(iIndex)(1:iNameLength(iIndex)))

C------------------------------------------------------------------
C.....Write a name: Cold water outlet temperature
      call AddToReport(rvPltHEXchgCTempOut%Identifier,
     &      fReport_HX_cold_temp_out( iIndex ),
     &      pcname(iIndex)(1:iNameLength(iIndex)))

      return

      END SUBROUTINE




C---------------------- NCHE_static_template ----------------------------
C
C Date:      September 10, 2007
C Author:    Julia Purdy
C Copyright: Natural Resources Canada
C
C ABSTRACT:
C This subroutine is the static template for the natural convection
C heat exchanger (NCHE) component.
C
C It checks whether the component is properly used in the user-defined
C plant network and it collects time-invariant plant component parameters
C stored in ESP-r's adata & bdata arrays, and stores them in the
C common block COMMON/NCHE_coefficients/.
C
C The NCHE component is represented with two nodes:
C   node 1 represents the forced flow side with fluid from the solar collector.
C   node 2 represents the natural convection side with fluid from the
C          storage tank
C
C The static template also ensures that the NCHE is connected to a stratified storage
C tank - and will issue an error message if this is not the case.
C
C INPUTS:
C    iComp_index  index number of the component under consideration (=IPCOMP)
C    NPCOMP       number of plant components in user-defined plant network
C    NCI(i)       holds the number of possible plant control variables for plant component `i'
C    ADATA(i,j)   holds `j'th data item for component `i' read from .pln file
C                 (derived from plant components database)
C    ICONTP(i)    holds the state variable index (ISV) of the sending node for connection `i'.
C    IUOUT        output channel to feedback to user
C    ITC          trace output index (>0 gives a trace output)
C    ITU          output channel for trace output
C    ITRACE(35)   flag indicating one of the plant traces (1= active)

C OUTPUTS:
C    NONE
C
C -------------------------------------------------------------------------------------------

      SUBROUTINE NCHE_static_template( iComp_index )

      IMPLICIT NONE

#include "building.h"
#include "plant.h"
#include "NCHE_coefficients.h"

C---------------------------------------------------------------------------------
C ESP-r Common Block Variables.
C---------------------------------------------------------------------------------

C.....Trace & reporting data
      COMMON/tc/itc,icnt
      COMMON/outin/iuout,iuin,ieout
      COMMON/trace/itcf,itrace(mtrace),izntrc(mcom),itu
C.....Trace/error reporting unit numbers
      INTEGER itc,icnt
      INTEGER iuout,iuin,ieout
      INTEGER itcf,itrace,izntrc,itu

C.....Network & control data
      COMMON/c9/npcomp,nci,cdata
      INTEGER npcomp            ! number of plant components
      INTEGER nci(mpcom)        ! number of controls / component
      REAL cdata(mpcom,mmiscd)  ! control data for each component

C.....Plant component configuration data - read from input file
      COMMON/pdbdt/adata,bdata
      REAL adata(mpcom,madata) ! miscellaneous data for component
      REAL bdata(mpcom,mbdata) ! more miscellaneous data for component

C.....Plant component connection data
      COMMON/pcond/convar, icontp, icondx
      REAL    convar(mpcon,mconvr)        ! connection ->  state variables,
                                          ! ...temperature/flow data
      INTEGER icontp(mpcon)               ! connection -> type
      INTEGER icondx(mpcom,mnodec,mpconc) ! connection -> index for each
                                          ! ...component / node / connection

C.....ESP-r common variable holding node numbers for plant components within plant
C     network
      COMMON/C12PS/NPCDAT,IPOFS1,IPOFS2
      INTEGER NPCDAT(MPCOM,9) ! Miscellanous plant data
      INTEGER IPOFS1(MCOEFG)
      INTEGER IPOFS2(MCOEFG,MPVAR)


      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON ! number of inter-component connections
      INTEGER IPC1  ! component number defining receiving component
      INTEGER IPN1  ! node number defining receiving component
                    ! ...(ie defining inlet node)
      INTEGER IPCT  ! plant component inter-connection type where:
                    !    1 = connected to identical environmental conditions
                    !        as the source component
                    !    2 = connected to constant and known environmental
                    !        conditions (defined by PCONSD(?,1) and PCONSD(?,2))
                    !    3 = connected to another component (as defined
                    !        by IPC2 and IPN2);
                    !    4 = connected to a building zone (as defined by
                    !        PCONSD(?,1)) or ambient conditions (PCONSD(?,1)=0)
      INTEGER IPC2  ! component number defining the sending component
      INTEGER IPN2  ! node number defining the sneding component
      REAL PCONDR   ! ratio of the mass flow rate through the current connection
      REAL PCONSD   ! Array containing a list of constant data items for the
                    ! ...current connection.

C.....Common storing status of plant initialization.
      common / plant_initialization / bInitialized
      logical bInitialized(mpcom)

C---------------------------------------------------------------------------------
C Declare local variables.
C---------------------------------------------------------------------------------
      INTEGER NumADATA, N_expect, IPCONC
      INTEGER iComp_index     ! index of component in plant network = IPCOMP
      INTEGER J               ! counter
      INTEGER iTemp_trace

C --- Plant connection index numbers
      integer iHX_cold_node_number ! Node number for heat exchanger component cold side
                                   ! ...node in plant network (Node 2)
      integer iHX_hot_node_number  ! Node number for heat exchanger component cold side
                                   ! ...node in plant network (Node 1)
      integer iHX_Conn_Index_in    ! index of incoming fluid connection
                                   ! ...to cold side (Node 2) of NCHE model
      integer iSendCompIndex       ! index of the sending component
      integer iSendCompNodeNumber  ! node number of the sending component
      integer iRecevCompIndex      ! index of the receiving component
      integer iRecevCompNodeNumber ! node number of the receiving component
      integer iCon                 ! counter - outgoing connection index numbers


C---Logical variables
      LOGICAL mistake,CLOSE

      LOGICAL bFatal_error     ! flag indicating error has occurred.

      bFatal_error = .false.

C Set the value of bInitialized to false
      bInitialized(iComp_index) = .false.



C---------------------------------------------------------------------------------
C Write out ADATA if there is a trace output. Note that there is no BDATA
C used with this component.
C---------------------------------------------------------------------------------

      IF( ITC>0 .AND. ITRACE(35) .NE. 0 ) THEN
        WRITE(ITU,*) ' Component ',iComp_index,
     &               ' pre-simulation data for a:'
        WRITE(ITU,*) ' 2-node NCHE  '
        NumADATA = 5  ! Number of ADATA items.
        WRITE(ITU,*) ' ADATA ',( ADATA(iComp_index,J),J = 1,NumADATA )
        IF( ITU==IUOUT ) THEN  ! trace output going to screen, not file
          iTemp_trace=( iComp_index/5 ) * 5
          IF( iTemp_trace == iComp_index .OR. iComp_index == NPCOMP )
     &                 call epagew ! write 5 lines at a time.
        END IF
      END IF

C---------------------------------------------------------------------------------
C Ensure that user has specified the correct number of control variables in
C .pln file. NCI(iComp_index) holds the number of possible plant control variables
C as specified in the .pln file. The NCHE component has no control variables.
C---------------------------------------------------------------------------------

      N_expect = 0
      IF( NCI(iComp_index) /= N_expect ) THEN
        WRITE(ITU,*) ' NCHE_static_template warning: incorrect number',
     &               ' of controlled variables specified.'
      ENDIF


C---------------------------------------------------------------------------------
C Check that each node in the component has the correct number of connections
C to other components.
C Variables used:
C    MPCONC           the maximum allowable connections to a node (from plant.h).
C    ICONDX(i,j,k)    the connection number for the k'th connection to the j'th node
C                     of component i. It is used as a pointer.
C    'mistake'        a flag indicating whether there are connection errors:
C                     .true. means there are errors.
C---------------------------------------------------------------------------------
      mistake = .false.

C-----There should be one connection to node 1.
      IF( ICONDX( iComp_index,1,1 ) == 0 ) mistake=.true.
      DO IPCONC=2,MPCONC
        IF( ICONDX( iComp_index,1,IPCONC ) /= 0 ) mistake=.true.
      END DO

C-----There should be one connection to node 2.
      IF( ICONDX( iComp_index,2,1 ) == 0 ) mistake=.true.
      DO IPCONC=2,MPCONC
        IF( ICONDX( iComp_index,2,IPCONC ) /= 0 ) mistake=.true.
      END DO

C-----Write error message if the number of connections to the nodes are incorrect.
      IF(mistake)THEN
        WRITE(IUOUT,*) ' NCHE_static_template: incorrect number of'
        WRITE(IUOUT,*) ' connection for component ',iComp_index
        STOP ' NCHE_static_template: unresolvable error'
      END IF


C---------------------------------------------------------------------------------
C Check that the connection to nodes 1 and 2 are of the correct type.
C The connections should be of type ISV=20 so that the NCHE component can
C be used in networks in which both first and second phase mass balances are formed.
C Variables used:
C    ICONTP(i)  holds the state variable index (ISV) of the sending
C                node for connection `i'.
C    ISV        defines nodal fluid type & coefficient generator model capabilities:
C               ISV = 0,10,20 node represents water + ....
C               ISV = 1,11,21 node represents dry air + ....
C               ISV = 9,19,29 node represents some solid material only
C               0  <= ISV < 10  model suitable for energy balance only
C               10 <= ISV < 20  model suitable for energy + single phase mass balance
C               20 <= ISV < 30  model suitable for energy + two phase mass balances
C---------------------------------------------------------------------------------
      mistake = .false.

      IF( ICONTP( ICONDX(iComp_index,1,1) ) /= 20 ) mistake=.true.
      IF( mistake )THEN
        WRITE( IUOUT,* )
     &   ' NCHE_static_template: incorrect connection type'
        WRITE( IUOUT,* ) ' to node 1 for component ',iComp_index
        STOP ' NCHE_static_template: unresolvable error'
      END IF
      IF( ICONTP( ICONDX(iComp_index,2,1) ) /= 20 ) mistake=.true.
      IF( mistake )THEN
        WRITE( IUOUT,* )
     &   ' NCHE_static_template: incorrect connection type'
        WRITE( IUOUT,* ) ' to node 2 for component ',iComp_index
        STOP ' NCHE_static_template: unresolvable error'
      END IF


C----------------------------------------------------------------------------------
C Check that the cold (natural convection) side of the heat exchanger is connected
C to an appropriate storage tank. This is required by the empirical model so that
C a pressure differential between the top and the bottom of the storage tank will
C promote natural convection flow.
C
C There should be a connection to/from:
C  - the stratified tank model (component code = 122); or
C  - the soon-to-be-completed hx-in-tank stratified tank model
C    (component code = ??)
C--------------------------------------------------------------------
      mistake = .false.

C     The cold-side of the NCHE component is node 2
      iHX_cold_node_number = 2
C     The hot-side of the NCHE component is node 1
      iHX_hot_node_number = 1


C-----Get index of incoming fluid connection
C.....Storage tank to node 2 (HX-cold side) water connection index
C.....  -> icondx (i,j,k) holds the connection number for the k'th
C.....     connection to the j'th node of component i. It is used
C.....     as a pointer.
      iHX_Conn_Index_in =
     &             ICONDX( iComp_index,iHX_Cold_Node_number,1 )


C-----Determine the connection type/index for the incoming and outgoing
C-----connections
C.....Incoming connection type
C.....  IPC2(iHX_Conn_Index_in)  ! <- sending component
      iSendCompIndex = IPC2( iHX_Conn_Index_in )
      iSendCompNodeNumber = IPN2( iHX_Conn_Index_in )


C.....Outgoing connection index
C.....  IPC2(iHX_Conn_Index_in)  ! <- receiving component
      DO iCon = 1, NPCON

         IF ( ( IPC2( iCon ) .EQ. iSendCompIndex )  .AND.
     &             ( IPN2( iCon ) .EQ. iSendCompNodeNumber ) ) THEN

             iRecevCompIndex = IPC2( iCon )
             iRecevCompNodeNumber = IPN2( iCon )
         ENDIF

      ENDDO


C-----If the index numbers for the incoming and outgoing connections are
C-----the same, then the heat exchanger is connected to only one component

      IF ( NPCDAT(iRecevCompIndex,4) ==
     &                             NPCDAT (iSendCompIndex,4) ) THEN


C......Determine if the NCHE cold-side connection is to:
C...... the stratified tank model (iRecevCompIndex = 122); or
C...... the soon-to-be-developed stratified in-tank heat exchanger model

C......Stratified storage tank
         IF ( NPCDAT(iRecevCompIndex,4) == 122 ) THEN

C........... Variable iStrat_tank_CompIndex is saved to the common block.
C........... It is used to get the average stratified tank temperature each timestep.
            iStrat_tank_CompIndex = iRecevCompIndex

C............Call to function to get tank height from stratified tank model
            fTank_Height = fHX_get_stratified_tank_height
     &                                        ( iRecevCompIndex )

C If there are more than 1 stratified tank components in the network, the NCHE model
C needs to know which component number (1 - maximum number of stratified tank components)
C is connected to it.
C This variable will be referenced every timestep to get the average stratified tank temperature
C For the first release, this value will be always set to one.
            iStrat_tank_number = 1    ! reference to the first stratified tank in the network

C---there is currently only one valid stratified tank model. When the in-tank heat
C---exchanger model is added to the stratified storage tank, this code will be
C---enabled.
C         ELSEIF ( iRecevCompIndex == 119 ) THEN
C...........Call to functions for appropriate case to get tank height and
C...........average temperature
C            fTank_Height = fHX_get_in_tank_hx_height( iRecevCompIndex )


         ELSE
C......Node 2 of the NCHE is not connected to an acceptable storage tank
            WRITE( IUOUT,* )
     &          ' NCHE_static_template: side-arm heat exchanger '
            WRITE( IUOUT,* ) ' must be connected to a stratified tank '
            STOP ' NCHE_static_template: unresolvable error'

         ENDIF

      ENDIF


C--------------------------------------------------------------------
C Read in the time-invariant user-input data (ADATA) from the
C plant input and store this data in the common block
C COMMON/NCHE_coefficients/.
C--------------------------------------------------------------------
C     Heat Exchanger Empirical Results
C--------------------------------------------------------------------

C.....Modified effectiveness vs. capacitance ratio
      fHX_eff_CR_a ( iComp_index ) = ADATA ( iComp_index, 1 )
      fHX_eff_CR_b ( iComp_index ) = ADATA ( iComp_index, 2 )

C.....Natural convection flow rate vs. pressure drop
      fHX_flow_P_c ( iComp_index ) = ADATA ( iComp_index, 3 )
      fHX_flow_P_d ( iComp_index ) = ADATA ( iComp_index, 4 )

C--------------------------------------------------------------------
C     Heat Exchanger height (m)
C--------------------------------------------------------------------
      fHX_height ( iComp_index ) = ADATA ( iComp_index, 5 )


C.....Is HX height close to zero?
      call eclose( ( fHX_height(iComp_index) ), 0.0, 1.0E-10,
     &             close )
      IF ( close ) THEN
         write (itu,*) 'NCHE Error: Specified '
     &        // 'height is zero!  '
         write (itu,'(A,I2,A,I2)')
     &                 '                 Check misc data item',
     &                 5,' in Component # ', iComp_index

         bFatal_error = .true.

      ENDIF

      IF (bFatal_error)
     &   stop 'NCHE_static_template (NCHE.F): Fatal error!'

      RETURN

      END SUBROUTINE


C ****************** fHX_get_stratified_tank_height ******************

C fHX_get_stratified_tank_height is a real function which returns the
C height (m) of the stratified tank model.
C This stratified tank is connected to the cold side (Node 2) of the
C NCHE - Natural Convection Heat Exchanger

      FUNCTION fHX_get_stratified_tank_height( iStrat_tank_CompIndex )

      IMPLICIT NONE
#include "building.h"
#include "plant.h"

      common/pdbdt/adata,bdata
      real adata(mpcom,madata) ! miscellaneous data for component
      real bdata(mpcom,mbdata) ! more miscellaneous data for component
      INTEGER iStrat_tank_CompIndex
      REAL fHX_get_stratified_tank_height


      fHX_get_stratified_tank_height =
     &                        ADATA( iStrat_tank_CompIndex,2 )


      RETURN
      END
C ********************************************************************

C ****************** fHX_get_stratified_tank_avg_temp ******************

C fHX_get_stratified_tank_avg_temp is a real function which returns the
C average temperature (degrees C) of the stratified tank model for the
C current timestep.

      FUNCTION fHX_get_stratified_tank_avg_temp( iStrat_tank )

      IMPLICIT NONE
#include "stratified_tank.h"

        INTEGER iStrat_tank
        REAL fHX_get_stratified_tank_avg_temp



        fHX_get_stratified_tank_avg_temp =
     &                  TAVGF_STANK( iStrat_tank )


      RETURN
      END
C ********************************************************************








