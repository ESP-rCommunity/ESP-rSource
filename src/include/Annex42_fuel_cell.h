C Define PARAMETERS for use with the Annex 42 SOFC model.

C Operational modes of SOFC-cogeneration device.
      REAL COLD                     ! Completely shut off.
      PARAMETER ( COLD = 1. )
      REAL NORMAL                   ! Producing heat and electricity.
      PARAMETER ( NORMAL = 2. )
      REAL STARTING_UP              ! In controlled start-up period.
      PARAMETER ( STARTING_UP = 3. )
      REAL SHUTTING_DOWN            ! In controlled shut-down period.
      PARAMETER ( SHUTTING_DOWN = 4. )

C Method used to establish air supply to FCPM.
      INTEGER const_excess_air_ratio  ! Constant excess air ratio method.
      PARAMETER ( const_excess_air_ratio = 1 )
      INTEGER air_f_elec_output       ! Air supply is function of elec output method.
      PARAMETER ( air_f_elec_output      = 2 )
      INTEGER air_f_fuel_flow         ! Air supply is function of fuel supply method.
      PARAMETER ( air_f_fuel_flow        = 3 )

C Method used to determine temperature of air entering blower.
      INTEGER air_from_room      ! Air is drawn from room containing SOFC-cogen unit.
      PARAMETER ( air_from_room     = 1 )
      INTEGER air_from_outdoors  ! Air is drawn from outdoors.
      PARAMETER ( air_from_outdoors = 2 )

C Method used to determine temperature of fuel entering compressor.
      INTEGER fuel_from_room          ! Fuel enters at room air temperature.
      PARAMETER ( fuel_from_room     = 1 )
      INTEGER fuel_from_outdoors      ! Fuel enters at outdoor air temperature.
      PARAMETER ( fuel_from_outdoors = 2 )

C Method used to determine temperature of water entering pump.
      INTEGER water_from_room    ! Water enters at room air temperature.
      PARAMETER ( water_from_room  = 1 )
      INTEGER water_from_mains   ! Water enters at mains water temperature.
      PARAMETER ( water_from_mains = 2 )

C Method to calculate heat exchanger from product gases to water.
      INTEGER HX_fixed_eff          ! HX calculated with a fixed effectiveness.
      PARAMETER ( HX_fixed_eff          = 1 )
      INTEGER HX_lmtd_empirical     ! HX calc with LMTD approach with UA=f(empirical data).
      PARAMETER ( HX_lmtd_empirical     = 2 )
      INTEGER HX_lmtd_deterministic ! HX calc with LMTD approach with UA given in deterministic form.
      PARAMETER ( HX_lmtd_deterministic = 3 )
      INTEGER HX_condensing         ! Sensible HX calc with LMTD approach + method to estimate condensing HX.
      PARAMETER ( HX_condensing         = 4 )

C Method to calculate heat losses from FCPM (i.e. skin losses).
      INTEGER const_skin_loss        ! Constant skin loss method.
      PARAMETER ( const_skin_loss  = 1 )
      INTEGER skin_f_dT              ! Skin losses a function of temp diff method.
      PARAMETER ( skin_f_dT        = 2 )
      INTEGER skin_f_fuel_flow       ! Skin losses a function of fuel flow rate method.
      PARAMETER ( skin_f_fuel_flow = 3 )

C Auxiliary burner.
      INTEGER Auxburn_yes           ! There is an auxiliary burner.
      PARAMETER ( Auxburn_yes = 1 )
      INTEGER Auxburn_no            ! There is no auxiliary burner.
      PARAMETER ( Auxburn_no  = 0 )
      INTEGER Auxburn_cap_in_W      ! Burner capacity specified in heat output (W).
      PARAMETER ( Auxburn_cap_in_W     = 1 )
      INTEGER Auxburn_cap_in_kmols  ! Burner capacity specified in fuel input (kmol/s).
      PARAMETER ( Auxburn_cap_in_kmols = 2 )
      INTEGER Auxburn_heat_to_room  ! Heat loss from burner added to room air.
      PARAMETER ( Auxburn_heat_to_room = 1 )
      INTEGER Auxburn_heat_to_air   ! Heat loss from burner added to FCPM air intake.
      PARAMETER ( Auxburn_heat_to_air  = 2 )

C Battery and PCU.
      INTEGER battery_heat_to_room  ! Heat loss from battery added to room air.
      PARAMETER ( battery_heat_to_room = 1 )
      INTEGER battery_heat_to_air   ! Heat loss from battery added to FCPM air intake.
      PARAMETER ( battery_heat_to_air  = 2 )
      INTEGER battery_heat_to_nowhere   ! Heat loss from battery lost (e.g. to dilution air).
      PARAMETER ( battery_heat_to_nowhere = 3 )
      INTEGER PCU_heat_to_room  ! Heat loss from PCU added to room air.
      PARAMETER ( PCU_heat_to_room = 1 )
      INTEGER PCU_heat_to_air   ! Heat loss from PCU added to FCPM air intake.
      PARAMETER ( PCU_heat_to_air  = 2 )
      INTEGER PCU_heat_to_nowhere   ! Heat loss from PCU lost (e.g. to dilution air).
      PARAMETER ( PCU_heat_to_nowhere = 3 )

C Dilution air system/HRV.
      INTEGER Dilution_yes           ! There is a dilution air system.
      PARAMETER ( Dilution_yes = 1 )
      INTEGER Dilution_no            ! There is no dilution air system.
      PARAMETER ( Dilution_no = 0 )
      INTEGER SOFC_HRV_yes           ! There is an HRV.
      PARAMETER ( SOFC_HRV_yes = 1 )
      INTEGER SOFC_HRV_no            ! There is not an HRV.
      PARAMETER ( SOFC_HRV_no = 0 )

C Stack cooling system
      INTEGER s_cool_yes
           PARAMETER (s_cool_yes = 1) ! There is a stack cooling system
      INTEGER s_cool_no
           PARAMETER (s_cool_no = 0)  ! There is no stack cooling system
      INTEGER no_flow
           PARAMETER (no_flow = 0)    ! no cold water flow into cogen-HX
      INTEGER no_bypass  
           PARAMETER (no_bypass = 1)     ! cogen-HX is in its normal state
      INTEGER bypass  
           PARAMETER (bypass = 2)     ! some of the water in cogen-HX flows through the bypass      


C Named constants for gases flowing into and our of control volumes.
      INTEGER N2
      INTEGER O2
      INTEGER H2O
      INTEGER Ar
      INTEGER CO2
      INTEGER H2
      INTEGER CH4
      INTEGER C2H6
      INTEGER C3H8
      INTEGER C4H10
      INTEGER C5H12
      INTEGER C6H14
      INTEGER CH3OH
      INTEGER C2H5OH
      INTEGER liq_H2O
      PARAMETER ( N2      =  1 )
      PARAMETER ( O2      =  2 )
      PARAMETER ( H2O     =  3 )
      PARAMETER ( Ar      =  4 )
      PARAMETER ( CO2     =  5 )
      PARAMETER ( H2      =  6 )
      PARAMETER ( CH4     =  7 )
      PARAMETER ( C2H6    =  8 )
      PARAMETER ( C3H8    =  9 )
      PARAMETER ( C4H10   = 10 )
      PARAMETER ( C5H12   = 11 )
      PARAMETER ( C6H14   = 12 )
      PARAMETER ( CH3OH   = 13 )
      PARAMETER ( C2H5OH  = 14 )
      PARAMETER ( liq_H2O = 15  )

C Named constants indicating property to calculate using Shomate coefficients.
      INTEGER deltaH
      PARAMETER ( deltaH     = 1 )
      INTEGER LHV_i
      PARAMETER ( LHV_i      = 2 )
      INTEGER heatcap
      PARAMETER ( heatcap    = 3 )
      INTEGER totalH
      PARAMETER ( totalH     = 4 )

C Molar masses (Data from webbook.nist.gov).
      REAL molar_mass_H2O
      PARAMETER ( molar_mass_H2O = 18.02 )
