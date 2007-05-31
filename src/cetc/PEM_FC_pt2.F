C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre 
C Natural Resources Canada, Government of Canada
C 2004. Please Contact Ian Beausoliel-Morrison for details 
C concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C===================== CURVES ==========================================
C     Created by: Alex Ferguson
C     Created on: March 2001
C     Copyright:  Alex Ferguson 2001 / CETC 2003.
C----------------------------------------------------------------------
C     ABSTRACT: 
C     This subroutine takes the hot and cold process stream compsoitions
C     determined by the subroutine PEM_Compositions and builds two arrays containing
C     composition, temperature and enthalpy data for each heating and 
C     cooling process stream. The two arrays are suitable for use in 
C     pinch analysis.
C 
C     This subroutine's principle output is two arrays:
C        * TH_cold : cold composite curve array
C        * TH_hot  : hot composite curve array
C     Both TH_cold and TH_hot are two dimensional arrays containing composite 
C     temperature/enthalpy profiles.  The arrays are structured similarly to 
C     Composition_cold and Composition_hot:
C   
C     TH_cold(composition/property,location),  TH_cold(composition/property,location)
C
C     Where: 
C         * The first rank contains 10 rows, describing the state of 
C           the heating / cooling stream:
C              
C               1. temperature
C               2. water molar flow rate
C               3. fuel molar flow rate
C               4. CO molar flow rate
C               5. CO2 molar flow rate
C               6. H2 molar flow rate
C               7. O2 molar flow rate
C               8. N2 molar flow rate
C               9. Integer index
C              10. Enthalpy
C     
C         * The second rank is used to differentaite between nodes on the 
C           heating and cooling curves.  Unlike the Composition_cold and Composition_hot arrays, 
C           the nodes in TH_cold and TH_hot do not represent process flows 
C           through the fuel cell system. Instead, they represent temperature 
C           sequences.
C     
C           For TH_cold:
C              * The first row in rank 2 in TH_cold represents the lowest 
C                temperature on the cold composite curve
C              * the last row in rank 2 in TH_cold represnts the highest
C                temperature on the cold composite curve
C             
C           Conversely, for TH_hot:
C              * The first row in rank 2  in TH_hot represents the 
C                highest temperature on the hot composite curve
C              * the last row in rank 2 on TH_hot represnts the lowest
C                temperature on the hot composite curve
C
C     An overview of this procedure is provided in Section B.1, in 
C     Appendix B, in Ferguson 2003.
C----------------------------------------------------------------------
C     REFERENCES
C
C     FERGUSON, A 2003. Fuel cell modelling for building cogeneration 
C        applications.  MASc Thesis, Dalhousie University
C
C     VAN WYLEN, G. et al. 1994. Fundamentals of classical t
C     thermodynamics. Wiley and Sons.
C     
C----------------------------------------------------------------------
C     REVISIONS:
C     SEPT 2003: Code was heavily revised to comply with CETC coding
C     standard and improve general readability.  
C======================================================================
      SUBROUTINE PEM_CURVES(
     &     Composition_cold,
     &     Composition_hot,
     &     TH_cold,
     &     TH_hot,
     &     AUX_inlet_data,
     &     cathode_air,
     &     Q_Cell,
     &     burner_air,
     &     H2_flow,
     &     AUX_outlet_data,
     &     dH_reformer,
     &     IPCOMP
     &     ) 
      IMPLICIT NONE
C----------------------------------------------------------------------------
C     Definitions of ESP-r common block variables
C----------------------------------------------------------------------------
      INCLUDE 'plant.h'
C----------------------------------------------------------------------------
C     ESP-r common block req'd for fuel cell description
C----------------------------------------------------------------------------
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
C----------------------------------------------------------------------------
C     Declaration of ESP-r common blocks, functions
C----------------------------------------------------------------------------
      REAL ADATA, BDATA
C----------------------------------------------------------------------------
C     Declare initiaization functon
C----------------------------------------------------------------------------
      REAL PEM_FCinit
C----------------------------------------------------------------------------
C     Declaration of passed variables
C----------------------------------------------------------------------------
      REAL Q_Cell               ! heat recovery from fuel cell (kWt, input)
      REAL dH_reformer          ! total enthalpy change occuring within the 
                                !    reformer (kW) (input)
      REAL H2_flow              ! hydrogen molar flow rate (kmol/s) (input)
      REAL burner_air(3)        ! Array containing conditions at  (input)
                                !    burner combustion air inlet (input)
      REAL AUX_inlet_data(7), AUX_outlet_data(7) ! Arrays containing inlet and exhaust 
                                !     compositions of auxiliary burner (input)
      REAL Composition_cold(10,30), Composition_hot(10,30)
                                ! Arrays containing stream compositions 
                                !    on a per/mol H2 basis for all 
                                !    cold (Composition_cold) and hot (Composition_hot) 
                                !    streams. Populated by subroutine
                                !    PEM_compositions (input)
      REAL TH_cold(10,30), TH_hot(10,30)
                                ! Arrays containing cold and hot 
                                !    temperature-enthalpy profiles 
                                !    for hot and cold streams  (output)
      REAL cathode_air(10,2)    ! Array containing air composition (input)
                                !    at cathode inlet and outlet (input)
      INTEGER IPCOMP            ! Pointer to component in plant network (input)
C----------------------------------------------------------------------------
C     Declaration of local variables 
C----------------------------------------------------------------------------
      REAL Comp_Inlet(7), Comp_Outlet(7) ! Holding arrays used to pass stream compositions
                                ! into subroutines 
      REAL H2_burner_air(3)     ! Array holding burner combustion air flow rate
      INTEGER i, j, k           ! Counters
      INTEGER Reactor_Mode      ! flag indicating if isothermal/adiabatic reactors
                                ! will be used
      REAL Pressure_Bars        ! holding variable used to store pressure in bars 
                                ! (bars) <- req'd for SATEMP/OCTANE/METHL routines
      REAL T_air_supply         ! Air supply temperture (K)
      REAL T_air_compressor     ! Air temperature @ outlet of compressor (K)
      REAL T_Water_Sat          ! saturation temperature of water (K)
      REAL T_CH4_supply         ! Natural gas supply temperature (K)
      REAL Hfg_FUEL             ! Enthalpy of vaporization of fuel (kJ/kmol)
      REAL T_fuel_sat           ! liquid fuel saturation temperature (K)
      REAL T_fuel_supply        ! fuel initial temperature (K)
      REAL T_H2O_supply         ! initial temperature of water (K)
      REAL T_start, T_end       ! stream start/end temperatures (K)
      LOGICAL DONE              ! Flag used to track status of loops
      REAL ATM_pressure         ! Atmospheric pressure (kPa)      
      REAL SWAP                 ! temporary variable used to swap temperatures 
      REAL T_PROX_i, T_PROX_o   ! PROX reactor inlet & outlet temperatures (K)
      REAL T_HTWS_i, T_HTWS_o   ! HTWS reactor inlet & outlet temperatures (K)
      REAL T_LTWS_i, T_LTWS_o   ! LTWS reactor inlet & outlet temperatures (K)
      REAL dH_streams           ! Change in enthalpy streams undergo as they 
                                ! are heated/cooled (kWt)
      REAL T_cathode_air        ! Temperature at which cathode air enters 
      REAL dH_HTWS,dH_LTWS,dH_PROX ! Enthalpy changes occuring in HTWS, LTWS & PROX reactors.
      REAL T_burner_o           ! Burner exhaust temperature (K)
C---- FUEL CELL DESCRIPTION (inputs)
      integer Fuel_Type         ! fuel type (1= methane, 3 = methanol)
      real reformer_heat_loss   ! reformer thermal loss coefficent (kW/deg k)
      real HTWS_heat_loss       ! HTWS thermal loss coefficent (kW/deg k)      
      real LTWS_heat_loss       ! LTWs thermal loss coefficent (kW/deg k)
      real PROX_heat_loss       ! PROX thermal loss coefficent (kW/deg k)
      real H2_burner_efficiency ! H2 burner efficiency (0->1)
      real Air_compressor_eff   ! air compressor isentropic efficiency  (0->1)
      real NG_compressor_efficiency ! mehtane compressor isentropic efficiency  (0->1)
      real T_reformer           ! Reformer operating temperature (K)
      real T_cell               ! Stack operating temperature (K)
      real T_surroundings       ! Ambient temperature
      real Pressure_oper        ! Compressor outlet pressures (kPa)
      REAL DUMMY_ARRAY(3)       ! Dummy array
      REAL DUMMY_SCALAR         ! Dummy scalar
C---- Indexes used to keep track of arrays
      INTEGER iTh2oI, iTFi,iTfsA,iTfsB,iTh2osA,iTh2osB,iTo2I,iTairL,
     &     iTproI,iTltwI,iThtwI,iTref,iThtwO,iTltwO,iTproO,iTcellA,
     &     iTcellB,iTcellC,iTburO,iTcellD 
C-------------------------------------------------------------------------------
C     Read Fuel cell description from ADATA array
C-------------------------------------------------------------------------------
C     Fuel Type
      Fuel_Type=int(ADATA(IPCOMP, 1))
C     Reformer thermal loss coefficient (kW/degC)
      reformer_heat_loss =  ADATA(IPCOMP,7)/1000.
C     HTWS thermal loss coefficient (kW/degC)
      HTWS_heat_loss =  ADATA(IPCOMP,9)/1000.
C     LTWS thermal loss coefficient (kW/degC)
      LTWS_heat_loss =  ADATA(IPCOMP,10)/1000.
C     PROX thermal loss coefficient (kW/degC)
      PROX_heat_loss =  ADATA(IPCOMP,11)/1000.
C     H2 Burner efficiency 
      H2_burner_efficiency = ADATA(IPCOMP,12)
C     Air Compressor isothermal efficiency
      Air_compressor_eff = ADATA(IPCOMP,14)
C     NG Compressor isothermal efficiency
      NG_compressor_efficiency = ADATA(IPCOMP,15)
C     Reformer operating temperature (K)
      T_reformer =  ADATA(IPCOMP, 32)+PEM_FCinit(0)
C     HTWS inlet temperature (K)
      T_HTWS_i = ADATA(IPCOMP, 33)+PEM_FCinit(0)
C     LTWS inlet temperature (K)
      T_LTWS_i = ADATA(IPCOMP, 34)+PEM_FCinit(0)
C     PROX inlet temperature  (K)
      T_PROX_i = ADATA(IPCOMP, 35)+PEM_FCinit(0)
C     Cell stack operating temperature (K)
      T_cell = ADATA(IPCOMP, 36)+PEM_FCinit(0)
C     Ambient temperature (K)
      T_surroundings = ADATA(IPCOMP, 39) + PEM_FCinit(0)
C     Compressor outlet pressures (kPa)
      Pressure_oper = ADATA(IPCOMP,40) 
C-----------------------------------------------------------------------------
C     The flag Reactor_Mode is used to indicate which reactor model should be used 
C     to characterize the HTWS, LTWS & PROX reactors:
C     Reactor_Mode may be set to specify if the HTWS, LTWS and PROX reactors
C     behave quasi-adiabatically (eg, reactor outlet temperature is higher
C     then he inlet temperature), or isothermally (inlet temperatures and
C     outlet temperatures are approxamently the same:
C     
C              Reactor_Mode = 1 : Isothermal reactors(default)
C                          Heat produced by the reactor is rejected
C                          isothermally, and must be removed through
C                          heat recovery or mechanical cooling
C              Reactor_Mode = 2 : quasi-adiabatical reactors
C                          Heat produced by the reactor raises the
C                          reactor product temperature. This is a
C                          slightly more optimistic model, because heat
C                          may be recovered at higher temperatures
C
C     See Section 3.2.1, Ferguson 2003.
C------------------------------------------------------------------------------
      Reactor_Mode = 1
C-------------------------------------------------------------------------------
C     Arrays TH_cold and TH_hot must be initialized to zero!
C-------------------------------------------------------------------------------
      do i = 1, 10
         do j = 1, 30
            TH_cold(i,j) = 0.
            TH_hot(i,j) = 0.
         enddo
      enddo
C-------------------------------------------------------------------------------
C     Initialize dummy array and scalar values to zero 
C-------------------------------------------------------------------------------
      DUMMY_SCALAR = 0.
      DUMMY_ARRAY(1) = 0.
      DUMMY_ARRAY(2) = 0.
      DUMMY_ARRAY(3) = 0.
C-------------------------------------------------------------------------------
C     Collect atmospheric pressure from initialization function
C-------------------------------------------------------------------------------
      ATM_pressure = PEM_FCinit(2)         ! (kPa)
C-------------------------------------------------------------------------------
C     Determine the saturation temperature of water @ operating pressure using 
C     ESP-r function SATTEMP. First, the operating pressure must be
C     converted from kPa to bar...
C-------------------------------------------------------------------------------
      Pressure_Bars = Pressure_oper/100. ! convert kPa to bar
      CALL SATTEMP(Pressure_Bars, T_Water_Sat) ! <- returns water saturation temp (oC)
C-------------------------------------------------------------------------------
C     Convert water saturation temperature from oC to K using constant saved
C     in PEM_FCinit initialization function
C-------------------------------------------------------------------------------      
      T_Water_Sat = T_Water_Sat + PEM_FCinit(0) ! water saturation temperature in K
C--------------------------------------------------------------------------------
C     Collect water supply temperature from initialization function
C--------------------------------------------------------------------------------
      T_H2O_supply = PEM_FCinit(3)  ! liquid water supply temperature (K)
C--------------------------------------------------------------------------------
C     Check fuel type: 
C       - If liquid fuel is used, determine saturation temperature.
C       - If gas fuel is used, determine temperature @ outlet of compressor
C--------------------------------------------------------------------------------
      IF(Fuel_Type.EQ.2) THEN
C--------------------------------------------------------------------------------
C     |  Fuel is octane. Call subroutine OCTANE to determine fuel saturation 
C     |  temp
C--------------------------------------------------------------------------------
         CALL PEM_OCTANE(Pressure_Bars, ! Operating pressure (input, bar)
     &        T_fuel_sat,       ! Saturation temperature (output, K)
     &        Hfg_fuel)         ! Enthalpy of vaporization (Dummy variable)
C--------------------------------------------------------------------------------
C     |  Collect fuel initial temperature from FC initialization function
C--------------------------------------------------------------------------------
         T_fuel_supply = PEM_FCinit(6) ! (K)
      ELSEIF(Fuel_Type.Eq.3) THEN
C--------------------------------------------------------------------------------
C     |  Fuel is Methanol. Call subroutine PEM_Methanol_P to determine fuel saturation 
C     |  temp.
C--------------------------------------------------------------------------------
         CALL PEM_Methanol_P(Pressure_Bars, ! Operating pressure (input, bar)
     &        T_fuel_sat,       ! Saturation temperature (output, K)
     &        Hfg_fuel)         ! Enthalpy of vaporization (Dummy variable)
C--------------------------------------------------------------------------------
C     |  Collect fuel initial temperature from FC initialization function
C--------------------------------------------------------------------------------
         T_fuel_supply = PEM_FCinit(6) ! fuel supply temperature (K)
      ELSEIF(Fuel_Type.EQ.1) THEN
C--------------------------------------------------------------------------------
C     |  Fuel is methane/natural gas. Collect supply temperature from PEM_FCinit  
C     |  initialization function.
C--------------------------------------------------------------------------------
         T_CH4_supply = PEM_FCinit(5) ! (K) 
C--------------------------------------------------------------------------------
C     |  Fill holding variable Comp_Inlet with composition of methane, and call 
C     |  subroutine COMPRE to characterize the compressor. Note, since the 
C     |  actual system operating point has yet to be determined, an arbitrary
C     |  fuel flow rate is assumed. Since the compressor isentropic efficiency 
C     |  is assumed to be constant, the outlet temperature from the compressor 
C     |  will be the same for all operating points. The actual work requirement 
C     |  of the compressor will be determined later, when the system fuel flow 
C     |  is known.
C--------------------------------------------------------------------------------
         Comp_Inlet(1) = 0.            
         Comp_Inlet(2) = 1.     ! Arbitrary fuel flow.
         Comp_Inlet(3) = 0.
         Comp_Inlet(4) = 0.
         Comp_Inlet(5) = 0.
         Comp_Inlet(6) = 0.
         Comp_Inlet(7) = 0.
         CALL PEM_COMPRESSOR(Comp_Inlet, ! Stream composition (input)
     &        ATM_pressure,     ! Inlet pressure (kPa, input)
     &        Pressure_oper,    ! Outlet pressure (kPa, input)
     &        T_CH4_supply,     ! Supply temperature @ compressor inlet (K, input)
     &        T_fuel_supply,    ! Exhaust temperature @ compressor outlet (K, output)
     &        NG_compressor_efficiency, ! Compressor isentropic efficiency (input)
     &        DUMMY_SCALAR)     ! Compressor work requirement (kWm, dummy variable))
C-------------------------------------------------------------------------------
C     |  Methane does not undergo phase changes. Set "saturation temperature"
C     |  to compressor outlet temperature, and set enthalpy of vaporization to
C     |  zero. This effectively eliminates the fuel vaporization segment from 
C     |  the heating composite curve.
C-------------------------------------------------------------------------------
         T_fuel_sat = T_fuel_supply ! K
         Hfg_fuel = 0.
      ENDIF
C-------------------------------------------------------------------------------
C     Collect air supply temperature from initialization function
C-------------------------------------------------------------------------------
      T_air_supply = PEM_FCinit(4)  ! K
C--------------------------------------------------------------------------------
C     Fill holding variable Comp_Inlet with composition of air, and call 
C     subroutine COMPRE to characterize the compressor. Note, since the 
C     actual system operating point has yet to be determined, an arbitrary
C     air flow rate is assumed. Since the compressor isentropic efficiency 
C     is assumed to be constant, the outlet temperature from the compressor 
C     will be the same for all operating points. The actual work requirement 
C     of the compressor will be determined later, when the system air flow 
C     is known.
C--------------------------------------------------------------------------------
      Comp_Inlet(1) = 0.
      Comp_Inlet(2) = 0.
      Comp_Inlet(3) = 0.
      Comp_Inlet(4) = 0.
      Comp_Inlet(5) = 0.
      Comp_Inlet(6) = 1.        ! O2 composition
      Comp_Inlet(7) = Comp_Inlet(6) * PEM_FCinit(13) ! N2 composition
      CALL PEM_COMPRESSOR(Comp_Inlet,   ! Stream composition (input)
     &     ATM_pressure,        ! Inlet pressure (kPa, input)
     &     Pressure_oper,       ! Outlet pressure (kPa, input)
     &     T_air_supply,        ! Supply temperature @ compressor inlet (K, input)
     &     T_air_compressor,    ! Exhaust temperature @ compressor outlet (K, output)
     &     Air_compressor_eff,  ! Compressor isentropic efficiency (input)
     &     DUMMY_SCALAR)        ! Compressor work requirement (kWm, dummy variable)
C--------------------------------------------------------------------------------
C     Determine if the compressor outlet temperature is above or below the 
C     fuel cell operating temperature:
C       - If the compressor outlet temperature is below the cell operating temp.,
C         air must be heated before entering the fuel cell.
C       - If the compressor outlet temperature is above the cell operating temp.,
C         air must be cooled before entering the fuel cell.
C--------------------------------------------------------------------------------
      IF (T_cell.GE.T_air_compressor) THEN             
C--------------------------------------------------------------------------------
C     |  Air will be heated.  Assign upper segement of cathode air profile
C     |  to the cell temperature (this makes the cathode air feed a real
C     |  profile on the cold composite curve)
C--------------------------------------------------------------------------------
         T_cathode_air = T_cell
      ELSE
C--------------------------------------------------------------------------------
C     |  Air will be cooled.  Assign upper segement of cathode air profile
C     |  to air inlet temperature (this makes the cathode air feed a single
C     |  point on the cold composite curve with no heating requirement)
C--------------------------------------------------------------------------------
         T_cathode_air = T_air_compressor
      ENDIF
C--------------------------------------------------------------------------------
C     Set named constants for cold profile index
C--------------------------------------------------------------------------------
      iTh2oI  = 1
      iTFi    = 2
      iTfsA   = 3
      iTfsB   = 4
      iTh2osA = 5
      iTh2osB = 6
      iTo2I   = 7
      iTairL  = 8
      iTproI  = 9
      iTltwI  = 10
      iThtwI  = 11
      iTref   = 12
C--------------------------------------------------------------------------------
C     Assign cold profile index to cold temperature-enthalpy profile 
C     array TH_cold. 
C--------------------------------------------------------------------------------
      TH_cold(9, 1) = REAL(iTh2oI)  + 0.1
      TH_cold(9, 2) = REAL(iTFi)    + 0.1
      TH_cold(9, 3) = REAL(iTfsA)   + 0.1
      TH_cold(9, 4) = REAL(iTfsB)   + 0.1
      TH_cold(9, 5) = REAL(iTh2osA) + 0.1
      TH_cold(9, 6) = REAL(iTh2osB) + 0.1
      TH_cold(9, 7) = REAL(iTo2I)   + 0.1
      TH_cold(9, 8) = REAL(iTairL)  + 0.1  
      TH_cold(9, 9) = REAL(iTproI)  + 0.1
      TH_cold(9, 10) = REAL(iTltwI) + 0.1
      TH_cold(9, 11) = REAL(iThtwI) + 0.1
      TH_cold(9, 12) = REAL(iTref)  + 0.1
C--------------------------------------------------------------------------------
C     Assign cold profile temperatures to cold temperature-enthalpy profile 
C     array TH_cold. 
C--------------------------------------------------------------------------------
      TH_cold(1, 1) = T_H2O_supply
      TH_cold(1, 2) = T_fuel_supply
      TH_cold(1, 3) = T_fuel_sat
      TH_cold(1, 4) = T_fuel_sat
      TH_cold(1, 5) = T_water_sat
      TH_cold(1, 6) = T_water_sat
      TH_cold(1, 7) = T_air_compressor
      TH_cold(1, 8) = T_cathode_air  
      TH_cold(1, 9) = T_PROX_i
      TH_cold(1, 10) = T_LTWS_i
      TH_cold(1, 11) = T_HTWS_i
      TH_cold(1, 12) = T_reformer
C--------------------------------------------------------------------------------
C     Sort array TH_cold by temperature (data in rank 1, row 1)
C--------------------------------------------------------------------------------
      DONE = .FALSE.        
      DO WHILE (.NOT. DONE) 
         DONE = .TRUE.     
         DO i = 1, 11           ! <-11 segments on the cold profile
C---------- If i temp and i+1 temp are not in the right order, 
C---------- switch them
            IF (TH_cold(1, i).GT.TH_cold(1, i + 1)) THEN
               SWAP = TH_cold(1, i)
               TH_cold(1, i) = TH_cold(1, i + 1)
               TH_cold(1, i + 1) = SWAP
               SWAP = TH_cold(9, i)
               TH_cold(9, i) = TH_cold(9, i + 1)
               TH_cold(9, i + 1) = SWAP
               DONE = .FALSE.
            ENDIF
         ENDDO
      ENDDO
C--------------------------------------------------------------------------------
C     This next series of loops searches through the array TH_cold for 
C     temperatures that correspond to to heating stream start-end temperatures.
C     If a temperature corresponds to a heating steam starting point, the molar
C     flow rates of that streams constituants (H2O,fuel,CO,CO2,H2,O2,N2), as 
C     described in the array Composition_cold, are added to the appropriate locations in 
C     TH_cold. Similarly, If the temperature correspods to a heating stream end
C     point, the molar flow rates of that streams constituants, as described
C     in the array Composition_cold, are subtracted to the appropriate locations in TH_cold.
C
C     The actual molar flow rates of all constituants are calculated by 
C     multiplying the per-mol-H2 composition calculated by the subroutine 
C     PEM_Compositions (and stored in array Composition_cold) by the reqyired hydrogen flow rate 
C     H2_flow.
C
C     The flag DONE is used to prevent a stream's constituent flow rates from 
C     being added/subtracted twice if multiple streams share the same inlet or
C     outlet temperatures.
C--------------------------------------------------------------------------------
      DONE = .FALSE.                
      DO i = 1, 12
         IF ((INT(TH_cold(9, i)).EQ.iTh2oI).AND.(.NOT. DONE)) THEN
C--------------------------------------------------------------------------------
C     |     Temperature corresponds to water inlet temperature; add total water 
C     |     rate to current & subsequent locations
C--------------------------------------------------------------------------------
            DO j = i, 12
               TH_cold(2, j) = Composition_cold(2,1) * H2_flow
            ENDDO
            DONE = .TRUE.    
         ENDIF
      ENDDO
      DONE = .FALSE.
      DO  i = 1, 12
         IF ((INT(TH_cold(9, i)).EQ.iTfI).AND.(.NOT. DONE)) THEN
C--------------------------------------------------------------------------------
C     |     Temperature corresponds to fuel inlet temperature, therefore add fuel
C     |     flow at current and subsequent locations.  Note: the suplementary
C     |     fuel consumption required by the aux burner is also added (AUX_inlet_data(2)).
C--------------------------------------------------------------------------------
            DO  j = i, 12
               TH_cold(3, j) = Composition_cold(3,4)
     &              * H2_flow + AUX_inlet_data(2)
            ENDDO
            DONE = .TRUE.
         ENDIF
      ENDDO
      DONE = .FALSE.
      DO i = 1, 12  
         IF ((INT(TH_cold(9, i)).EQ.iTo2I).AND.(.NOT. DONE)) THEN
C--------------------------------------------------------------------------------
C     |     Temperature corresponds to air inlet temperature, therefore add air
C     |     flow (oxygen and nitrogen) at current and subsequent locations. Note: 
C     |     the suplementary air consumption required by the aux burner is also 
C     |     added ( AUX_inlet_data(6) & AUX_inlet_data(7) ).
C--------------------------------------------------------------------------------
            DO  j = i, 12     
               TH_cold(7, j) = Composition_cold(7,5)
     &              * H2_flow + AUX_inlet_data(6)
     &              + cathode_air(7,1) * H2_flow     
               TH_cold(8, j) = Composition_cold(8,5)
     &              * H2_flow + AUX_inlet_data(7)
     &              +  cathode_air(8,1) * H2_flow    
            ENDDO
            DONE = .TRUE.
         ENDIF
      ENDDO
      DONE = .FALSE.
      DO i = 1, 12  
         IF ((INT(TH_cold(9, i)).EQ.iTairL ).AND.(.NOT. DONE)) THEN
C--------------------------------------------------------------------------------
C     |     Temperature corresponds to point at which cathode air exits heating
C     |     streams (T_cathode_air). Subtract cathode feed (oxygen and nitrogen) from
C     |     current and subsequent locations
C--------------------------------------------------------------------------------
            DO  j = i, 12     
               TH_cold(7, j) = TH_cold(7, j)- cathode_air(7,1) * H2_flow     
               TH_cold(8, j) = TH_cold(8, j)- cathode_air(8,1) * H2_flow     
            ENDDO
            DONE = .TRUE.
         ENDIF
      ENDDO
      DONE = .FALSE.
      DO i = 1, 12
         IF ((INT(TH_cold(9, i)).EQ.iTproI).AND.(.NOT. DONE)) THEN
C--------------------------------------------------------------------------------
C     |     Temperature corresponds to prox inlet temp: remove air flow (oxygen
C     |     and nytrogen) from current and subsequent locations.
C--------------------------------------------------------------------------------
            DO j = i, 12
               TH_cold(7, j) = TH_cold(7, j) 
     &              - Composition_cold(7,5)*H2_flow
               TH_cold(8, j) = TH_cold(8, j) 
     &              - Composition_cold(8,5)*H2_flow
            ENDDO
            DONE = .TRUE.
         ENDIF
      ENDDO
      DONE = .FALSE.
      DO i = 1, 12
         IF ((INT(TH_cold(9, i)).EQ.iTltwI).AND.(.NOT. DONE)) THEN
C--------------------------------------------------------------------------------
C     |     Temperature corresponds to LTWS inlet: take fraction a_ltws out of water
C     |     flow at current and subsequent locations
C--------------------------------------------------------------------------------
            DO j = i, 12
               TH_cold(2, j) = Composition_cold(2,7)*H2_flow
            ENDDO
            DONE = .TRUE.
         ENDIF
      ENDDO

      DONE = .FALSE.
      DO i = 1, 12
         IF ((INT(TH_cold(9, i)).EQ.iThtwI).AND.(.NOT. DONE)) THEN
C--------------------------------------------------------------------------------
C     |     Temperature corresponds to HTWS outlet: take fraction a_htws out of water
C     |     flow at current and subsequent locations
C--------------------------------------------------------------------------------
            DO j = i, 12
               TH_cold(2, j) = Composition_cold(2,8)*H2_flow
            ENDDO
            DONE = .TRUE.
         ENDIF
      ENDDO
C-------------------------------------------------------------------------------
C     Calculate the enthalpy change that occurs between sequential nodes 
C     on the heating curve (ie TH_cold(X,i) and TH_cold(X,i+1))
C-------------------------------------------------------------------------------
      DO i = 1, 11              !<- 11 segments in TH_cold
C-------------------------------------------------------------------------------
C     |  Save composition of heating segment in holding array Comp_Inlet
C-------------------------------------------------------------------------------
         DO j = 1, 7            !<- 7 species
            Comp_Inlet(j) = TH_cold(j+1, i)
         ENDDO
C-------------------------------------------------------------------------------
C     |  Assign start and end temperatures to holding variables T_start and T_end
C-------------------------------------------------------------------------------
         T_start = TH_cold(1,i) ! start temperature (K)
         T_end = TH_cold(1,i+1) ! end temperature (K)
C-------------------------------------------------------------------------------
C     |  Call subroutine PEM_del_H to determine the change in enthalpy of the
C     |  heating stream between temperatures T_start and T_end
C-------------------------------------------------------------------------------
         CALL PEM_del_H(Comp_Inlet,  ! stream composition (kmol/s, input)
     &        T_start,          ! stream start temperature (K, input)
     &        T_end,            ! stream target temperature (K, input)
     &        T_water_sat,      ! Water saturation temperature (K, input)
     &        dH_streams,       ! Total change in enthalpy (output, kW)
     &        T_fuel_sat,       ! Fuel saturation temp (K, input)
     &        Hfg_fuel,         ! Fuel vaporization enthalpy (K, input)
     &        Fuel_Type,        ! fuel type (input)
     &        .TRUE.)           ! flag indicating if vaporization will occur
C------------------------------------------------------------------------------
C     |  set the enthalpy for the current point on the heating curve as the
C     |  sum of the enthalpy of the previous point and dH_streams
C------------------------------------------------------------------------------
         IF (i.EQ.1) TH_cold(10, i) = 0. ! Start curve from zero 
         TH_cold(10, i+1) = dH_streams + TH_cold(10, i) !(kW)
      ENDDO
C------------------------------------------------------------------------------
C     Calculate the enthalpy change that occurs inside the reformer.  Fill
C     holding arrays Comp_Inlet and Comp_Outlet with reformer inlet and outlet 
C     comositions, and call subroutine dHref to evaluate reformer enthalpy 
C     change. 
C------------------------------------------------------------------------------
C---- Reformer inlet
      Comp_Inlet(1) = Composition_cold(2,9)*H2_flow ! H2O
      Comp_Inlet(2) = Composition_cold(3,9)*H2_flow ! FUEL
      Comp_Inlet(3) = Composition_cold(4,9)*H2_flow ! CO
      Comp_Inlet(4) = Composition_cold(5,9)*H2_flow ! CO2
      Comp_Inlet(5) = Composition_cold(6,9)*H2_flow ! H2
      Comp_Inlet(6) = Composition_cold(7,9)*H2_flow ! O2
      Comp_Inlet(7) = Composition_cold(8,9)*H2_flow ! N2
C---- Reformer outler
      Comp_Outlet(1) = Composition_hot(2,1)*H2_flow ! H2O
      Comp_Outlet(2) = Composition_hot(3,1)*H2_flow ! FUEL
      Comp_Outlet(3) = Composition_hot(4,1)*H2_flow ! CO
      Comp_Outlet(4) = Composition_hot(5,1)*H2_flow ! CO2
      Comp_Outlet(5) = Composition_hot(6,1)*H2_flow ! H2 
      Comp_Outlet(6) = Composition_hot(7,1)*H2_flow ! O2
      Comp_Outlet(7) = Composition_hot(8,1)*H2_flow ! N2
C---- Call dHref to characterize reformer
      CALL PEM_dH_reformer(Comp_Inlet,    ! inlet composition (kmol/s, input)
     &     Comp_Outlet,         ! outlet composition (kmol/s, input)
     &     T_reformer,          ! reformer operating temp. (K, input)
     &     dH_reformer,         ! reformer enthalpy change (kW, input)
     &     reformer_heat_loss,  ! reformer thermal loss coefficient (kW/oC,input)
     &     T_surroundings,      ! ambient temperature (K, input)
     &     Fuel_Type)           ! fuel type (input)
C------------------------------------------------------------------------------
C     Add the reformer enthalpy change to the fuel cell cold side composite
C     cuve in row 13. The temperature of the last (rightmost) location on
C     the cold composite curve is set to T_ref, because the heat addition
C     to the reformer is assumed to be isothemal.
C------------------------------------------------------------------------------
      TH_cold(1, 13) = T_reformer                    ! 13th segment!
      TH_cold(10, 13) = TH_cold(10, 12) + dH_reformer ! 13th segment!

C==============================================================================
C     The fuel processor heating streams have been fully characterized. Now 
C     do the same for the cooling streams.
C==============================================================================
C------------------------------------------------------------------------------
C     Before the cooling stream composite temperature-enthalpy profiles can 
C     be built, the outlet temperatures of the HTWS, LTWS & PROX reactors
C     must be determined.
C
C     1. Determine HTWS outlet temperature. Fill holding arrays Comp_Inlet and 
C        Comp_Outlet with HTWS inlet and outlet compositions
C------------------------------------------------------------------------------
      DO i = 1, 7
         Comp_Inlet(i) = Composition_hot(i+1,2) * H2_flow ! HTWS inlet
         Comp_Outlet(i) = Composition_hot(i+1,3) * H2_flow ! HTWS outlet
      ENDDO
C------------------------------------------------------------------------------
C     Determine which reactor mode was specificed:
C       - Reactor_Mode = 1: isothermal reactor model, Tin=Tout
C       - Reactor_Mode = 2: quasi-adiabatic reactor model, dH ~ 0.
C------------------------------------------------------------------------------
      IF (Reactor_Mode.EQ.1) then
C------------------------------------------------------------------------------
C     |  Set outlet temperature = inlet temperature + 0.05. This ensures that
C     |  the inlet and outlet temperatures can be correctly identified in 
C     |  later.
C------------------------------------------------------------------------------
         T_HTWS_o = T_HTWS_i + 0.05 
C------------------------------------------------------------------------------
C     |  Call subroutine dHref to determine the heat thats transferred from 
C     |  the isothermal reactor to cooling streams 
C------------------------------------------------------------------------------
         CALL PEM_dH_reformer(Comp_Inlet, ! inlet composition (kmol/s, input)
     &        Comp_Outlet,      ! outlet composition (kmol/s, input)
     &        T_HTWS_i,         ! HTWS operating temp. (K, input)
     &        dH_HTWS,          ! HTWS enthalpy change (kW, output)
     &        HTWS_heat_loss,   ! HTWS thermal loss coefficient (kW/oC,input)
     &        T_surroundings,   ! ambient temperature (K, input)
     &        Fuel_Type)        ! fuel type (input)
      ELSE
C-----------------------------------------------------------------------------
C     |  Adiabatic reactor model: Hin = Hout.  Call TempO to determine
C     |  the reacotr outlet temperature
C-----------------------------------------------------------------------------
         CALL PEM_Temp_O(Comp_Inlet, ! inlet composition (kmol/s, input)
     &        Comp_Outlet,      ! outlet composition (kmol/s, input)
     &        T_HTWS_i,         ! HTWS inlet temp. (K, input)
     &        T_HTWS_o,         ! HTWS outlet temp (K, output)
     &        DUMMY_ARRAY,      ! Dummy array (= no combustion air)
     &        DUMMY_SCALAR,     ! Dummy scalar 
     &        HTWS_heat_loss,   ! HTWS thermal loss coefficient (kW/oC,input)
     &        T_surroundings,   ! ambient temperature (K, input)
     &        Fuel_Type,        ! fuel type (input)
     &        2)                ! Flag indicating caluclation type (use
                                ! heat loss coefficient to determine burner
                                ! efficiency)
      ENDIF
C-----------------------------------------------------------------------------
C     2. Determine LTWS outlet temperature. Fill holding arrays Comp_Inlet and 
C        Comp_Outlet with LTWS inlet and outlet compositions
C------------------------------------------------------------------------------
      DO i = 1, 7
         Comp_Inlet(i) = Composition_hot(i+1,4) * H2_flow ! LTWS Inlet
         Comp_Outlet(i) = Composition_hot(i+1,5) * H2_flow ! LTWS outlet
      ENDDO
C------------------------------------------------------------------------------
C     Determine which reactor mode was specificed:
C       - Reactor_Mode = 1: isothermal reactor model, Tin=Tout
C       - Reactor_Mode = 2: quasi-adiabatic reactor model, dH ~ 0.
C------------------------------------------------------------------------------
      IF (Reactor_Mode.EQ.1) THEN
C------------------------------------------------------------------------------
C     |  Set outlet temperature = inlet temperature + 0.05. This ensures that
C     |  the inlet and outlet temperatures can be correctly identified in 
C     |  later.
C------------------------------------------------------------------------------
         T_LTWS_o = T_LTWS_i + 0.05
C------------------------------------------------------------------------------
C     |  Call subroutine dHref to determine the heat thats transferred from 
C     |  the isothermal reactor to cooling streams 
C------------------------------------------------------------------------------
         CALL PEM_dH_reformer(Comp_Inlet,     ! inlet composition (kmol/s, input)
     &        Comp_Outlet,           ! outlet composition (kmol/s, input)
     &        T_LTWS_i,           ! LTWS operating temp. (K, input)
     &        dH_LTWS,           ! LTWS enthalpy change (kW, output)
     &        LTWS_heat_loss,   ! LTWS thermal loss coefficient (kW/oC,input)
     &        T_surroundings,           ! ambient temperature (K, input)
     &        Fuel_Type)             ! fuel type (input)
      ELSE
C-----------------------------------------------------------------------------
C     |  Adiabatic reactor model: Hin = Hout.  Call TempO to determine
C     |  the reacotr outlet temperature
C-----------------------------------------------------------------------------
         CALL PEM_Temp_O(Comp_Inlet, ! inlet composition (kmol/s, input)
     &        Comp_Outlet,      ! outlet composition (kmol/s, input)
     &        T_LTWS_i,         ! HTWS inlet temp. (K, input)
     &        T_LTWS_o,         ! HTWS outlet temp (K, output)
     &        DUMMY_ARRAY,      ! Dummy array (= no combustion air)
     &        DUMMY_SCALAR,     ! Dummy scalar 
     &        LTWS_heat_loss,   ! HTWS thermal loss coefficient (kW/oC,input)
     &        T_surroundings,   ! ambient temperature (K, input)
     &        Fuel_Type,        ! fuel type (input)
     &        2)                ! Flag indicating caluclation type (use
                                ! heat loss coefficient to determine burner
                                ! efficiency)
      ENDIF
C-----------------------------------------------------------------------------
C     3. Determine PROX outlet temperature. Fill holding arrays Comp_Inlet and 
C        Comp_Outlet with PROX inlet and outlet compositions
C------------------------------------------------------------------------------
      DO i = 1, 7
         Comp_Inlet(i) = Composition_hot(i+1,6) * H2_flow
         Comp_Outlet(i) = Composition_hot(i+1,7) * H2_flow
      ENDDO
C------------------------------------------------------------------------------
C     Determine which reactor mode was specificed:
C       - Reactor_Mode = 1: isothermal reactor model, Tin=Tout
C       - Reactor_Mode = 2: quasi-adiabatic reactor model, dH ~ 0.
C------------------------------------------------------------------------------
      IF (Reactor_Mode.EQ.1) then
C------------------------------------------------------------------------------
C     |  Set outlet temperature = inlet temperature + 0.05. This ensures that
C     |  the inlet and outlet temperatures can be correctly identified in 
C     |  later.
C------------------------------------------------------------------------------
         T_PROX_o = T_PROX_i + 0.05
C------------------------------------------------------------------------------
C     |  Call subroutine dHref to determine the heat thats transferred from 
C     |  the isothermal reactor to cooling streams 
C------------------------------------------------------------------------------
         CALL PEM_dH_reformer(Comp_Inlet, ! inlet composition (kmol/s, input)
     &        Comp_Outlet,      ! outlet composition (kmol/s, input)
     &        T_PROX_i,         ! PROX operating temp. (K, input)
     &        dH_PROX,          ! PROX enthalpy change (kW, output)
     &        PROX_heat_loss,   ! PROX thermal loss coefficient (kW/oC,input)
     &        T_surroundings,   ! ambient temperature (K, input)
     &        Fuel_Type)        ! fuel type (input)
      ELSE
C-----------------------------------------------------------------------------
C     |  Adiabatic reactor model: Hin = Hout.  Call TempO to determine
C     |  the reacotr outlet temperature
C-----------------------------------------------------------------------------
         CALL PEM_Temp_O(Comp_Inlet, ! inlet composition (kmol/s, input)
     &        Comp_Outlet,      ! outlet composition (kmol/s, input)
     &        T_PROX_i,         ! PROX inlet temp. (K, input)
     &        T_PROX_o,         ! PROX outlet temp (K, output)
     &        DUMMY_ARRAY,      ! Dummy array (= no combustion air)
     &        DUMMY_SCALAR,     ! Dummy scalar 
     &        PROX_heat_loss,   ! PROX thermal loss coefficient (kW/oC,input)
     &        T_surroundings,   ! ambient temperature (K, input)
     &        Fuel_Type,        ! fuel type (input)
     &        2)                ! Flag indicating caluclation type (use
                                ! heat loss coefficient to determine burner
                                ! efficiency)
      ENDIF
C-----------------------------------------------------------------------------
C     4. Determine burner outlet temperature. Fill holding arrays Comp_Inlet and 
C        Comp_Outlet with burner inlet and outlet compositions
C------------------------------------------------------------------------------
      DO I = 1, 7
         Comp_Inlet(i) = Composition_hot(i+1, 10)*H2_flow ! burner inlet (not including combustion air)
         Comp_Outlet(i) = Composition_hot(i+1, 11)*H2_flow ! burner exhaust 
      ENDDO
C------------------------------------------------------------------------------
C     The array H2_burner_air holds data about the burner combustion air.  
C     It's identical to burner_air, except that its values are scaled by the actual 
C     hydrogen molar flow rate to reflect the actual air flow rate through the system
C------------------------------------------------------------------------------
      H2_burner_air(1) = T_air_compressor ! Temperature (K)
      H2_burner_air(2) = burner_air(2)*H2_flow ! O2 flow rate (kmol/s)
      H2_burner_air(3) = burner_air(3)*H2_flow ! N2 flow rate (kmol/s)
C------------------------------------------------------------------------------
C     Call subroutine BurT to determine burner outlet temperature
C------------------------------------------------------------------------------
      CALL PEM_Temp_O(Comp_Inlet,    ! inlet composition (kmol/s, input)
     &     Comp_Outlet,         ! outlet composition (kmol/s, input)
     &     T_cell,              ! Burner inlet temp. (K, input)
     &     T_burner_o,          ! Burner outlet temp (K, output)
     &     H2_burner_air,       ! Combustiona air compositioon
     &     H2_burner_efficiency, ! Burner efficiency 
     &     DUMMY_SCALAR,        ! dummy arguement
     &     DUMMY_SCALAR,        ! dummy arguement 
     &     Fuel_Type,           ! fuel type (input)
     &     1)                   ! Flag indicating caluclation type (use
                                ! burner efficiency to characterize heat loss)
C------------------------------------------------------------------------------
C     Set named constants for TH_hot index
C------------------------------------------------------------------------------
      iTref    = 1              ! Reformer temperature (K)
      iThtwI   = 2              ! HTWS inlet temperature (K)
      iThtwO   = 3              ! HTWS outlet temperature (K)
      iTltwI   = 4              ! LTWS inlet temperature (K)
      iTltwO   = 5              ! LTWS outlet temperature (K)
      iTproI   = 6              ! PROX inlet temperature (K)
      iTproO   = 7              ! PROX outlet temperature (K)
      iTairL   = 8              ! Cathode air feed temperature (K)
      iTcellA  = 9              ! Anode inlet temperature (K)
      iTcellB  = 10             ! Anode exhaust temperature (K)
      iTcellC  = 11             ! Burner fuel inlet temperature (K)
      iTburO   = 12             ! Burner exhaust temperature (K)
      iTcellD  = 13             ! Burner rejection temperature
C------------------------------------------------------------------------------
C     Assign index to TH_hot
C------------------------------------------------------------------------------
      TH_hot(9,1)  = iTref       ! Reformer temperature (K)
      TH_hot(9,2)  = iThtwI      ! HTWS inlet temperature (K)
      TH_hot(9,3)  = iThtwO      ! HTWS outlet temperature (K)
      TH_hot(9,4)  = iTltwI      ! LTWS inlet temperature (K)
      TH_hot(9,5)  = iTltwO      ! LTWS outlet temperature (K)
      TH_hot(9,6)  = iTproI      ! PROX inlet temperature (K)
      TH_hot(9,7)  = iTproO      ! PROX outlet temperature (K)
      TH_hot(9,8)  = iTairL      ! Cathode air feed temperature (K)
      TH_hot(9,9)  = iTcellA     ! Anode inlet temperature (K)
      TH_hot(9,10) = iTcellB     ! Anode exhaust temperature (K)
      TH_hot(9,11) = iTcellC     ! Burner fuel inlet temperature (K)
      TH_hot(9,12) = iTburO      ! Burner exhaust temperature (K)
      TH_hot(9,13) = iTcellD     ! Burner exhaust rejection temperature (K)
C------------------------------------------------------------------------------
C     Assign calculated hot profile temperatures to hot temperature-enthalpy 
C     profile array TH_hot. 
C------------------------------------------------------------------------------
      TH_hot(1,1)  = T_reformer ! Reformer temperature (K)
      TH_hot(1,2)  = T_HTWS_i   ! HTWS inlet temperature (K)
      TH_hot(1,3)  = T_HTWS_o   ! HTWS outlet temperature (K)
      TH_hot(1,4)  = T_LTWS_i   ! LTWS inlet temperature (K)
      TH_hot(1,5)  = T_LTWS_o   ! LTWS outlet temperature (K)
      TH_hot(1,6)  = T_PROX_i   ! PROX inlet temperature (K)
      TH_hot(1,7)  = T_PROX_o   ! PROX outlet temperature (K)
      TH_hot(1,8)  = T_cathode_air ! Cathode air feed temperature (K)
      TH_hot(1,9)  = T_cell     ! Anode inlet temperature (K)
      TH_hot(1,10) = T_cell     ! Anode exhaust temperature (K)
      TH_hot(1,11) = T_cell     ! Burner fuel inlet temperature (K)
      TH_hot(1,12) = T_burner_o ! Burner exhaust temperature (K)
C------------------------------------------------------------------------------
C     Set burner exhaust rejection temperature (the temperature at which the 
C     burner exhaust is rejected to the atmosphere).  At this point in the code, we 
C     assume that the burner exhaust leaves at the cell temperature.  It will be
C     adjusted later to reflect the optimal exhaust temperature.
C------------------------------------------------------------------------------
      TH_hot(1,13) = T_cell      ! (K)
C--------------------------------------------------------------------------------
C     Sort array TH_hot by temperature (data in rank 1, row 1)
C--------------------------------------------------------------------------------
      DONE = .FALSE.      
      DO WHILE (.NOT. DONE)
         DONE  = .TRUE. 
         DO  i = 1, 12  
C---------- If i temp and i+1 temp are not in the right order, 
C --------- switch them
            IF (TH_hot(1, i).LT.TH_hot(1, i + 1)) THEN
               SWAP = TH_hot(1, i)
               TH_hot(1, i) = TH_hot(1, i + 1)
               TH_hot(1, i + 1) = SWAP
               SWAP = TH_hot(9, i)
               TH_hot(9, i) = TH_hot(9, i + 1)
               TH_hot(9, i + 1) = SWAP
               DONE = .FALSE.
            ENDIF
         ENDDO
      ENDDO
C--------------------------------------------------------------------------------
C     This next series of loops searches through the array TH_hot for 
C     temperatures that correspond to to cooling stream start-end temperatures.
C     If a temperature corresponds to a cooling steam starting point, the molar
C     flow rates of that streams constituants (H2O,fuel,CO,CO2,H2,O2,N2), as 
C     described in the array Composition_hot, are added to the appropriate locations in 
C     TH_hot. Similarly, If the temperature correspods to a cooling stream end
C     point, the molar flow rates of that stream's constituants, as described
C     in the array Composition_hot, are subtracted to the appropriate locations in TH_hot.
C
C     The actual molar flow rates of all constituants are calculated by 
C     multiplying the per-mol-H2 composition calculated by the subroutine 
C     PEM_Compositions (and stored in array Composition_hot) by the required hydrogen flow rate 
C     H2_flow.
C
C     The flag DONE is used to prevent a stream's constituent flow rates from 
C     being added/subtracted twice if multiple streams share the same inlet or
C     outlet temperatures.
C--------------------------------------------------------------------------------
      DONE = .FALSE.       
      DO  i = 1, 13             !<-13 nodes
         IF ((INT(TH_hot(9, i)).EQ.iTburO).AND.(.NOT. DONE)) THEN
C--------------------------------------------------------------------------------
C     |     Temperature corresponds to burner outlet temperature, therefore add
C     |     burner outlet Composition to current and subsequent locations 
C--------------------------------------------------------------------------------
            DO   j = i, 12 
               DO  k = 2, 8
                  TH_hot(k, j) = TH_hot(k, j) + 
     &                 Composition_hot(k, 11) * H2_flow
               ENDDO
            ENDDO
            DONE = .TRUE.  
         ENDIF
      ENDDO
      DONE = .FALSE.
      DO   i = 1, 13
         IF ((int(TH_hot(9, i)).EQ.iTref).AND.(.NOT. DONE)) THEN
C--------------------------------------------------------------------------------
C     |     Temperature corresponds to reformer exhaust temperature, therefore add
C     |     reformer exhaust to current and subsequent locations
C--------------------------------------------------------------------------------
            DO   j = i, 12      
               DO  k = 2, 8
                  TH_hot(k, j) = TH_hot(k, j) +
     &                 Composition_hot(k, 1)*H2_flow +
     &                 AUX_outlet_data(k-1)
               ENDDO
            ENDDO
            DONE = .TRUE.

         ENDIF
      ENDDO
      DONE = .FALSE.
      DO   i = 1, 13
         IF ((int(TH_hot(9, i)).EQ.iThtwI).AND.(.NOT. DONE)) THEN
C--------------------------------------------------------------------------------
C     |     Temperature corresponds to HTWS inlet temperature, therefore subtract
C     |     reformer exhaust from current and subsequent locations.
C--------------------------------------------------------------------------------
            DO  j = i, 12      
               DO  k = 2, 8
                  TH_hot(k,j)  = TH_hot(k,j) -
     &                 Composition_hot(k,1) * H2_flow
               ENDDO
            ENDDO
            DONE = .TRUE.
         ENDIF
      ENDDO
      DONE = .FALSE.
      DO   i = 1, 13
         IF ((int(TH_hot(9,i)).EQ.iThtwO).AND.(.NOT. DONE)) THEN
C--------------------------------------------------------------------------------
C     |     Temperature corresponds to HTWS outlet temperature, therefore add HTWS
C     |     exhaust to current and subsequent locations.
C--------------------------------------------------------------------------------
            DO  j = i, 12     
               DO  k = 2, 8
                  TH_hot(k, j) = TH_hot(k,j) + 
     &                 Composition_hot(k,3) * H2_flow
               ENDDO
            ENDDO
            DONE = .TRUE.
         ENDIF
      ENDDO
      DONE = .FALSE.
      DO   i = 1, 13
         IF ((int(TH_hot(9, i)).EQ.iTltwi).AND.(.NOT. DONE)) THEN
C--------------------------------------------------------------------------------
C     |     Temerature corresponds to LTWS inlet temperature, therefore subtract
C     |     HTWS exhaust from current and subsequent locations
C--------------------------------------------------------------------------------
            DO  j = i, 12 
               DO  k = 2, 8
                  TH_hot(k, j) = TH_hot(k, j) - 
     &                 Composition_hot(k, 3) * H2_flow
               ENDDO
            ENDDO
            DONE = .TRUE.
         ENDIF
      ENDDO
      DONE = .FALSE.
      DO   i = 1, 13
         IF ((int(TH_hot(9, i)).EQ.iTltwO).AND.(.NOT. DONE)) THEN
C--------------------------------------------------------------------------------
C     |     Temperature corresponds to LTWS exhaust temperature, therefore add
C     |     LTWS exhaust from to current and subsequent locations.
C--------------------------------------------------------------------------------
            DO  j = i, 12     
               DO  k = 2, 8
                  TH_hot(k, j) = TH_hot(k, j) + 
     &                 Composition_hot(k, 5) * H2_flow
               ENDDO
            ENDDO
            DONE = .TRUE.
         ENDIF
      ENDDO
      DONE = .FALSE.
      DO   i = 1, 13
         IF ((int(TH_hot(9, i)).EQ.iTproI).AND.(.NOT. DONE)) THEN
C--------------------------------------------------------------------------------
C     |     Temperature corresponds to PROX inlet temperature, therefore subtract
C     |     LTWS exhaust from current and subsequent locations
C--------------------------------------------------------------------------------
            DO j = i, 12      
               DO  k = 2, 8
                  TH_hot(k, j) = TH_hot(k, j) - 
     &                 Composition_hot(k, 5) *H2_flow
               ENDDO
            ENDDO
            DONE = .TRUE.
         ENDIF
      ENDDO
      DONE = .FALSE.
      DO   i = 1, 13
         IF ((int(TH_hot(9, i)).EQ.iTproO).AND.(.NOT. DONE)) THEN
C--------------------------------------------------------------------------------
C     |     Temperature corresponds to PROX exhaust temperature, therefore add
C     |     PROX exhaust to current and subsequent locations
C--------------------------------------------------------------------------------
            DO j = i, 12      
               DO  k = 2, 8
                  TH_hot(k, j) = TH_hot(k, j) + 
     &                 Composition_hot(k, 7) * H2_flow
               ENDDO
            ENDDO
            DONE = .TRUE.
         ENDIF
      ENDDO
      DONE = .FALSE.
      DO i = 1, 13 
         IF ((int(TH_hot(9, i)).EQ.iTairL).AND.(.NOT. DONE)) THEN
C--------------------------------------------------------------------------------
C     |     Temperature corresponds to cathode feed from cold side, therefore
C     |     add cathode feed to current and subsequent locations
C--------------------------------------------------------------------------------
            DO j = i, 12  
               TH_hot(7, j) = TH_hot(7, j) + cathode_air(7, 1) *H2_flow
               TH_hot(8, j) = TH_hot(8, j) + cathode_air(8, 1) *H2_flow
            ENDDO
            DONE = .TRUE.
         ENDIF
      ENDDO
      DONE = .FALSE.
      DO   i = 1, 13
         IF ((int(TH_hot(9, i)).EQ.iTcellD).AND.(.NOT. DONE)) THEN
C--------------------------------------------------------------------------------
C     |     Temperature corresponds to Exhaust gas rejection temperature,
C     |     therefore subtract exhaust gas flow from current and subsequent 
C     |     locations
C--------------------------------------------------------------------------------
            DO j = i, 12    
               DO  k = 2, 8
                  TH_hot(k, j) = TH_hot(k, j) - 
     &                 Composition_hot(k, 11) *H2_flow
     &                 - AUX_outlet_data(k-1)
               ENDDO
            ENDDO
            DONE = .TRUE.
         ENDIF
      ENDDO
C-------------------------------------------------------------------------------
C     Calculate the enthalpy change that occurs between sequential nodes 
C     on the heating curve (ie TH_hot(X,i) and TH_hot(X,i+1)). For now, set the
C     enthalpy of the first point (corresponding to the highest temperature) 
C     on the curve to zero
C     
C-------------------------------------------------------------------------------
      TH_hot(10, 1) = 0.  
      DO i = 1, 11             
C-------------------------------------------------------------------------------
C     |  Save composition of cooling segment in holding array Comp_Inlet
C-------------------------------------------------------------------------------
         DO j = 1, 7
            Comp_Inlet(j) = TH_hot(j+1, i)
         ENDDO
C-------------------------------------------------------------------------------
C     |  Assign start and end temperatures to holding variables T_start and T_end
C-------------------------------------------------------------------------------
         T_start= TH_hot(1, i )
         T_end = TH_hot(1, i+1)
C-------------------------------------------------------------------------------
C     |  Check which reactor model is being used. 
C-------------------------------------------------------------------------------
         IF (Reactor_Mode.EQ.1) THEN
C-------------------------------------------------------------------------------
C     |     Isothermal reactors.  If node i equals reactor inlet
C     |     temperature and node i+1 equals reactor outlet temperature,
C     |     this segment corresponds to the isothermal heat rejection
C     |     segment occuring in the reactor (Recall that in the isothermal
C     |     reactor model the inlet and outlet temperatures are set to be 
C     |     within 0.05 K of each other.)
C-------------------------------------------------------------------------------
            IF ((INT(TH_hot(9,i)).EQ.iTltwO).AND.
     &           (INT(TH_hot(9,i+1)).EQ.iTltwI)) THEN
C-------------------------------------------------------------------------------
C     |        This is the ltws reactor segment.  Add rejected heat
C     |        to the temperature-enthalpy profile
C-------------------------------------------------------------------------------
               TH_hot(10,i+1) = TH_hot(10, i) + dH_LTWS
            ELSEIF ((INT(TH_hot(9,i)).EQ.iThtwO).AND.
     &           (INT(TH_hot(9,i+1)).EQ.iThtwI)) THEN
C-------------------------------------------------------------------------------
C     |        This is the htws reactor segment.  Add rejected heat
C     |        to the temperature-enthalpy profile
C-------------------------------------------------------------------------------
               TH_hot(10,i+1) = TH_hot(10, i) + dH_HTWS
            ELSEIF ((INT(TH_hot(9,i)).EQ.iTproO).AND.
     &           (INT(TH_hot(9,i+1)).EQ.iTproI)) THEN
C-------------------------------------------------------------------------------
C     |        This is the prox reactor segment.  Add rejected heat
C     |        to the temperature-enthalpy profile
C-------------------------------------------------------------------------------
               TH_hot(10,i+1) = TH_hot(10, i) + dH_PROX
            ELSE
C-------------------------------------------------------------------------------
C     |        All other segments on the profile correspond to sensible cooling.
C     |        Call subroutine PEM_del_H to determine the change in enthalpy of the 
C     |        cooling stream between temperatures T_start and T_end
C-------------------------------------------------------------------------------
               CALL PEM_del_H(Comp_Inlet,
     &              T_start,
     &              T_end,
     &              T_water_sat,
     &              dH_streams,
     &              T_fuel_sat,
     &              Hfg_fuel,
     &              Fuel_Type,
     &              .FALSE.
     &              )
               TH_hot(10, i+1) = dH_streams + TH_hot(10, i)
            ENDIF
         ELSE
C-------------------------------------------------------------------------------
C     |     Adiabatic reactor model.  All segments exhibit sensible cooling
C     |     behavior.  Call subroutine PEM_del_H to determine the change in enthalpy 
C     |     of the cooling stream between temperatures T_startand T_end
C-------------------------------------------------------------------------------
            CALL PEM_del_H(Comp_Inlet,
     &           T_start,
     &           T_end,
     &           T_water_sat,
     &           dH_streams,
     &           T_fuel_sat,
     &           Hfg_fuel,
     &           Fuel_Type,
     &           .FALSE.
     &           )
            TH_hot(10, i+1) = dH_streams + TH_hot(10, i)
         ENDIF
      ENDDO
C-------------------------------------------------------------------------------
C     Add the last segment to hot temperature enthalpy profile. The temperature 
C     of node 13 is equal to fuel cell temperature. The enthalpy change between
C     nodes 12 and 13 is equal to the heat released by the fuel cell.
C-------------------------------------------------------------------------------
      TH_hot(1,13) = T_cell      ! (K)
      TH_hot(10, 13) = TH_hot(10, 12)-Q_Cell ! (kW)
C-------------------------------------------------------------------------------
C     Shift hot profile to start from zero: TH_hot(10,13) = 0.
C-------------------------------------------------------------------------------
      DO  i = 1, 13             !<- 13 nodes
         TH_hot(10, i) = TH_hot(10, i) - TH_hot(10, 13)
      ENDDO

      RETURN
      END

C===================== TempO  ===================================================
C     Created by: Alex Ferguson
C     Created on: March 2001
C     Copyright:  Alex Ferguson 2001 / CETC 2003.
C-------------------------------------------------------------------------------
C     ABSTRACT: 
C     This routine calcuates the exhaust temperature of a "Quasi-adiabatic"
C     burner, in which the only heat transferred to/from the reactor is 
C     exchanged with the surroundings. This subroutine permits the combustion
C     air to be described separately from the reforate inlet data, and allows
C     the inefficiencies in the reactor to be described using an overall 
C     reactor efficiency.
C
C-------------------------------------------------------------------------------
C     REFERENCES
C
C     FERGUSON, A 2003. Fuel cell modelling for building cogeneration 
C        applications.  MASc Thesis, Dalhousie University
C
C     VAN WYLEN, G. et al. 1994. Fundamentals of classical t
C        thermodynamics. Wiley and Sons.
C     
C-------------------------------------------------------------------------------
C     REVISIONS:
C     Sept 2003: This routine was combined with another subroutine, 
C                BurT, which provided similar functionality.
C                Code was heavily revised to comply with CETC coding
C                standard and improve general readability.  
C-------------------------------------------------------------------------------
      SUBROUTINE PEM_Temp_O(
     &     Comp_Inlet, 
     &     Comp_Outlet, 
     &     T_inlet, 
     &     T_outlet,
     &     burner_air, 
     &     H2_burner_efficiency,
     &     Q_Loss_Coeff,
     &     T_surroundings,
     &     Fuel_Type,mode)
      IMPLICIT NONE
C--------------------------------------------------------------------------------
C     Declare passed variables
C--------------------------------------------------------------------------------
      REAL Comp_Inlet(7), Comp_Outlet(7) ! Burner inlet & outlet composition (input)
      REAL burner_air(3)        ! Burner combustion air data (input)
      REAL T_inlet              ! Burner inlet temperature (K,input)
      REAL T_outlet             ! Burner outlet temperature (K,output)
      REAL H2_burner_efficiency ! Burner efficiency (input)
      REAL Q_Loss_Coeff         ! Burner heat loss coefficient (kW/oC, input)
      REAL T_surroundings                ! Ambient temperature (K, input)
      INTEGER Fuel_Type         ! Fuel type (input)
      INTEGER mode              ! Flag indicating type of calculation to be used 
                                ! to detemine burner efficiency (input)
C--------------------------------------------------------------------------------
C     Declare initiaization function
C--------------------------------------------------------------------------------
      REAL PEM_FCinit
C--------------------------------------------------------------------------------
C     Declare enthalpy functions - These functions return the specific
C     enthalpy of a species when passed its temperature.
C--------------------------------------------------------------------------------
      REAL PEMf_HSTEAM, PEMF_HCO, PEMF_HCO2, PEMf_hN2, 
     &     PEMF_HO2, PEMF_HCH4, PEMF_HOCTG, PEMf_HMETHg, PEMf_hH2
C--------------------------------------------------------------------------------
C     Local variables 
C--------------------------------------------------------------------------------
      REAL REF_temperature      ! Reference temperature (K)
      REAL Hf_H2Ov              ! water vapor enthalpy of formation (kJ/kmol)
      REAL Hf_H2Ol              ! liquid water enthalpy of formation (kJ/kmol)
      REAL Hf_CO                ! Carbon Monoxide enthalpy of formation (kJ/kmol)
      REAL Hf_CO2               ! Carbon Dioxide enthalpy of formation (kJ/kmol)
      REAL Hf_H2                ! Hydrogen enthalpy of formation (kJ/kmol)
      REAL Hf_O2                ! Oxygen enthalpy of formation (kJ/kmol)
      REAL Hf_N2                ! Nitrogen enthalpy of formation (kJ/kmol)
      REAL Hf_CH4               ! Methane enthalpy of formation (kJ/kmol)
      REAL Hf_OCT               ! Octane enthalpy of formation (kJ/kmol)
      REAL Hf_Meth              ! Methanol enthalpy of formation (kJ/kmol)      
      REAL HTH2O, HTCO, HTCO2, HTN2Ca, HTO2Ca, HTF, HTH2, HTO2, HTN2, 
     &     HTN2Cb, HTO2Cb       ! enthalpies of species (kW)
      REAL Ht_In                ! Total enthalpy at inlet of burner (kW)
      REAL HTout                ! Total enthalpy at outlet of burner (kW)
      REAL HF_out               ! Sum of product enthalpy of formation (kW)
      REAL Hs_Out               ! Sum of product sensible heating enthalpy change (kW)
      REAL Error                ! Error in outlet enthalpy estimate (kW)
      REAL Tu, Tl               ! Upper & lower limits of temperature root search (K)
      REAL Q_Loss               ! Heat transfer from reactor and surroundings (kW)
      REAL Hfo_CO, Hfo_CO2, Hfo_N2, Hfo_O2, Hfo_H2, Hfo_F, Hfo_H2O 
                                !enthalpies of formation (kW) at reactor outlet
      REAL HS_CO, HS_CO2, HS_N2, HS_O2, HS_H2, HS_F, HS_H2O !sensible enthalpy change (kW)
      LOGICAL DONE              ! Flag indicating convergence
      INTEGER ii                ! Counter
      REAL T_tolerance          ! Tolerance for temperature iteration (K)
C-------------------------------------------------------------------------------
C     Collect enthalpies of formation from PEM_FCinit function (kJ/kmol @ 298 K)
C-------------------------------------------------------------------------------
      Hf_H2Ov = PEM_FCinit(16)       ! Water vapor
      Hf_H2Ol = PEM_FCinit(17)       ! liquid water
      Hf_CO   = PEM_FCinit(18)       ! Carbon Monoxide
      Hf_CO2  = PEM_FCinit(19)       ! Carbon Dioxide
      Hf_H2   = PEM_FCinit(20)       ! Hydrogen
      Hf_O2   = PEM_FCinit(21)       ! Oxygen
      Hf_N2   = PEM_FCinit(22)       ! Nitrogen
      Hf_CH4  = PEM_FCinit(23)       ! Methane
      Hf_OCT  = PEM_FCinit(24)       ! Octane
      Hf_Meth = PEM_FCinit(25)       ! Methanol
C-------------------------------------------------------------------------------
C     Tolerance for temperature iteration 
C-------------------------------------------------------------------------------
      T_tolerance = PEM_FCinit(40)
C-------------------------------------------------------------------------------
C     Collect reference temperature from PEM_FCinit initialization function
C-------------------------------------------------------------------------------
      REF_temperature = PEM_FCinit(14)          ! K
C-------------------------------------------------------------------------------
C     Determine total enthalpy of reactants: Ref = 298K
C     This section calculates the enthalpy for each species i 
C     using the formula:
C       Enthalpy = #mols * (E_formation + H_funct(T_in) - Hfunct(T_ref) )
C     
C       Where: 
C         * # mols = number of mols of species i
C         * E_formation = enthalpy of formation of species i
C         * H_funct = enthalpy function for species i ( located at
C                     the end of this file
C         * T_in = inlet temperature
C         * T_ref = reference temperature (298K)
C
C     1. Water 
C-------------------------------------------------------------------------------
      HTH2O = Comp_Inlet(1) * 
     &     (Hf_H2Ov + PEMf_HSTEAM(T_inlet) 
     &     - PEMf_HSTEAM(REF_temperature)) ! (kW)
C-------------------------------------------------------------------------------
C     2. Fuel 
C-------------------------------------------------------------------------------
      IF (Fuel_Type.EQ.1) THEN
C------- METHANE:
         HTF = Comp_Inlet(2) 
     &        * (Hf_CH4 + PEMf_HCH4(T_inlet) 
     &        - PEMf_HCH4(REF_temperature)) ! (kW)
      ELSEIF (Fuel_Type.EQ.2) THEN
C------- OCTANE:
         HTF = Comp_Inlet(2) 
     &        * (Hf_OCT + PEMf_HOCTg(T_inlet) 
     &        - PEMf_HOCTg(REF_temperature)) ! (kW)  
      ELSEIF (Fuel_Type.EQ.3) THEN
C------- METHYNOL
         HTF = Comp_Inlet(2) 
     &        * (Hf_Meth + PEMf_HMETHg(T_inlet) 
     &        - PEMf_HMETHg(REF_temperature)) ! (kW) 
      ENDIF
C-------------------------------------------------------------------------------
C     3. Carbon monoxide
C-------------------------------------------------------------------------------
      HTCO = Comp_Inlet(3) 
     &     * (Hf_CO + PEMf_HCO(T_inlet) - PEMf_HCO(REF_temperature)) ! (kW)
C-------------------------------------------------------------------------------
C     4. Carbon dioxide
C-------------------------------------------------------------------------------
      HTCO2 = Comp_Inlet(4) 
     &     * (Hf_CO2 + PEMf_HCO2(T_inlet) - PEMf_HCO2(REF_temperature)) ! (kW)
C-------------------------------------------------------------------------------
C     5. Hydrogen
C-------------------------------------------------------------------------------
      HTH2 = Comp_Inlet(5) 
     &     * (Hf_H2 + PEMf_HH2(T_inlet) - PEMf_HH2(REF_temperature)) ! (kW)
C-------------------------------------------------------------------------------
C     6a. Oxygen - reformate stream 
C-------------------------------------------------------------------------------
      HTO2Ca = Comp_Inlet(6) 
     &     * (Hf_O2 + PEMf_HO2(T_inlet) - PEMf_HO2(REF_temperature)) ! (kW)
C-------------------------------------------------------------------------------
C     6b. Oxygen - combustion air stream 
C-------------------------------------------------------------------------------
      HTO2Cb = burner_air(2) * (Hf_O2 + PEMf_HO2(burner_air(1)) 
     &     - PEMf_HO2(REF_temperature)) ! (kW)
C-------------------------------------------------------------------------------
C     7a. Nitrogen - reformate stream 
C-------------------------------------------------------------------------------
      HTN2Ca = Comp_Inlet(7) 
     &     * (Hf_N2 + PEMf_hN2(T_inlet) - PEMf_hN2(REF_temperature)) ! (kW)
C-------------------------------------------------------------------------------
C     7b. Nitrogen - combustion air stream
C-------------------------------------------------------------------------------
      HTN2Cb = burner_air(3) * (Hf_N2 + PEMf_hN2(burner_air(1)) 
     &     - PEMf_hN2(REF_temperature)) ! (kW)
C-------------------------------------------------------------------------------
C     Sum inlet enthalpies
C-------------------------------------------------------------------------------
      Ht_In=HTH2O +HTF +HTCO +HTCO2 +HTH2 
     &     +HTN2Ca +HTO2Ca +HTN2Cb +HTO2Cb ! (kW)
C-------------------------------------------------------------------------------
C     Estimate exhaust enthalpy by performing bisection root search:
C       1. Guess exhaust temperature
C       2. Calculate outlet enthalpy
C       3. Compare outlet enthalpy and inlet enthalpy
C       4. Adjust exhaust temperature and repeat
C
C     First check to see that the incomming enthalpy is positive and non-zero
C------------------------------------------------------------------------------
      If(ABS(Ht_In).GE.1E-10) THEN
C------------------------------------------------------------------------------
C     |  Set bounds for temperature root search. Note: lower bound set to
C     |  inlet temperature, since combustion is always exothermic
C------------------------------------------------------------------------------
         Tl = 0.                ! Lower limit (K)     
         Tu = 2800.             ! Upper limit (K) 
C------------------------------------------------------------------------------
C     |  Initialize flag for convergence, and start root search
C------------------------------------------------------------------------------         
         DONE = .FALSE.
         DO WHILE (.NOT. DONE )
C------------------------------------------------------------------------------
C     |     Set estimated outlet temperature to midpoint in upper and lower bounds
C------------------------------------------------------------------------------
            T_outlet = (Tu + Tl) / 2.
C------------------------------------------------------------------------------
C     |     Determine total enthalpy of products: Ref = 298K
C     |     This procedure is similar to that used for the reactants above, 
C     |     except it has been broken into two steps to accomidate use of
C     |     the efficiency term
C     |        Step A: Determine the enthalpy of formation of the products for
C     |                each speices i:
C     |                  H_f_prod_i. = # mols * h_f_species_i
C     |        Step B: Determine the enthalpy change that each species i 
C     |                undergos as it is heated from the reference temperature
C     |                to the estimated outlet temperature:
C     |                  dH_prod. = #mols *(H_funct(T_out )- H_funct(T_ref))
C     |
C     |     1. Water
C-------------------------------------------------------------------------------
            Hfo_H2O = Comp_Outlet(1) * (Hf_H2Ov) ! enthalpy of formation(kW)
            HS_H2O = Comp_Outlet(1) 
     &           * (PEMf_HSTEAM(T_outlet) 
     &           - PEMf_HSTEAM(REF_temperature)) ! sensible heat change (kW)
C-------------------------------------------------------------------------------
C     |     2. Fuel enthalpy of formation
C-------------------------------------------------------------------------------
            IF (Fuel_Type.EQ.1) THEN
C------------- METHANE:
               Hfo_F = Comp_Outlet(2) * (Hf_CH4) !  enthalpy of formation (kW)
               HS_F = Comp_Outlet(2) 
     &              * (PEMf_HCH4(T_outlet) - PEMf_HCH4(REF_temperature)) ! 
            ELSEIF (Fuel_Type.EQ.2) THEN
C------------- OCTANE:
               Hfo_F = Comp_Outlet(2) * (Hf_OCT) ! (kW)
               HS_F = Comp_Outlet(2) 
     &              * (PEMf_HOCTg(T_outlet) 
     &              - PEMf_HOCTg(REF_temperature))
            ELSEIF (Fuel_Type.EQ.3) THEN
C------------- METHANOL
               Hfo_F = Comp_Outlet(2) * (Hf_Meth) ! (kW)
               HS_F = Comp_Outlet(2) 
     &              * (PEMf_HMETHg(T_outlet) 
     &              - PEMf_HMETHg(REF_temperature))
            ENDIF
C-------------------------------------------------------------------------------
C     |     3. Carbon monoxide enthalpy of formation
C-------------------------------------------------------------------------------
            Hfo_CO = Comp_Outlet(3) * (Hf_CO) ! (kW)
            HS_CO = Comp_Outlet(3) 
     &           * (PEMf_HCO(T_outlet) - PEMf_HCO(REF_temperature)) ! (kW)
C-------------------------------------------------------------------------------
C     |     4. Carbon dioxide enthalpy of formation
C-------------------------------------------------------------------------------
            Hfo_CO2 = Comp_Outlet(4) * (Hf_CO2) ! (kW)
            HS_CO2 = Comp_Outlet(4) 
     &           * (PEMf_HCO2(T_outlet) - PEMf_HCO2(REF_temperature)) !(kW)
C-------------------------------------------------------------------------------
C     |     5. Hydrogen enthalpy of formation
C-------------------------------------------------------------------------------
            Hfo_H2 = Comp_Outlet(5) * (Hf_H2) ! (kW)
            HS_H2 = Comp_Outlet(5) 
     &           * (PEMf_HH2(T_outlet) - PEMf_HH2(REF_temperature)) ! (kW)
C-------------------------------------------------------------------------------
C     |     6. Oxygen enthalpy of formation
C-------------------------------------------------------------------------------
            Hfo_O2 = Comp_Outlet(6) * (Hf_O2) ! (kW)
            HS_O2 = Comp_Outlet(6) 
     &           * (PEMf_HO2(T_outlet) - PEMf_HO2(REF_temperature))  ! (kW)
C-------------------------------------------------------------------------------
C     |     7. Nitrogen enthalpy of formation
C-------------------------------------------------------------------------------
            Hfo_N2 = Comp_Outlet(7) * (Hf_N2) ! (kW)
            HS_N2 = Comp_Outlet(7) 
     &           * (PEMf_hN2(T_outlet) - PEMf_hN2(REF_temperature))  ! (kW)
C-------------------------------------------------------------------------------
C     |     Sum all enthalies of formation
C-------------------------------------------------------------------------------
            Hf_out= Hfo_H2O + Hfo_F + Hfo_CO 
     &           + Hfo_CO2 + Hfo_H2 + Hfo_N2 +Hfo_O2 ! (kW)
            HS_out= HS_H2O + HS_F + HS_CO 
     &           + HS_CO2+  HS_H2 + HS_N2 + HS_O2 ! (kW)
C-------------------------------------------------------------------------------
C     |     Evaluate the enthalpy conservation in the reformer using the 
C     |     specificed calculation type
C-------------------------------------------------------------------------------
            IF ( mode .EQ. 1 ) THEN
C-------------------------------------------------------------------------------
C     |        Efficiency calculation is to be used:
C     | 
C     |           H_out = efficiency * (Hin - H_formation_out)
C     | 
C-------------------------------------------------------------------------------
               Error = (Ht_In-Hf_out) - Hs_Out/H2_burner_efficiency ! (kW)
            ELSE 
C-------------------------------------------------------------------------------
C     |        Heat loss coefficient calculation is to be used:
C     |
C     |          Q_loss = Loss coefficient * (T out - T surroundings),
C     |
C     |          Hout = Hin - Qloss
C     |
C-------------------------------------------------------------------------------
               Q_Loss = Q_Loss_Coeff * ( T_outlet - T_surroundings ) ! (kW)
               Error  = Ht_In - ( Hf_out + Hs_Out ) - Q_Loss ! (kW)
            ENDIF
C-------------------------------------------------------------------------------
C     |     Adjust root search bounds
C-------------------------------------------------------------------------------
            If (Error .GT. 0.)  Tl = T_outlet
            If (Error .LE. 0.)  Tu = T_outlet
C------------------------------------------------------------------------------
C     |     Check for convergence 
C------------------------------------------------------------------------------
            IF ((Tu - Tl).LT.T_tolerance) DONE = .TRUE.
         ENDDO
      ELSE
C-------------------------------------------------------------------------------
C    |   Inlet enthalpies are zero - set outlet temperature to inlet temperature
C-------------------------------------------------------------------------------
         T_outlet = T_inlet ! (K)
      ENDIF
      RETURN
      END
C===================== PEM_H_SHIFT  ===================================================
C     Created by: Alex Ferguson
C     Created on: March 2001
C     Copyright:  Alex Ferguson 2001 / CETC 2003.
C-------------------------------------------------------------------------------
C     ABSTRACT: 
C     This routine is used to evaluate the enthalpy shift required to
C     preserve the minimum temperature difference (dTmin) between the hot
C     and cold composite curves.
C
C     For every node i on the cold-side composite curve, the enthapy on the
C     hot side composite curve is evaluated at temperature Ti + dTmin.  The
C     minimum shift required at node i is calculated as follows:
C
C              PEM_H_Shift = H_int_hot(Ti+dTmin)-H_int_cold(Ti)         (1)
C
C     Note: the minimum required shift may be negative, indicating that the
C     minimum temperature difference is preserved in the current curve
C     configuration.
C
C     Similarly, for every node i on the hot side comosite curve, the
C     enthalpy on the cold side composite curve is evaluated at temperature
C     Ti-dTmin.  The minimum shift required at node i is calculated as 
C     follows:
C
C               PEM_H_Shift = H_int_hot(Ti)-H_int_cold(Ti-dTmin)        (2)
C 
C     To preserve the minumum temperature difference between hot and
C     cold composite curves, the enthalpy of all nodes on the cold curve is
C     increased by the largest PEM_H_Shift amount (PEM_H_Shift)max, therefore shifting
C     the cold composite curve to the right.  
C
C     A complete discussion of this procedure is presented in Section B.2
C     in Appendix B, in Ferguson 2003.
C-------------------------------------------------------------------------------
C     REFERENCES
C
C     FERGUSON, A 2003. Fuel cell modelling for building cogeneration 
C        applications.  MASc Thesis, Dalhousie University
C
C     VAN WYLEN, G. et al. 1994. Fundamentals of classical t
C        thermodynamics. Wiley and Sons.
C     
C-------------------------------------------------------------------------------
C     REVISIONS:
C     Sept 2003: Code was heavily revised to comply with CETC coding
C                standard and improve general readability.  
C-------------------------------------------------------------------------------
      SUBROUTINE PEM_H_Shift(
     &     TH_cold, 
     &     TH_hot, 
     &     AUX_outlet_data,
     &     Pressure_oper,
     &     dT_P_heat_rec,
     &     Fuel_Type
     &     )
      IMPLICIT NONE 
C--------------------------------------------------------------------------------
C     Declare initiaization functon
C--------------------------------------------------------------------------------
      REAL PEM_FCinit
C--------------------------------------------------------------------------------
C     Declare passed variables 
C--------------------------------------------------------------------------------
      REAL TH_cold(10,30), TH_hot(10,30)
                                ! Arrays containing cold and hot 
                                !    temperature-enthalpy profiles 
                                !    for hot and cold streams 
      REAL AUX_outlet_data(7)   ! Temporaray variable containing exhaust 
                                !     composition of auxiliary burner
      REAL Pressure_oper        ! Operating pressure (kPa)
      REAL dT_P_heat_rec        ! Minimum apporach temperautre (K)
      INTEGER Fuel_Type         ! Fuel type
C--------------------------------------------------------------------------------
C     Local variables
C--------------------------------------------------------------------------------
      REAL Temp_comp(7)         ! Compositions 
      REAL Pressure_Bars        ! Operating pressure in bar 
      REAL T_water_sat          ! Water saturation temperature (K)
      REAL T_fuel_sat           ! Fuel staturation temperature (K)
      REAL Hfg_fuel             ! Fuel enthalpy of vaporization (kJ/kmol)
      REAL T1,T2                ! Node temperatures used in shift analysis (K)
      REAL T_int                ! Intermediate temperature used in shift analysis (K)
      REAL H1,H2                ! node enthalpies used in shift analysis (kW)
      REAL H_cold, H_Hot        ! Cold and Hot profile enthalpies used (kW)
      REAL dH_streams           ! Change in enthalpy streams undergo while 
                                ! heating/cooling (kW)
      REAL H_shift_amount       ! Required enthalpy shift (kW)
      REAL H_hot_lowest         ! Enthalpy at lowest point on heating curve (kW)
      INTEGER i,j,k             ! counters
      LOGICAL FIRST             ! Flag indicating if enthalpy shift has not been 
                                ! calculated previously
C-------------------------------------------------------------------------------
C     Call ESP-r function SATTEMP to determine saturation temperature - first
C     convert operating pressure from kPa to bar
C-------------------------------------------------------------------------------    
      Pressure_Bars = Pressure_oper/100. ! convert kPa to bar
      CALL SATTEMP(Pressure_Bars, T_water_sat)
C-------------------------------------------------------------------------------
C     Convert water saturation temperature from oC to K
C-------------------------------------------------------------------------------
      T_Water_Sat = T_water_sat + PEM_FCinit(0)   ! water saturation temperature in K
C-------------------------------------------------------------------------------
C     Get saturation temperature/enthalpy of vaporization for fuel 
C-------------------------------------------------------------------------------
      IF (Fuel_Type.EQ.2) THEN
C------- Fuel is octane/ call OCTANE subroutine to get T_sat and H_fg
         CALL PEM_OCTANE(Pressure_Bars, T_fuel_sat, Hfg_fuel)
      ELSEIF(Fuel_Type.EQ.3) THEN
C------- Fuel is methanol/ call METHANOL subroutine to get T_sat and H_fg
         CALL PEM_Methanol_P(Pressure_Bars, T_fuel_sat, Hfg_fuel)
      ELSEIF(Fuel_Type.EQ.1) THEN
C------- Fuel is methane - H_fg = 0
         Hfg_fuel = 0.
      ENDIF
C-------------------------------------------------------------------------------
C     Set flag to TRUE, indicating that variable PEM_H_SHIFT has not yet been 
C     calculated.
C-------------------------------------------------------------------------------
      FIRST = .TRUE.
C-------------------------------------------------------------------------------
C     Search through cold composite curve for locations in which the 
C     temperature difference between the curves is less than the specified 
C     minimum. 
C-------------------------------------------------------------------------------
      DO  i = 1, 13             !<- 13 nodes on the cold side profile 
C-------------------------------------------------------------------------------
C     |  Set the intermediate temperature (T_int) as Ti + dTmin
C-------------------------------------------------------------------------------
         T_int = TH_cold(1, i) + dT_P_heat_rec
C-------------------------------------------------------------------------------
C     |  search through hot array until we find two nodes (j & j+1) such
C     |  that T_j > T_int > T_j+1.  (remember, node # 1 corresponds 
C     |  to the lowest temperature in TH_cold and the _HIGHEST_ temperature
C     |  in TH_hot.  See subroutine CURVES for details.
C-------------------------------------------------------------------------------
         DO  j = 1, 12          !<-12 segments on the hot curve!
C-------------------------------------------------------------------------------
C     |     Assign two sequential nodes on the hot composite curve to lower and
C     |     upper bounds
C-------------------------------------------------------------------------------
            T1 = TH_hot(1, j)    ! Hotside upper bound (K)
            T2 = TH_hot(1, j + 1) ! hotside lower bound (K)
C-------------------------------------------------------------------------------
C     |     If the intermediate temperature is between the lower and upper bounds,
C     |     proceed with the analysis.  Otherwise, move onto the next two points.
C-------------------------------------------------------------------------------
            IF ((T_int.LE.T1).AND.(T_int.GE.T2)) THEN
C-------------------------------------------------------------------------------
C     |        Assign lower and upper bound enthalpies to holding variables
C     |        variables H1 and H2
C-------------------------------------------------------------------------------
               H1 = TH_hot(10, j) ! (kW)
               H2 = TH_hot(10, j + 1) ! (kW)
C-------------------------------------------------------------------------------
C     |        Assign current coldnode (node i) enthalpy to variable HiC
C-------------------------------------------------------------------------------
               H_cold = TH_cold(10, i) ! (kW)
C-------------------------------------------------------------------------------
C     |        Determine enthalpy of cooling stream at intermediate temperature
C     |        1. fill temporary array Temp_comp with molar flow rates at node j
C     |        2. Call subroutine PEM_del_H to determine the enthalpy change the 
C     |           cooling stream wull undergo between temperatures T2 and T_int
C-------------------------------------------------------------------------------
               DO  k = 2, 8
                  Temp_comp(k-1) = TH_hot(k, j)
               ENDDO
               CALL PEM_del_H(Temp_comp,! <- Composition of stream (input)
     &              T2,         ! <- Start temperature (input)
     &              T_int,      ! <- end temperature (input)
     &              T_water_sat,     ! <- water saturation temperature(dummy variable)
     &              dH_streams,        ! <- enthalpy change (output, kW)
     &              T_fuel_sat,       ! <- fuel saturatiom  temperature (dummy variable)
     &              Hfg_fuel,      ! <- fuel enthalpy of vaporization (dummy variable)
     &              Fuel_Type,  ! <- fuel type (input)
     &              .FALSE.)    ! <- flag indicating if water will vaporize (input)
C-------------------------------------------------------------------------------
C     |        Determine enthalpy of intermediate node:
C     |           * H_int = enthalpy at node j+1 
C     |                      + change between node j+1 and intermediate node
C-------------------------------------------------------------------------------
               H_Hot = H2 + dH_streams
C-------------------------------------------------------------------------------
C     |        If the calculated PEM_H_shift amount is greater
C     |        than the previously calculated shift, or if the shift 
C     |        has not been previously calculated: Calculate new enthalpy shift.
C-------------------------------------------------------------------------------
               IF ((FIRST).OR.((H_Hot-H_cold).GT.H_shift_amount)) THEN
                   H_shift_amount =  H_Hot - H_cold
                   FIRST = .FALSE.
              ENDIF
            ENDIF
         ENDDO
      ENDDO
C-------------------------------------------------------------------------------
C     Search through hot composite curve for locations in which the 
C     temperature difference between the curves is less than the specified 
C     minimum. 
C-------------------------------------------------------------------------------
      DO  i = 1, 13             !<- 13 nodes on the hot side curve
C-------------------------------------------------------------------------------
C     |  Set the intermediate temperature (T_int) as Ti - dTmin
C-------------------------------------------------------------------------------
         T_int = TH_hot(1, i) - dT_P_heat_rec
C-------------------------------------------------------------------------------
C     |  search through cold array until we find two nodes (j & j+1) such
C     |  that T_j < T_int < T_j+1.  (remember, node # 1 corresponds 
C     |  to the lowest temperature in TH_cold and the _HIGHEST_ temperature
C     |  in TH_hot.  See subroutine CURVES for details.
C-------------------------------------------------------------------------------
         DO j = 1, 12           !<-12 segments on the cold curve!
C-------------------------------------------------------------------------------
C     |     Assign two sequential nodes on the hot compsite
C     |     curve to lower and upper bounds
C-------------------------------------------------------------------------------
            T1 = TH_cold(1, j)   ! (K)
            T2 = TH_cold(1, j + 1) ! (K)
C-------------------------------------------------------------------------------
C     |     If the intermediate temperature is between the lower and upper 
C     |     bounds proceed with the analysis.  Otherwise, pick move onto the 
C     |     next two points.
C-------------------------------------------------------------------------------
            IF ((T_int .GE. T1).AND.(T_int .LE. T2)) THEN
C-------------------------------------------------------------------------------
C     |        Assign lower and upper bound enthalpies to temporay 
C     |        variables H1 and H2
C-------------------------------------------------------------------------------
               H1 = TH_cold(10, j)
               H2 = TH_cold(10, j + 1)
               H_Hot = TH_hot(10, i)
C-------------------------------------------------------------------------------
C     |        Determine enthalpy of cooling stream at intermediate temperature
C     |        * fill temporary array with molar flow rates at node j
C-------------------------------------------------------------------------------
               DO k = 2,8
                  Temp_comp(k-1) = TH_cold(k, j)
               ENDDO
C-------------------------------------------------------------------------------
C     |        Call PEM_del_H(...) to determine the change in the enthalpy
C     |        between nodes j+1 and intermediate node
C-------------------------------------------------------------------------------
               CALL PEM_del_H(
     &              Temp_comp, 
     &              T1,
     &              T_int,
     &              T_water_sat,
     &              dH_streams,
     &              T_fuel_sat,
     &              Hfg_fuel,
     &              Fuel_Type,
     &              .TRUE.
     &              )
C-------------------------------------------------------------------------------
C     |        Determine enthalpy of intermediate node:
C     |           * H_int = enthalpy at node j 
C     |                      + change between node j and intermediate node
C-------------------------------------------------------------------------------
               H_cold = H1 + dH_streams
C-------------------------------------------------------------------------------
C     |        If the calculated shift amount is greater
C     |        than the previously calculated shift, or if the shift 
C     |        has not been previously calculated: Calculate new enthalpy shift.
C-------------------------------------------------------------------------------
               IF ((FIRST).OR.(H_Hot-H_cold.GT.H_shift_amount)) THEN
                  H_shift_amount = H_Hot-H_cold
                  FIRST = .FALSE.
               ENDIF
            ENDIF

         ENDDO
      ENDDO
C-------------------------------------------------------------------------------
C     Check that the minimum shift does not push TH_cold(10,1)
C     to the left of the leftmost point on TH_hot. Determine the 
C     lowest enthalpy on TH_cold after the shift:
C-------------------------------------------------------------------------------
      H_hot_lowest = TH_cold(10,1)+H_shift_amount
C-------------------------------------------------------------------------------
C     If lowest enthalpy on TH_cold is less than the lowest 
C     enthalpy on TH_hot, constrain the required enthalpy 
C     shift such that the lowest points on the two curves
C     have the same enthalpy.
C-------------------------------------------------------------------------------
      IF (H_hot_lowest.LT.TH_hot(10,13)) THEN
         H_shift_amount = H_shift_amount -H_hot_lowest
      ENDIF
C-------------------------------------------------------------------------------
C     Shift the cold composite curve by amount H_shift_amount
C-------------------------------------------------------------------------------
      DO i = 1, 13
         TH_cold(10, i) = TH_cold(10, i) + H_shift_amount
      ENDDO

      RETURN
      END
C===================== COMPRE ===================================================
C     Created by: Alex Ferguson
C     Created on: March 2001
C     Copyright:  Alex Ferguson 2001 / CETC 2003.
C-------------------------------------------------------------------------------
C     ABSTRACT: 
C     This routine characterizes the behavior of air/natural gas compressors
C     using ideal-gas analysis. The performance of the compressor is quantified
C     using a constant isentropic efficiency.
C-------------------------------------------------------------------------------
C     REFERENCES
C
C     FERGUSON, A 2003. Fuel cell modelling for building cogeneration 
C        applications.  MASc Thesis, Dalhousie University
C
C     VAN WYLEN, G. et al. 1994. Fundamentals of classical t
C        thermodynamics. Wiley and Sons.
C     
C-------------------------------------------------------------------------------
C     REVISIONS:
C     Sept 2003: Code was heavily revised to comply with CETC coding
C                standard and improve general readability.  
C-------------------------------------------------------------------------------
      SUBROUTINE PEM_COMPRESSOR(
     &     CompInlet, 
     &     PInlet, 
     &     Poutlet, 
     &     TInlet, 
     &     Toutlet, 
     &     IsentropicEfficiency, 
     &     CompressorWork )
      IMPLICIT NONE
C--------------------------------------------------------------------------------
C     Declare initiaization functon
C--------------------------------------------------------------------------------
      REAL PEM_FCinit
C--------------------------------------------------------------------------------
C     Declare entropy / enthalpy functions
C--------------------------------------------------------------------------------
      REAL PEMf_HSTEAM, PEMf_HCH4, PEMf_HCO, 
     &     PEMf_HCO2, PEMf_HH2,PEMf_hN2, PEMf_HO2
      REAL PEMf_SSTEAM, PEMf_SCH4, PEMf_SCO, 
     &     PEMf_SCO2, PEMf_SH2,PEMf_SN2, PEMf_SO2
C--------------------------------------------------------------------------------
C     Declare passed variables
C--------------------------------------------------------------------------------
      REAL CompInlet(7)        ! Array containing constituant flow rates of gas 
                                ! stream to be compressed (input)
      REAL PInlet, POutlet    ! Compressor inlet and outlet pressure (kPa,inputs)
      REAL TInlet              ! Inlet temperature (K, input)      
      REAL TOutlet             ! Outlet temperature (K, output)
      REAL IsentropicEfficiency ! Isentropic efficiency of compressor (input)
      REAL CompressorWork      ! Compressor mechanical work requirement (kW, output)
C--------------------------------------------------------------------------------
C     Declare local variables
C--------------------------------------------------------------------------------
      INTEGER ii                ! Counter
      REAL R_universal          ! Universial gas constant (kJ/kmol K)
      REAL HInlet              ! Compressor inlet enthalpy (kW)
C.....Inlet enthalpy of species (kW)      
      REAL Hi_H2O, Hi_CH4, Hi_CO, Hi_CO2, Hi_H2, Hi_O2, Hi_N2 
C.....Outlet enthalpy of species (kW)                                
      REAL Ho_H2O, Ho_CH4, Ho_CO, Ho_CO2, Ho_H2, Ho_O2, Ho_N2             
      REAL HOutletIsentropic  ! Enthalpy at outlet of isentropic compressor (kW)
      REAL HOutletActual      ! Enthalpy at outlet of actual compressor (kW)
      REAL HOutletGuess       ! outlet enthalpy estimate
      REAL HError              ! Error in temperatrure calculation (kW)
      REAL dSstandard          ! Standard entropy change for isentropic processes (kW/K)
C.....Standard entropy changes for species in isentropic processes ( kW/K)
      REAL dS_H2O, dS_CH4, dS_CO, dS_CO2, dS_H2, dS_O2, dS_N2
      REAL SInlet              ! inlet entropy (kW/K)
C.....Entropy of species at inlet (kW/K)
      REAL Si_H2O, Si_CH4, Si_CO, Si_CO2, Si_H2, Si_O2, Si_N2 
      REAL SOutletIsentropic  ! entropy at outlet (kW/K)
      REAL SOutletGuess       ! entropy at outlet, estimated (kW/K)
      REAL S_error              ! Error in isentropic temperature calculation (kW
C.....Entropy of species at outlet (kW/K)
      REAL So_H2O, So_CH4, So_CO, So_CO2, So_H2, So_O2, So_N2  
      REAL TOutletUB, TOutletLB ! lower and upper bounds for temperature root search (K)
      REAL P_Reference          ! Reference pressure (kPa)
      REAL MOLsum               ! Molar sum of all consitiuants in a species (mol)
      REAL TOutletIsentropic  ! Outlet temperature under isentropic conditions (K)
      REAL P_Pressure           ! Partial pressure of stream constituant (kPa)/K)

      REAL T_tolerance          ! Tolerance for temperature convergence (K)

      LOGICAL bPressRise        ! flag indicating that pressure will rise 
                                ! in unit (ie system is not a turbine) 

      LOGICAL DONE              ! Controls for while loops
      INTEGER iIterCount        !
      LOGICAL bBounded          ! flag indicating bound has been found

c.....Debug
      real      
     &     old_Poutlet, 
     &     old_Toutlet, 
     &     old_CompressorWork 



C--------------------------------------------------------------------------------
C     Universal gas constant R (kJ/kmol K)
C--------------------------------------------------------------------------------
      R_universal = PEM_FCinit(36)
C--------------------------------------------------------------------------------
C     Tolerance for temperature convergence
C-------------------------------------------------------------------------------
      T_tolerance = PEM_FCinit(40)  ! K
C--------------------------------------------------------------------------------
C     Determine enthalpy at inlet of compressor (kW)
C     
C     - Multiply stream composition by specific enthalpy returned by functions
C       H_XXX(T)
C--------------------------------------------------------------------------------
      Hi_h2o = CompInlet(1) * PEMf_hSTEAM(TInlet)
      Hi_ch4 = CompInlet(2) * PEMf_hCH4(TInlet)
      Hi_co  = CompInlet(3) * PEMf_hCO(TInlet)
      Hi_co2 = CompInlet(4) * PEMf_hCO2(TInlet)
      Hi_h2  = CompInlet(5) * PEMf_hH2(TInlet)
      Hi_o2  = CompInlet(6) * PEMf_hO2(TInlet)
      Hi_n2  = CompInlet(7) * PEMf_hN2(TInlet)
C--------------------------------------------------------------------------------
C     Sum inlet enthalpies of all species (kJ/kmol)
C--------------------------------------------------------------------------------
      HInlet = Hi_h2o + Hi_ch4 + Hi_co 
     &     + Hi_co2 + Hi_h2 + Hi_o2 + Hi_n2


C--------------------------------------------------------------------------------
C     Calculate standard entropy change for isentropic compression (kW/ K)
C
C                                   P2
C         dS_o = R_universal * log ----    (see eqn 7.27, VAN WYLEN 1994.)
C                                   P1
C--------------------------------------------------------------------------------
      dS_h2o = CompInlet(1) * R_universal * log(POutlet/PInlet)
      dS_ch4 = CompInlet(2) * R_universal * log(POutlet/PInlet)
      dS_co  = CompInlet(3) * R_universal * log(POutlet/PInlet)
      dS_co2 = CompInlet(4) * R_universal * log(POutlet/PInlet)
      dS_h2  = CompInlet(5) * R_universal * log(POutlet/PInlet)
      dS_o2  = CompInlet(6) * R_universal * log(POutlet/PInlet)
      dS_n2  = CompInlet(7) * R_universal * log(POutlet/PInlet)
C--------------------------------------------------------------------------------
C     Sum standard enthalpy change for all species
C--------------------------------------------------------------------------------
      dSstandard = dS_h2o + dS_ch4 + dS_co 
     &     + dS_co2 + dS_h2 + dS_o2 + ds_n2

C--------------------------------------------------------------------------------
C     Calculate standard entropy of species i at inlet temperature  (kW/ K)
C     using function S_XXXX(Temperature, Partial_pressure)
C--------------------------------------------------------------------------------
      P_Reference = PEM_FCinit(15)  ! Reference pressure (kPa)
C.....Calculate total molar flow rate
      MOLsum = 0.0
      DO ii = 1,7
         MOLsum = MOLsum + CompInlet(ii)
      ENDDO
C.....Get partial pressure of each species, and then
C.....determine entropy @ inlet conditions
      P_Pressure =  P_Reference * CompInlet(1)/MOLsum ! Partial pressure of water 
      Si_h2o = CompInlet(1) * PEMf_sSTEAM(TInlet,P_Pressure) ! entropy at inlet of water
      P_Pressure =  P_Reference * CompInlet(2)/MOLsum ! Partial pressure of CH4 
      Si_ch4 = CompInlet(2) * PEMf_sCH4(TInlet,P_Pressure) ! entropy of CH4
      P_Pressure =  P_Reference *CompInlet(3)/MOLsum ! Partial pressure of CO 
      Si_co = CompInlet(3) * PEMf_sCO(TInlet,P_Pressure) ! entropy of co
      P_Pressure =  P_Reference * CompInlet(4)/MOLsum ! Partial pressure of CO2 
      Si_co2 = CompInlet(4) * PEMf_sCO2(TInlet,P_Pressure) !entropy of CO2
      P_Pressure =  P_Reference * CompInlet(5)/MOLsum! Partial pressure of H2 
      Si_h2 = CompInlet(5) * PEMf_sH2(TInlet,P_Pressure) ! entropy of H2
      P_Pressure =  P_Reference * CompInlet(6)/MOLsum! Partial pressure of O2 
      Si_o2 = CompInlet(6) * PEMf_sO2(TInlet,P_Pressure) ! Oxygen entropy
      P_Pressure =  P_Reference * CompInlet(7)/MOLsum ! Partial pressure of N2
      Si_n2 = CompInlet(7) * PEMf_sN2(TInlet,P_Pressure) ! Nitrogen entropy
C-----------------------------------------------------------------------------
C     Sum inlet entropy for all species
C-----------------------------------------------------------------------------
      SInlet = Si_h2o + Si_ch4 + Si_co + Si_co2 + Si_h2 + Si_o2 + Si_n2 ! (kW/K)

C-----------------------------------------------------------------------------
C     Calculate entropy at outlet for isentropic processes
C-----------------------------------------------------------------------------
C.....1. Calculate standard entropy at outlet temperature (isentropic process)
      SOutletIsentropic = SInlet + dSstandard

C.....2. Check if system operates as a compresser or turbine
      if ( POutlet .gt. PInlet ) then ! system is compressor 

         bPressRise = .TRUE. 

C........Temperature will rise. Set outlet lower bound to inlet
         TOutletLB = TInlet

      else
         
         bPressRise = .FALSE. 

C........Temperature will fall. Set outlet upper bound to inlet         
         TOutletUB = TInlet 

      endif
C.....2. Determine isentropic temperature through iteration (Bisection method)
C.....   Note: this loop does 2 things: (a) establishes the appropriate bounds
C.....   for the bisection root search, and (b) performs the root search.
C.....   the approprate mode is determined by the value of the bBounded control.
      DONE = .FALSE. 
      bBounded = .FALSE.        ! flag indicating that bound has not been found
      iIterCount = 0            ! iteration counter
      DO WHILE (.NOT. DONE)
         iIterCount = iIterCount + 1

         if ( .not. bBounded ) then
C...........Loop is establishing search bounds.
C...........Increment working temperature until appropriate 
C...........bound is found
            if ( bPressRise ) then
               
               TOutletUB = TInlet + 100. * real ( iIterCount ) ! (K)

               TOutletIsentropic = TOutletUB

            else
               
               TOutletLB = TInlet - 100. * real ( iIterCount ) ! (K)
               
               if ( TOutletLB .LT.  0. ) then

                  TOutletLB = 0. ! bound cannot be lower than absolute zero!

                  bBounded = .true.

               endif

               TOutletIsentropic = TOutletLB

            endif

         endif

         if ( bBounded ) then
C...........Bound has been found. Set working temperature
C...........to midpoint of bounds.
            TOutletIsentropic = (TOutletLB+TOutletUB)/2. ! (K) 
         end if 

C........Determine standard entropy at intermediate temperature (kJ/kmol K)
         P_Pressure = P_Reference * CompInlet(1)/MOLsum
         So_h2o = CompInlet(1) 
     &        * PEMf_sSTEAM(TOutletIsentropic,P_Pressure)
         P_Pressure = P_Reference * CompInlet(2)/MOLsum
         So_ch4 = CompInlet(2) 
     &        * PEMf_sCH4(TOutletIsentropic,P_Pressure)
         P_Pressure = P_Reference * CompInlet(3)/MOLsum
         So_co = CompInlet(3) 
     &        * PEMf_sCO(TOutletIsentropic,P_Pressure)
         P_Pressure = P_Reference * CompInlet(4)/MOLsum
         So_co2 = CompInlet(4) 
     &        * PEMf_sCO2(TOutletIsentropic,P_Pressure)
         P_Pressure = P_Reference * CompInlet(5)/MOLsum
         So_h2 = CompInlet(5) 
     &        * PEMf_sH2(TOutletIsentropic,P_Pressure)
         P_Pressure = P_Reference * CompInlet(6)/MOLsum
         So_o2 = CompInlet(6) 
     &        * PEMf_sO2(TOutletIsentropic,P_Pressure)
         P_Pressure = P_Reference * CompInlet(7)/MOLsum
         So_n2 = CompInlet(7) 
     &        * PEMf_sN2(TOutletIsentropic,P_Pressure)
C........Sum entropy at outlet.
         SOutletGuess = So_h2o + So_ch4 + So_co + So_co2 +
     &        So_h2 + So_o2 + So_n2

C........Compare calculated entropy to isentropic entropy.
         S_error = SOutletIsentropic - SOutletGuess
         
         if ( .not. bBounded ) then

c...........Establish if bound temperature contains 
C...........outlet entropy

            if ( (bPressRise .and. S_error .lt. 0.0  ) .or.
     &           (.not. bPressRise .and. S_error .gt. 0.0 ))then
               bBounded = .true. 
            end if

         else            

C........Adjust temperature bounds as req'd
            IF (S_error.GT.0.) TOutletLB = TOutletIsentropic
            IF (S_error.LE.0.) TOutletUB = TOutletIsentropic
C........Check temperature bounds against tolerance, and terminate
C........iteration as required.
            IF ((TOutletUB-TOutletLB).LT.T_tolerance) DONE = .TRUE.

         end if

      ENDDO
 
C.....3. Determine entropy at outlet corresponding to iseontropic temperature
C.....   calculated in 2.
      Ho_h2o = CompInlet(1) * PEMf_hSTEAM(TOutletIsentropic)
      Ho_ch4 = CompInlet(2) * PEMf_hCH4(TOutletIsentropic)
      Ho_co  = CompInlet(3) * PEMf_hCO(TOutletIsentropic)
      Ho_co2 = CompInlet(4) * PEMf_hCO2(TOutletIsentropic)
      Ho_h2  = CompInlet(5) * PEMf_hH2(TOutletIsentropic)
      Ho_o2  = CompInlet(6) * PEMf_hO2(TOutletIsentropic)
      Ho_n2  = CompInlet(7) * PEMf_hN2(TOutletIsentropic)
      HOutletIsentropic = Ho_h2o + Ho_ch4 + Ho_co + Ho_co2 + Ho_h2 + 
     &     Ho_O2 + Ho_n2 ! (kW/K)

C------------------------------------------------------------------------------
C     Check if unit operates as a compressor, or a turbine
C------------------------------------------------------------------------------

      if ( bPressRise ) then 

C------------------------------------------------------------------------------
C     Unit is  a compressor: Determine actual enthalpy at outlet:
C
C     HOutlet = HInlet + dHIsentropic / isentropic efficiency.
C
C     See eqn 3.3.5, Ferguson 2003.
C------------------------------------------------------------------------------

         HOutletActual = HInlet 
     &        + (HOutletIsentropic - HInlet) / IsentropicEfficiency

C........Set appropriate bounds for temperature bisection root search
         TOutletLB = TInlet
                  
      else

C------------------------------------------------------------------------------
C     Unit is a turbine: Determine actual enthalpy at outlet:
C
C     HOutlet = HInlet + dHIsentropic * isentropic efficiency.
C
C     See eqn 7.22, Van Whylen 2004.
C------------------------------------------------------------------------------

         HOutletActual = HInlet 
     &        + (HOutletIsentropic - HInlet) * IsentropicEfficiency

C........Set approptiate bounds for temperature bisection root search
         TOutletUB = TInlet     ! (K) 

      end if

C------------------------------------------------------------------------------
C     Calculate compressor work (kW, -ive o-> )
C------------------------------------------------------------------------------

C.....Compressor work (kW) 
      CompressorWork = HOutletActual - HInlet



C------------------------------------------------------------------------------
C     Determine actual temperature at outlet of comressor. Iterate using 
C     bisection method
C------------------------------------------------------------------------------
C.....2. Determine isentropic temperature through iteration (Bisection method)
C.....   Note: this loop does 2 things: (a) establishes the appropriate bounds
C.....   for the bisection root search, and (b) performs the root search.
C.....   the approprate mode is determined by the value of the bBounded control.
      DONE = .FALSE. 
      bBounded = .FALSE.        ! flag indicating that bound has not been found
      iIterCount = 0            ! iteration counter
      DO WHILE (.NOT. DONE)
         iIterCount = iIterCount + 1

         if ( .not. bBounded ) then
C...........Loop is establishing search bounds.
C...........Increment working temperature until appropriate 
C...........bound is found
            if ( bPressRise ) then
               
               TOutletUB = TInlet + 100. * real ( iIterCount ) ! (K)

               TOutlet = TOutletUB

            else
               
               TOutletLB = TInlet - 100. * real ( iIterCount ) ! (K)
               
               if ( TOutletLB .LT.  0. ) then

                  TOutletLB = 0. ! bound cannot be lower than absolute zero!

                  bBounded = .true.

               endif

               TOutlet = TOutletLB
      
            endif

         endif

         if ( bBounded ) then
C...........Bound has been found. Set working temperature
C...........to midpoint of bounds.
            TOutlet = (TOutletLB+TOutletUB)/2. ! (K) 
         end if 

C........Determine standard enthalpy at intermediate temperature (kW/ K)
         Ho_h2o = CompInlet(1) * PEMf_hSTEAM(TOutlet)
         Ho_ch4 = CompInlet(2) * PEMf_hCH4(TOutlet)
         Ho_co  = CompInlet(3) * PEMf_hCO(TOutlet)
         Ho_co2 = CompInlet(4) * PEMf_hCO2(TOutlet)
         Ho_h2  = CompInlet(5) * PEMf_hH2(TOutlet)
         Ho_O2  = CompInlet(6) * PEMf_hO2(TOutlet)
         Ho_n2  = CompInlet(7) * PEMf_hN2(TOutlet)
C........Sum outlet enthalpies
         HOutletGuess = Ho_h2o + Ho_ch4 + Ho_co + Ho_co2 +
     &        Ho_h2 + Ho_O2 + Ho_n2
C........Compare actual and guessed enthalpies
         HError = HOutletActual-HOutletGuess


        
         if ( .not. bBounded ) then

c...........Establish if bound temperature contains 
C...........outlet entropy

            if ( (bPressRise .and. HError .lt. 0.0  ) .or.
     &           (.not. bPressRise .and. HError .gt. 0.0 )) then
               bBounded = .true. 
            end if

         else 
  
C........Adjust bounds of bisection search
            IF (HError.GT.0.) TOutletLB = TOutlet
            IF (HError.LE.0.) TOutletUB = TOutlet
C........Check bounds agianst tolerance and halt iteration as req'd.
            IF((TOutletUB-TOutletLB).LT.T_tolerance) DONE = .TRUE.

         endif

      ENDDO




    
      RETURN
      END

C***************************PEM_del_H  **********************************************
C     
C     This subroutine determines the sensible or latent enthalpy change 
C     a process composite stream undergos between two temperatures. It assumes
C     that the stream will undergo sensible, or latent heating/cooling, but 
C     not both.
C
C-------------------------------------------------------------------------------
C     REFERENCES
C
C     FERGUSON, A 2003. Fuel cell modelling for building cogeneration 
C        applications.  MASc Thesis, Dalhousie University
C
C     VAN WYLEN, G. et al. 1994. Fundamentals of classical t
C        thermodynamics. Wiley and Sons.
C     
C------------------------------------------------------------------------------

      SUBROUTINE PEM_del_H(
     &     Temp_comp,           ! input: array containg stream compositions
     &     T_start,             ! input: start temperature
     &     T_end,               ! input: end temperature
     &     T_water_sat,         ! input: water saturation temperature
     &     dH_streams,          ! output: enthalpy change in stream
     &     T_fuel_sat,          ! input: fuel saturation temperature
     &     Hfg_fuel,            ! input: fuel saturation enthalpy
     &     Fuel_Type,           ! input: fuel type
     &     vapor)               ! input: flag indicating if vaporization will
                                !        occur.

      IMPLICIT NONE
C--------------------------------------------------------------------------------
C     Declarations
C--------------------------------------------------------------------------------
C.....Initialization function
      REAL PEM_FCinit
C.....ESP-r functions
      REAL SHTFLD, LTHT
C.....Enthalpy functions
      REAL PEMf_HSTEAM, PEMf_HCH4, PEMf_HCO, 
     &     PEMf_HCO2, PEMf_HH2,PEMf_hN2, PEMf_HO2
C.....Variables
      REAL  Temp_comp(7)        ! array containing the molar  
                                ! composition of the heating/cooling stream (kmol/s)
      REAL T_start              ! cooling/heaing start temperature (K)
      REAL T_end                ! cooling/heaing target temperature (K) 
      REAL T_start_oC           ! start temperature in deg-C. used for ESP-r functions
      REAL T_end_oC             ! end temperature in deg-C. used for ESP-r functions
      REAL T_water_sat          ! water saturation temperature (K)
      REAL T_water_sat_oC       ! water saturation temperature (oC)
      REAL dH_streams           ! total change in enthalpy (kW)
      REAL T_fuel_sat           ! fuel saturation temperature (K)  
      REAL Hfg_fuel             ! fuel enthalpy of vaporization (kJ/kg) 
      Integer Fuel_Type         ! fuel type 
      Logical vapor             ! logical flag indicating if vaporization will occur
      REAL mm_water             ! H2O molar mass (kg/kmol)
      REAL Hfg_water            ! Water enthalpy of vaporization (kJ/kmol)
      REAL Cp_water_l           ! Specific heat capacity of liquid water (kJ/kg K)
      LOGICAL debug             ! flag for debugging
      INTEGER i                 ! counter
      LOGICAL CLOSE_1, CLOSE_2  ! logical variable for close to equal comparisons
C.....Enthalpy change of species (kW)
      REAL dH_H2O, dH_FUEL, dH_CO, dH_CO2, dH_H2, dH_O2, dH_N2      
C.....Initialize molar mass
      mm_water = PEM_FCinit(9)      ! (kg/kmol)
C.....Set variables in degrees celcius for use with esp-r function calls
      T_start_oC     = T_start - PEM_FCinit(0) ! oC
      T_end_oC       = T_end - PEM_FCinit(0)   ! oC
      T_water_sat_oC = T_water_sat - PEM_FCinit(0) ! oC
C-------------------------------------------------------------------------------
C     Determine state and dH of water.
C-------------------------------------------------------------------------------
      CALL ECLOSE(T_start,T_water_sat,0.001,CLOSE_1)
      CALL ECLOSE(T_end  ,T_water_sat,0.001,CLOSE_2)
      IF ((T_start.LT.T_water_sat).AND.(vapor)) THEN
C........Temperature is below water saturation temperature. Calculate water 
C........specific heat using ESP-r function SHTFLD.
         Cp_Water_L = (SHTFLD(3,T_start_oC)   ! (kJ/kg K)
     &        + SHTFLD(3,T_end_oC)) / 2000.
C........Calculate total enthalpy change in water
         dH_H2O = Temp_comp(1) 
     &        * (Cp_Water_L * mm_water) * (T_end - T_start)
      ELSEIF (CLOSE_1.AND.CLOSE_2.AND.(vapor)) THEN
C........Temperature is equal to water saturation temperatutre, water will
C........vapourize. Call ESP-r function LTHT to determine the enthalpy of
C........vaporization.
         Hfg_water= LTHT(2,T_water_sat_oC) * mm_water / 1000. ! <- convert from J to kJ
         dH_H2O = Temp_comp(1) * Hfg_water 
      ELSE
C........Temperature is above water saturation temperature. Use function PEMf_HSTEAM
C........to determine temperatures
         dH_H2O = ( PEMf_hSTEAM(T_end) 
     &        - PEMf_hSTEAM(T_start) ) * Temp_comp(1)
      ENDIF
C-------------------------------------------------------------------------------
C     Determine state and dH of fuel.
C-------------------------------------------------------------------------------
      IF (Fuel_Type.eq.1) THEN
C........Fuel is methane: model as ideal gas at all temperatures using function
C........PEMf_HCH4
         dH_FUEL = Temp_comp(2) 
     &        * (PEMf_HCH4(T_end) - PEMf_HCH4(T_start))
      ELSE
         write (*,*) 'PEMFC: FATAL ERROR!'
         write (*,*) 'Note: octane and mehtanol fuel types are not'
         write (*,*) 'presently supported'
         stop
      ENDIF
C.....UNUSED CODE...
C$$$      ELSEIF (Fuel_Type.EQ.2) THEN
C$$$C........Fuel is octane: determine state & enthalpy change
C$$$         IF ((T_start.LT.T_fuel_sat).AND.(vapor)) THEN
C$$$C     Octane is liquid
C$$$            rHtF = Temp_comp(2) * (PEMf_hOCTl(T_end) - PEMf_hOCTl(T_start))
C$$$         ELSEIF ((T_start.EQ.T_fuel_sat).AND.(T_end.EQ.T_fuel_sat)
C$$$     &           .AND.(vapor)) THEN
C$$$C     Octane will vapourize
C$$$            rHtF = Temp_comp(2) * Hfg_fuel
C$$$         ELSE
C$$$C     Octane is ideal gas
C$$$            rHtF=Temp_comp(2) * (PEMf_hOCTg(T_end) - PEMf_hOCTg(T_start))
C$$$         ENDIF
C$$$      ELSEIF(Fuel_Type.EQ.3) THEN
C$$$C     Fuel is methanol, determine state and enthalpy change
C$$$         IF ((T_start.LT.T_fuel_sat).AND.(vapor)) THEN
C$$$C     Octane is liquid
C$$$            rHtF = Temp_comp(2) 
C$$$     &           * (PEMf_hMETHl(T_end) - PEMf_hMETHl(T_start))
C$$$         ELSEIF ((T_start.EQ.T_fuel_sat).AND.(T_end.EQ.T_fuel_sat)
C$$$     &           .AND.(vapor)) THEN
C$$$C     Octane will vapourize
C$$$            rHtF = Temp_comp(2) * Hfg_fuel
C$$$         ELSE
C$$$C     Octane is ideal gas
C$$$            rHtF=Temp_comp(2) 
C$$$     &           * (PEMf_hMETHg(T_end) - PEMf_hMETHg(T_start))
C$$$         ENDIF
C$$$      ENDIF
      dH_CO = Temp_comp(3) * (PEMf_HCO(T_end) - PEMf_HCO(T_start))
      dH_CO2 = Temp_comp(4) * (PEMf_HCO2(T_end) - PEMf_HCO2(T_start))
      dH_H2 = Temp_comp(5) * (PEMf_HH2(T_end) - PEMf_HH2(T_start))
      dH_O2 = Temp_comp(6) * (PEMf_HO2(T_end) - PEMf_HO2(T_start))
      dH_N2 = Temp_comp(7) * (PEMf_hN2(T_end) - PEMf_hN2(T_start))

      dH_streams = dH_H2O+dH_FUEL+dH_CO+dH_CO2+dH_H2+dH_N2+dH_O2


      RETURN
      END

C--------------------- PEM_Aux_Burner --------------------------------------------------
C     
C     This subroutine is used to characterize the system's auxiliary burner.
C     It calculates the fuel and air compositions at the burner inlet and 
C     the exhast composition at the burner outlet. It then evaluates the 
C     enthalpy conservation in the reformer and determines the am
C
C-------------------------------------------------------------------------------
C     REFERENCES
C
C     FERGUSON, A 2003. Fuel cell modelling for building cogeneration 
C        applications.  MASc Thesis, Dalhousie University
C
C     VAN WYLEN, G. et al. 1994. Fundamentals of classical t
C        thermodynamics. Wiley and Sons.
C     
C------------------------------------------------------------------------------
      SUBROUTINE PEM_Aux_Burner(
     &     AUX_inlet_data,      ! composition at aux burner inlet (output, kmol/s)
     &     AUX_outlet_data,     ! composition at aux burner outlet (output, kmol/s)
     &     Aux_Bur_EA,          ! aux burner excess air ratio (input)
     &     Aux_Bur_util,        ! aux burner fuel utilization (input)
     &     T_reformer,          ! reformer operating temperature (input, K)
     &     Q_aux_burner,        ! heat delivered by aux burner (output, kW)
     &     AUX_fuel_flow,          ! aux burner fuel flow rate (input, kmol/s)
     &     Fuel_Type            ! flag indicating fuel type (input)
     &     )
      IMPLICIT NONE
C--------------------------------------------------------------------------------
C     Declarations
C--------------------------------------------------------------------------------
C.....Initialization function
      REAL PEM_FCinit
C.....Enthalpy functions
      REAL PEMf_HSTEAM, PEMf_HCH4, PEMf_HCO, PEMf_HCO2, 
     &     PEMf_HH2, PEMf_hN2, PEMf_HO2, PEMf_HMETHg, PEMf_HOCTg
C.....Variables
      INTEGER Fuel_Type         ! Integer flag indicating the type of fuel used
      REAL H_species_inlet(7)   ! Array containing species enthalpy at inlet (kW)
      REAL H_species_outlet(7)  ! Array containing species enthalpy at outlet (kW)
      REAL AUX_inlet_data(7)    ! composition at aux burner inlet (output, kmol/s)
      REAL AUX_outlet_data(7)   ! composition at aux burner outlet (output, kmol/s)
      REAL hf_H2Ol              ! Enthalpy of formation of liquid water (kj/mol)
      REAL hf_H2Ov              ! Enthalpy of formation of vapor water (kj/mol)
      REAL hf_CH4               ! Enthalpy of formation of methane (kj/kmol)
      REAL hf_CO                ! Enthalpy of formation of carbon monoxide (kj/kmol)
      REAL hf_CO2               ! Enthalpy of formation of carbon dioxide (kj/kmol)
      REAL hf_H2                ! Enthalpy of formation of hydrogen (kj/kmol)
      REAL hf_N2                ! Enthalpy of formation of nitrogen (kj/kmol)
      REAL hf_O2                ! Enthalpy of formation of oxygen (kj/kmol)
      REAL hf_OCT               ! Enthalpy of formation of octane (kj/kmol)
      REAL hf_Meth              ! Enthalpy of formation of methane (kj/kmol)
      REAL hf_FUEL              ! Enthalpy of formation of fuel (kj/kmol)
      INTEGER x,y,z             ! factions of carbon (x), hydrogen (y) and oxygen (z) 
                                ! in fuel 
      REAL REF_temperature      ! Reference temperature for enthalpy of formationd data

      REAL AUX_bur_EA           ! aux burner excess air ratio 
      REAL AUX_bur_util         ! aux burner fuel utilization 
      REAL T_reformer           ! temperature of reformer (K)
      REAL Q_aux_burner         ! heat released in auxiliary burners (kW)
      REAL AUX_fuel_flow        ! flow of fuel into aux burner
      INTEGER i                 ! counter 

      REAL H_inlet, H_outlet    ! enthalpy at burner inlet and outlet (kW)

C.....Reference temperature for enthalpies of formation
      REF_temperature = PEM_FCinit(14) ! K
C.....Enthalpies of formation, kJ/kmol @ 1 atm, 298K
      Hf_H2Ov = PEM_FCinit(16)       ! Water vapor
      Hf_H2Ol = PEM_FCinit(17)       ! liquid water
      Hf_CO   = PEM_FCinit(18)       ! Carbon Monoxide
      Hf_CO2  = PEM_FCinit(19)       ! Carbon Dioxide
      Hf_H2   = PEM_FCinit(20)       ! Hydrogen
      Hf_O2   = PEM_FCinit(21)       ! Oxygen
      Hf_N2   = PEM_FCinit(22)       ! Nitrogen
      Hf_CH4  = PEM_FCinit(23)       ! Methane
      Hf_OCT  = PEM_FCinit(24)       ! Octane
      Hf_Meth = PEM_FCinit(25)       ! Methanol
C.....Set hydrogen/oxygen/carbon composition of fuel and 
C.....fuel enthalpy of formation based on fuel type
      IF (Fuel_Type.EQ.1) THEN
C........Methane
         Hf_FUEL = Hf_CH4
         x = 1.
         y = 4.
         z = 0.
      ELSEIF (Fuel_Type.EQ.2) THEN
C........Octane
         Hf_FUEL = Hf_OCT
         x= 8.
         y = 18.
         z = 0.
      ELSEIF (Fuel_Type.EQ.3) THEN
C........Mehtanol
         Hf_FUEL = Hf_Meth
         x = 1.
         y = 4.
         z = 1.
      ENDIF
C.....Set fuel and air compositions required by burner per mol of fuel used in the 
C.....burner
      AUX_inlet_data(1) = 0.    ! water
      AUX_inlet_data(2) = 1.    ! fuel
      AUX_inlet_data(3) = 0.    ! co
      AUX_inlet_data(4) = 0.    ! co2
      AUX_inlet_data(5) = 0.    ! h2
      AUX_inlet_data(6) = Aux_Bur_EA * (x+y/4.-z/2.) ! o2
      AUX_inlet_data(7) = AUX_inlet_data(6) * PEM_FCinit(13) ! n2

      AUX_outlet_data(1) = y/2.*Aux_Bur_util ! water
      AUX_outlet_data(2) = 1. - Aux_Bur_util ! fuel
      AUX_outlet_data(3) = 0.   ! co
      AUX_outlet_data(4) = Aux_Bur_util ! co2
      AUX_outlet_data(5) = 0.   ! h2
      AUX_outlet_data(6) = 2. * (Aux_Bur_EA-Aux_Bur_util) ! o2
      AUX_outlet_data(7) = AUX_inlet_data(7) ! n2

C.....Scale per mol reactants by actual aux fuel flow (AUX_fuel_flow)
      DO  i = 1, 7
         AUX_inlet_data(i)  = AUX_inlet_data(i)  * AUX_fuel_flow
         AUX_outlet_data(i) = AUX_outlet_data(i) * AUX_fuel_flow
      ENDDO
C--------------------------------------------------------------------------------
C     Calculate enthalpy of species at inlet:
C
C     H = flow_rate * (Hf + H(reformer temperature) - H(reference temperature))
C--------------------------------------------------------------------------------
      H_species_inlet(1) = AUX_inlet_data(1) 
     &     * (Hf_H2Ov + PEMf_HSTEAM(T_reformer) 
     &     - PEMf_HSTEAM(REF_temperature))
      IF (Fuel_Type.EQ.1) THEN
         H_species_inlet(2) = AUX_inlet_data(2) 
     &        * (Hf_FUEL + PEMf_HCH4(T_reformer) 
     &        - PEMf_HCH4(REF_temperature))
      ELSEIF (Fuel_Type.EQ.2) THEN
         H_species_inlet(2) = AUX_inlet_data(2) 
     &        * (Hf_FUEL + PEMF_HOCTG(T_reformer) 
     &        - PEMf_HOCTg(REF_temperature))
      ELSEIF (Fuel_Type.EQ.3) THEN
         H_species_inlet(2) = AUX_inlet_data(2) 
     &        * (Hf_FUEL + PEMf_HMETHg(T_reformer) 
     &        - PEMf_HMETHg(REF_temperature))
      END IF
      H_species_inlet(3) = AUX_inlet_data(3) 
     &     * (Hf_CO + PEMf_HCO(T_reformer) - PEMf_HCO(REF_temperature))
      H_species_inlet(4) = AUX_inlet_data(4) 
     &     * (Hf_CO2 + PEMf_HCO2(T_reformer) 
     &     - PEMf_HCO2(REF_temperature))
      H_species_inlet(5) = AUX_inlet_data(5) 
     &     * (Hf_H2 + PEMf_HH2(T_reformer) - PEMf_HH2(REF_temperature))
      H_species_inlet(6) = AUX_inlet_data(6) 
     &     * (Hf_O2 + PEMf_HO2(T_reformer) - PEMf_HO2(REF_temperature))
      H_species_inlet(7) = AUX_inlet_data(7) 
     &     * (Hf_N2 + PEMf_hN2(T_reformer) - PEMf_hN2(REF_temperature))

C.....And do the same for species at outlet
      H_species_outlet(1) = AUX_outlet_data(1) 
     &     * (Hf_H2Ov + PEMf_HSTEAM(T_reformer) 
     &     - PEMf_HSTEAM(REF_temperature))
      IF (Fuel_Type.EQ.1) THEN
         H_species_outlet(2) = AUX_outlet_data(2) 
     &        * (Hf_FUEL + PEMf_HCH4(T_reformer) 
     &        - PEMf_HCH4(REF_temperature))
      ELSEIF (Fuel_Type.EQ.2) THEN
         H_species_outlet(2) = AUX_outlet_data(2) 
     &        * (Hf_FUEL + PEMF_HOCTG(T_reformer) 
     &        - PEMf_HOCTg(REF_temperature))
      ELSEIF (Fuel_Type.EQ.3) THEN
         H_species_outlet(2) = AUX_outlet_data(2) 
     &        * (Hf_FUEL + PEMf_HMETHg(T_reformer) 
     &        - PEMf_HMETHg(REF_temperature))
      END IF
      H_species_outlet(2) = AUX_outlet_data(2) 
     &     * (Hf_CH4 + PEMf_HCH4(T_reformer) 
     &     - PEMf_HCH4(REF_temperature))
      H_species_outlet(3) = AUX_outlet_data(3) 
     &     * (Hf_CO + PEMf_HCO(T_reformer) - PEMf_HCO(REF_temperature))
      H_species_outlet(4) = AUX_outlet_data(4) 
     &     * (Hf_CO2 + PEMf_HCO2(T_reformer) 
     &     - PEMf_HCO2(REF_temperature))
      H_species_outlet(5) = AUX_outlet_data(5) 
     &     * (Hf_H2 + PEMf_HH2(T_reformer) - PEMf_HH2(REF_temperature))
      H_species_outlet(6) = AUX_outlet_data(6)
     &     * (Hf_O2 + PEMf_HO2(T_reformer) - PEMf_HO2(REF_temperature))
      H_species_outlet(7) = AUX_outlet_data(7) 
     &     * (Hf_N2 + PEMf_hN2(T_reformer) - PEMf_hN2(REF_temperature))
C.....Sum inlet and outlet enthalpies
      H_inlet  = 0.
      H_outlet = 0.
      DO  i = 1, 7
         H_inlet  = H_inlet  + H_species_inlet(i)
         H_outlet = H_outlet + H_species_outlet(i)
      ENDDO
C.....Evaluate enthalpy conservation on reformer to calcuate 
C.....exhaust enthalpies 
      Q_aux_burner = H_inlet - H_outlet

      RETURN
      END

C****************************** dHreformer  ************************************
C     
C     This code determines the isothermal enthalpy change that occurs inside
C     the reforner vessel. It's very simple
C
C-------------------------------------------------------------------------------
C     REFERENCES
C
C     FERGUSON, A 2003. Fuel cell modelling for building cogeneration 
C        applications.  MASc Thesis, Dalhousie University
C
C     VAN WYLEN, G. et al. 1994. Fundamentals of classical t
C        thermodynamics. Wiley and Sons.
C     
C------------------------------------------------------------------------------

      SUBROUTINE PEM_dH_reformer(
     &     Reformer_Inlet,      ! input
     &     Reformer_Outlet,     ! input
     &     T_reformer,          ! input 
     &     dH_reformer,         ! output
     &     reformer_heat_loss,  ! input 
     &     T_surroundings,      ! input
     &     Fuel_Type            ! input
     &     )
      IMPLICIT NONE 
C--------------------------------------------------------------------------------
C     Declaration
C--------------------------------------------------------------------------------
      REAL PEM_FCinit
C.....Enthalpy functions
      REAL PEMf_HSTEAM, PEMf_HCH4, PEMf_HCO, PEMf_HCO2, 
     &     PEMf_HH2, PEMf_hN2, PEMf_HO2, PEMf_HMETHg, PEMf_HOCTg
      INTEGER Fuel_Type
      REAL Reformer_Inlet(7), Reformer_Outlet(7)
      REAL hf_H2Ol              ! Enthalpy of formation of liquid water (kj/mol)
      REAL hf_H2Ov              ! Enthalpy of formation of vapor water (kj/mol)
      REAL hf_CH4               ! Enthalpy of formation of methane (kj/kmol)
      REAL hf_CO                ! Enthalpy of formation of carbon monoxide (kj/kmol)
      REAL hf_CO2               ! Enthalpy of formation of carbon dioxide (kj/kmol)
      REAL hf_H2                ! Enthalpy of formation of hydrogen (kj/kmol)
      REAL hf_N2                ! Enthalpy of formation of nitrogen (kj/kmol)
      REAL hf_O2                ! Enthalpy of formation of oxygen (kj/kmol)
      REAL hf_OCT               ! Enthalpy of formation of octane (kj/kmol)
      REAL hf_Meth              ! Enthalpy of formation of methane (kj/kmol)
      REAL hf_FUEL              ! Enthalpy of formation of fuel (kj/kmol)
      REAL T_reformer           ! temperature of reformer (K)
      REAL REF_temperature      ! Reference temperature for enthalpy of formationd data
      REAL T_Surroundings       ! Ambient temperature
      REAL dH_reformer          ! enthalpy change within reformer (kW) 
      REAL reformer_heat_loss   ! reformer heat loss coeff. (kW/K)
      REAL Q_loss               ! reformer heat loss (kW)
C.....species enthalpy change 
      REAL dH_H2O, dH_FUEL, dH_CO, dH_CO2, dH_H2, dH_N2, dH_O2
C.....Get reference temperature for enthalpies of formation
      REF_temperature = PEM_FCinit(14) ! K
C.....enthalpies of formation (kJ/Kmol)
      Hf_H2Ov = PEM_FCinit(16)      ! Water vapor
      Hf_H2Ol = PEM_FCinit(17)      ! liquid water
      Hf_CO   = PEM_FCinit(18)      ! Carbon Monoxide
      Hf_CO2  = PEM_FCinit(19)      ! Carbon Dioxide
      Hf_H2   = PEM_FCinit(20)      ! Hydrogen
      Hf_O2   = PEM_FCinit(21)      ! Oxygen
      Hf_N2   = PEM_FCinit(22)      ! Nitrogen
      Hf_CH4  = PEM_FCinit(23)      ! Methane
      Hf_OCT  = PEM_FCinit(24)      ! Octane
      Hf_Meth = PEM_FCinit(25)      ! Methanol
C.....Calculate heat loss
      Q_Loss = reformer_heat_loss*(T_reformer- T_surroundings) ! (kW)
C.....Caculate enthalpy changes of all species 
      dH_H2O = (Reformer_Outlet(1) - Reformer_Inlet(1)) *
     &     (Hf_H2Ov + PEMf_HSTEAM(T_reformer) 
     &     - PEMf_HSTEAM(REF_temperature))
      IF (Fuel_Type.EQ.1) THEN
C........Methane
         dH_FUEL = (Reformer_Outlet(2) - Reformer_Inlet(2)) *
     &        (Hf_CH4 + PEMf_HCH4(T_reformer) 
     &        - PEMf_HCH4(REF_temperature))
      ELSEIF (Fuel_Type.EQ.2) THEN
C........Octane
         dH_FUEL = (Reformer_Outlet(2) - Reformer_Inlet(2)) *
     &        (Hf_OCT + PEMf_HOCTg(T_reformer) 
     &        - PEMf_HOCTg(REF_temperature))
      ELSEIF (Fuel_Type.EQ.3) THEN
C........Methanol
         dH_FUEL = (Reformer_Outlet(2) - Reformer_Inlet(2)) *
     &        (Hf_Meth + PEMf_HMETHg(T_reformer) 
     &        - PEMf_HMETHg(REF_temperature))
      ENDIF
      dH_CO = (Reformer_Outlet(3) - Reformer_Inlet(3)) 
     &     * (Hf_CO + PEMf_HCO(T_reformer) - PEMf_HCO(REF_temperature))
      dH_CO2 = (Reformer_Outlet(4) - Reformer_Inlet(4)) *
     &     (Hf_CO2 + PEMf_HCO2(T_reformer) - PEMf_HCO2(REF_temperature))
      dH_H2 = (Reformer_Outlet(5) - Reformer_Inlet(5)) 
     &     * (Hf_H2 + PEMf_HH2(T_reformer) - PEMf_HH2(REF_temperature))
      dH_O2 = (Reformer_Outlet(6) - Reformer_Inlet(6)) 
     &     * (Hf_O2 + PEMf_HO2(T_reformer) - PEMf_HO2(REF_temperature))
      dH_N2 = (Reformer_Outlet(7) - Reformer_Inlet(7)) 
     &     * (Hf_N2 + PEMf_hN2(T_reformer) 
     &     - PEMf_hN2(REF_temperature))
      dH_reformer = dH_H2O+ dH_FUEL+ dH_CO+ dH_CO2
     &     + dH_H2+ dH_O2+ dH_N2+ Q_Loss

      RETURN
      END
C******************ENTHALPY FUNCTIONS *********************************


      REAL FUNCTION PEMf_hN2(rTemp)

      a = 28.9
      b = -0.001571
      c = 0.000008081
      d = -0.000000002873
      PEMf_hN2 = a * rTemp + b * rTemp ** 2. / 2. +
     &     c * rTemp ** 3. / 3. + d * rTemp ** 4. / 4.

      END


      REAL FUNCTION PEMf_HO2(Temp)

      a = 25.48
      b = 0.0152
      c = -0.000007155
      d = 0.000000001312
      PEMf_HO2 = a * Temp + b * Temp ** 2. / 2. +
     &     c * Temp ** 3. / 3. + d * Temp ** 4. / 4.

      END

      REAL FUNCTION PEMf_HH2(Temp)

      a = 29.11
      b = -0.001916
      c = 0.000004003
      d = -0.0000000008704
      PEMf_HH2 = a * Temp + b * Temp ** 2. / 2. +
     &     c * Temp ** 3. / 3. + d * Temp ** 4. / 4.

      END


      REAL FUNCTION PEMf_HCO2(Temp)

      a = 22.26
      b = 0.05981
      c = -0.00003501
      d = 0.000000007469
      PEMf_HCO2 = a * Temp + b * Temp ** 2. / 2. +
     &     c * Temp ** 3. / 3. + d * Temp ** 4. / 4.

      END

      REAL FUNCTION PEMf_HCO(Temp)

      a = 28.16
      b = 0.001675
      c = 0.000005372
      d = -0.000000002222
      PEMf_HCO = a * Temp + b * Temp ** 2. / 2. +
     &     c * Temp ** 3. / 3. + d * Temp ** 4. / 4.

      END

      REAL FUNCTION PEMf_hCH4(Temp)

      a = 19.89
      b = 0.05024
      c = 0.00001269
      d = -0.00000001101
      PEMf_HCH4 = a * Temp + b * Temp ** 2. / 2. +
     &     c * Temp ** 3. / 3. + d * Temp ** 4. / 4.
      END

      REAL FUNCTION PEMf_HSTEAM(Temp)

      a = 32.24
      b = 0.001923
      c = 0.00001055
      d = -0.000000003595
      PEMf_HSTEAM = a * Temp + b * Temp ** 2. / 2. +
     &     c * Temp ** 3. / 3. + d * Temp ** 4. / 4.

      END

      REAL FUNCTION PEMf_HOCTg(Temp)
C     Data from G.V. Reklaitis - Introduction to Material and
C     Energy Balances - John Wiley and Sons, 1983

      R = 0.0729
      a = 5.16e01
      b = 2.9556e-01
      c = 9.66806e-04
      d = -1.63e-06
      e = 7.68E-10

      PEMf_HOCTg = a * Temp + b * Temp ** 2. / 2. +
     &     c * Temp ** 3. / 3. + d * Temp ** 4. / 4. +
     &     e * Temp **5. / 5.

      END

      REAL FUNCTION PEMf_HOCTl(Temp)
C     Data from G.V. Reklaitis - Introduction to Material and
C     Energy Balances - John Wiley and Sons, 1983

      R = 0.0729
      a = 3.82405e01
      b = 1.14275
      c = -2.1303e-03
      d = 2.39204e-06

      PEMf_HOCTl = a * Temp + b * Temp ** 2. / 2. +
     &     c * Temp ** 3. / 3. + d * Temp ** 4. / 4.

      END

      REAL FUNCTION PEMf_HMETHl(Temp)
C     Data from G.V. Reklaitis - Introduction to Material and
C     Energy Balances - John Wiley and Sons, 1983

      a = -2.5825e02
      b = 3.35820
      c = -1.16388e-02
      d = 1.40516e-05

      PEMf_HMETHl = a * Temp + b * Temp ** 2. / 2. +
     &     c * Temp ** 3. / 3. + d * Temp ** 4. / 4.

      END


      REAL FUNCTION PEMf_HMETHg(Temp)
C     Data from G.V. Reklaitis - Introduction to Material and
C     Energy Balances - John Wiley and Sons, 1983

      a = 9.80108
      b = 8.43064e-03
      c = 6.66919e-6
      d = -8.202898e-9
      e = 2.05564e-12

      PEMf_HMETHg = a * Temp + b * Temp ** 2. / 2. +
     &     c * Temp ** 3. / 3. + d * Temp ** 4. / 4. +
     &     e * Temp **5. / 5.

      END
C*****************ENTROPY FUNCTIONS  **************************

      REAL FUNCTION PEMf_SN2(Temp,P)
      if (p.eq.0.0) P = 101.32
      Pa = 101.32
      Ru = 8.314

      a = 28.9
      b = -0.001571
      c = 0.000008081
      d = -0.000000002873
      PEMf_SN2 = a * Log(Temp) + b * Temp +
     &     c * Temp ** 2. / 2. + d * Temp ** 3. / 3.
     &     -Ru*log(P/Pa)

      END

      REAL FUNCTION PEMf_SO2(Temp,P)
      if (p.eq.0.0) P = 101.32
      Pa = 101.32
      Ru = 8.314

      a = 25.48
      b = 0.0152
      c = -0.000007155
      d = 0.000000001312
      PEMf_SO2 = a * Log(Temp) + b * Temp +
     &     c * Temp ** 2. / 2. + d * Temp ** 3. / 3.
     &     -Ru*log(P/Pa)

      END

      REAL FUNCTION PEMf_SH2(Temp,P)
      if (p.eq.0.0) P = 101.32
      Pa = 101.32
      Ru = 8.314

      a = 29.11
      b = -0.001916
      c = 0.000004003
      d = -0.0000000008704
      PEMf_SH2 = a * Log(Temp) + b * Temp +
     &     c * Temp ** 2. / 2. + d * Temp ** 3. / 3.
     &     -Ru*log(P/Pa)

      END

      REAL FUNCTION PEMf_SCO2(Temp,P)
      if (p.eq.0.0) P = 101.32
      Pa = 101.32
      Ru = 8.314

      a = 22.26
      b = 0.05981
      c = -0.00003501
      d = 0.000000007469
      PEMf_SCO2 = a * Log(Temp) + b * Temp +
     &     c * Temp ** 2. / 2. + d * Temp ** 3. / 3.
     &     -Ru*log(P/Pa)


      END

      REAL FUNCTION PEMf_SCO(Temp,P)
      if (p.eq.0.0) P = 101.32
      Pa = 101.32
      Ru = 8.314

      a = 28.16
      b = 0.001675
      c = 0.000005372
      d = -0.000000002222
      PEMf_SCO = a * Log(Temp) + b * Temp +
     &     c * Temp ** 2. / 2. + d * Temp ** 3. / 3.
     &     -Ru*log(P/Pa)

      END

      REAL FUNCTION PEMf_SCH4(Temp,P)
      if (p.eq.0.0) P = 101.32
      Pa = 101.32
      Ru = 8.314 

      a = 19.89
      b = 0.05024
      c = 0.00001269
      d = -0.00000001101
      PEMf_SCH4 = a * Log(Temp) + b * Temp +
     &     c * Temp ** 2. / 2. + d * Temp ** 3. / 3.
     &     -Ru*log(P/Pa)

      END

      REAL FUNCTION PEMf_sSTEAM(Temp,P)
      if (p.eq.0.0) P = 101.32
      Pa = 101.32
      Ru = 8.314

      a = 32.24
      b = 0.001923
      c = 0.00001055
      d = -0.000000003595
      PEMf_sSTEAM = a * Log(Temp) + b * Temp +
     &     c * Temp ** 2. / 2. + d * Temp ** 3. / 3.
     &     -Ru*log(P/Pa)

      END

      REAL FUNCTION PEMf_sOCTg(Temp,P)
C     Data from G.V. Reklaitis - Introduction to Material and
C     Energy Balances - John Wiley and Sons, 1983
      if (p.eq.0.0) P = 101.32
      Pa = 101.32
      Ru = 8.314

      R = 0.0729
      a = 5.16e01
      b = 2.9556e-01
      c = 9.66806e-04
      d = -1.63e-06
      e = 7.68E-10

      PEMf_sOCTg = a *log(Temp) + b * Temp + c * Temp ** 2. / 2. +
     &     d * Temp ** 3. / 3. + e * Temp ** 4. / 4. -
     &     Ru * log (P/Pa)

      END

      REAL FUNCTION PEMf_sMETHg(Temp,P)
C     Data from G.V. Reklaitis - Introduction to Material and
C     Energy Balances - John Wiley and Sons, 1983
      if (p.eq.0.0) P = 101.32
      Pa = 101.32
      Ru = 8.314

      a = 9.80108
      b = 8.43064e-03
      c = 6.66919e-6
      d = -8.202898e-9
      e = 2.05564e-12

      PEMf_sMETHg = a *log(Temp) + b * Temp + c * Temp ** 2. / 2. +
     &     d * Temp ** 3. / 3. + e * Temp ** 4. / 4. -
     &     Ru * log (P/Pa)

      END

C************************PEM_Methanol_P ***********************
C     This routine determines the saturation temperature and
C     enthalpy of vapourization of Methynol at a given
C     pressure.
C     
C     Data from Perry's Chemical Engineering Handbook

      SUBROUTINE PEM_Methanol_P(Psat,Tsat,Hfg)

      DIMENSION T(25),P(25), H(25)

      P(1) = 3.33E-02
      P(2) = 6.21E-02
      P(3) = 0.1094
      P(4) = 0.186
      P(5) = 0.3043
      P(6) = 0.4817
      P(7) = 0.7395
      P(8) = 1.0012
      P(9) = 1.1044
      P(10) = 1.6082
      P(11) = 2.288
      P(12) = 3.188
      P(13) = 4.357
      P(14) = 5.845
      P(15) = 7.703
      P(16) = 10.0
      P(17) = 12.83
      P(18) = 16.26
      P(19) = 20.4
      P(20) = 25.3
      P(21) = 31.08
      P(22) = 37.8
      P(23) = 45.61
      P(24) = 54.66
      P(25) = 65.17

      T(1) = 270.0
      T(2) = 280.0
      T(3) = 290.0
      T(4) = 300.0
      T(5) = 310.0
      T(6) = 320.0
      T(7) = 330.0
      T(8) = 337.5
      T(9) = 340.0
      T(10) = 350.0
      T(11) = 360.0
      T(12) = 370.0
      T(13) = 380.0
      T(14) = 390.0
      T(15) = 400.0
      T(16) = 410.0
      T(17) = 420.0
      T(18) = 430.0
      T(19) = 440.0
      T(20) = 450.0
      T(21) = 460.0
      T(22) = 470.0
      T(23) = 480.0
      T(24) = 490.0
      T(25) = 500.0

      h(1) = 1209.2
      h(2) = 1196.0
      h(3) = 1182.2
      h(4) = 1167.3
      h(5) = 1151.4
      h(6) = 1134.4
      h(7) = 1116.2
      h(8) = 1101.4
      h(9) = 1096.4
      h(10) = 1075.5
      h(11) = 1053.1
      h(12) = 1029.1
      h(13) = 1004.9
      h(14) = 971.7
      h(15) = 941.9
      h(16) = 915.1
      h(17) = 886.3
      h(18) = 851.9
      h(19) = 809.7
      h(20) = 759.2
      h(21) = 700.2
      h(22) = 631.7
      h(23) = 552.2
      h(24) = 458.9
      h(25) = 350.6

      DO 10 i=1,24
         IF((Psat.LE.P(i+1)).AND.(Psat.GT.P(i))) THEN
            Tsat=T(i)+(T(i+1)-T(i))*(Psat-P(i))/(P(i+1)-P(i))
            Hfg=(H(i)+(H(i+1)-H(i))*(Psat-P(i))/(P(i+1)-P(i)))*32.04
            GO TO 20
         ENDIF

 10   CONTINUE

 20   RETURN
      END

C     ************************ OCTANE ***********************

C     Data from Perry's Chemical Engineering Handbook
      SUBROUTINE PEM_Octane(Psat,Tsat,Hfg)

      DIMENSION T(20),P(20), H(20)

      Psat = Psat/100.

      P(1) = 0.0014
      P(2) = 0.0061
      P(3) = 0.0207
      P(4) = 0.0575
      P(5) = 0.1384
      P(6) = 0.3
      P(7) = 0.5856
      P(8) = 1.0507
      P(9) = 1.758
      P(10) = 2.79
      P(11) = 4.246
      P(12) = 6.201
      P(13) = 8.75
      P(14) = 12.15
      P(15) = 16.46
      P(16) = 21.98


      T(1) = 260
      T(2) = 280
      T(3) = 300
      T(4) = 320
      T(5) = 340
      T(6) = 360
      T(7) = 380
      T(8) = 400
      T(9) = 420
      T(10) = 440
      T(11) = 460
      T(12) = 480
      T(13) = 500
      T(14) = 520
      T(15) = 540
      T(16) = 560

      h(1) = 386.3
      h(2) = 375.1
      h(3) = 363.8
      h(4) = 350
      h(5) = 336.7
      h(6) = 323.3
      h(7) = 309.6
      h(8) = 294.7
      h(9) = 280.5
      h(10) = 265
      h(11) = 247.2
      h(12) = 228.3
      h(13) = 208.4
      h(14) = 182.6
      h(15) = 150.9
      h(16) = 93.7



      DO 10 i=1,15
         IF((Psat.LE.P(i+1)).AND.(Psat.GT.P(i))) THEN
            Tsat=T(i)+(T(i+1)-T(i))*(Psat-P(i))/(P(i+1)-P(i))
            Hfg=(H(i)+(H(i+1)-H(i))*(Psat-P(i))/(P(i+1)-P(i)))*114.23
            GO TO 20
         ENDIF

 10   CONTINUE

 20   RETURN
      END



