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

C This file contains the following routines:

C tank_intank_hx_coeff_gen    Coefficient generator for a water storage tank with an immersed
C                             heat exchanger
C tank_intank_hx_static       Static matrix checker for tank intank HX component

C *********************************************************************************************
C References:
C
C   Frank Kreith and Mark S. Bohn. Principles of Heat Transfer. 4th Edition. Harper and Row
C   Publishers Inc. 1986.
C
C   Haddad, K. "Tank with submerged heat exchanger model for ESP-r". CETC Internal Report.
C   February 2006.
C
C   Ajele, O.J., 1995, "Natural Convection Heat Transfer from Enclosed Healical Coils", Ph.D.
C   Thesis, Technical University of Nova Scotia, Halifax, Nova Scotia.
C
C   Allen, P.L. and Taherian, H., 1998, "Experimental Study of Natural Convection Shell-and-Coil
C   Heat Exchangers". AIAA/ASME Joint Thermo-Physics and Heat Transfer Conference, Vol. 2 1998.
C *********************************************************************************************

C *********************************************************************************************
C ****************************** tank_intank_hx_coeff_gen *************************************
C Created by: Kamel Haddad
C Initial Creation Date: February 17th, 2006
C Copyright CETC 2006

C This subroutine is the coefficient generator for a water storage tank with a submerged heat
C exchanger. The tank is reprented by node 1 and the heat exchanger is node 2. Heat transfer
C between the tube fluid and the tank fluid is accomplished by natural convection on the outside
C of the tube walls.

C INPUTS:
C     IPCOMP     Component number within the user defined plant component network
C     ISTATS     Flag indicating whether energy balance or water mass balance is sought
C     ICONDX     ESP-r common variable holding number for plant network connections associated
C                with component
C     NPCDAT     ESP-r common variable holding node numbers for plant components within plant
C                network
C     ICSV       ESP-r common variable to indicate whether a node state variable is flagged for
C                iteration within the plant domain
C     CSVI       ESP-r common variable holding value of node state variable flagged for iteration
C                within plant domain
C     ADATA(IPCOMP,1)
C                Mass of the tank water and solids
C     ADATA(IPCOMP,2)
C                Average specific heat of tank water and solids
C     ADATA(IPCOMP,3)
C                Mass of the tube water and solids
C     ADATA(IPCOMP,4)
C                Specific heat of tube water and solids
C     ADATA(IPCOMP,5)
C                UA-value to the environment of the tank
C     ADATA(IPCOMP,6)
C                Total length of tube
C     ADATA(IPCOMP,7)
C                Inside tube diameter
C     ADATA(IPCOMP,8)
C                Tube outside diameter
C     ADATA(IPCOMP,9)
C                Tube wall thermal conductivity
C     ADATA(IPCOMP,10)
C                Type of coil
C                =1 use correlations for free convection for horizontal tubes to estimate
C                heat transfer on the tank side. Use correlations for straight pipe with
C                forced convection flow
C                =? add more cases in the future
C     CSVP       ESP-r common variable for present time state variables for nodes in plant
C                network
C     CSVF       ESP-r common variable for future time state variables for nodes in plant network
C     CONVAR     ESP-r common variable for state variables of plant node incoming flows
C     PCONDR     ESP-r common variable for fraction of flows from incoming connections for node
C                that are actually delivered to node in question
C     IMPEXP     ESP-r state variable equation indicating type of explicit/implicit finite
C                difference equation weighting to be used
C     RATIMP     ESP-r common varialbe for user specified weighting for explicit/implicit
C                weighting to be used for finite difference solution
C     TIMSEC     ESP-r common variable for plant time step in seconds
C     PCRP       ESP-r common variable for mass flow rate specific heat product for a certain
C                node connection for present time step
C     PCTP       ESP-r common variable for temperature for a certain node connection for present
C                time step
C     PCNTMP     ESP-r common variable for temperature of containment environment for component
C                for present time step
C     PCNTMF     ESP-r common variable for temperature of containment environment for component
C                for present time step
C     PCQP       ESP-r common variable for heat injection for node for present time step

C OUTPUTS:
C     COUT       List of matrix coefficients for tank and in tank heat exchanger
C     PCOUT(1)   Total heat transfer between heat exchanger node and tank node
C     PCOUT(2)   Heat transfer coefficient on the tube side of heat exchanger
C     PCOUT(3)   Heat transfer coefficient on the tank side
C     PCOUT(4)   Total heat transfer coefficient based on the inside heat transfer area of the
C                tube
C -------------------------------------------------------------------------------------------
      SUBROUTINE tank_intank_hx_coeff_gen(IPCOMP,COUT,ISTATS)
      IMPLICIT NONE
#include "plant.h"
#include "building.h"

C----------------------------------------------------------------------
C     Passed arguments
C----------------------------------------------------------------------
      INTEGER IPCOMP        ! Number for plant component
      REAL COUT(MPCOE)      ! Array for matrix coefficients for component
      INTEGER ISTATS        ! Flag indicating type of coefficients to be
                            ! generated (=1: energy balance; =2 1st phase
                            ! water mass blance; =3 2nd phase vapour mass
                            ! balance)

C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------
C-----Trace/error unit reporting numbers
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER IUOUT,IUIN,IEOUT
      INTEGER ITC,ICNT
      INTEGER ITCF,ITRACE,IZNTRC,ITU

C-----Plant component connection data
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR    ! States variables for connections
      INTEGER ICONTP ! Type of each connection
      INTEGER ICONDX ! Pointer to connections for each
                     ! component node

C-----Network and control data
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      INTEGER NPCDAT ! Miscelanous plant data
      INTEGER IPOFS1
      INTEGER IPOFS2

C-----Plant iteration data
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)
      INTEGER MAXITP
      INTEGER ITRCLP
      INTEGER ICSV              ! flag marking nodes for iteration
      REAL PERREL
      REAL PERTMP
      REAL PERFLX
      REAL PERMFL
      REAL CSVI                 ! 'initial' values of state variables
                                ! at start of iteration

C-----Simulation time steps
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,idynow
      INTEGER ihrp
      INTEGER ihrf
      INTEGER idyp
      INTEGER idyf
      INTEGER idwp
      INTEGER idwf
      INTEGER nsinc             ! time step number
      INTEGER its,idynow

C-----Common for implicit/explcit handling of plant equations
      COMMON/PCEQU/IMPEXP,RATIMP
      INTEGER IMPEXP            ! identifies implicit/explicit handling of plant
      REAL RATIMP            ! user-specified implicit weighting fraction

C-----Common for plant side simulation time step
      COMMON/Pctime/TIMSEC
      REAL TIMSEC               ! Plant side simulation time step

C-----Common for component time constant
      COMMON/PCTC/TC(MPCOM)
      REAL TC                   ! Component time constant

C-----Common for component data
      COMMON/PDBDT/ADATA(MPCOM,MADATA),BDATA(MPCOM,MBDATA)
      REAL ADATA                ! Component data
      REAL BDATA

C-----Common block for node state variables
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      REAL CSVF                 ! Future values of node state variables
      REAL CSVP                 ! Present values of node state variables

C-----Common for various coefficients appearing in finite difference form
C-----of energy balance equations
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      REAL PCTF                 ! Future temperature of sending node
      REAL PCRF                 ! Future time row product of incoming
                                ! flow mass flow x specific heat
      REAL PUAF                 ! Future time row UA value for node to the
                                ! surrounding environment
      REAL PCQF                 ! Future time row for heat input to node
      REAL PCNTMF               ! Future time row for containment temperature
                                ! for node
      REAL PCTP                 ! Present time row temperature of sending node
      REAL PCRP                 ! Present time row product of incoming
                                ! flow mass flow x specific heat
      REAL PUAP                 ! Present time row UA value for node to the
                                ! surrounding environment
      REAL PCQP                 ! Present time row for heat input to node
      REAL PCNTMP               ! Present time row for containment temperature
                                ! for node

C-----Common containing ratio for flow rate diversions
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      INTEGER NPCON,IPC1,IPN1,IPCT,IPC2,IPN2
      REAL PCONDR               ! Flow rate diversions
      REAL PCONSD

C-----Common containing additional output parameters for results analysis
      COMMON/PCRES/QDATA(MPCOM),PCAOUT(MPCOM,MPCRES),napdat(mpcom)
      REAL QDATA,PCAOUT
      INTEGER napdat

C-----Common contaning total number of plant components in user defined
C-----plant network
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA

C----------------------------------------------------------------------
C     ESP-r functions
C----------------------------------------------------------------------
      REAL glycol_water_density    ! Function for density of glycol-water mixture
      REAL glycol_water_viscosity  ! Function for viscosity of glycol-water mixture
      REAL glycol_water_tboil      ! Function for boiling temperature of glycol-water mixture
      REAL glycol_water_tfreeze    ! Function for freezing point temperature of glycol-water mixture
      REAL glycol_water_cond       ! Function for thermal conductivity of glycol
      REAL glycol_water_cp         ! Function for glycol-water specific heat
      REAL glycol_vol_fraction_find ! Function for finding volume fraction of glycol inside
                                   ! water-glycol mixture

C----------------------------------------------------------------------
C     Local variables and commons
C----------------------------------------------------------------------
      COMMON/tank_intank_hx/total_heat_transfer,tank_UAvalue,
     &                      tube_side_hi,tank_side_ho,
     &                      intank_coil_total_Uvalue,cont_temp
      INTEGER tank_node_conn       ! Connection number for tank node
      INTEGER tank_node_num        ! Node number for tank node
      INTEGER intank_hx_node_conn  ! Connection number for in-tank HX
      INTEGER intank_hx_node_num   ! Node number for in-tank HX node
      REAL tank_mass               ! Mass of the tank water and solids
      REAL tank_specific_heat      ! Average specific heat of tank
                                   ! water and solids
      REAL tube_mass               ! Mass of the tube water and solids
      REAL tube_specific_heat      ! Specific heat of tube water and solids
      REAL tube_out_dia            ! Outside tube diameter
      REAL tube_in_dia             ! Inside tube diameter
      REAL tube_total_length       ! Total length of tube
      INTEGER type_coil            ! Type of in-tank coil
      REAL tank_fluid_density      ! Density of fluid inside tank
      REAL tube_fluid_density      ! Density of fluid inside tubes
      REAL tank_fluid_temp_degC    ! Temperature of fluid inside tank (Deg C)
      REAL tank_fluid_temp_degK    ! Temperature of fluid inside tank (Deg K)
      REAL tank_fluid_cp           ! Specific heat of fluid inside tank
      REAL tank_fluid_beta         ! Coefficient of thermal expansion of fluid inside tank
      REAL tank_fluid_thermal_cond ! Tank fluid thermal conductivity
      REAL tank_fluid_vis          ! Tank fluid dynamic viscosity
      REAL tube_inlet_temp         ! Tube inlet temperature
      REAL tube_exit_temp          ! Tube exit temperature
      REAL tube_mean_temp          ! Tube mean temperature
      REAL A                       ! Constant A in tank side heat transfer coeff equation
      REAL gravity_const           ! Acceleration of gravity
      REAL tank_side_ho            ! Tank side heat transfer coefficient
      REAL tube_side_mass_flowrate ! Tube side mass flow rate
      REAL reynolds_number         ! Tube side Reynolds Number of flow
      REAL tube_fluid_vis          ! Tube fluid dynamic viscosity
      REAL tube_side_hi            ! Tube side heat transfer coefficient
      REAL tube_fluid_prandtl      ! Tube side fluid Prandtl Number
      REAL tube_fluid_cp           ! Tube side fluid specific heat
      LOGICAL close                ! Logical variable used to check how close a number is
                                   ! to another number
      REAL tube_wall_Rvalue        ! Thermal resistance of tube wall
      REAL intank_coil_total_Uvalue ! Total U value of the in tank HX
      REAL tube_wall_thermal_cond  ! Thermal conductivity of tube wall
      REAL total_heat_transfer     ! Total heat transfer between tank fluid and
                                   ! tube fluid
      REAL tank_UAvalue            ! U-value of tank to the environment
      REAL tank_mass_cp_product    ! Tank mass and specific heat product
      REAL tube_mass_cp_product    ! Tube mass and specific heat product
      REAL tank_inflow_temp        ! Tank incoming flow temperature
      REAL tube_inflow_cp          ! Tube incoming flow cp
      REAL tube_mdot_cp_product    ! Mass flow rate and specific heat product of tube
                                   ! incoming flow
      REAL tank_mdot_cp_product    ! Mass flow rate and specific heat product of incoming
                                   ! flow
      REAL alpha                   ! Weighting factor for implicit/explcit handling of plant
                                   ! equations
      REAL small                   ! small real value
      REAL fluid_temp_diff         ! temperature difference between tank fluid
                                   ! and tube fluid
      REAL tank_mdot_cp_present    ! Tank node mass flow rate and cp product for incoming flow
                                   ! for present time step
      REAL tank_fluid_temp_present ! Tank node present fluid temperature
      REAL tank_inflow_temp_present ! Tank node incoming flow temperature
      REAL cont_temp_present       ! Present value temperature of the containment of the tank
      REAL cont_temp               ! Future value of the containment temperature of the tank
      REAL heat_transfer_present   ! Present value of the heat transfer between the coil and the tank
      REAL tube_mdot_cp_present    ! Present time row value of tube mass flow rate x specific heat of
                                   ! incoming flow
      REAL tube_fluid_temp_present ! Present time row value of tube temperature
      REAL tube_inflow_temp_present ! Present time row value of tube incoming flow temperature
      INTEGER num_matrix_coeff     ! Number of Matrix coefficient
      INTEGER I                    ! Counter
      REAL tube_fluid_thermal_cond ! Tube fluid thermal conductivity
      REAL tank_inflow_cp          ! Tank incoming flow cp value
      INTEGER IX1
      REAL tank_time_constant      ! Time constant of tank node
      REAL tube_time_constant      ! Time constant of tube node
      REAL tube_glycol_mass_fraction
                                   ! Glycol mass fraction in HX incoming flow
      REAL tboil                   ! Boiling temperature of glycol-water mixture
      REAL tfreeze                 ! Freezing temperature of glycol-water mixture
      REAL tube_temp               ! Mean fluid temperature inside tube accounting for lower and upper
                                   ! bounds
      REAL tube_glycol_vol_fraction
                                   ! Volume fraction of glycol inside water-glycol mixture
      REAL coil_dia                ! Diameter of helical coil
      REAL tube_radius             ! Radius of coil tube
      REAL coil_radius             ! Radius of coil
      REAL dean_number             ! Dean Number for flow inside tube
      REAL critical_reynolds       ! Critical Reynolds number for transition from laminar to turbulent flow inside tube
      REAL X3                      ! Constant in correlation equation for Nusselt Number inside helical coil
      REAL X4                      ! Constant in correlation equation for Nusselt Number inside helical coil
      REAL shell_dia               ! Inside diameter of tank containing coil
      REAL coil_height             ! Coil height
      REAL wetted_area             ! Wetted area for coil/shell heat exchanger
      REAL coil_in_dia             ! Inside diameter of coil
      REAL coil_out_dia            ! Outside diameter of coil
      REAL equiv_dia               ! Equivalent diameter defined to find hydraulic diameter of coil HX
      REAL cross_sec_area          ! Cross sectional area for coil/shell HX
      REAL coil_hyd_dia            ! Hydraulic diamter used for Nusselt Number correlation for coil HX
      REAL rayleigh_number         ! Rayleigh Number for natural convection flow outside of coil
      REAL tank_fluid_prandtl      ! Prandtl Number of tank fluid
      REAL tank_fluid_dyn_vis      ! Dynamic viscosity of tank fluid
      REAL tube_mean_temp_heat_trans
                                   ! Mean temperature inside tube used for heat transfer calculations
                                   ! between fluids inside tube and tank
      REAL delta_temp_heat_trans   ! Temperature difference used for heat transfer calculations
      REAL hx_eff                  ! Effectivenss of HX
      REAL tank_side_mass_flowrate ! Mass flow rate on tank side
      REAL tank_glycol_mass_fraction
                                   ! Mass fraction of the glycol on tank side
      REAL tank_glycol_vol_fraction
                                   ! Volume fraction of glycol on tank side

C ----Trace output
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) WRITE(ITU,*)
     &   ' Entering subroutine tank_intank_hx_coeff_gen'

C ----Small real value
      small = 1e-15

C-----Acceleration of gravity
      gravity_const = 9.8

C ----Initialize pointers to the 2 connections to the tank node and in-tank
C ----heat exchanger node. Tank node is node 1 and tube node is node 2.
      tank_node_conn = ICONDX(IPCOMP,1,1)
      intank_hx_node_conn = ICONDX(IPCOMP,2,1)
      tank_node_num = NPCDAT(IPCOMP,9)
      intank_hx_node_num = NPCDAT(IPCOMP,9) + 1

C ----Heat transfer from in-tank HX is based on temperatures so mark 1st and
C ----2nd nodal temps and flows for iteration.
      ICSV(tank_node_num,1) = 1
      CSVI(tank_node_num,1) = CSVF(tank_node_num,1)
      ICSV(intank_hx_node_num,1) = 1
      CSVI(intank_hx_node_num,1) = CSVF(intank_hx_node_num,1)
      ICSV(tank_node_num,2) = 1
      CSVI(tank_node_num,2) = CSVF(tank_node_num,2)
      ICSV(intank_hx_node_num,2) = 1
      CSVI(intank_hx_node_num,2) = CSVF(intank_hx_node_num,2)
      ICSV(tank_node_num,3) = 1
      CSVI(tank_node_num,3) = CSVF(tank_node_num,3)
      ICSV(intank_hx_node_num,3) = 1
      CSVI(intank_hx_node_num,3) = CSVF(intank_hx_node_num,3)

C ----Component variables

C ----Mass of the tank water and solids
      tank_mass = ADATA(IPCOMP,1)
C ----Average specific heat of tank water and solids
      tank_specific_heat = ADATA(IPCOMP,2)
C ----Mass of the tube water and solids
      tube_mass = ADATA(IPCOMP,3)
C ----Specific heat of tube water and solids
      tube_specific_heat = ADATA(IPCOMP,4)
C ----UA-value to the environment of the tank
      tank_UAvalue = ADATA(IPCOMP,5)
C ----Total length of tube
      tube_total_length = ADATA(IPCOMP,6)
C ----Inside tube diameter
      tube_in_dia = ADATA(IPCOMP,7)
C ----Tube outside diameter
      tube_out_dia = ADATA(IPCOMP,8)
C ----Tube wall thermal conductivity
      tube_wall_thermal_cond = ADATA(IPCOMP,9)
C ----Diameter for helical coil
      coil_dia = ADATA(IPCOMP,10)
C ----Height of coil
      coil_height = ADATA(IPCOMP,11)
C ----Shell inside dimater
      shell_dia = ADATA(IPCOMP,12)
C ----Type of coil
      type_coil = int(ADATA(IPCOMP,13))

C ----Make sure that containment is specified for tank
      CALL ECLOSE(PCNTMF(IPCOMP),-99.0,0.001,CLOSE)
      IF(CLOSE)THEN
        WRITE(IUOUT,*) ' tank_intank_hx_coeff_gen: '
        WRITE(IUOUT,*) ' the storage tank must be contained within '
        WRITE(IUOUT,*) ' a room.'
        STOP ' tank_intank_hx_coeff_gen: unresolvable error'
      ENDIF

C ----Generate coefficients for energy balance equation
      IF(ISTATS.EQ.1) THEN


C ------Thermal resistance of tube wall
        tube_wall_Rvalue = tube_in_dia *
     &                log(tube_out_dia / tube_in_dia) /
     &                (2 * tube_wall_thermal_cond)

C ------Tank fluid properties

C ------Tank side mass flow rate: sum of water and secondary fluid
C ------mass flow rates such as glycol
        tank_side_mass_flowrate = PCONDR(tank_node_conn) *
     &                            (CONVAR(tank_node_conn,2) +
     &                             CONVAR(tank_node_conn,3))

C ------Set mass fraction of glycol in incoming flow to tank. This is a %
C ------for glycol-water mixture properties functions use % for volume fraction
        tank_glycol_mass_fraction = 100 * CONVAR(tank_node_conn,3) /
     &                         (tank_side_mass_flowrate + small)

C ------Temperature of fluid inside tank (Deg C)
        tank_fluid_temp_degC = CSVF(tank_node_num,1)

C-------Estimate volume fraction of glycol on tank side based on mass fraction
        tank_glycol_vol_fraction =
     &  glycol_vol_fraction_find(tank_glycol_mass_fraction,
     &                           tank_fluid_temp_degC)

C-------Boiling point of glycol-water mixture inside tank
        tboil = glycol_water_tboil(tank_glycol_vol_fraction)
C-------Freezing point of glycol-water mixture inside tank
        tfreeze = glycol_water_tfreeze(tank_glycol_vol_fraction)
C-------Set value for temperature inside tube using lower and upper bounds
        tank_fluid_temp_degC = AMAX1(tfreeze,AMIN1(tboil,
     &  tank_fluid_temp_degC))
C ------Temperature of fluid inside tank (Deg K)
        tank_fluid_temp_degK = tank_fluid_temp_degC + 273.15

C ------Density of fluid inside tank
        tank_fluid_density =
     &  glycol_water_density(tank_glycol_vol_fraction,
     &  tank_fluid_temp_degC)
C ------Specifc heat of fluid inside tank
        tank_fluid_cp =
     &  glycol_water_cp(tank_glycol_vol_fraction,tank_fluid_temp_degC)
C ------Coefficient of thermal expansion of fluid inside tank
        tank_fluid_beta = 1 / tank_fluid_temp_degK
C ------Thermal conductivity
        tank_fluid_thermal_cond =
     &  glycol_water_cond(tank_glycol_vol_fraction,tank_fluid_temp_degC)
C ------Dynamic viscosity of fluid inside tank
        tank_fluid_vis =
     &  glycol_water_viscosity(tank_glycol_vol_fraction,
     &  tank_fluid_temp_degC)
C ------Tank fluid Prandtl Number
        tank_fluid_prandtl =
     &      tank_fluid_vis * tank_fluid_cp / tank_fluid_thermal_cond
C -----Tank fluid kinematic viscosity
        tank_fluid_dyn_vis = tank_fluid_vis / tank_fluid_density

C ------Tube fluid properties

C ------Tube side mass flow rate: sum of water and secondary fluid
C ------mass flow rates such as glycol
        tube_side_mass_flowrate = PCONDR(intank_hx_node_conn) *
     &                            (CONVAR(intank_hx_node_conn,2) +
     &                             CONVAR(intank_hx_node_conn,3))

C ------Set mass fraction of glycol in incoming flow to HX. This is a %
C ------as glycol-water mixture properties function use % for volume fraction
        tube_glycol_mass_fraction =
     &                         100 * CONVAR(intank_hx_node_conn,3) /
     &                         (tube_side_mass_flowrate + small)

C-------Check first that there is flow through the tube
        CALL ECLOSE(tube_side_mass_flowrate,0.0,0.0001,CLOSE)

C-------If there is flow, then set tube inlet fluid temperature to
C-------temperature of the incoming flow
        IF(.NOT.CLOSE) THEN
          tube_inlet_temp = CONVAR(intank_hx_node_conn,1)
C-------In the case there is no flow, fluid inlet temperature is
C-------set to the same as exit temperature
        ELSEIF(CLOSE) THEN
          tube_inlet_temp = CSVF(intank_hx_node_num,1)
        ENDIF

C-------Temperature of fluid leaving tube
        tube_exit_temp = CSVF(intank_hx_node_num,1)

C-------Mean tube side fluid temperature
        tube_mean_temp = 0.5 * (tube_inlet_temp + tube_exit_temp)

C-------Estimate volume fraction of glycol based on mass fraction
        tube_glycol_vol_fraction =
     &  glycol_vol_fraction_find(tube_glycol_mass_fraction,
     &  tube_mean_temp)

C-------Boiling point of glycol-water mixture
        tboil = glycol_water_tboil(tube_glycol_vol_fraction)

C-------Freezing point of glycol-water mixture
        tfreeze = glycol_water_tfreeze(tube_glycol_vol_fraction)

C-------Set value for mean temperature inside tube using lower and upper bounds
        tube_temp = AMAX1(tfreeze,AMIN1(tboil,tube_mean_temp))

C ------Density of fluid inside tubes based on mean fluid temperature
        tube_fluid_density =
     &  glycol_water_density(tube_glycol_vol_fraction,tube_temp)

C ------Dynamic viscosity of fluid on tube side
        tube_fluid_vis =
     &  glycol_water_viscosity(tube_glycol_vol_fraction,tube_temp)

C ------Thermal conductivity of tube side fluid
        tube_fluid_thermal_cond =
     &  glycol_water_cond(tube_glycol_vol_fraction,tube_temp)

C ------Specific heat of tube side fluid at mean temperature
        tube_fluid_cp =
     &  glycol_water_cp(tube_glycol_vol_fraction,tube_temp)

C ------Prandtl Number of fluid inside tubes
        tube_fluid_prandtl = tube_fluid_vis * tube_fluid_cp /
     &                       tube_fluid_thermal_cond

C -----Set fluid mean temperature based on steady-state solution of heat
C -----transfer between tube and tank fluids
       tube_mean_temp_heat_trans = tank_fluid_temp_degC +
     & tube_side_mass_flowrate * tube_fluid_cp * (tube_inlet_temp -
     & tank_fluid_temp_degC) * (1. - exp(-intank_coil_total_Uvalue *
     & (3.14159 * tube_in_dia) * tube_total_length /
     & (tube_side_mass_flowrate * tube_fluid_cp + small))) /
     & (intank_coil_total_Uvalue * (3.14159 * tube_in_dia) *
     &  tube_total_length + small)


C------------------------------------------------------------------------
C The present formulation uses a steady-state analysis estimate of the
C effectiveness of the heat transfer between the tube fluid and the tank fluid.
C The model then is intended to be used with a tube heat exchanger mass that
C is negligible.
C If a larger mass of HX tube is used, then it is possible to over-predict the
C heat transfer between the tube and tank fluids. In this case the tube exit
C fluid temperature can be greater than tank fluid temperature when tube fluid
C is being heated. Similarily the tube fluid exit temperature can be less than
C tank fluid temperature when tube fluid is being cooled. Both of these
C conditions are not physically possible.
C The following conditionals are intended to capture these situations when they
C happen.
C------------------------------------------------------------------------

C------Check whether tank temperature is greater than tube exit temperature
C -----when tube fluid is cooled. If this is the case, issue message
C      diff = tank_fluid_temp_degC - tube_exit_temp
C      IF((tube_inlet_temp.GT.tank_fluid_temp_degC).AND.
C     &  (diff.GT.0.1).AND.(BPLTCON)) THEN
C        WRITE(IUOUT,*) ' tank_intank_hx_coeff_gen: '
C        WRITE(IUOUT,*) ' Tube exit temperature is less than tank '
C        WRITE(IUOUT,*) ' temperature. Make sure that small mass is '
C        WRITE(IUOUT,*) ' used for heat exchanger inside tank. When '
C        WRITE(IUOUT,*) ' a smaller HX mass is used, a smaller time '
C        WRITE(IUOUT,*) ' step might have to be used to get '
C        WRITE(IUOUT,*) ' convergence. '
C      ENDIF

C------Check whether tank temperature is less than tube exit temperature
C -----when tube fluid is heated. If this is the case, issue message
C      diff = tube_exit_temp - tank_fluid_temp_degC
C      IF((tube_inlet_temp.LT.tank_fluid_temp_degC).AND.
C     &  (diff.GT.0.1).AND.(BPLTCON)) THEN
C        WRITE(IUOUT,*) ' tank_intank_hx_coeff_gen: '
C        WRITE(IUOUT,*) ' Tube exit temperature is greater than '
C        WRITE(IUOUT,*) ' tank temperature. Make sure that small '
C        WRITE(IUOUT,*) ' mass is used for heat exchanger inside tank '
C        WRITE(IUOUT,*) ' When a smaller HX mass is used, a smaller '
C        WRITE(IUOUT,*) ' time step might have to be used to get '
C        WRITE(IUOUT,*) ' convergence. '
C      ENDIF

C ------Check that there is flow through the tubes
        CALL ECLOSE(tube_side_mass_flowrate,0.,0.0001,close)

C ------Set temperature difference for Rayleigh Number calculations
C ------If there is flow through the tube, then use mean temperature of fluid
C ------inside tube based on steady-state calculations: tube_mean_temp_heat_trans
        IF(.NOT.close) THEN
          delta_temp_heat_trans = abs(tube_mean_temp_heat_trans -
     &                              tank_fluid_temp_degC)
C ------If there is no flow through the tube, then use tube_mean_temp
C ------This is needed because expression used for tube_mean_temp_heat_trans
C ------will always give a temperature diffence of 0. when there is no flow
        ELSEIF(close) THEN
          delta_temp_heat_trans = abs(tube_mean_temp -
     &                              tank_fluid_temp_degC)
        ENDIF

C -----Depending on the type of the coil, use different tank side and
C -----tube side heat transfer coefficient correlations:
C      type_coil = 1    correlation for natural convection for horizontal
C                       tube for tank side
C                       correlation for forced convection for straight pipe
C                       for tube side
        IF(type_coil.eq.1) THEN

C --------Constant A in tank-side heat transfer coeff equation
          A = gravity_const * tank_fluid_beta * tank_fluid_cp *
     &        tank_fluid_density**2 /
     &        (tank_fluid_vis * tank_fluid_thermal_cond)

C --------Tank side heat transfer coeff
          tank_side_ho = 0.53 * tank_fluid_thermal_cond *
     &    A**0.25 * delta_temp_heat_trans**0.25 / tube_out_dia**0.25

C --------Helical coil
C --------Outside Nusselt number correlation is based on equation 11 in
C --------paper by Allen 1998
        ELSEIF(type_coil.eq.2) THEN
          wetted_area = 3.14159 * tube_out_dia * tube_total_length +
     &                  3.14159 * shell_dia * coil_height
          coil_in_dia = coil_dia - tube_out_dia
          coil_out_dia = coil_dia + tube_out_dia
          equiv_dia = sqrt(coil_out_dia**2 - coil_in_dia**2)
          cross_sec_area = 3.14159 * (shell_dia**2 - equiv_dia**2) / 4.
          coil_hyd_dia = 4. * cross_sec_area * coil_height / wetted_area
          rayleigh_number = tank_fluid_beta * gravity_const *
     &           delta_temp_heat_trans *
     &           coil_hyd_dia**3 * tank_fluid_Prandtl *
     &           coil_height / (tank_fluid_dyn_vis**2 *
     &           tube_total_length)
          tank_side_ho = 0.182 * rayleigh_number**0.394 *
     &                   tank_fluid_thermal_cond / coil_hyd_dia

C --------In the case that the Rayleigh number is less than 2e3 and greater than 2e7
C --------Then issue warning indicating that correlation is being used outside of
C --------intended range
            IF((rayleigh_number.LT.2e3).OR.(rayleigh_number.GT.2E7))
     &      THEN
C        WRITE(IUOUT,*) ' tank_intank_hx_coeff_gen: '
C        WRITE(IUOUT,*) ' Rayleigh Number = ', rayleigh_number
C        WRITE(IUOUT,*) ' Rayleigh Number for natural convection flow'
C        WRITE(IUOUT,*) ' outside of coil is outside of range for'
C        WRITE(IUOUT,*) ' Nusselt Number correlation'
            ENDIF

        ENDIF

C ------Reynold's number for flow inside pipe
        reynolds_number = 4. * tube_side_mass_flowrate /
     &               (3.14159 * tube_in_dia * tube_fluid_vis)

C ------In the case there is flow through the tubes
        IF(.NOT.close) THEN

C --------In case of straight tube
          IF(type_coil.EQ.1) THEN

C ----------If the Reynolds Number is less than 2300, flow is laminar and
C ----------Nusselt number is constant = 3.66 for constant wall temperature
C ----------boundary condition
            IF(reynolds_number.LE.2300.) THEN

              tube_side_hi = 3.66 * tube_fluid_thermal_cond /
     &                       tube_in_dia

C ----------If Reynolds Number > 2300., then assume turbulent flow and use the Dittus-
C ----------Boelter equation
            ELSE

C ------------If the fluid inside the tubes is being heated
              IF(tube_exit_temp.LE.tank_fluid_temp_degC) THEN

                tube_side_hi = 0.023 * reynolds_number**0.8 *
     &                         tube_fluid_prandtl**0.4 *
     &                         tube_fluid_thermal_cond / tube_in_dia

C ------------If the fluid inside the tubes is being cooled
              ELSEIF(tube_exit_temp.GT.tank_fluid_temp_degC) THEN

                tube_side_hi = 0.023 * reynolds_number**0.8 *
     &                         tube_fluid_prandtl**0.3 *
     &                         tube_fluid_thermal_cond / tube_in_dia

              ENDIF

            ENDIF

C --------In the case of a helical type coil
          ELSEIF(type_coil.eq.2) THEN

C ----------Tube radius
            tube_radius = tube_in_dia / 2.
C ----------Helix radius
            coil_radius = coil_dia / 2.
C ----------Dean Number
            dean_number = reynolds_number *
     &                    (tube_radius / coil_radius)**0.5
C ----------Critical reynolds number for transition from laminar to turbulent flow
C ----------This is based on equation 2.7.4 in thesis by Ajele 1995
            critical_reynolds =
     &         2100. * (1. + 12. * (tube_radius / coil_radius)**0.5)
C ----------Check whether there is a turbulent flow inside the tube. If it is
C ----------then issue warning to indicate that flow is turbulent when
C ----------correlation used is for laminar flow
            IF(reynolds_number.GT.critical_reynolds) THEN
C        WRITE(IUOUT,*) ' tank_intank_hx_coeff_gen:'
C        WRITE(IUOUT,*) ' Reynolds Number = ',reynolds_number
C        WRITE(IUOUT,*) ' Critical Reynolds Number = ',critical_reynolds
C        WRITE(IUOUT,*) ' Flow inside tube is turbulent. Heat transfer'
C        WRITE(IUOUT,*) ' correlation used is for laminar flow '
            ENDIF
C ----------Calculate Nusselt number inside tube based on equation 2.7.8 in Thesis
C ----------by Ajele
            X3 = (1. + 1342. / (dean_number**2 * tube_fluid_prandtl))**2
            X4 = 1. + 1.15 / tube_fluid_prandtl
            tube_side_hi = ((4.364 + 4.636 / X3)**3 +
     &                       1.816 * (dean_number / X4)**1.5)**0.333 *
     &                       tube_fluid_thermal_cond / tube_in_dia

          ENDIF

C ------In the case there is no flow through the tubes
        ELSEIF(close) THEN

C --------In this case the inside heat transfer coefficient is based on
C --------conduction heat trasnfer
          tube_side_hi = 2 * tube_fluid_thermal_cond / tube_in_dia

C ------End of IF construct to check whether there is flow inside tubes to
C ------determine inside tube heat transfer transfer coefficient
        ENDIF

C ------Total heat transfer coefficient of in-tank HX. Based on inside surface
C-------area of tube available for heat transfer
        intank_coil_total_Uvalue = 1. / (1. / (tube_side_hi+small) +
     &                    tube_wall_Rvalue + 1. / (tank_side_ho+small))

C ------Effectiveness of HX
        hx_eff = 1. - exp(-intank_coil_total_Uvalue *
     &        (3.14159 * tube_out_dia) * tube_total_length /
     &        (tube_side_mass_flowrate * tube_fluid_cp + small))

C ------Total heat transfer between tube fluid and tank fluid
C ------If there is flow through tubes use effectiveness relation
C ------to find heat transfer
       IF(.NOT.close) THEN
           total_heat_transfer = hx_eff * tube_side_mass_flowrate *
     &                          tube_fluid_cp * (tube_inlet_temp -
     &                          tank_fluid_temp_degC)

C ----When there is no flow through the tubes, use tube_mean_temp to find
C ----heat transfer
        ELSEIF(close) THEN
          total_heat_transfer = intank_coil_total_Uvalue * 3.14159 *
     &                          tube_in_dia * tube_total_length *
     &                         (tube_mean_temp - tank_fluid_temp_degC)
        ENDIF

C ------Specific heat mass product for tank node
        tank_mass_cp_product = tank_mass * tank_specific_heat

C ------Tube side mass and specific heat product
        tube_mass_cp_product = tube_mass * tube_specific_heat

C ------Temperature of incoming flow to the tank
        tank_inflow_temp = CONVAR(tank_node_conn,1)

C ------Specific heat of incoming flow to the tank
        tank_inflow_cp =
     &  glycol_water_cp(tank_glycol_vol_fraction,tank_inflow_temp)

C ------Mass flow rate and specific heat product of tank node incoming flow
        tank_mdot_cp_product = tank_side_mass_flowrate * tank_inflow_cp

C ------Tube inlet flow specific heat
        tube_inflow_cp = glycol_water_cp(tube_glycol_vol_fraction,
     &                                   tube_inlet_temp)

C ------Mass flow rate and specific heat product of tube side incoming flow
        tube_mdot_cp_product = tube_side_mass_flowrate *
     &                         tube_inflow_cp

C ------Difference in temperature between tube side fluid and tank side
C ------fluid. Used in the definition of the component time-constant.
C ------"small" is added to avoid division by 0. later in the time constant
C ------equation.
        fluid_temp_diff =
     &  AMAX1(abs(tank_fluid_temp_degC -
     &        tube_mean_temp_heat_trans),small)

C ------Time constant of component taken as the max of the two time constants
C ------for the tube side and the tank side

C ------Tank node time constant
        tank_time_constant =
     &  tank_mass_cp_product / AMAX1(small,(tank_mdot_cp_product +
     &  abs(total_heat_transfer) / fluid_temp_diff +
     &  tank_UAvalue))

C ------Tube node time constant
        tube_time_constant =
     &  tube_mass_cp_product / AMAX1(small,(tube_mdot_cp_product +
     &  abs(total_heat_transfer) / fluid_temp_diff))

C ------Time constant for component
        TC(IPCOMP) = AMAX1(tank_time_constant,tube_time_constant)

C ------Set up implicit/explicit weighting factor ALPHA (1 = fully implicit)
C ------Fully implicit.
        IF(IMPEXP.EQ.1) THEN
           ALPHA = 1.
C ------Implicit/explicit with user-specified (fixed) weighting.
        ELSEIF(IMPEXP.EQ.2) THEN
           ALPHA = RATIMP
C ------General case: implicit/explicit with calculated weighting.
        ELSEIF(IMPEXP.EQ.3) THEN
          IF(TIMSEC.GT.0.63 * TC(IPCOMP)) THEN
             ALPHA = 1.
          ELSE
             ALPHA = RATIMP
          END IF
C ------Steady-state.
        ELSEIF(IMPEXP.EQ.4) THEN
            tank_mass_cp_product = 0.
            tube_mass_cp_product = 0.
            ALPHA = 1.
        ENDIF

C ------Establish matrix equation self-coupling coefficient for tank node
        COUT(1) = ALPHA * (-tank_mdot_cp_product - tank_UAvalue)-
     &            tank_mass_cp_product / TIMSEC

C ------Establish matrix equation self-coupling couefficient for tube node
        COUT(2) = -ALPHA * tube_mdot_cp_product -
     &            tube_mass_cp_product / TIMSEC

C ------Matrix cross-coupling coefficient for tank node
        COUT(3) = ALPHA * tank_mdot_cp_product

C ------Matrix cross coupling coefficient for tube node
        COUT(4) = ALPHA * tube_mdot_cp_product

C ------Tank node mass flow rate and specific heat product for
C ------present time step
        tank_mdot_cp_present = PCRP(tank_node_conn)

C ------Tank node present temperature
        tank_fluid_temp_present = CSVP(tank_node_num,1)

C ------Tank present temperature of incoming flow
        tank_inflow_temp_present = PCTP(tank_node_conn)

C ------Containment present temperature
        cont_temp_present = PCNTMP(IPCOMP)

C ------Containment future temperature
        cont_temp = PCNTMF(IPCOMP)

C ------Heat transfer between tank node and HX node present value
        heat_transfer_present = PCQP(tank_node_num)

C ------Establish present and known coefficients for tank node
        COUT(5) =
     &       ((1. - ALPHA) * (tank_mdot_cp_present + tank_UAvalue) -
     &       tank_mass_cp_product / TIMSEC) * tank_fluid_temp_present
     &       - (1. - ALPHA) * tank_mdot_cp_present *
     &       tank_inflow_temp_present
     &       - (1. - ALPHA) * tank_UAvalue * cont_temp_present
     &       - ALPHA * tank_UAvalue * cont_temp
     &       - ALPHA * total_heat_transfer
     &       - (1. - ALPHA) * heat_transfer_present

C ------Tube node mass flow rate and specific heat product for
C ------present time step
        tube_mdot_cp_present = PCRP(intank_hx_node_conn)

C ------Tube node present temperature
        tube_fluid_temp_present = CSVP(intank_hx_node_num,1)

C ------Tube present temperature of incoming flow
        tube_inflow_temp_present = PCTP(intank_hx_node_conn)

C ------Establish present and known coefficients for coil node
        COUT(6) = ((1. - ALPHA) * tube_mdot_cp_present -
     &            tube_mass_cp_product / TIMSEC) *
     &            tube_fluid_temp_present
     &            - (1. - ALPHA) * tube_mdot_cp_present *
     &            tube_inflow_temp_present
     &            + ALPHA * (total_heat_transfer)
     &            + (1. - ALPHA) * (heat_transfer_present)

C -------Store future values of different variables

C -------Future temperature of tank incoming flow
         PCTF(tank_node_conn) = CONVAR(tank_node_conn,1)

C -------Future temperature of coil incoming flow
         PCTF(intank_hx_node_conn) = CONVAR(intank_hx_node_conn,1)

C -------Future value of mass flow rate and specific heat product of tank
C -------incoming flow
         PCRF(tank_node_conn) = tank_mdot_cp_product

C -------Tank future UA value
         PUAF(tank_node_num) = tank_UAvalue

C -------Future value of mass flow rate and specific heat product of tube
C -------incoming flow
         PCRF(intank_hx_node_conn) = tube_mdot_cp_product

C -------Heat injection to tank node from coil
         PCQF(tank_node_num) = total_heat_transfer

C -------Heat injection to tube node
         PCQF(intank_hx_node_num) = - total_heat_transfer

C -------Additional output for results analysis.
         NAPDAT(IPCOMP) = 4

C -------Total heat transfer between coil and tank fluid
         PCAOUT(IPCOMP,1) = total_heat_transfer

C -------Tube side heat transfer coeff
         PCAOUT(IPCOMP,2) = tube_side_hi

C -------Tank heat transfer coeff
         PCAOUT(IPCOMP,3) = tank_side_ho

C -------Total Ui-value
         PCAOUT(IPCOMP,4) = intank_coil_total_Uvalue

C ----1st phase mass (ie. "water") mass balance coefficients
      ELSE IF(ISTATS.EQ.2) THEN
         COUT(1) = 1.
         COUT(2) = 1.
         COUT(3) = -PCONDR(tank_node_conn)
         COUT(4) = -PCONDR(intank_hx_node_conn)
         COUT(5) = 0.
         COUT(6) = 0.

C ----2nd phase mass (ie. "vapour") mass balance coefficients
      ELSE IF(ISTATS.EQ.3) THEN
         COUT(1) = 1.
         COUT(2) = 1.
         COUT(3) = -PCONDR(tank_node_conn)
         COUT(4) = -PCONDR(intank_hx_node_conn)
         COUT(5) = 0.
         COUT(6) = 0.
      END IF

C ----Trace.
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &   ITRACE(37).NE.0) THEN
         WRITE(ITU,*) ' Component      ',IPCOMP,':'
         WRITE(ITU,*) ' 2 node in-tank HX model'
         WRITE(ITU,*) ' Matrix node(s) ',tank_node_num,
     &                                   intank_hx_node_num
         WRITE(ITU,*) ' Connection(s)  ',tank_node_conn,
     &                                   intank_hx_node_conn
         IF(ISTATS.EQ.1) THEN
            WRITE(ITU,*) ' Tank m cp = ',
     &                     tank_mass_cp_product,' (J/K)'
            WRITE(ITU,*) ' Tube m cp = ',
     &                     tube_mass_cp_product,' (J/K)'
            WRITE(ITU,*) ' Tank mdot cp = ',
     &                     tank_mdot_cp_product,' (W/K)'
            WRITE(ITU,*) ' Tube mdot cp = ',
     &                     tube_mdot_cp_product,' (W/K)'
            WRITE(ITU,*) ' TC = ', TC(IPCOMP), '(s)'
            WRITE(ITU,*) ' ALPHA = ', ALPHA,'(-)'
            WRITE(ITU,*) ' Tank ho = ',
     &                     tank_side_ho,'(W/m2-K)'
            WRITE(ITU,*) ' Tube hi = ',
     &                     tube_side_hi,'(W/m2-K)'
            WRITE(ITU,*) ' Total Ui = ',
     &                     intank_coil_total_Uvalue,
     &                     '(W/m2-K)'
            WRITE(ITU,*) ' Tube wall R-value = ',
     &      tube_wall_Rvalue,'(m2-K/W)'
         END IF
         WRITE(ITU,*) ' Matrix coefficients for ISTATS = ',ISTATS
         num_matrix_coeff = 6
         WRITE(ITU,*) (COUT(I), I = 1, num_matrix_coeff)
         IF(ITU.EQ.IUOUT) THEN   ! trace output going to screen, not file
            IX1 = (IPCOMP/4)*4
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew   ! write 4 lines at a time
         END IF
         WRITE(ITU,*) ' Leaving sub tank_intank_hx_coeff_gen'
      END IF

      RETURN
      END

C *********************************************************************************************
C ****************************** tank_intank_hx_static ****************************************
C Created by: Kamel Haddad
C Initial Creation Date: February 22nd, 2006
C Copyright CETC 2006

C This subroutine checks the number of controlled variables, connections, and fluid
C type for the component for tank-intank heat exchanger component

C INPUTS:
C     IPCOMP     Component number within the user defined plant component network
C     NCI        ESP-r common variable for number of controlled variable for component
C     ICONDX     ESP-r common variable holding number for plant network connections associated
C                with component
C     ICONTP     ESP-r common variable for type of fluid for connections
C     NPCDAT     ESP-r common variable holding node numbers for plant components within plant
C                network
C     ADATA(IPCOMP,1)
C                Flag indicating whether mains water temperature is based on user inputs or on
C                the Moore's model
C     ADATA(IPCOMP,i)
C                i = 1,12; Mains water temperature for each month of the year
C     ADATA(IPCOMP,i)
C                i = 13,37; Mains water draw in L/hr for each hour of the day

C OUTPUTS:
C     COUT       List of matrix coefficients for tank and in tank heat exchanger
C -------------------------------------------------------------------------------------------
      SUBROUTINE tank_intank_hx_static(IPCOMP)

      IMPLICIT NONE
#include "plant.h"
#include "building.h"

C----------------------------------------------------------------------
C     Passed arguments
C----------------------------------------------------------------------
      INTEGER IPCOMP        ! Number for plant component

C----------------------------------------------------------------------
C     ESP-r commons
C----------------------------------------------------------------------
C-----Trace/error unit reporting numbers
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      INTEGER IUOUT,IUIN,IEOUT
      INTEGER ITC,ICNT
      INTEGER ITCF,ITRACE,IZNTRC,ITU

C-----Common containing total number of plant components in user defined
C-----plant network
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      INTEGER NPCOMP,NCI
      REAL CDATA

C-----Plant component connection data
      COMMON/PCOND/CONVAR(MPCON,MCONVR),ICONTP(MPCON),
     &             ICONDX(MPCOM,MNODEC,MPCONC)
      REAL CONVAR    ! States variables for connections
      INTEGER ICONTP ! Type of each connection
      INTEGER ICONDX ! Pointer to connections for each
                     ! component node

C----------------------------------------------------------------------
C     Local variables
C----------------------------------------------------------------------
      INTEGER NCITM  ! Number of controlled variables for component
      INTEGER NCONPN ! Number of connections per node
      INTEGER NCONS  ! Number of connections
      INTEGER IPCON1 ! Incoming connecction number to first node of
                     ! component
      INTEGER IPCON2 ! Second connection to second node of component
      INTEGER IX1,IPCONC

C-----Trace output
      IF(ITC.GT.0.AND.ITRACE(35).NE.0) THEN
      WRITE(ITU,*) ' Component ',IPCOMP,' pre-simulation data for a:'
      WRITE(ITU,*) ' 2-node tank intank HX model'
         IF(ITU.EQ.IUOUT) THEN
            IX1 = (IPCOMP/5)*5
            IF(IX1.EQ.IPCOMP.OR.IPCOMP.EQ.NPCOMP) call epagew
         END IF
      END IF

C-----Check user specified number of controlled variables
      NCITM = 0
      IF(NCI(IPCOMP).NE.NCITM)
     &   WRITE(ITU,*) ' mains_temp_draw_static warning: user specified',
     &                ' wrong number of controlled variables'

C-----Check component has 1st connection to water
      NCONS = 2
      NCONPN = 1

C-----Loop through max number of connections for each node
      DO 10 IPCONC = 1,MPCONC

      IPCON1 = ICONDX(IPCOMP,1,IPCONC)
      IPCON2 = ICONDX(IPCOMP,2,IPCONC)

      IF(IPCONC.LE.NCONPN) THEN

C-----In case there is no connection to component nodes, then issue error
C-----message
        IF((IPCON1.EQ.0).OR.(IPCON2.EQ.0)) THEN
C-----Error handling
      WRITE(IUOUT,*) ' tank_intank_hx_static: connection error'
      WRITE(IUOUT,*) ' for component ',IPCOMP,' should be ',NCONS
      WRITE(IUOUT,*) ' connections to water'
      STOP ' tank_intank_hx_static: unresolvable error'
C-----In the case that the connection fluid is not water then issue
C-----error message
        ELSE IF((ICONTP(IPCON1).NE.20).OR.
     &                       (ICONTP(IPCON2).NE.20)) THEN
C-----Error handling
      WRITE(IUOUT,*) ' tank_intank_hx_static: connection error'
      WRITE(IUOUT,*) ' for component ',IPCOMP,' should be ',NCONS
      WRITE(IUOUT,*) ' connections to water'
      STOP ' tank_intank_hx_static: unresolvable error'
        END IF

C-----If there are more connections than allowed for component then
C-----issue error message
      ELSE IF((IPCON1.NE.0).OR.(IPCON2.NE.0)) THEN
C-----Error handling
      WRITE(IUOUT,*) ' tank_intank_hx_static: connection error'
      WRITE(IUOUT,*) ' for component ',IPCOMP,' should be ',NCONS
      WRITE(IUOUT,*) ' connections to water'
      STOP ' tank_intank_hx_static: unresolvable error'
      END IF

   10 CONTINUE

      RETURN

      END

C *******************************************************************************************
C ************************* tank_intank_hx_H3Kreports_module*********************************
C Created by: Kamel Haddad based on previous subroutine written by Alex Ferguson
C Initial Creation Date: March 15, 2006
C
C Copyright CETC 2006
C
C This subroutine transfers data from the water storage tank with in-tank heat exchanger model
C to the h3kreports object.
C--------------------------------------------------------------------------------------------

      Subroutine tank_intank_hx_H3Kreports_module(iComponentIndex)
      use h3kmodule
      implicit none

#include "plant.h"

C.....Passed variables
      integer iComponentIndex   ! index of tank with in-tank HX in plant component
                                ! network
C.....ESP-r Commons
      common/pcnam/pcname(mpcom)        ! Plant component names
      character*15 pcname

      common/PCVAL/
     &     CSVF(MPNODE,MPVAR),  ! Plant future state variables
     &     CSVP(MPNODE,MPVAR)   ! Plant present state variables
      real CSVF, CSVP

C.....Plant component present/future variables
c      common/pcvar/pctf(mpcon),pcrf(mpcon),puaf(mpnode),pcqf(mpnode),
c     & pcntmf(mpcom),pctp(mpcon),pcrp(mpcon),puap(mpnode),
c     & pcqp(mpnode),pcntmp(mpcom)

c      real pctf,pcrf,puaf,pcqf,pcntmf,pctp, pcrp, puap, pcqp, pcntmp

C-----Network and control data
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)
      INTEGER NPCDAT ! Miscelanous plant data
      INTEGER IPOFS1
      INTEGER IPOFS2

C.....Common for plant time step results for storage tank with in-tank HX
C.....model
      COMMON/tank_intank_hx/total_heat_transfer,tank_UAvalue,
     &                      tube_side_hi,tank_side_ho,
     &                      intank_coil_total_Uvalue,cont_temp
      real total_heat_transfer   ! total heat transfer from coil to tank water
      real tank_UAvalue          ! tank UA value
      real tube_side_hi          ! heat transfer coefficient inside coil
      real tank_side_ho          ! tank side heat transfer coefficient of the coil
      real intank_coil_total_Uvalue
                                 ! total U-value between inside and outside of coil
      real  cont_temp            ! surrounding temperature

C.....Local Variables
      integer tank_node_num      ! node number for tank
      integer lnblnk             ! function
      character*24 hold_name     ! temporary buffer for pcname
C.....Calculated values
      real tank_jacket_heat_loss    ! losses to environment from tank (W)

C.....Plant network node number for water tank
      tank_node_num = NPCDAT(iComponentIndex,9)

C.....Get component name
      write(hold_name,'(a)') pcname(iComponentIndex)
     &               (1:lnblnk(pcname(iComponentIndex)))

C....set name for total heat transfer
C....add to XML reporting
      call AddToReport(rvPltTHeatTrans%Identifier,
     &      total_heat_transfer,
     &      hold_name)

C....Heat loss from tank to surroundings
      tank_jacket_heat_loss = tank_UAvalue *
     &     (csvf(tank_node_num,1) - cont_temp)

C.....set name for tank jacket loss
C.....add to XML reporting facility
      call AddToReport(rvPltTHeatTransJacketHeatLoss%Identifier,
     &      tank_jacket_heat_loss,
     &      hold_name)

C.....set name for tube side heat transfer coefficient
C.....add to XML reporting facility
      call AddToReport(rvPltTHeatTransTubeSideHi%Identifier,
     &      tube_side_hi,
     &      hold_name)

C.....set name for tank side heat transfer coefficient
C.....add to XML reporting facility
      call AddToReport(rvPltTHeatTransTankSideHo%Identifier,
     &      tank_side_ho,
     &      hold_name)

C.....set name for tank side heat transfer coefficient
C.....add to XML reporting facility
      call AddToReport(rvPltTHeatTransIntankCoilTUValue%Identifier,
     &      intank_coil_total_Uvalue,
     &      hold_name)

      return
      end
