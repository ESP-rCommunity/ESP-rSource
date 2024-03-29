# Special materials database held in ASCII format and containing
# data for various special components.  Entires are divided into
# 4 fields: 
#
#    1: component type (1-special materials, 2-conductors, 3-power only 
#       components, 4-building electrical loads, 5-plant motors and generators)
#    2: material name and id
#    3: specific data items
#    4: text field
# The field entries have the same format.
***********************************************************************************
*SPMDB
*CATEGORY
# The categories of special components currently supported in the database.
# Categories 1-5 are reserved for building integrated renewables all other
# categories are for active materials.
8
crystalline PV
amorphous PV
simple PV
solar collector backplate
wind turbines
thermochromic glazing
phase change material
evaporating surface
*ENDCATEGORY
*SPECMATL
*IDDATA
# Category | Assoc. special 
#            material model no 
     1            1 
*DESC
BP380_poly_36cell    
*SPECDATA
13                  
22.1      Open circuit voltage. (V)
4.80      Short circuit current. (I)
17.6      Voltage at maximum power point (V)
4.55      Current at maximum power point (A)
1000.     Reference insolation. (W/m^2)
298.      Reference temperature. (K)
36.       Number of series connected cells (not panels) (-)
1.        Number of parallel connected branches. (-)
5.        Number of panels in surface. (-)
10.       Empirical value used in calculation of Io
0.        Load type (0-maximum power,1-fixed R,2-fixed V)
0.        Load value - resistance or voltage (-)
0.        Shading treatment (0-def,1-prop.,2-total,3-diff.)
*TEXT
Data from BP solar:
BP Saturn (BP 585)
Module type             BP 380 polycrystalline
Module size             1209 x 537 mm
Weight                  7.5 kg
Construction            Low-Iron Glass, EVA (cells 400 microns thick)
Cells in series         36
Cells in parallel       1
Peak power  at STC      80
Voc at STC              22.1
Isc at STC              4.8
Vmax at STC             17.6
Imax at STC             4.55

Construction:
                                         k     rho     c
3mm low-iron glass                      1.05   2500    750
1mm EVA tedlar (inc. cells)             0.38    920   2100
3mm low-iron glass                      1.05   2500    750

*ENDTEXT
*END*SPECMATL
*IDDATA
# Category | Assoc. special 
#            material model no 
     1            1 
*DESC
BP_saturn_36cell    
*SPECDATA
13                  
22.03     Open circuit voltage. (V)
5.00      Short circuit current. (I)
18.0      Voltage at maximum power point (V)
4.72      Current at maximum power point (A)
1000.     Reference insolation. (W/m^2)
298.      Reference temperature. (K)
36.       Number of series connected cells (not panels) (-)
1.        Number of parallel connected branches. (-)
2.        Number of panels in surface. (-)
10.       Empirical value used in calculation of Io
0.        Load type (0-maximum power,1-fixed R,2-fixed V)
0.        Load value - resistance or voltage (-)
0.        Shading treatment (0-def,1-prop.,2-total,3-diff.)
*TEXT
Data from BP solar:
BP Saturn (BP 585)
Module type             BP Saturn (BP 585)
Module size             1183 x 525 mm
Weight                  7.5 kg
Construction            Low-Iron Glass, EVA (cells 400 microns thick)
Cells in series         36
Cells in parallel       1
Peak power  at STC      85
Voc at STC              22.03
Isc at STC              5.00
Vmax at STC             18.0
Imax at STC             4.72

Construction:
                                         k     rho     c
3mm toughened glass                     1.05   2500    750
1mm EVA tedlar (inc. cells)             0.38    920   2100

Values apply to both the framed and laminated models

*ENDTEXT
*END
*SPECMATL
*IDDATA
# Category | Assoc. special 
#            material model no 
     1            1 
*DESC
Solarex_MSX550    
*SPECDATA
13                   
20.9      Open circuit voltage. (V)
3.08      Short circuit current. (I)
16.9      Voltage at maximum power point (V)
2.91      Current at maximum power point (A)
1000.     Reference insolation. (W/m^2)
298.      Reference temperature. (K)
36.       Number of series connected cells (not panels) (-)
1.        Number of parallel connected branches. (-)
2.        Number of panels in surface. (-)
10.       Empirical value used in calculation of Io
0.        Load type (0-maximum power,1-fixed R,2-fixed V)
0.        Load value - resistance or voltage (-)
0.        Shading treatment (0-def,1-prop.,2-total,3-diff.)
*TEXT

Information from Solarex data sheet:

Solarex MSX 550 polycrystalline panel
Module type             SOLAREX MSX 550
Module size             931 x 492 mm
Weight                  UNKNOWN
Construction            Glass,EVA-tedlar
Cells in series         36
Cells in parallel       1
Peak power  at STC      85
Voc at STC              20.90
Isc at STC               3.08
Vmax at STC             16.90
Imax at STC              2.91

Construction (from existing ESP-r model):
                                  k     rho     c
2mm toughened glass             1.05   2500    750
1mm EVA+tedlar                  0.38    920   2100

Optical properties (estimated):
MSX 550     :PV-hybrid-cell_3mm
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   2  0.000  0.079  0.850  5.320
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.013 0.015 0.016 0.017 0.017
  1.520 0.837 0.828 0.791 0.660 0.418

*ENDTEXT
*END
*SPECMATL
*IDDATA
   1      1
*DESC
RMS100
*SPECDATA
13                 
43.8      Open circuit voltage. (V)
3.10      Short circuit current. (I)
34.1      Voltage at maximum power point (V)
2.85      Current at maximum power point (A)
1000.     Reference insolation. (W/m^2)
298.      Reference temperature. (K)
72.       Number of series connected cells (not panels) (-)
1.        Number of parallel connected branches. (-)
2.        Number of panels in surface. (-)
10.       Empirical value used in calculation of Io
0.        Load type (0-maximum power,1-fixed R,2-fixed V)
0.        Load value - resistance or voltage (-)
0.        Shading treatment (0-def,1-prop.,2-total,3-diff.)
*TEXT
PV Model:

Module type             RMS100
Module size             1327 x 708 mm
Weight                  11.1 kg
Construction            Glass, EVA, White Tedlar + AL Backsheet
Cells in series         72
Cells in parallel       1
Voc at STC              43.8
Isc at STC              3.10
Vmax at STC             34.1
Imax at STC             2.85

Construction details:
                                 k     rho      Cp 
Glass low iron ESG  4mm     1.05   2500    750
Resin               1mm     0.40    920   2100
Silicon (PV)        1mm     0.18    700   1004
Resin               1mm     0.40    920   2100
Aluminium backing   2mm    210.0    2700    880

This is the data supplied by Ispra - applicable for 100W modules.

Optical properties (estimated):
system transmission 0.00
absorption at the various layers:
                         0deg      40deg   55deg   70deg   80deg
Glass low iron ESG  4mm  0.03       0.03    0.03    0.03    0.03
(typical for glass with 89% transmission, 8% reflection)
Resin               1mm  0.05       0.05    0.05    0.05    0.05
Silicon (PV)        1mm  0.84       0.84    0.84    0.84    0.84
Resin               1mm  0.00       0.00    0.00    0.00    0.00
Aluminium backing   2mm  0.00       0.00    0.00    0.00    0.00

Assume absorptivity as above - but this information should be obtained
from the manufacturer if possible, or by measurement by a pyranometer
facing the PV panel.

*ENDTEXT
*END
*SPECMATL
*IDDATA
# Category | Assoc. special 
#            material model no 
     1            1 
*DESC
Eurosolare_poly    
*SPECDATA
13                 
21.67     Open circuit voltage. (V)
2.98      Short circuit current. (I)
17.42     Voltage at maximum power point (V)
2.73      Current at maximum power point (A)
1000.0    Reference insolation. (W/m^2)
297.90    Reference temperature. (K)
36.       Number of series connected cells (not panels) (-)
1.        Number of parallel connected branches. (-)
2.        Number of panels in surface. (-)
10.       Empirical value used in calculation of Io
0.        Load type (0-maximum power,1-fixed R,2-fixed V)
0.        Load value - resistance or voltage (-)
0.        Shading treatment (0-def,1-prop.,2-total,3-diff.)
*TEXT
Data from Eurosolare:
Eurosolare polycrystalline module  (5% uncertainty in module parameters)
Module type             Eurosolare 36 cell polycrystalline
Module size             995.5 x 450 mm 
Cell dims               938.5 x 416 mm (102.5 x 102.5 mm each)
Cell spacing            2mm             
Weight                  UNKNOWN
Construction            Low-Iron Glass, EVA (cells 400 microns thick)
Cells in series         36
Cells in parallel       1
Peak power  at STC      48W
Voc at STC              21.50 (V)
Isc at STC               3.00 (A)
Vmax at STC             17.80 (V)
Imax at STC              2.98 (A)

Construction:
                                          k     rho     c
4.0mm toughened glass                     1.05   2500    750
1.6mm EVA tedlar (inc. cells)             0.38    920   2100

Optical properties (estimated):
Eurosolare     :PV-hybrid-cell_3.6mm
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   2  0.000  0.079  0.824  5.320
# direct trn @ 5 angles, total heat gain @ 5 angles
  0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000 0.000
# refr index, absorption @ 5 angles for each tmc layer
  1.520 0.026 0.030 0.032 0.034 0.034
  1.520 0.824 0.813 0.775 0.643 0.401

*ENDTEXT
*END
*SPECMATL
*ID DATA
   3     2
*DESC
Simple_pv
*SPECDATA
1
15.0      Efficiency of the PV material 
*TEXT
Very simple model of a PV material which simply converts a fraction (the 
efficiecy) of the solar radiation falling on a surface to power output. 
*ENDTEXT
*END
*SPECMATL
*IDDATA
    2     1
*DESC
TNO_amorphous_pv
*SPECDATA 
13
99.48     Open circuit voltage. (V)
0.438     Short circuit current. (I)
77.76     Voltage at maximum power point (V)
0.378     Current at maximum power point (A)
1000.     Reference insolation. (W/m^2)
298.      Reference temperature. (K)
57.       Number of series connected cells (not panels) (-)
180.      Number of parallel connected branches. (-)
1.        Number of panels in surface. (-)
100.      Empirical value used in calculation of Io
0.        Load type (0-maximum power,1-fixed R,2-fixed V)
0.        Load value - resistance or voltage (-)
0.        Shading treatment (0-def,1-prop.,2-total,3-diff.)
*TEXT
PV Model:

Module type             Amorphous PV pst-semi
Module size             0.6m sq (cell area 0.4599m sq)
Weight                  UNKNOWN
Construction            Glass,PV+EVA,Glass Backsheet
Cells in series         72
Cells in parallel       1
Voc at STC              43.8
Isc at STC              3.10
Vmax at STC             34.1
Imax at STC             2.85

Construction details:
                                 k     rho      Cp 
    Glass low iron ESG   3mm     1.05   2500    750
    EVA+PV               1mm     0.38    920   2100
    GLASS backing     5.25mm    210.0    2700    880

This is the data supplied by TNO


Optical properties (estimated): 

DTNO_glass    :TNO-pv-testcell
# def lyr, tmc lyr, vis trn, sol refl, sol absor, U val
  1   3  0.816  0.141  0.147  2.800
# direct trn @ 5 angles, total heat gain @ 5 angles
# 0deg      40deg   55deg   70deg   80deg
  0.712 0.686 0.612 0.427 0.204 0.768 0.745 0.673 0.484 0.249
# refr index, absorption @ 5 angles for each tmc layer
# 0deg      40deg   55deg   70deg   80deg
  1.520 0.074 0.081 0.088 0.097 0.100
  1.000 0.001 0.001 0.001 0.001 0.001
  1.520 0.073 0.078 0.078 0.069 0.050
*ENDTEXT
*END
*SPECMATL
*IDDATA
    5     3
*DESC
ducted_wind_turbine
*SPECDATA 
15
1         Zone containing outlet (-)
1         Surface containing outlet (-)
1         Outlet ourface type (-)
0.        Inlet azimuth angle (deg)
0.        Outlet azimuth angle (deg)
10.0      Height of the turbine inlet (m)
1.0       Turbine cross sectional area (m^2)
1.0       Duct velocity coefficient (-)
4.0       Cut-in wind speed (m/s)
1.0       Number of turbines in the surface (-)
1.0       Location (1-urban,2-suburban,3-open)
10.0      Reference height (m)
0.0       Wind speed statistics (ON/OFF)
0.0       Wind speed/turbulence profile (ON/OFF)
0.2       Default turbulence intensity (-)
*TEXT
This ducted wind turbine model is developed from that described in
"Development of building integrated wind turbines" by Grant, Danecker
and Nicholson.  The generated power (W) is calculated based on the 
following formula: 

P=(Cv/(3.3^(1/2))).rho.A.d^(2/3).V^(1/3)

Where Cv is the duct velocity coefficient, A is the duct frontal 
area, d is the differential pressure coefficient (Cp1-Cp2) and
V is the free stream velocity of the wind. 

The differential pressure coefficient is calculated by ESP-r or
calculated using a curve fit formula based on the user's own data.
If using the user's data then curve fit coefficients are required
to calculate d as a function of the wind incidence angle.

*ENDTEXT
*END
*SPECMATL
*IDDATA
    5     6
*DESC
2.5_wind_turbine
*SPECDATA 
11
10.0      Height of the turbine inlet (m)
2.54      Turbine cross sectional area (m^2)
0.4       Power velocity coefficient (-)
3.0       Cut-in wind speed (m/s)
12.5      Rated wind speed (m/s)
1.0       Number of turbines (-)
1.0       Location (1-urban,2-suburban,3-open)
10.0      Reference height (m)
0.0       Wind speed statistics (ON/OFF)
0.0       Wind speed/turbulence profile (ON/OFF)
0.2       Default turbulence intensity (-)
*TEXT
This wind turbine model is developed from that described in
"Development of building integrated wind turbines" by Grant, Danecker
and Nicholson.  The generated power (W) is calculated based on the 
following formula: 

P=Cp(1/2).rho.A.V^3

Where Cp is the power velocity coefficient, A is the swept area 
area, and V is the free stream velocity of the wind or the rated
wind speed if V is greater than the rated wind speed. 

*ENDTEXT
*END
*SPECMATL
*IDDATA
    5     6
*DESC
5kw_wind_turbine
*SPECDATA 
11
14.0      Height of the turbine inlet (m)
22.75     Turbine cross sectional area (m^2)
0.4       Power velocity coefficient (-)
3.0       Cut-in wind speed (m/s)
12.0      Rated wind speed (m/s)
1.0       Number of turbines (-)
1.0       Location (1-urban,2-suburban,3-open)
14.0      Reference height (m)
0.0       Wind speed statistics (ON/OFF)
0.0       Wind speed/turbulence profile (ON/OFF)
0.2       Default turbulence intensity (-)
*TEXT
This wind turbine model approximates a 5kW R9000 3 blade self
regulating turbine 5.5m rotor diameter (Evancewind.com data).
Performance model developed from that described in
"Development of building integrated wind turbines" by Grant, Danecker
and Nicholson.  The generated power (W) is calculated based on the 
following formula: 

P=Cp(1/2).rho.A.V^3

Where Cp is the power velocity coefficient, A is the swept area 
area, and V is the free stream velocity of the wind or the rated
wind speed if V is greater than the rated wind speed. 

*ENDTEXT
*END
*SPECMATL
*IDDATA
# Category | Assoc. special 
#          | material model no
     7            53
*DESC
PCM_Cap
*SPECDATA
7
20.0      Melting temperature (C)
21.0      Solidification temperature (C)
0.40      Conductivity in solid phase (W/m.C)
0.80      Conductivity in liquid phase (W/m.C)
1000.0    Specific heat (J/kg.C)
5000.0    Latent heat member a - see info (J/kg.C^2)
100000.0  Latent heat member b - see info (J/kg.C)
*TEXT

PHASE CHANGE MATERIAL
Special, phase change material should be defined for middle 
node of the layer in interest. Calculation being proced for
all nodes represented this surface.

Following informations are required to define phase change mat.
- melting temperature,          C
- solidification temperature,   C
- conductivity in solid phase,  W/m.C
- conductivity in liquid phase, W/m.C
- specific heat,                J/kg.C
- latent heat,                  J/kg.C

Phase change occured between melting Tm (PCM becomes melts) and
solidification Ts (PCM becomes solidified) temperatures. Below
Tm PCM is considered as a solid and the conductivity of the layer
is equal to conductivity in solid phase. Over Tm PCM is considered
as a liquid and conductivity of the layer is equal to conductivity
in liquid phase.

Beyond phase change temperature range latent heat of material is
equal to 0. In phase change temperature range latent heat is:
1 - constant LHT=b
2 - linear function of temperature LHT=aT+b
according to this two cases can be considered:
1 - latent heat is constant in temperature range (Ts-Tm),
    y=ax+b (a=0, b=latent heat [J/kg*K])
2 - latent heat is a linear function of temperature,
    y=ax+b (a=latent heat [J/kg*K^2], b=latent heat [J/kg*K])

*ENDTEXT
*END
*SPECMATL
*IDDATA
# Category | Assoc. special 
#            material model no 
     8            52
*DESC
Evaporative_surf   
*SPECDATA
 1                   
 1.0      Calculation mode (1.0 - default).        
*TEXT
This material calculated the heat loss and evaporation rate for
a saturated surface. The evaporation into the adjacent space and 
evaporative heat loss for the surface are calculated.

The functions used in the calculation are as follows: 

Evaporation (kg) - from the Lewis relationship: 

Ev=hc.Asurf.(w surf - w air)/cp (kg/s).

w surf is the saturated moisture content at t surf (kg/kg da)
w air is the moisture content of the air in the zone (kg/kg da)
hc is the surface convective heat transfer coefficient (W/m^2K))
Asurf is the surface area (m^2)
cp is the specific heat capacity of the air (J/kgK)

The evaporative loss for the surface is: 

Qev = Ev*(Hvap-Hsl) (W)

Hvap is the enthalpy of the vapour at the surface temperature
Hsl is the enthalpy of liquid water at the surface temperature

*ENDTEXT
*SPECMATL
*IDDATA
# Category | Assoc. special 
#            material model no 
     4            4 
*DESC
Solar_collector_back_plate    
*SPECDATA
5                 
0.002     Water flow rate (l/s)
12.00     Water inlet temperature (oC)
50.0      No. of tubes (-)
0.01      Tube diameter (m)
1.0       Tube length (m)
*TEXT
Basic data for a 1mx1m solar thermal collector backplate with 1cm 
diamater circular tubes.

*ENDTEXT
*END
*SPECMATL
*IDDATA
# Category | Assoc. special 
#            material model no 
     1            5 
*DESC
WATSUN-PV_multic
*SPECDATA
16                    
22.1      Open circuit voltage at ref (V)
4.80      Short circuit current at ref (A)
17.6      Voltage at maximum power point at ref (V)
4.55      Current at maximum power point at ref (A)
1000.     Reference insolation. (W/m^2)
298.      Reference temperature. (K)
0.00065   Temperature coefficient of Isc (/K)
-0.0036   Temperature coefficient of Voc (/K)
1.        Coefficient of logarithm of irradiance for Voc (-)
36.       Number of series connected cells (not modules) (-)
1.        Number of parallel connected branches. (-)
2.        Number of modules in surface. (-)     
0.        Load type (0-maximum power, 1-fixed V)
0.        Load value - voltage (V)
0.        Shading treatment (0-def,1-prop,2-total,3-diff.)
0.10      Miscellaneous loss factor (-)
*TEXT
Data from BP solar:
BP Saturn (BP 380)
Module type             BP Saturn (BP 380)
Module size             1197 x 530 mm
Weight                  6.1 kg
Construction            Clear universal frame with standard junction box
Cells in series         36
Cells in parallel       1
Peak power  at STC      80
Voc at STC              22.1
Isc at STC              4.80
Vmax at STC             17.6
Imax at STC             4.55
*ENDTEXT
*END
*SPECMATL
*IDDATA
# Category | Assoc. special 
#            material model no 
     2            5 
*DESC
WATSUN-PV_amorph
*SPECDATA
16                    
42.0      Open circuit voltage at ref (V)
5.10      Short circuit current at ref (A)
30.0      Voltage at maximum power point at ref (V)
4.10      Current at maximum power point at ref (A)
1000.     Reference insolation. (W/m^2)
298.      Reference temperature. (K)
0.001     Temperature coefficient of Isc (/K)
-0.0038   Temperature coefficient of Voc (/K)
1.        Empirical coefficient beta used in calc of Voc (-)
20.       Number of series connected cells (not modules) (-)
1.        Number of parallel connected branches. (-)
2.        Number of modules in surface. (-)
0.        Load type (0-maximum power, 1-fixed V)
0.        Load value - voltage (V)
0.        Shading treatment (0-def,1-prop,2-total,3-diff.)
0.10      Miscellaneous loss factor (-)     
*TEXT
Data from Uni-Solar:
Uni-Solar ECO-Series
Module type             Uni-Solar ES-124
Module size             2459 x 792 mm
Weight                  20.5 kg
Construction            Framed module with terminal housing assembly
Cells in series         20
Cells in parallel       1
Peak power  at STC      124
Voc at STC              42.0
Isc at STC              5.10
Vmax at STC             30.0
Imax at STC             4.10
*ENDTEXT
*ENDSPMDB
# Additional classes of entity to be added here same format as above.
# Power only components for use with an ESP-r electrical network
*POWRODB
*CATEGORY
# The categories of special material currently supported in the database
6
Wind Turbines
non-HVAC electrical loads
HVAC electrical loads
Transformation and network losses
Off-site loads and generation
Batteries
*ENDCATEGORY
*POWOCOMP
*IDDATA
# Category | Assoc. special 
#            power only model no 
     1            1 
*DESC
Simple_WTG 
*SPECDATA
9  1     
5000.     Rated power output (W)   
12.00     Rated wind speed of turbine (m/s)            
5.00      Wind speed for turbine cut-in (m/s)
30.0      Cut-out windspeed (m/s)
4.72      Power output coefficient (a)
2.17.     Power output coefficient (b)
220.      Operating L-N voltage (V)
0.9       Generator power factor (-)
1.        Generator type (1-2) 1-Asynchronous 2-Syncronous
UNKNOWN   
Climate file associated with the wind turbine. 
*TEXT
This is a very simple wind turbine mode based on that described by
"Twiddly and Weir".

*ENDTEXT
*END
*POWOCOMP
*IDDATA
# Category | Assoc. special 
#            power only model no 
     2            10  
*DESC
non-HVAC_elec_load
*SPECDATA
1  1
1.        Scalar multiplier for loads in .fcl file
UNKNOWN
non-HVAC electrical loads input file (.fcl)
*TEXT
This allows non-HVAC electrical loads stored in .fcl files
to be imposed upon the electrical network.

*ENDTEXT
*END
*POWOCOMP
*IDDATA
# Category | Assoc. special 
#            power only model no 
     3            11  
*DESC
HVAC_elec_load
*SPECDATA
0  
*TEXT
This component allows the electrical loads from CETC's idealized 
plant component models to be imposed on the electrical network

*ENDTEXT
*END
*POWOCOMP
*IDDATA
# Category | Assoc. special 
#            power only model no 
     5            12
*DESC
Outside_elec_load
*SPECDATA
1  1
1.        Scalar multiplier for loads in .fcl file
UNKNOWN
Electrical loads input file (.elec)
*TEXT
This allows electrical loads stored in .elec files for
other buildings to be imposed upon the electrical network.

*ENDTEXT
*END
*POWOCOMP
*IDDATA
# Category | Assoc. special
#            power only model no
     4            13
*DESC
Wind_transformer_loss
*SPECDATA
1  0
0.96       Transformer efficiency
*TEXT
This component allows transformer losses to be imposed
as a load on the electrical network.
*ENDTEXT
*END
*POWOCOMP
*IDDATA
# Category | Assoc. special
#            power only model no
     4            14
*DESC
Wind_TxDx_loss
*SPECDATA
1 0
0.95
*TEXT
This component allows transmission and distribution
losses to be imposed as a load on the electrical network.
*ENDTEXT
*END
*POWOCOMP
*IDDATA
# Category | Assoc. special
#            power only model no
     4            15
*DESC
PCU_loss
*SPECDATA
1 0
0.95
*TEXT
This component allows losses from the power conditioning
unit that converts DC power from the electrical generation
device to AC power to be imposed as a load on the electrical network.
*ENDTEXT
*END
*POWOCOMP
*IDDATA
# Category | Assoc. special 
#            power only model no 
     5            16
*DESC
Off_site_generated_power
*SPECDATA
1  1
1.        Scalar multiplier for supply in .wnd file
wind-generated electrical supply input file (.wnd)
*TEXT
This allows wind-generated electrical supply stored in .wnd files
to be imposed upon the electrical network.
*ENDTEXT
*END
*POWOCOMP
*IDDATA
# Category | Assoc. special
#            power only model no
     4            20
*DESC
DC-AC_inverter
*SPECDATA
6 0
1.        Operating mode of PCU (-)           
2000.     Nominal power (W)
8.975E-06 Idling constant (-)
3.65      Set-point voltage (V)
1.227E07  Internal resistance constant (V^2)
0.        Auxiliary power (W)
*TEXT
This component determines the power sink term for the
node sending current to the PCU and the power source term
for the node receiving current from the PCU.
*ENDTEXT
*END
*POWOCOMP
*IDDATA
# Category | Assoc. special
#            power only model no
     6            17
*DESC
Power-sonic battery (CETC battery model)
*SPECDATA
47 0
1        Number of battery cells in parallel in a pack (-)
6        Number of packs in series in a battery (-)
1.66     Capacity at operating temperature (Ah/cell)
20.      Operating temperature recommended by manufacturer (C)
2.15     Maximum manufacturer voltage of a battery cell (V)
1.94     Minimum manufacturer voltage of a battery cell (V)
1.5      Maximum recommended charge current of a battery cell (A)
2.2      Maximum recommended charge voltage of a battery cell (V)
30.      Initial battery temperature (oC)
0.       Initial battery depth of discharge (-)
1666.    Thermal mass of one battery cell (J/oC)
0.01     Battery heat loss coefficient of one battery cell (W/oC)
600.     Resistance of the heating elements for one battery cell (ohms)
15.      Temperature below which the thermal management system takes priority (C)
2.15     Coefficient A for battery free voltage correlation (V)
-0.21    Coefficient B for battery free voltage correlation (V)
0.       Coefficient C for battery free voltage correlation (V)
0.       Coefficient D for battery free voltage correlation (V)
1.       Exponential coefficient B for battery free voltage correlation (-)
0.       Exponential coefficient C for battery free voltage correlation (-)
0.       Exponential coefficient D for battery free voltage correlation (-)
0.8      Coefficient A for cell capacity/temperature correlation (-)
0.0095   Coefficient B for cell capacity/temperature correlation (/C)
-0.00006 Coefficient C for cell capacity/temperature correlation (/C^2)
0.2      Coefficient CA for cell internal resistance correlation in charge mode
0.       Coefficient CB for cell internal resistance correlation in charge mode
0.       Coefficient CC for cell internal resistance correlation in charge mode
0.       Coefficient CD for cell internal resistance correlation in charge mode
0.       Coefficient CH for cell internal resistance correlation in charge mode
0.       Exponential coefficient EB for cell internal resistance correlation in charge mode
0.       Exponential coefficient EC for cell internal resistance correlation in charge mode
0.       Exponential coefficient ED for cell internal resistance correlation in charge mode
0.       Exponential coefficient EH1 for cell internal resistance correlation in charge mode
0.       Exponential coefficient EH2 for cell internal resistance correlation in charge mode
0.       Exponential coefficient EH3 for cell internal resistance correlation in charge mode
0.2      Coefficient CA for cell internal resistance correlation in discharge mode
0.       Coefficient CB for cell internal resistance correlation in discharge mode
0.       Coefficient CC for cell internal resistance correlation in discharge mode
0.       Coefficient CD for cell internal resistance correlation in discharge mode
0.       Coefficient CH for cell internal resistance correlation in discharge mode
0.       Exponential coefficient EB for cell internal resistance correlation in discharge mode
0.       Exponential coefficient EC for cell internal resistance correlation in discharge mode
0.       Exponential coefficient ED for cell internal resistance correlation in discharge mode
0.       Exponential coefficient EH1 for cell internal resistance correlation in discharge mode
0.       Exponential coefficient EH2 for cell internal resistance correlation in discharge mode
0.       Exponential coefficient EH3 for cell internal resistance correlation in discharge mode
1        Index number of zone containing battery
*TEXT
Battery model developed by CETC (Patrice Pinel, modified Hajo Ribberink).
*ENDTEXT
*END
*POWOCOMP
*IDDATA
# Category | Assoc. special 
#            power only model no 
     2            18
*DESC
non-HVAC_elec_load_from_BCD
*SPECDATA
4  1
1.        Scalar multiplier for loads 
1.        Interpolation method (1=step, 2=linear)
1.        Flag for managing hydrogen loads (0=ignore loads, 1=deduct loads)
1.        Flag for loads source (1: .fcl file, 2: .bcd file
UNKNOWN
Name of .fcl file, or boundary condition column corresponding to electric load
*TEXT
This component is an improved version of 'non-HVAC_elec_load'. In 
addition to allowing loads read from the boundary condition facility
to be imposed on the electric network, it is 'hydrogen-appliance-aware',
and can deduct energy used by hydrogen equipment.
*ENDTEXT
*END
*POWOCOMP
*IDDATA
# Category | Assoc. special 
#            power only model no 
     1            19 
*DESC
wind_turbine2
*SPECDATA 
8 2
10.0      Height of the turbine (m)
1.0       Number of turbines (-)
1.0       Location (1-urban,2-suburban,3-open)
10.0      Reference height (m)
0.0       Wind speed statistics (ON/OFF)
0.0       Wind speed data source (0=clm file w/o corr, 1= clm file w/ corr for height, 2= bcd file)
0.2       Default turbulence intensity (-)
1.0       Wind speed interpolation method (1 = step, 2 = linear)
UNKNOWN
Wind turbine power output profile file. 
UNKNOWN
Column heading corresponding to wind speed data (if using bcd data source). 
*TEXT
A simple user configurable wind turbine model. 
*ENDTEXT
*END
*ENDPOWERODB
*ECONDDB 
*CATEGORY
# The categories of special material currently supported in the database
2
Electrical conductors
Transformers
*ENDCATEGORY
*CONCOMP
*IDDATA
# Category | Assoc  connector model no. 
     1            1 
*DESC
dc_cable
*SPECDATA
2       
0.0       Conductor series resistance Rs (ohms/m)   
1.0       Length (m)
*TEXT
None
*ENDTEXT
*END
*CONCOMP
*IDDATA
# Category | Assoc  connector model no. 
     1            2 
*DESC
one_phase_cable
*SPECDATA
6       
0.0       Phase conductor series resistance Rs (ohms/m)   
0.0       Phase conductor series reactance Xs (=jwLs) (ohms/m)            
0.0       Neutral series resistance Rn (ohms/m)
0.0       Neutral series reactance Xn (=jwLn) (ohms/m)
0.0       Phase-Neutral mutual inductive reactance Xm (=jwLm) (ohms/m)
1.0       Length (m)
*TEXT
This is a very simple short line model that ignores the capacitive properties 
of the conductor. 
*ENDTEXT
*END
*CONCOMP
*IDDATA
# Category | Assoc  connector model no. 
     1            3 
*DESC
two_phase_cable
*SPECDATA
7       
0.0       Phase conductor series resistance Rs (ohms/m)   
0.0       Phase conductor series reactance Xs (=jwLs) (ohms/m)
0.0       Phase-Phase mutual inductive reactance Xm (=jwLm) (ohms/m)           
 
0.0       Neutral series resistance Rn (ohms/m)
0.0       Neutral series reactance Xn (=jwLn) (ohms/m)
0.0       Phase-Neutral mutual inductive reactance Xm (=jwLm) (ohms/m)
1.0       Length (m)
*TEXT
This is a very simple short line model that ignores the capacitive properties 
of the conductor. 
*ENDTEXT
*END
*CONCOMP
*IDDATA
# Category | Assoc  connector model no. 
     1            4 
*DESC
three_phase_cable
*SPECDATA
7       
0.0       Phase conductor series resistance Rs (ohms/m)   
0.0       Phase conductor series reactance Xs (=jwLs) (ohms/m) 
0.0       Phase-Phase mutual inductive reactance Xm (=jwLm) (ohms/m)           
  
0.0       Neutral series resistance Rn (ohms/m)
0.0       Neutral series reactance Xn (=jwLn) (ohms/m)
0.0       Phase-Neutral mutual inductive reactance Xm (=jwLm) (ohms/m)
1.0       Length (m)
*TEXT
This is a very simple short line model that ignores the capacitive properties 
of the conductor. 
*ENDTEXT
*END
*POWOCOMP
*IDDATA
# Category | Assoc. special
#            power only model no
     6            22
*DESC
Lithium Ion Battery (CETC Battery subroutine)
*SPECDATA
81 0
34       Number of cells connected in parrallel
35       Number of cells connected in series
1.5      reference capacity for a unit cell (Amp h)
25.      battery reference operating temperature
4.3      DOD=0 (V) of a single cell
3.15     DOD=100% (V) of a single cell
24.2     Maximum charge/discharge current of one cell(Amps)
4.3      Maximum charge/discharge voltage of one cell(V)
25.      initial battery temperature  (C) 
0.       initial depth of discharge (probably zero for a charged battery)
27.3     thermal mass of a single cell -mass*Cp (J/C)
42.64    heat loss factor h*A (W/C)
582.86   resistance of the heaters (Ohm)
1.0      maximum DOD of battery (in percentage... probably 0.8)
0.       minimum DOD of battery (in percentage... probably 0)
60.      Maximum battery temperature (C)
-40.     Minimum battery temperature (C)
-.05712  E5 Exponential coefficent for OCV
-.05255  E4 Exponential coefficent for OCV
-.20969  E3 Exponential coefficent for OCV
-.34512  E2 Exponential coefficent for OCV
-.41175  E1 Exponential coefficent for OCV
4.2228   E0 Exponential coefficent for OCV
0.1152   Rint5 Exponential coefficent for Rint
-0.2556  Rint4 Exponential coefficent for Rint
0.2007   Rint3 Exponential coefficent for Rint
-0.0651  Rint2 Exponential coefficent for Rint
0.0265   Rint1 Exponential coefficent for Rint
0.0202   Rint0 Exponential coefficent for Rint
0.0605   The first current discharged at (Amps)
1.00     The alpha required to discharge/charge at first rate
0.1210   The second current discharged at (Amps)
1.00     The alpha required to discharge/charge at second rate
0.2419   The third current discharged at (Amps) 
1.00     The alpha required to discharge/charge at third rate 
1.2096   The fourth current discharged at (Amps) 
0.985    The alpha required to discharge/charge at fourth rate 
6.0480   The fifth current discharged at (Amps)
0.895    The alpha required to discharge/charge at fifth rate
12.096   The sixth current discharged at (Amps)
0.775    The alpha required to discharge/charge at sixth rate
24.192   The seventh current discharged at (Amps)
0.593    The alpha required to discharge/charge at seventh rate
999.     The eighth current discharged at (Amps)
0.593    The alpha required to discharge/charge at eighth rate
-40.     The first temperature discharged/charged at in Celsius
0.905    The beta required to discharge/charge at the first temperature
0.1870   The voltage offset required to discharge/charge at the first temperature (V)
-20.     The second temperature discharged/charged at in Celsius
0.985    The beta required to discharge/charge at the second temperature
0.0366   The voltage offset required to discharge/charge at the second temperature (V)
0.       The third temperature discharged/charged at in Celsius
1.       The beta required to discharge/charge at the third temperature
0.0123   The voltage offset required to discharge/charge at the third temperature (V)
10.      The fourth temperature discharged/charged at in Celsius 
1.       The beta required to discharge/charge at the fourth temperature 
0.0062   The voltage offset required to discharge/charge at the fourth temperature (V) 
20.      The fifth temperature discharged/charged at in Celsius
1.       The beta required to discharge/charge at the fifth temperature
0.0024   The voltage offset required to discharge/charge at the fifth temperature (V)
30.      The sixth temperature discharged/charged at in Celsius
1.       The beta required to discharge/charge at the sixth temperature
-1.5e-5  The voltage offset required to discharge/charge at the sixth temperature (V)
40.      The seventh temperature discharged/charged at in Celsius
1.       The beta required to discharge/charge at the seventh temperature
-0.0012  The voltage offset required to discharge/charge at the seventh temperature (V)
60.      The eighth temperature discharged/charged at in Celsius
1.       The beta required to discharge/charge at the eighth temperature
-0.0012  The voltage offset required to discharge/charge at the eighth temperature (V)
0.       self discharge constant in amps (keep zero if unknown or for zero self discharge)
0.       Activation energy for self-discharge in kJ.kmol-1 (keep zero if unknown  or for zero self discharge)
1        The index of the zone where the battery is located (-)
0.       The power required to run the li-on BMS circuitry (W)
0.1972   Second optional Exponential coefficent for Rint
-0.4749  Second optional Exponential coefficent for Rint
0.4118   Second optional Exponential coefficent for Rint
-0.1472  Second optional Exponential coefficent for Rint
0.0380   Second optional Exponential coefficent for Rint
0.0154   Second optional Exponential coefficent for Rint
16.      If the current is above this value, the internal resistance will be calculated with this 2nd set (A)
3000.    The crucial cycle to indicate the end of the cell's life
*TEXT
Lithium Ion Battery
*ENDTEXT
*END
*ENDCONDDB 
#*LOADSDB
#*MOGENDB
*ENDDB
