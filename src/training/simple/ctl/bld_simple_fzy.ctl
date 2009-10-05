bld_simple_fzy.ctl  # overall descr 
* Building
convective heating with fuzzy control  # bld descr 
   1  # No. of functions
* Control function
# measures dry bulb temperature in reception
    1    0    0    0  # sensor data
# actuates the air point in zone. 1
    1    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
    0   17   0.000  # ctl type, law (fuzzy logic PI-PD ctl), start @
      7.  # No. of data items
  1.000 1.000 21.000 2.000 1.000 1.000 0.900
# (Above) Data set no, PI mode, Set point, COA defuz,
#  Scale (error), Scale (change of error), Scale (output)
1 # Control function for Zone 1.
* Fuzzy Control
1      # Number of fuzzy data sets.
1      # Set number.
5      # Number of membership functions for "change of error" variable.
NB  NS  ZR  PS  PB  # labels for "change of error" membership functions
-100 0, -2.0 1, -0.5 0,  -1.0 0, -0.1 1, 0 0,  -0.1 0, 0 1, 0.1 0,  0 0, 0.1 1, 1.0 0,  0.5 0, 2.0 1, 100 0        # Function coords.
5      # Number of membership functions for "error" variable.
NB  NS  ZR  PS  PB  # labels for "error" membership functions
-100 0, -2.0 1, -0.5 0,  -1.0 0, -0.1 1, 0 0,  -0.1 0, 0 1, 0.1 0,  0 0, 0.1 1, 1.0 0,  0.5 0, 2.0 1, 100 0        # Function coords.
5      # Number of membership functions for "output" variable.
NB  NS  ZR  PS  PB  # labels for "output" membership functions
-8000 0, -1400 1, -200 0,  -1400 0, -200 1, 0 0,  -40 0, 0 1, 40 0,  0 0, 200 1, 1400 0, 200 0, 1400 1, 8000 0,    # Function coords.
PB   PB   PB  PS  ZR                   |
PB   PS   PS  ZR  NS                   |
PB   PS   ZR  NS  NB                   | Look-up table.
PS   ZR   NS  NS  NB                   |
ZR   NS   NB  NB  NB                   |
# Function:Zone links
  1
