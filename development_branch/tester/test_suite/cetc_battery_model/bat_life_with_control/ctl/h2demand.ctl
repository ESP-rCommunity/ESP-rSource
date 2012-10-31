control for RES-H2 sys  # overall descr
* Building
no descrip  # bld descr
   1  # No. of functions
* Control function
# measures the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (ideal control), start @
      7.  # No. of data items
  20000.000 0.000 0.000 0.000 15.000 100.000 0.000
# Function:Zone links
   1  1  1  1
* Plant
no descrip  # plant descr
   1  # No. of loops
* Control loops
# Hydrogen demand controller
   -1    5    1    0 # <- These are dummy arguments / we'll define sensors below.
# plant component  h2-storage
   -1    4    1      # <- Pointer to hydrogen storage/supply unit to be controlled
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
        1  # No. of periods in day
    12   11   0.000  # ctl type, law (Hydrogen demand control), start @
    6.0  # No. of data items
# Arguements are: number of explicit components, and index of each comp.
#                 number of implicit components, and index of each comp.  
   1.000  2.000  3.0  9.000  10.000  11.000
