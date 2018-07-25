control for CCHT house  # overall descr
* Plant
no descrip  # plant descr
  3  # No. of loops
* Control loops
# measures dummy sensor 
  -1    1    1    0 # sensor
# plant component: water flow source - temperature
  -1    1    1    0 # actuator ( src component temperature )
  1                 # No. day types
  1  365            # valid Fri  1 Jan - Fri 30 Apr
  1                 # No. of periods in day
  0  12  0.000      # ctl type, law (Boundary condition control), start @
  2.                # No. of data items
  1.0 1.0           # corresponding BC column, and interpolation method (1=step)
* Control loops
# measures dummy sensor
  -1    1    1    0 # sensor
# plant component: water flow source - temperature
  -1    1    2    0 # actuator (source flow rate)
  1                 # No. day types
  1  365            # valid Fri  1 Jan - Fri 30 Apr
  1                 # No. of periods in day
  0   12  0.000     # ctl type, law (Boundary condition control), start @
  2.                # No. of data items
  2.00 1.00         # corresponding BC column, and interpolation method (1=step)
* Control loops
# measures dummy sensor
  -1    1    1    0 # sensor
# plant component: SE control mode
  -1    2    1    0 # actuator: SE control mode variable
  1                 # No. day types
  1  365            # valid Fri  1 Jan - Fri 30 Apr
  1                 # No. of periods in day
  0   12  0.000     # ctl type, law (Boundary condition control), start @
  2.                # No. of data items
  3.00 1.00         # corresponding BC column, and interpolation method (1=step)
