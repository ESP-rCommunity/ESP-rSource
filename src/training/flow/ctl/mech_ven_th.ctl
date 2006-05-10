proj cntrl  # overall descr 
* Mass Flow
no descrip  # flow descr 
   3  # No. of controls
* Control mass
# measures  mass flow node or connection.
   -4    0    0    0  # sensor data
# actuates mass flow component:   6 damp_1
   -4    6    1  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
   24    1   0.000  # ctl type (delt T > flow), law (proportional + hysteresis), start @
      5.  # No. of data items
  -5.000 100.000 5.000 10.000 0.000
fan_out       west          damp_1        west          reception   
* Control mass
# measures  mass flow node or connection.
   -4    0    0    0  # sensor data
# actuates mass flow component:   7 damp_2
   -4    7    1  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
   24    1   0.000  # ctl type (delt T > flow), law (proportional + hysteresis), start @
      5.  # No. of data items
  -5.000 1.000 5.000 90.000 0.000
fan_out       mix           damp_2        west          reception   
* Control mass
# measures  mass flow node or connection.
   -4    0    0    0  # sensor data
# actuates mass flow component:   8 damp_3
   -4    8    1  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
   25    1   0.000  # ctl type (abs Delt T > flow), law (proportional + hysteresis), start @
      5.  # No. of data items
  -5.000 100.000 5.000 10.000 0.000
west          mix           damp_3        west          reception   
