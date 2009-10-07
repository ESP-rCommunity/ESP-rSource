proj cntrl
* Mass Flow
no descrip
   3  # No. of controls
* Control mass
# sensor node(s) not yet defined
   -4    0    0    0  # sensor data
# actuates flow component:   6 damp_1
   -4    6    1  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
   24    1   0.000  # ctl type (delt T > flow), law (proportional + hysteresis: signal lower limit -5.00 valve lower limit (%)**** signal upper limit 5.00 valve upper limit (%) 10.0 dS 0.00.), starting @
      5.  # No. of data items
  -5.00000 100.00000 5.00000 10.00000 0.00000
fan_out       west          damp_1        west          reception   
* Control mass
# sensor node(s) not yet defined
   -4    0    0    0  # sensor data
# actuates flow component:   7 damp_2
   -4    7    1  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
   24    1   0.000  # ctl type (delt T > flow), law (proportional + hysteresis: signal lower limit -5.00 valve lower limit (%) 1.0 signal upper limit 5.00 valve upper limit (%) 90.0 dS 0.00.), starting @
      5.  # No. of data items
  -5.00000 1.00000 5.00000 90.00000 0.00000
fan_out       mix           damp_2        west          reception   
* Control mass
# sensor node(s) not yet defined
   -4    0    0    0  # sensor data
# actuates flow component:   8 damp_3
   -4    8    1  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
   25    1   0.000  # ctl type (abs Delt T > flow), law (proportional + hysteresis: signal lower limit -5.00 valve lower limit (%)**** signal upper limit 5.00 valve upper limit (%) 10.0 dS 0.00.), starting @
      5.  # No. of data items
  -5.00000 100.00000 5.00000 10.00000 0.00000
west          mix           damp_3        west          reception   
