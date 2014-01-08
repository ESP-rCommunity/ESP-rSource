proj cntrl
* Mass Flow
flow control applied to F4 hall to roof, F0 hall door & window, F1 hall window
   4  # No. of controls
* Control mass    1
# senses node (1) F4_hall
   -4    1    0    0  # sensor data
# actuates flow connection:   6 F4_hall - roof via atri-roof
   -3    6    0  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekday     
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      2.  # No. of data items
  100.00000 1.00000
    1    1   6.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 24.00 valve lower limit (%) 0.0 signal upper limit 32.00 valve upper limit (%)100.0 dS 0.00.), starting @
      5.  # No. of data items
  24.00000 0.00000 32.00000 100.00000 0.00000
    1    0  22.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      2.  # No. of data items
  100.00000 1.00000
* Control mass    2
# senses node (5) F0_hall
   -4    5    0    0  # sensor data
# actuates flow connection:   5 ground - F0_hall via Door
   -3    5    0  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekday     
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      2.  # No. of data items
  100.00000 1.00000
    1    1   6.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 24.00 valve lower limit (%) 0.0 signal upper limit 32.00 valve upper limit (%)100.0 dS 0.00.), starting @
      5.  # No. of data items
  24.00000 0.00000 32.00000 100.00000 0.00000
    1    0  22.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      2.  # No. of data items
  100.00000 1.00000
* Control mass    3
# senses node (5) F0_hall
   -4    5    0    0  # sensor data
# actuates flow connection:  18 F0_hall - F0_win via Window
   -3   18    0  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekday     
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      2.  # No. of data items
  100.00000 1.00000
    1    1   8.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 28.00 valve lower limit (%) 0.0 signal upper limit 32.00 valve upper limit (%)100.0 dS 0.00.), starting @
      5.  # No. of data items
  28.00000 0.00000 32.00000 100.00000 0.00000
    1    0  19.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      2.  # No. of data items
  100.00000 1.00000
* Control mass    4
# senses node (4) F1_hall
   -4    4    0    0  # sensor data
# actuates flow connection:  19 F1_hall - F1_win via Window
   -3   19    0  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekday     
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      2.  # No. of data items
  100.00000 1.00000
    1    1   8.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 28.00 valve lower limit (%) 0.0 signal upper limit 32.00 valve upper limit (%)100.0 dS 0.00.), starting @
      5.  # No. of data items
  28.00000 0.00000 32.00000 100.00000 0.00000
    1    0  19.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      2.  # No. of data items
  100.00000 1.00000
