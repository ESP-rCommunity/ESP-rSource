proj cntrl  # overall descr 
* Mass Flow
flow control applied to F4 hall to roof, F0 hall door & window, F1 hall window 
   4  # No. of controls
* Control mass
# senses node (  1) F4_hall
   -4    1    0    0  # sensor data
# actuates flow connection:   6 F4_hall - roof via atri-roof
   -3    6    0  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
    1    1   6.000  # ctl type (dry bulb > flow), law (proportional + hysteresis), start @
      5.  # No. of data items
  24.000 0.000 32.000 100.000 0.000
    1    0  22.000  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
* Control mass
# senses node (  5) F0_hall
   -4    5    0    0  # sensor data
# actuates flow connection:   5 ground - F0_hall via Door
   -3    5    0  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
    1    1   6.000  # ctl type (dry bulb > flow), law (proportional + hysteresis), start @
      5.  # No. of data items
  24.000 0.000 32.000 100.000 0.000
    1    0  22.000  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
* Control mass
# senses node (  5) F0_hall
   -4    5    0    0  # sensor data
# actuates flow connection:  18 F0_hall - F0_win via Window
   -3   18    0  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
    1    1   8.000  # ctl type (dry bulb > flow), law (proportional + hysteresis), start @
      5.  # No. of data items
  28.000 0.000 32.000 100.000 0.000
    1    0  19.000  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
* Control mass
# senses node (  4) F1_hall
   -4    4    0    0  # sensor data
# actuates flow connection:  19 F1_hall - F1_win via Window
   -3   19    0  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
    1    1   8.000  # ctl type (dry bulb > flow), law (proportional + hysteresis), start @
      5.  # No. of data items
  28.000 0.000 32.000 100.000 0.000
    1    0  19.000  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
