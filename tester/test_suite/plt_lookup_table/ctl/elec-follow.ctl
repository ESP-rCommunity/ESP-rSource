demo A
* Building
plant injects heat to main
   2  # No. of functions
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  6.000 2.000 5.000 99000.000 99000.000 5.000 1.000
# Function:Zone links
 1,2,1,1
* Plant
DG controller
   4  # No. of loops
* Control loops
# senses var in compt.  2:hot-water-tank @ node no.  1
   -1    2    1    0  # sensor 
# plant component   2:hot-water-tank @ node no.  2
   -1    2    2    0  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 50.00000 60.00000 1.00000 0.00000 0.00000 0.00000
* Control loops
# senses dry bulb temperature in main_second.
    2    0    0    0  # sensor 
# plant component   5:coil-fan @ node no.  1
   -1    5    1    0  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 19.50000 20.50000 0.31000 0.00000 0.00000 0.00000
* Control loops
# senses dry bulb temperature in main_second.
    2    0    0    0  # sensor 
# plant component   4:coil-pump @ node no.  1
   -1    4    1    0  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 19.50000 20.50000 0.00030 0.00000 0.00000 0.00000
* Control loops
# senses var in compt.  2:hot-water-tank @ node no.  1
   -1    2    1    0  # sensor 
# plant component   2:hot-water-tank @ node no.  2
   -1    2    2    0  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
   12   10   0.000  # ctl type, law (Cogen. controller), start @
     18.  # No. of data items
  2000.00000 2.00000 2.00000 1.00000 -1.00000 -1.00000 1.00000 365.00000
  2.00000 62.00000 72.00000
  0.80000 0.80000 0.00004 0.00000 2.00000 0.25
  1.00000
