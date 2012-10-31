AHU system
* Building
AHU plant coupled to zone 1
   2  # No. of functions
* Control function
# senses dry bulb temperature in reception.
    1    0    0    0  # sensor data
# actuates the air point in reception.
    1    0    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  7.000 1.000 1.000 10000.000 10000.000 2.000 1.000
* Control function
# senses dry bulb temperature in office.
    2    0    0    0  # sensor data
# actuates the air point in office.
    2    0    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  8.000 1.000 1.000 10000.000 10000.000 2.000 1.000
# Function:Zone links
 1,2,0
* Plant
Single zone AHU.
   2  # No. of loops
* Control loops
# senses var in compt.  4:4 @ node no.  1
   -1    4    1    0    0  # sensor 
# plant component   2:2 @ node no.  1
   -1    2    1    0  # actuator 
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
      9.  # No. of data items
  1.00000 10000.00000 0.00000 29.00000 3.00000 0.00000 0.00000 0.00000 0.00000
* Control loops
# senses var in compt.  4:4 @ node no.  1
   -1    4    1    0    0  # sensor 
# plant component   3:3 @ node no.  1
   -1    3    1    0  # actuator 
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
      9.  # No. of data items
  -1.00000 6000.00000 0.00000 29.00000 3.00000 0.00000 0.00000 0.00000 0.00000
* Mass Flow
VAV_sys
   2  # No. of controls
* Control mass
# senses node (7) recepn
   -4    7    0    0  # sensor data
# actuates flow connection:   6 n4 - recepn via VAV1
   -3    6    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    1    1   0.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 18.00 valve lower limit (%)**** signal upper limit 25.00 valve upper limit (%) 10.0 dS 0.00.), starting @
      5.  # No. of data items
  18.00000 100.00000 25.00000 10.00000 0.00000
* Control mass
# senses node (8) office
   -4    8    0    0  # sensor data
# actuates flow connection:   8 n4 - office via VAV2
   -3    8    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    1    1   0.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 18.00 valve lower limit (%)**** signal upper limit 25.00 valve upper limit (%) 10.0 dS 0.00.), starting @
      5.  # No. of data items
  18.00000 100.00000 25.00000 10.00000 0.00000
