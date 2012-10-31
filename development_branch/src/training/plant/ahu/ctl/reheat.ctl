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
   4  # No. of loops
* Control loops
# senses var in compt.  4:fan1 @ node no.  1
   -1    4    1    0    0  # sensor 
# plant component   2:heat_coil @ node no.  1
   -1    2    1    0  # actuator 
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
      9.  # No. of data items
  1.00000 6000.00000 0.00000 20.00000 3.00000 1.00000 100.00000 0.00000 0.00000
* Control loops
# senses var in compt.  4:fan1 @ node no.  1
   -1    4    1    0    0  # sensor 
# plant component   3:cool_coil @ node no.  1
   -1    3    1    0  # actuator 
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
      9.  # No. of data items
  -1.00000 6000.00000 0.00000 23.00000 2.00000 1.00000 150.00000 0.00000 0.00000
* Control loops
# senses dry bulb temperature in reception.
    1    0    0    0    0  # sensor 
# plant component   7:re_heat1 @ node no.  1
   -1    7    1    0  # actuator 
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
      9.  # No. of data items
  1.00000 3000.00000 0.00000 20.00000 2.00000 1.00000 100.00000 0.00000 0.00000
* Control loops
# senses dry bulb temperature in office.
    2    0    0    0    0  # sensor 
# plant component   8:re_heat2 @ node no.  1
   -1    8    1    0  # actuator 
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
      9.  # No. of data items
  1.00000 3000.00000 0.00000 20.00000 2.00000 1.00000 100.00000 0.00000 0.00000
