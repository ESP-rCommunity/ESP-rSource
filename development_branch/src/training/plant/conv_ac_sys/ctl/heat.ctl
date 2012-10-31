AHU system
* Building
AHU plant with return coupled to zone 1
   1  # No. of functions
* Control function    1
# senses dry bulb temperature in reception.
    1    0    0    0  # sensor data
# actuates the air point in reception.
    1    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sat-01-Jan - Sun-31-Dec
     1  # No. of periods in day: weekday     
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  5.000 4.000 1.000 1000.000 1000.000 6.000 1.000
# Function:Zone links
 1,0,0
* Plant
Single zone AHU.
   1  # No. of loops
* Control loops    1
# senses var in compt.  6:return_duct @ node no.  1
   -1    6    1    0    0  # sensor 
# plant component   5:heater @ node no.  1
   -1    5    1    0  # actuator 
    1 # No. day types
    1  365  # valid Sat-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekday     
    0    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.00000 6000.00000 0.00000 19.00000 2.00000 0.00000 0.00000
    0    0  18.000  # ctl type, law (period off), start @
      0.  # No. of data items
