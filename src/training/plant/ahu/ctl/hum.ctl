AHU system
* Building
AHU plant coupled to zone 1
   1  # No. of functions
* Control function    1
# senses dry bulb temperature in reception.
    1    0    0    0  # sensor data
# actuates the air point in reception.
    1    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sun-31-Dec
     1  # No. of periods in day: weekday     
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  11.000 1.000 1.000 100000.000 100000.000 13.000 1.000
# Function:Zone links
 1,0,0
* Plant
Single zone AHU.
   4  # No. of loops
* Control loops    1
# senses var in compt.  5:supply_duct3 @ node no.  1
   -1    5    1    0    0  # sensor 
# plant component   4:prehtg_coil @ node no.  1
   -1    4    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekday     
    0    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (PID flux control.), start @
     14.  # No. of data items
  1.00000 3500.00000 0.00000 -1001.00000 2.00000 0.00000 0.00000 1.00000 1.00000 2.00000 12.00000 13.00000 19.00000 50.00000
    0    0  18.000  # ctl type, law (period off), start @
      0.  # No. of data items
* Control loops    2
# senses var in compt.  7:supply_duct4 @ node no.  1
   -1    7    1    0    0  # sensor 
# plant component   6:humidifier @ node no.  1
   -1    6    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekday     
   11    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
   11    2   7.000  # ctl type, law (PID flow control.), start @
     14.  # No. of data items
  1.00000 0.00500 0.00100 -1001.00000 12.00000 0.00000 0.00000 3.00000 3.00000 2.00000 12.00000 13.00000 19.00000 50.00000
   11    0  18.000  # ctl type, law (period off), start @
      0.  # No. of data items
* Control loops    3
# senses var in compt. 11:supply_duct6 @ node no.  1
   -1   11    1    0    0  # sensor 
# plant component  10:re-heat_coil @ node no.  1
   -1   10    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekday     
    0    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (PID flux control.), start @
     14.  # No. of data items
  1.00000 5000.00000 0.00000 -1001.00000 2.00000 0.00000 0.00000 1.00000 2.00000 2.00000 12.00000 13.00000 19.00000 50.00000
    0    0  18.000  # ctl type, law (period off), start @
      0.  # No. of data items
* Control loops    4
# senses var in compt.  9:supply_duct5 @ node no.  1
   -1    9    1    0    0  # sensor 
# plant component   8:cooling_coil @ node no.  1
   -1    8    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekday     
   11    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
   10    1   7.000  # ctl type, law (PID flux control.), start @
     14.  # No. of data items
  -1.00000 800.00000 0.00000 -1001.00000 2.00000 0.00000 0.00000 1.00000 2.00000 2.00000 12.00000 13.00000 19.00000 50.00000
   11    0  18.000  # ctl type, law (period off), start @
      0.  # No. of data items
