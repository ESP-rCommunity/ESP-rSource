no overall control description supplied
* Building
no zone control description supplied
   2  # No. of functions
* Control function    1
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sat-31-Dec
     1  # No. of periods in day: weekdays    
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  6.000 2.000 5.000 20000.000 0.000 7.000 1.000
* Control function    2
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sat-31-Dec
     1  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  30000.000 0.000 30000.000 0.000 10.000 25.000 0.000
# Function:Zone links
 2,1
* Plant
no plant control description supplied
   2  # No. of loops
* Control loops    1
# senses dry bulb temperature in house.
    2    0    0    0    0  # sensor 
# plant component   7:blower @ node no.  1
   -1    7    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sat-31-Dec
     1  # No. of periods in day: weekdays    
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.50000 21.50000 0.60000 0.00000 0.00000 0.00000
* Control loops    2
# senses var in compt.  2:tank @ node no.  1
   -1    2    1    0    0  # sensor 
# plant component   2:tank @ node no.  2
   -1    2    2    0  # actuator 
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sat-31-Dec
     1  # No. of periods in day: weekdays    
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 50.00000 60.00000 1.00000 0.00000 0.00000 0.00000
