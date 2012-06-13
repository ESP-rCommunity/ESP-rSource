proj cntrl
* Building
no zone control description supplied
   1  # No. of functions
* Control function    1
# senses dry bulb temperature in house.
    1    0    0    0  # sensor data
# actuates mixed convective/radiant flux in house.
   -2    1   50  # actuator data
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sat-31-Dec
     1  # No. of periods in day: weekdays    
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  2.000 1.000 2.000 99000.000 99000.000 0.000 0.000
# Function:Zone links
 1
* Plant
no plant control description supplied
   1  # No. of loops
* Control loops    1
# senses a mix of db T and MRT in house.
   -2    1   100    0    0  # sensor 
# plant component   1:boiler @ node no.  1
   -1    1    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sat-31-Dec
     1  # No. of periods in day: weekdays    
    0    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 19.00000 21.00000 5000.00000 0.00000 0.00000 0.00000
