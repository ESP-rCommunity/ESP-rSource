no overall control description supplied
* Building
no zone control description supplied
   3  # No. of functions
* Control function    1
# senses dry bulb temperature in zone1.
    1    0    0    0  # sensor data
# actuates the air point in zone1.
    1    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  10000.000 0.000 10000.000 0.000 20.000 24.000 0.000
* Control function    2
# senses dry bulb temperature in zone2.
    2    0    0    0  # sensor data
# actuates the air point in zone2.
    2    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  10000.000 0.000 10000.000 0.000 20.000 24.000 0.000
* Control function    3
# senses dry bulb temperature in zone3.
    3    0    0    0  # sensor data
# actuates the air point in zone3.
    3    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  10000.000 0.000 10000.000 0.000 20.000 100.000 0.000
# Function:Zone links
 1,2,3
