control for CCHT house
* Building
no descrip
   2  # No. of functions
* Control function    1
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: weekday     
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function    2
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     3  # No. of periods in day: weekday     
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  20000.000 0.000 0.000 0.000 18.000 100.000 0.000
    0    1   8.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  20000.000 0.000 0.000 0.000 21.000 100.000 0.000
    0    1  20.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  20000.000 0.000 0.000 0.000 18.000 100.000 0.000
# Function:Zone links
 1,2,1,1
