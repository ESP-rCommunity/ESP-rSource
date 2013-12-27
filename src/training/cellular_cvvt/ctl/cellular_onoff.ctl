Represents separate on off setpoint for heating and cooling. Heating on below 18C and off above 20C. Cooling on above 24C and off below 22C. Loop 1 is for manager_a loop 2 equiv on/off manager_b and the corridor (note how the two offices perform).
* Building
this is a variant of the base case set of assumptions for testing a different control scheme in manager_a and manager_b.
   2  # No. of functions
* Control function    1
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekdays    
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0   10   7.000  # ctl type, law (separate ON/OFF flux), start @
      6.  # No. of data items
  1500.000 1500.000 18.000 20.000 26.000 24.000
    0    2  19.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function    2
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekdays    
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1500.000 0.000 1500.000 0.000 18.000 26.000 0.000
    0    2  19.000  # ctl type, law (free floating), start @
      0.  # No. of data items
# Function:Zone links
 1,2,2
