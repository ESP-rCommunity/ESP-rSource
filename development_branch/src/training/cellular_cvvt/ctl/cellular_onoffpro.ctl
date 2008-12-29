Represents separate on off setpoint for heating and cooling with cycle limits. Heating on below 18C and off above 20C. Cooling on above 24C and off below 22C. Loop 1 is for manager_a loop 2 equiv on/off manager_b and loop 3 the corridor.
* Building
manager_a uses the simple separate ON OFF controller and manager_b uses the separate ON OFF controller with ON OFF cycle time limits.
   3  # No. of functions
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0   10   7.000  # ctl type, law (separate ON/OFF flux), start @
      6.  # No. of data items
  1500.000 1500.000 18.000 20.000 26.000 24.000
    0    2  19.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0   13   7.000  # ctl type, law (time-proportional ctl), start @
     12.  # No. of data items
  1500.000 1500.000 18.000 22.000 26.000 24.000 10.000 5.000 5.000 10.000 5.000 5.000
    0    2  19.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1500.000 0.000 1500.000 0.000 18.000 26.000 0.000
    0    2  19.000  # ctl type, law (free floating), start @
      0.  # No. of data items
# Function:Zone links
 1,2,3
