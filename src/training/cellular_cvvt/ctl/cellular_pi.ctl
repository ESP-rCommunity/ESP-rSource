Represents P+I control for heating and cooling. Heating sp 20C with 2deg prop band and 10min integral action. Cooling sp 25C with 2deg prop band and 10min integral action. Loop 1 is for manager_a loop 2 manager_b (with no integral) and the corridor
* Building
this is a variant of the base case for testing an air based P+I controller. Air based system with constrained capacity.
   3  # No. of functions
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
    0    5   7.000  # ctl type, law (basic proportional), start @
     10.  # No. of data items
  2.000 1000.000 0.000 20.000 2.000 1000.000 0.000 25.000 2.000 600.000
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
    0    5   7.000  # ctl type, law (basic proportional), start @
      9.  # No. of data items
  1.000 1000.000 0.000 20.000 2.000 1000.000 0.000 25.000 2.000
    0    2  19.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function    3
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
  1000.000 0.000 1000.000 0.000 20.000 24.000 0.000
    0    2  18.000  # ctl type, law (free floating), start @
      0.  # No. of data items
# Function:Zone links
 1,2,3
