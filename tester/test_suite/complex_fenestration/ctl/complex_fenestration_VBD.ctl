no overall control description supplied
* Building
no zone control description supplied
   3  # No. of functions
* Control function    1
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  10000.000 0.000 10000.000 0.000 20.000 24.000 0.000
* Control function    2
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  10000.000 0.000 10000.000 0.000 20.000 24.000 0.000
* Control function    3
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  10000.000 0.000 10000.000 0.000 20.000 100.000 0.000
# Function:Zone links
 1,2,3
* CFC
no complex fen. control description supplied
   6  # No. of functions
* Control function    1
# senses incident sol rad on Wall-1 in zone1.
   -4    1    1    0  # sensor data
# actuates Shading ON/OFF in CFC type  1 in zone1.
    0    1    1  # actuator data
    1  # all daytypes
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: weekdays    
    3    1   0.000  # ctl type, law (basic control), start @
      2.  # No. of data items
  200.000 150.000
* Control function    2
# senses incident sol rad on Wall-1 in zone1.
   -4    1    1    0  # sensor data
# actuates Shading ON/OFF in CFC type  2 in zone1.
    0    1    2  # actuator data
    1  # all daytypes
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: weekdays    
    3    1   0.000  # ctl type, law (basic control), start @
      2.  # No. of data items
  200.000 150.000
* Control function    3
# no sensor - schedule only.
    0    0    0    0  # sensor data
# actuates Shade ON/OFF and slat angle(schedule)  1 in zone2.
    2    2    1  # actuator data
    0 # day types follow calendar  3
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day: weekdays    
    7    2   0.000  # ctl type, law (schedule), start @
      2.  # No. of data items
  0.000 45.000
    7    2  12.000  # ctl type, law (schedule), start @
      2.  # No. of data items
  1.000 89.000
    7    2  15.000  # ctl type, law (schedule), start @
      2.  # No. of data items
  0.000 45.000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: saturday    
    7    2   0.000  # ctl type, law (schedule), start @
      2.  # No. of data items
  1.000 89.000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: sunday      
    7    2   0.000  # ctl type, law (schedule), start @
      2.  # No. of data items
  0.000 45.000
* Control function    4
# no sensor - schedule only.
    0    0    0    0  # sensor data
# actuates Shade ON/OFF and slat angle(schedule)  2 in zone2.
    2    2    2  # actuator data
    0 # day types follow calendar  3
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day: weekdays    
    7    2   0.000  # ctl type, law (schedule), start @
      2.  # No. of data items
  0.000 45.000
    7    2  12.000  # ctl type, law (schedule), start @
      2.  # No. of data items
  1.000 89.000
    7    2  15.000  # ctl type, law (schedule), start @
      2.  # No. of data items
  0.000 45.000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: saturday    
    7    2   0.000  # ctl type, law (schedule), start @
      2.  # No. of data items
  1.000 89.000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: sunday      
    7    2   0.000  # ctl type, law (schedule), start @
      2.  # No. of data items
  0.000 45.000
* Control function    5
# senses dry bulb temperature in zone3.
    3    0    0    0  # sensor data
# actuates Slat angle in CFC type  1 in zone3.
    1    3    1  # actuator data
    1  # all daytypes
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: weekdays    
    2    1   0.000  # ctl type, law (basic control), start @
      4.  # No. of data items
  22.000 15.000 30.000 0.000
* Control function    6
# senses dry bulb temperature in zone3.
    3    0    0    0  # sensor data
# actuates Slat angle in CFC type  2 in zone3.
    1    3    2  # actuator data
    1  # all daytypes
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: weekdays    
    2    1   0.000  # ctl type, law (basic control), start @
      4.  # No. of data items
  22.000 15.000 30.000 0.000
