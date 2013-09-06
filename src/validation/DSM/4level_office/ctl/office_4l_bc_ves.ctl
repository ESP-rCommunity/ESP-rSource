Four level office building base case heating to 22.22C and cooling to 23.8C all hours. Supply and extract zones not yet used
* Building
Office base case zone controls (setback is treated as a separate ECM)
   3  # No. of functions
* Control function    1
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # day types follow calendar  3
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  22000.000 0.000 22000.000 0.000 22.220 23.800 0.000
    0    1   7.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  22000.000 0.000 22000.000 0.000 22.220 23.800 0.000
    0    1  18.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  22000.000 0.000 22000.000 0.000 22.220 23.800 0.000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day: saturday    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  22000.000 0.000 22000.000 0.000 22.220 23.800 0.000
    0    1   7.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  22000.000 0.000 22000.000 0.000 22.220 23.800 0.000
    0    1  18.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  22000.000 0.000 22000.000 0.000 22.220 23.800 0.000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day: sunday      
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  22000.000 0.000 22000.000 0.000 22.220 23.800 0.000
    0    1   7.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  22000.000 0.000 22000.000 0.000 22.220 23.800 0.000
    0    1  18.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  20000.000 0.000 22000.000 0.000 22.220 23.800 0.000
* Control function    2
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # day types follow calendar  3
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day: weekdays    
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   7.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  40000.000 0.000 40000.000 0.000 22.220 23.800 0.000
    0    2  18.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day: saturday    
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   7.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  40000.000 0.000 40000.000 0.000 22.220 23.800 0.000
    0    2  18.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day: sunday      
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   7.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  40000.000 0.000 40000.000 0.000 22.220 23.800 0.000
    0    2  18.000  # ctl type, law (free floating), start @
      0.  # No. of data items
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
  20000.000 0.000 20000.000 0.000 18.000 28.000 0.000
# Function:Zone links
 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,3,0
