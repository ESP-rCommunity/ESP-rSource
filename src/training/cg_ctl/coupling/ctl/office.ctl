proj cntrl
* Building
no descrip
   1  # No. of functions
* Control function    1
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # day types follow calendar  3
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day: weekday     
    0    1   0.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  2000.000 0.000 2000.000 0.000 12.000 100.000
    0    1   7.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  2000.000 0.000 2000.000 0.000 21.000 26.000
    0    1  18.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  2000.000 0.000 2000.000 0.000 12.000 100.000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: saturday    
    0    1   0.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  2000.000 0.000 2000.000 0.000 12.000 100.000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: sunday      
    0    1   0.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  2000.000 0.000 2000.000 0.000 12.000 100.000
# Function:Zone links
 1
