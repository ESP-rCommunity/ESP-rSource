ideal convective representation free flt till 6 19-23 till 9 then free float until 17h00 19-23C until 23h00 then free float
* Building
approximates the control applied in the detailed systems model
   1  # No. of functions
* Control function    1
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # day types follow calendar  4
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     5  # No. of periods in day: weekdays    
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   6.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  4000.000 0.000 4000.000 0.000 21.000 100.000 0.000
    0    2   9.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1  17.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  4000.000 0.000 4000.000 0.000 21.000 100.000 0.000
    0    2  23.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     5  # No. of periods in day: saturday    
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   6.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  4000.000 0.000 4000.000 0.000 21.000 100.000 0.000
    0    2   9.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1  17.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  4000.000 0.000 4000.000 0.000 21.000 100.000 0.000
    0    2  23.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     5  # No. of periods in day: sunday      
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   6.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  4000.000 0.000 4000.000 0.000 21.000 100.000 0.000
    0    2   9.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1  17.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  4000.000 0.000 4000.000 0.000 21.000 100.000 0.000
    0    2  23.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     5  # No. of periods in day: holiday     
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   6.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  4000.000 0.000 4000.000 0.000 21.000 100.000 0.000
    0    2   9.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1  17.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  4000.000 0.000 4000.000 0.000 21.000 100.000 0.000
    0    2  23.000  # ctl type, law (free floating), start @
      0.  # No. of data items
# Function:Zone links
 1,1
