heating 5C overnight, 21C until late evening. Bedrooms are 18C. All days the same.  Each loop sized based on inspection of Jan demands.
* Building
lower buffer zones heated (but a bit cooler) than the flat.
  12  # No. of functions
* Control function    1
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Wed-01-Jan - Wed-31-Dec
     4  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  3000.000 0.000 1000.000 0.000 15.000 100.000 0.000
    0    1   6.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  3000.000 0.000 1000.000 0.000 21.000 100.000 0.000
    0    1   9.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  3000.000 0.000 1000.000 0.000 20.000 100.000 0.000
    0    1  18.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  3000.000 0.000 1000.000 0.000 21.000 100.000 0.000
* Control function    2
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Wed-01-Jan - Wed-31-Dec
     4  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2000.000 0.000 1000.000 0.000 5.000 100.000 0.000
    0    1   7.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 1000.000 0.000 18.000 100.000 0.000
    0    1   9.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 1000.000 0.000 15.000 100.000 0.000
    0    1  18.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 1000.000 0.000 18.000 100.000 0.000
* Control function    3
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Wed-01-Jan - Wed-31-Dec
     3  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  3000.000 0.000 1000.000 0.000 5.000 100.000 0.000
    0    1   6.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  3000.000 0.000 1000.000 0.000 21.000 100.000 0.000
    0    1  23.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  3000.000 0.000 1000.000 0.000 5.000 100.000 0.000
* Control function    4
# senses a mix of db T and MRT in bedroom1.
   -2    3   35    0  # sensor data
# actuates mixed convective/radiant flux in bedroom1.
   -2    3   80  # actuator data
    1  # all daytypes
    1  365  # valid Wed-01-Jan - Wed-31-Dec
     3  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2400.000 0.000 1000.000 0.000 5.000 100.000 0.000
    0    1   6.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  2400.000 0.000 1000.000 0.000 18.000 100.000 0.000
    0    1  23.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2400.000 0.000 1000.000 0.000 5.000 100.000 0.000
* Control function    5
# senses a mix of db T and MRT in kiten_util.
   -2    2   35    0  # sensor data
# actuates mixed convective/radiant flux in kiten_util.
   -2    2   80  # actuator data
    1  # all daytypes
    1  365  # valid Wed-01-Jan - Wed-31-Dec
     3  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 1000.000 0.000 5.000 100.000 0.000
    0    1   6.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 1000.000 0.000 21.000 100.000 0.000
    0    1  23.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 1000.000 0.000 5.000 100.000 0.000
* Control function    6
# senses a mix of db T and MRT in bath.
   -2    4   35    0  # sensor data
# actuates mixed convective/radiant flux in bath.
   -2    4   80  # actuator data
    1  # all daytypes
    1  365  # valid Wed-01-Jan - Wed-31-Dec
     3  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  800.000 0.000 1000.000 0.000 5.000 100.000 0.000
    0    1   6.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  800.000 0.000 1000.000 0.000 21.000 100.000 0.000
    0    1  23.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  800.000 0.000 1000.000 0.000 5.000 100.000 0.000
* Control function    7
# senses a mix of db T and MRT in living.
   -2    6   35    0  # sensor data
# actuates mixed convective/radiant flux in living.
   -2    6   80  # actuator data
    1  # all daytypes
    1  365  # valid Wed-01-Jan - Wed-31-Dec
     3  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 1000.000 0.000 5.000 100.000 0.000
    0    1   6.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 1000.000 0.000 21.000 100.000 0.000
    0    1  23.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 1000.000 0.000 5.000 100.000 0.000
* Control function    8
# senses a mix of db T and MRT in hall.
   -2    7   35    0  # sensor data
# actuates mixed convective/radiant flux in hall.
   -2    7   80  # actuator data
    1  # all daytypes
    1  365  # valid Wed-01-Jan - Wed-31-Dec
     3  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1600.000 0.000 1000.000 0.000 5.000 100.000 0.000
    0    1   6.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  1600.000 0.000 1000.000 0.000 21.000 100.000 0.000
    0    1  23.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1600.000 0.000 1000.000 0.000 5.000 100.000 0.000
* Control function    9
# senses a mix of db T and MRT in study.
   -2    8   35    0  # sensor data
# actuates mixed convective/radiant flux in study.
   -2    8   80  # actuator data
    1  # all daytypes
    1  365  # valid Wed-01-Jan - Wed-31-Dec
     3  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  800.000 0.000 1000.000 0.000 5.000 100.000 0.000
    0    1   6.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  800.000 0.000 1000.000 0.000 21.000 100.000 0.000
    0    1  23.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  800.000 0.000 1000.000 0.000 5.000 100.000 0.000
* Control function   10
# senses a mix of db T and MRT in family.
   -2    9   35    0  # sensor data
# actuates mixed convective/radiant flux in family.
   -2    9   80  # actuator data
    1  # all daytypes
    1  365  # valid Wed-01-Jan - Wed-31-Dec
     3  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1600.000 0.000 1000.000 0.000 5.000 100.000 0.000
    0    1   6.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  1600.000 0.000 1000.000 0.000 21.000 100.000 0.000
    0    1  23.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1600.000 0.000 1000.000 0.000 5.000 100.000 0.000
* Control function   11
# senses a mix of db T and MRT in bedroom2.
   -2    5   35    0  # sensor data
# actuates mixed convective/radiant flux in bedroom2.
   -2    5   80  # actuator data
    1  # all daytypes
    1  365  # valid Wed-01-Jan - Wed-31-Dec
     3  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1500.000 0.000 1000.000 0.000 5.000 100.000 0.000
    0    1   6.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  1500.000 0.000 1000.000 0.000 18.000 100.000 0.000
    0    1  23.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1500.000 0.000 1000.000 0.000 5.000 100.000 0.000
* Control function   12
# senses a mix of db T and MRT in guest_room.
   -2    1   35    0  # sensor data
# actuates mixed convective/radiant flux in guest_room.
   -2    1   80  # actuator data
    1  # all daytypes
    1  365  # valid Wed-01-Jan - Wed-31-Dec
     3  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1500.000 0.000 1000.000 0.000 5.000 100.000 0.000
    0    1   6.500  # ctl type, law (basic control), start @
      7.  # No. of data items
  1500.000 0.000 1000.000 0.000 18.000 100.000 0.000
    0    1  23.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1500.000 0.000 1000.000 0.000 5.000 100.000 0.000
# Function:Zone links
 12,5,4,6,11,7,8,9,10,0,0,0,0
