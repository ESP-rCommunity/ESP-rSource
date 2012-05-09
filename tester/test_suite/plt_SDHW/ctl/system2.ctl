no overall control description supplied
* Building
no zone control description supplied
   1  # No. of functions
* Control function    1
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: weekday     
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  100000.000 0.000 0.000 0.000 21.000 100.000 0.000
# Function:Zone links
 1,0,1,1,1,0
* Plant
no plant control description supplied
   4  # No. of loops
* Control loops    1
# senses var in compt.  3:dhw_tank @ node no.  1
   -1    3    1    0    0  # sensor 
# plant component   3:dhw_tank @ node no.  1
   -1    3    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: weekday     
   24    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 54.00000 56.00000 1.00000 0.00000 0.00000 0.00000
* Control loops    2
# sen var diff bet compt.  1:solar_collector @ node  1and compt  4:solar_tank @ node  1
   -1    1    1    4    1  # sensor 
# plant component   2:collector_pump @ node no.  1
   -1    2    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: weekday     
   24    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 1.00000 5.55000 0.00000 0.00004 0.00000 0.00000
* Control loops    3
# sen var diff bet compt.  1:solar_collector @ node  1and compt  4:solar_tank @ node  1
   -1    1    1    4    1  # sensor 
# plant component   6:tank_pump @ node no.  1
   -1    6    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: weekday     
   24    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 1.00000 5.55000 0.00000 0.00004 0.00000 0.00000
* Control loops    4
# sen var diff bet compt.  1:solar_collector @ node  1and compt  4:solar_tank @ node  1
   -1    1    1    4    1  # sensor 
# plant component   2:collector_pump @ node no.  1
   -1    2    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: weekday     
   25    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 54.00000 56.00000 1.00000 0.00000 0.00000 0.00000
