UK_DE ASHP w/ buffer + MVHR + sol therm (DHW)
* Building
HEAT
   2  # No. of functions
* Control function    1
# senses dry bulb temperature in living.
    1    0    0    0  # sensor data
# actuates the air point in living.
    1    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sat-31-Dec
     1  # No. of periods in day: weekdays    
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  24.000 1.000 1.000 99000.000 99000.000 26.000 1.000
* Control function    2
# senses dry bulb temperature in non_living.
    2    0    0    0  # sensor data
# actuates the air point in non_living.
    2    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sat-31-Dec
     1  # No. of periods in day: weekdays    
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  25.000 1.000 1.000 99000.000 99000.000 26.000 1.000
# Function:Zone links
 1,2,0
* Plant
UK-DE: ASHP - w/Buffer Tank
   7  # No. of loops
* Control loops    1
# senses var in compt.  5:DHW_tank @ node no.  1
   -1    5    1    0    0  # sensor 
# plant component  16:hw_pump @ node no.  1
   -1   16    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sat-31-Dec
     5  # No. of periods in day: weekdays    
   12    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
    1    8   5.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 45.00000 55.00000 0.15000 0.00000 0.00000 0.00000
   12    0   9.000  # ctl type, law (period off), start @
      0.  # No. of data items
    1    8  16.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 45.00000 55.00000 0.15000 0.00000 0.00000 0.00000
   12    0  23.000  # ctl type, law (period off), start @
      0.  # No. of data items
* Control loops    2
# senses dry bulb temperature in living.
    1    0    0    0    0  # sensor 
# plant component   16:hw_pump @ node no.  1
   -1    16    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sat-31-Dec
     5  # No. of periods in day: weekdays    
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 12.00000 16.00000 0.15000 0.00000 0.00000 0.00000
    1    8   6.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 19.00000 23.00000 0.15000 0.00000 0.00000 0.00000
    1    8   9.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 12.00000 16.00000 0.15000 0.00000 0.00000 0.00000
    1    8  17.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 19.00000 23.00000 0.15000 0.00000 0.00000 0.00000
    1    8  23.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 12.00000 16.00000 0.15000 0.00000 0.00000 0.00000
* Control loops    3
# senses var in compt.  5:DHW_tank @ node no.  1
   -1    5    1    0    0  # sensor 
# plant component   4:DHW_divert @ node no.  1
   -1    4    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sat-31-Dec
     5  # No. of periods in day: weekdays    
   12    3   0.000  # ctl type, law (Prop numerical ctrl.), start @
      5.  # No. of data items
  0.00000 0.00000 55.00000 45.00000 0.00000
   12    3   6.000  # ctl type, law (Prop numerical ctrl.), start @
      5.  # No. of data items
  0.00000 1.00000 55.00000 45.00000 0.00000
   12    3   9.000  # ctl type, law (Prop numerical ctrl.), start @
      5.  # No. of data items
  0.00000 0.00000 55.00000 45.00000 0.00000
   12    3  17.000  # ctl type, law (Prop numerical ctrl.), start @
      5.  # No. of data items
  0.00000 1.00000 55.00000 45.00000 0.00000
   12    3  23.000  # ctl type, law (Prop numerical ctrl.), start @
      5.  # No. of data items
  0.00000 0.00000 55.00000 45.00000 0.00000
* Control loops    4
# senses dry bulb temperature in living.
    1    0    0    0    0  # sensor 
# plant component   8:hcoil_3PDV @ node no.  1
   -1    8    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sat-31-Dec
     5  # No. of periods in day: weekdays    
   12    3   0.000  # ctl type, law (Prop numerical ctrl.), start @
      5.  # No. of data items
  0.00000 1.00000 16.00000 12.00000 0.00000
   12    3   6.000  # ctl type, law (Prop numerical ctrl.), start @
      5.  # No. of data items
  0.00000 1.00000 23.00000 19.00000 0.00000
   12    3   9.000  # ctl type, law (Prop numerical ctrl.), start @
      5.  # No. of data items
  0.00000 1.00000 16.00000 12.00000 0.00000
   12    3  17.000  # ctl type, law (Prop numerical ctrl.), start @
      5.  # No. of data items
  0.00000 1.00000 23.00000 19.00000 0.00000
   12    3  23.000  # ctl type, law (Prop numerical ctrl.), start @
      5.  # No. of data items
  0.00000 1.00000 16.00000 12.00000 0.00000
* Control loops    5
# senses var in compt.  16:hw_pump @ node no.  1
   -1    16    1    0    0  # sensor 
# plant component   16:hw_pump @ node no.  1
   -1    16    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sat-31-Dec
     1  # No. of periods in day: weekdays    
   41   14   0.000  # ctl type, law (Multi-sensor), start @
      7.  # No. of data items
  1.00000 1.00000 2.00000 0.15000 0.00000 1.00000 0.00000
* Control loops    3
# sen var diff bet compt. 15:Collector_C @ node  1and compt  5:DHW_tank @ node  3
   -1   15    1    5    3  # sensor 
# plant component  12:sol_pump @ node no.  1
   -1   12    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sat-31-Dec
     1  # No. of periods in day: weekdays    
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  -1.00000 5.00000 2.00000 0.03000 0.00000 0.00000 0.00000
* Control loops    4
# senses var in compt. 17:buff_tank @ node no.  1
   -1   17    1    0    0  # sensor 
# plant component   1:ASHP @ node no.  1
   -1    1    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sat-31-Dec
     1  # No. of periods in day: weekdays    
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 55.00000 65.00000 1.00000 0.00000 0.00000 0.00000
