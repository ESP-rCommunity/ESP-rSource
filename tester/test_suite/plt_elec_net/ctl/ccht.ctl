control for CCHT house
* Building
no descrip
   3  # No. of functions
* Control function    1
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: weekday     
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function    2
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    3 # No. day types using dates of validity
    1  120  # valid Fri-01-Jan - Fri-30-Apr
     1  # No. of periods in day: weekday     
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  0.000 0.000 0.000 0.000 0.000 100.000 0.000
  121  273  # valid Sat-01-May - Thu-30-Sep
     1  # No. of periods in day: saturday    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  0.000 0.000 0.000 0.000 0.000 100.000 0.000
  274  365  # valid Fri-01-Oct - Fri-31-Dec
     1  # No. of periods in day: sunday      
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  0.000 0.000 0.000 0.000 20.000 100.000 0.000
* Control function    3
# senses dry bulb temperature in main_second.
    2    0    0    0  # sensor data
# actuates the air point in main_second.
    2    0    0  # actuator data
    3 # No. day types using dates of validity
    1  120  # valid Fri-01-Jan - Fri-30-Apr
     1  # No. of periods in day: weekday     
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  6.000 2.000 1.000 99000.000 99000.000 10.000 1.000
  121  273  # valid Sat-01-May - Thu-30-Sep
     1  # No. of periods in day: saturday    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  0.000 0.000 0.000 0.000 0.000 100.000 0.000
  274  365  # valid Fri-01-Oct - Fri-31-Dec
     1  # No. of periods in day: sunday      
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  6.000 2.000 1.000 99000.000 99000.000 10.000 1.000
# Function:Zone links
 1,3,1,1
* Plant
no descrip
   4  # No. of loops
* Control loops    1
# senses var in compt.  2:hot-water-tank @ node no.  1
   -1    2    1    0    0  # sensor 
# plant component   2:hot-water-tank @ node no.  2
   -1    2    2    0  # actuator 
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: weekday     
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 50.00000 60.00000 1.00000 0.00000 0.00000 0.00000
* Control loops    2
# senses dry bulb temperature in main_second.
    2    0    0    0    0  # sensor 
# plant component   5:coil-fan @ node no.  1
   -1    5    1    0  # actuator 
    3 # No. day types using dates of validity
    1  120  # valid Fri-01-Jan - Fri-30-Apr
     1  # No. of periods in day: weekday     
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 19.50000 20.50000 0.31000 0.00000 0.00000 0.00000
  121  273  # valid Sat-01-May - Thu-30-Sep
     1  # No. of periods in day: saturday    
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 10.00000 50.00000 0.00000 0.00000 0.00000 0.00000
  274  365  # valid Fri-01-Oct - Fri-31-Dec
     1  # No. of periods in day: sunday      
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 19.50000 20.50000 0.31000 0.00000 0.00000 0.00000
* Control loops    3
# senses dry bulb temperature in main_second.
    2    0    0    0    0  # sensor 
# plant component   4:coil-pump @ node no.  1
   -1    4    1    0  # actuator 
    3 # No. day types using dates of validity
    1  120  # valid Fri-01-Jan - Fri-30-Apr
     1  # No. of periods in day: weekday     
    0    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 19.50000 20.50000 0.00030 0.00000 0.00000 0.00000
  121  273  # valid Sat-01-May - Thu-30-Sep
     1  # No. of periods in day: saturday    
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 10.00000 50.00000 0.00030 0.00000 0.00000 0.00000
  274  365  # valid Fri-01-Oct - Fri-31-Dec
     1  # No. of periods in day: sunday      
    0    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 19.50000 20.50000 0.00030 0.00000 0.00000 0.00000
* Control loops    4
# senses var in compt.  2:hot-water-tank @ node no.  1
   -1    2    1    0    0  # sensor 
# plant component   2:hot-water-tank @ node no.  2
   -1    2    2    0  # actuator 
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: weekday     
   12   10   0.000  # ctl type, law (Cogen. controller), start @
     18.  # No. of data items
  2700.00000 2.00000 2.00000 1.00000 -1.00000 -1.00000 1.00000 365.00000 3.00000 62.00000 72.00000 0.80000 0.80000 0.00004 0.00000 2.00000 0.25000
  1.00000
