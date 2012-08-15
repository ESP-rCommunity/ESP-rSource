tp_subs.ctl
* Building
summer/winter t-p substitution
   4  # No. of functions
* Control function    1
# senses dry bulb temperature in reception.
    1    0    0    3  # sensor data
# actuates the air point in reception.
    1    0    0  # actuator data
    0 # day types follow calendar  3
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     3  # No. of periods in day: weekday     
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  10000.000 0.000 1000.000 0.000 25.000 100.000
    0    2  18.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: saturday    
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: sunday      
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function    2
# senses dry bulb temperature in office.
    2    0    0    4  # sensor data
# actuates the air point in office.
    2    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     3  # No. of periods in day: weekday     
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  10000.000 0.000 1000.000 0.000 25.000 100.000
    0    2  18.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function    3
#  construction substitution.
  -99   20 -100    0  # sensor data
# actuates construction substitution to surfaces:  1, 2, 4,
    1    2    4  # actuator data
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: weekday     
    0    1   0.000  # ctl type, law (substitution), start @
      4.  # No. of data items
  1.000 10.000 10.000 10.000
* Control function    4
#  construction substitution.
  -99   20 -100    0  # sensor data
# actuates construction substitution to surfaces:  3, 4, 0,
    3    4    0  # actuator data
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: weekday     
    0    1   0.000  # ctl type, law (substitution), start @
      4.  # No. of data items
  1.000 10.000 10.000 10.000
# Function:Zone links
 1,2,0
