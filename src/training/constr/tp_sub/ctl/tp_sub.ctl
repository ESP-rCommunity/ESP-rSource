tp_subs.ctl  # overall descr 
* Building
summer/winter t-p substitution  # bld descr 
   4  # No. of functions
* Control function
# measures dry bulb temperature in reception
    1    0    0    3  # sensor data
# actuates the air point in zone. 1
    1    0    0  # actuator data
    0 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     3  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  10000.000 0.000 1000.000 0.000 25.000 100.000
    0    2  18.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function
# measures dry bulb temperature in office
    2    0    0    4  # sensor data
# actuates the air point in zone. 2
    2    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     3  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  10000.000 0.000 1000.000 0.000 25.000 100.000
    0    2  18.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function
#  construction substitution.
  -99   20 -100    0  # sensor data
# actuates construction substitution to surfaces:  1, 2, 4,
    1    2    4  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (control law not used), start @
      4.  # No. of data items
  1.000 11.000 11.000 11.000
* Control function
#  construction substitution.
  -99   20 -100    0  # sensor data
# actuates construction substitution to surfaces:  3, 4, 0,
    3    4    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (control law not used), start @
      4.  # No. of data items
  1.000 11.000 11.000 11.000
# Function:Zone links
  1  2  0
