no overall control description supplied
* Building
no zone control description supplied
   3  # No. of functions
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sat-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function
# senses dry bulb temperature in manager_a.
    1    0    0    0  # sensor data
# actuates the air point in manager_a.
    1    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sat-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  13.000 1.000 1.000 99000.000 99000.000 15.000 1.000
* Control function
# senses dry bulb temperature in manager_b.
    2    0    0    0  # sensor data
# actuates the air point in manager_b.
    2    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sat-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  14.000 1.000 1.000 99000.000 99000.000 16.000 1.000
# Function:Zone links
 2,3,0
* Plant
no plant control description supplied
   6  # No. of loops
* Control loops
# senses var in compt. 17:converge @ node no.  1
   -1   17    1    0    0  # sensor 
# plant component  10:re-heat_coil @ node no.  1
   -1   10    1    0  # actuator 
    1 # No. day types
    1  365  # valid Sat-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.00000 3000.00000 0.00000 19.00000 4.00000 0.00000 0.00000
    0    0  18.000  # ctl type, law (period off), start @
      0.  # No. of data items
* Control loops
# senses var in compt. 17:converge @ node no.  1
   -1   17    1    0    0  # sensor 
# plant component   8:cooling_coil @ node no.  1
   -1    8    1    0  # actuator 
    3 # No. day types
    1  120  # valid Sat-01-Jan - Sun-30-Apr
     3  # No. of periods in day
    0    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  -1.00000 0.00000 0.00000 23.00000 4.00000 0.00000 0.00000
    0    0  18.000  # ctl type, law (period off), start @
      0.  # No. of data items
  121  273  # valid Mon-01-May - Sat-30-Sep
     3  # No. of periods in day
    0    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  -1.00000 3000.00000 0.00000 23.00000 4.00000 0.00000 0.00000
    0    0  18.000  # ctl type, law (period off), start @
      0.  # No. of data items
  274  365  # valid Sun-01-Oct - Sun-31-Dec
     3  # No. of periods in day
    0    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  -1.00000 0.00000 0.00000 23.00000 4.00000 0.00000 0.00000
    0    0  18.000  # ctl type, law (period off), start @
      0.  # No. of data items
* Control loops
# senses var in compt.  5:supply_duct3 @ node no.  1
   -1    5    1    0    0  # sensor 
# plant component   4:prehtg_coil @ node no.  1
   -1    4    1    0  # actuator 
    1 # No. day types
    1  365  # valid Sat-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.00000 1500.00000 0.00000 10.00000 4.00000 0.00000 0.00000
* Control loops
# senses var in compt. 17:converge @ node no.  1
   -1   17    1    0    0  # sensor 
# plant component  12:fan1 @ node no.  1
   -1   12    1    0  # actuator 
    3 # No. day types
    1  120  # valid Sat-01-Jan - Sun-30-Apr
     3  # No. of periods in day
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 10.00000 0.00700 0.00700 0.00000 0.00000
    1    8   7.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 10.00000 0.07500 0.07500 0.00000 0.00000
    1    8  18.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 10.00000 0.00700 0.00700 0.00000 0.00000
  121  273  # valid Mon-01-May - Sat-30-Sep
     1  # No. of periods in day
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 10.00000 0.07500 0.07500 0.00000 0.00000
  274  365  # valid Sun-01-Oct - Sun-31-Dec
     3  # No. of periods in day
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 10.00000 0.00700 0.00700 0.00000 0.00000
    1    8   7.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 10.00000 0.07500 0.07500 0.00000 0.00000
    1    8  18.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 10.00000 0.00700 0.00700 0.00000 0.00000
* Control loops
# senses var in compt. 17:converge @ node no.  1
   -1   17    1    0    0  # sensor 
# plant component  18:fan2 @ node no.  1
   -1   18    1    0  # actuator 
    3 # No. day types
    1  120  # valid Sat-01-Jan - Sun-30-Apr
     3  # No. of periods in day
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 10.00000 0.00700 0.00700 0.00000 0.00000
    1    8   7.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 10.00000 0.07500 0.07500 0.00000 0.00000
    1    8  18.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 10.00000 0.00700 0.00700 0.00000 0.00000
  121  273  # valid Mon-01-May - Sat-30-Sep
     1  # No. of periods in day
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 10.00000 0.07500 0.07500 0.00000 0.00000
  274  365  # valid Sun-01-Oct - Sun-31-Dec
     3  # No. of periods in day
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 10.00000 0.00700 0.00700 0.00000 0.00000
    1    8   7.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 10.00000 0.07500 0.07500 0.00000 0.00000
    1    8  18.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 10.00000 0.00700 0.00700 0.00000 0.00000
* Control loops
# senses var in compt. 17:converge @ node no.  1
   -1   17    1    0    0  # sensor 
# plant component   6:humidifier @ node no.  1
   -1    6    1    0  # actuator 
    1 # No. day types
    1  365  # valid Sat-01-Jan - Sun-31-Dec
     3  # No. of periods in day
   11    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
   11    2   7.000  # ctl type, law (PID flow control.), start @
      7.  # No. of data items
  1.00000 0.00010 0.00000 50.00000 10.00000 0.00000 0.00000
   11    0  18.000  # ctl type, law (period off), start @
      0.  # No. of data items
