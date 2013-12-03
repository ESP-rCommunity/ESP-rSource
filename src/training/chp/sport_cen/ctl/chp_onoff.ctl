proj cntrl
* Building
no descrip
   4  # No. of functions
* Control function    1
# senses dry bulb temperature in changing.
    1    0    0    0  # sensor data
# actuates the air point in changing.
    1    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: weekday     
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  23.000 1.000 1.000 99000.000 99000.000 29.000 1.000
* Control function    2
# senses dry bulb temperature in gym.
    2    0    0    0  # sensor data
# actuates the air point in gym.
    2    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: weekday     
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  22.000 1.000 1.000 99000.000 99000.000 28.000 1.000
* Control function    3
# senses dry bulb temperature in main-hall.
    3    0    0    0  # sensor data
# actuates the air point in main-hall.
    3    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: weekday     
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  24.000 1.000 1.000 99000.000 99000.000 27.000 1.000
* Control function    4
# senses dry bulb temperature in entrance.
    4    0    0    0  # sensor data
# actuates the air point in entrance.
    4    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: weekday     
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  25.000 1.000 1.000 99000.000 99000.000 26.000 1.000
# Function:Zone links
 1,2,3,4,0
* Plant
no descrip
   8  # No. of loops
* Control loops    1
# senses var in compt.  3:pump1 @ node no.  1
   -1    3    1    0    0  # sensor 
# plant component   1:chp_unit @ node no.  1
   -1    1    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     3  # No. of periods in day: weekday     
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 80.00000 90.00000 0.00000 0.00000 0.00000 0.00000
   12    8   7.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 80.00000 90.00000 100.00000 0.00000 0.00000 0.00000
   12    8  23.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 80.00000 90.00000 0.00000 0.00000 0.00000 0.00000
* Control loops    2
# senses var in compt.  5:calorifier @ node no.  1
   -1    5    1    0    0  # sensor 
# plant component   9:3_port_valve @ node no.  1
   -1    9    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day: weekday     
   18    5   0.000  # ctl type, law (Prop`l damper ctl.), start @
      6.  # No. of data items
  1.00000 0.10000 80.00000 70.00000 22.00000 9.00000
* Control loops    3
# senses dry bulb temperature in main-hall.
    3    0    0    0    0  # sensor 
# plant component  33:ahu_dv @ node no.  1
   -1   33    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     3  # No. of periods in day: weekday     
   18    5   0.000  # ctl type, law (Prop`l damper ctl.), start @
      6.  # No. of data items
  1.00000 0.10000 16.00000 12.00000 48.00000 46.00000
   18    5   7.000  # ctl type, law (Prop`l damper ctl.), start @
      6.  # No. of data items
  1.00000 0.10000 22.00000 18.00000 48.00000 46.00000
   18    5  23.000  # ctl type, law (Prop`l damper ctl.), start @
      6.  # No. of data items
  0.01000 1.00000 16.00000 12.00000 48.00000 46.00000
* Control loops    4
# senses var in compt. 10:converge2 @ node no.  1
   -1   10    1    0    0  # sensor 
# plant component  11:boiler_1 @ node no.  1
   -1   11    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     3  # No. of periods in day: weekday     
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 50.00000 65.00000 0.00000 0.00000 0.00000 0.00000
   12    8   7.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 50.00000 66.00000 1.00000 0.00000 0.00000 0.00000
   12    8  23.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 50.00000 65.00000 0.00000 0.00000 0.00000 0.00000
* Control loops    5
# senses var in compt. 10:converge2 @ node no.  1
   -1   10    1    0    0  # sensor 
# plant component  12:boiler_2 @ node no.  1
   -1   12    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     3  # No. of periods in day: weekday     
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 60.00000 75.00000 0.00000 0.00000 0.00000 0.00000
   12    8   7.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 60.00000 75.00000 1.00000 0.00000 0.00000 0.00000
   12    8  23.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 60.00000 75.00000 0.00000 0.00000 0.00000 0.00000
* Control loops    6
# senses var in compt. 10:converge2 @ node no.  1
   -1   10    1    0    0  # sensor 
# plant component  13:boiler_3 @ node no.  1
   -1   13    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     3  # No. of periods in day: weekday     
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 70.00000 85.00000 0.00000 0.00000 0.00000 0.00000
   12    8   7.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 70.00000 85.00000 1.00000 0.00000 0.00000 0.00000
   12    8  23.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 70.00000 85.00000 0.00000 0.00000 0.00000 0.00000
* Control loops    7
# senses dry bulb temperature in changing.
    1    0    0    0    0  # sensor 
# plant component  20:supply_fan @ node no.  1
   -1   20    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     3  # No. of periods in day: weekday     
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 20.00000 3.50000 3.50000 1.00000 1.00000
    1    8   7.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 20.00000 9.00000 9.00000 1.00000 1.00000
    1    8  23.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 20.00000 3.50000 3.50000 1.00000 1.00000
* Control loops    8
# senses dry bulb temperature in changing.
    1    0    0    0    0  # sensor 
# plant component  21:extract_fan @ node no.  1
   -1   21    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     3  # No. of periods in day: weekday     
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 20.00000 3.50000 3.50000 1.00000 1.00000
    1    8   7.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 20.00000 9.00000 9.00000 1.00000 1.00000
    1    8  23.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 20.00000 20.00000 3.50000 3.50000 1.00000 1.00000
