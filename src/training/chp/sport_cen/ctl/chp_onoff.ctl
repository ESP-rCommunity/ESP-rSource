proj cntrl  # overall descr 
* Building
no descrip  # bld descr 
   4  # No. of functions
* Control function
# measures dry bulb temperature in changing
    1    0    0    0  # sensor data
# actuates the air point in zone. 1
    1    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  23.000 1.000 1.000 99000.000 99000.000 29.000 1.000
* Control function
# measures dry bulb temperature in gym
    2    0    0    0  # sensor data
# actuates the air point in zone. 2
    2    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  22.000 1.000 1.000 99000.000 99000.000 28.000 1.000
* Control function
# measures dry bulb temperature in main-hall
    3    0    0    0  # sensor data
# actuates the air point in zone. 3
    3    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  24.000 1.000 1.000 99000.000 99000.000 27.000 1.000
* Control function
# measures dry bulb temperature in entrance
    4    0    0    0  # sensor data
# actuates the air point in zone. 4
    4    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  25.000 1.000 1.000 99000.000 99000.000 26.000 1.000
# Function:Zone links
  1  2  3  4  0
* Plant
no descrip  # plant descr 
   8  # No. of loops
* Control loops
# measures var in compt.  3:pump1 @ node no.  1
   -1    3    1  # sensor 
# plant component   1:chp_unit @ node no.  1
   -1    1    1  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     3  # No. of periods in day
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 80.000 90.000 0.000 0.000 0.000 0.000
   12    8   7.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 80.000 90.000 100.000 0.000 0.000 0.000
   12    8  23.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 80.000 90.000 0.000 0.000 0.000 0.000
* Control loops
# measures var in compt.  5:calorifier @ node no.  1
   -1    5    1  # sensor 
# plant component   9:3_port_valve @ node no.  1
   -1    9    1  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
   18    5   0.000  # ctl type, law (Prop`l damper ctl.), start @
      6.  # No. of data items
  1.000 0.1000 80.000 70.000 22.000 9.000
* Control loops
# measures dry bulb temperature in zone.   main-hall
    3    0    0  # sensor 
# plant component  33:ahu_dv @ node no.  1
   -1   33    1  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     3  # No. of periods in day
   18    5   0.000  # ctl type, law (Prop`l damper ctl.), start @
      6.  # No. of data items
  1.000 0.1000 16.000 12.000 48.000 46.000
   18    5   7.000  # ctl type, law (Prop`l damper ctl.), start @
      6.  # No. of data items
  1.000 0.1000 22.000 18.000 48.000 46.000
   18    5  23.000  # ctl type, law (Prop`l damper ctl.), start @
      6.  # No. of data items
  0.010 1.000 16.000 12.000 48.000 46.000
* Control loops
# measures var in compt. 10:converge2 @ node no.  1
   -1   10    1  # sensor 
# plant component  11:boiler_1 @ node no.  1
   -1   11    1  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     3  # No. of periods in day
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 50.000 65.000 0.000 0.000 0.000 0.000
   12    8   7.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 50.000 66.000 1.000 0.000 0.000 0.000
   12    8  23.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 50.000 65.000 0.000 0.000 0.000 0.000
* Control loops
# measures var in compt. 10:converge2 @ node no.  1
   -1   10    1  # sensor 
# plant component  12:boiler_2 @ node no.  1
   -1   12    1  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     3  # No. of periods in day
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 60.000 75.000 0.000 0.000 0.000 0.000
   12    8   7.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 60.000 75.000 1.000 0.000 0.000 0.000
   12    8  23.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 60.000 75.000 0.000 0.000 0.000 0.000
* Control loops
# measures var in compt. 10:converge2 @ node no.  1
   -1   10    1  # sensor 
# plant component  13:boiler_3 @ node no.  1
   -1   13    1  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     3  # No. of periods in day
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 70.000 85.000 0.000 0.000 0.000 0.000
   12    8   7.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 70.000 85.000 1.000 0.000 0.000 0.000
   12    8  23.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 70.000 85.000 0.000 0.000 0.000 0.000
* Control loops
# measures dry bulb temperature in zone.    changing
    1    0    0  # sensor 
# plant component  20:supply_fan @ node no.  1
   -1   20    1  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     3  # No. of periods in day
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 20.000 20.000 3.500 3.500 1.000 1.000
    1    8   7.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 20.000 20.000 9.000 9.000 1.000 1.000
    1    8  23.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 20.000 20.000 3.500 3.500 1.000 1.000
* Control loops
# measures dry bulb temperature in zone.    changing
    1    0    0  # sensor 
# plant component  21:extract_fan @ node no.  1
   -1   21    1  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     3  # No. of periods in day
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 20.000 20.000 3.500 3.500 1.000 1.000
    1    8   7.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 20.000 20.000 9.000 9.000 1.000 1.000
    1    8  23.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 20.000 20.000 3.500 3.500 1.000 1.000
