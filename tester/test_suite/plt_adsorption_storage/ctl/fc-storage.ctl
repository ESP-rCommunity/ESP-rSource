control for CCHT house  # overall descr
* Building
no descrip  # bld descr
   3  # No. of functions
* Control function
# measures the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function
# measures the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    3 # No. day types
    1  120  # valid Fri  1 Jan - Fri 30 Apr
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
0 0.000 0.000 0.000 20.000 100.000 0.000
  121  273  # valid Sat  1 May - Thu 30 Sep
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  0.000 0.000 0.000 0.000 0.000 100.000 0.000
  274  365  # valid Fri  1 Oct - Fri 31 Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
0 0.000 0.000 0.000 20.000 100.000 0.000
* Control function
# measures dry bulb temperature in main_second.
    2    0    0    0  # sensor data
# actuates the air point in main_second.
    2    0    0  # actuator data
    3 # No. day types
    1  120  # valid Fri  1 Jan - Fri 30 Apr
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  6.000 1.000 1.000 99000.000 99000.000 5.000 1.000
  121  273  # valid Sat  1 May - Thu 30 Sep
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  0.000 0.000 0.000 0.000 0.000 100.000 0.000
  274  365  # valid Fri  1 Oct - Fri 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  6.000 1.000 1.000 99000.000 99000.000 5.000 1.000
# Function:Zone links
   1  3  1  1
* Plant
no descrip  # plant descr
   5  # No. of loops
* Control loops
# measures var in compt.  1:main-water-tank @ node no.  1
   -1    3    1    0 # sensor
# plant component   1:main-water-tank @ node no.  2
   -1    3    2    0  # actuator
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 50.000 60.000 1.000 0.000 0.000 0.000
* Control loops
# measures dry bulb temperature in main_second.
    2    0    0    0 # sensor
# plant component  5:ahu-fan @ node no.  1
   -1   5    1    0 # actuator
    3 # No. day types
    1  120  # valid Fri  1 Jan - Fri 30 Apr
     1  # No. of periods in day
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 19.500 20.500 0.310 0.000 0.000 0.000
  121  273  # valid Sat  1 May - Thu 30 Sep
     1  # No. of periods in day
   1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 10.00 50.00 0.310 0.000 0.000 0.000
  274  365  # valid Fri  1 Oct - Fri 31 Dec
     1  # No. of periods in day
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 19.500 20.500 0.310 0.000 0.000 0.000
* Control loops
# measures dry bulb temperature in main_second.
    2    0    0    0 # sensor
# plant component  21:pump-tank-E @ node no.  1
   -1    21    1    0 # actuator
    3 # No. day types
    1  120  # valid Fri  1 Jan - Fri 30 Apr
     1  # No. of periods in day
    0    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 19.500 20.500 0.0003 0.000 0.000 0.000
  121  273  # valid Sat  1 May - Thu 30 Sep
     1  # No. of periods in day
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 10.00 50.00 0.0003 0.000 0.000 0.000
  274  365  # valid Fri  1 Oct - Fri 31 Dec
     1  # No. of periods in day
    0    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 19.500 20.500 0.0003 0.000 0.000 0.000
* Control loops
# measures var in compt.  1: main-water-tank @ node no.  1
   -1    3    1    0 # sensor
# plant component   1: main-water-tank @ node no.  2
   -1    3    2   0  # actuator
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
   12    13   0.000  # ctl type, law (On-Off control.), start @
     18.   # No. of data items
     2.0        1.0       3.0     1.0       1.0       3.0    
     274.00     120.00    121.00   273.00   75.00     65.00      
     65.00      55.00     12.0    15.0  9.0   18.0
* Control loops
# measures var in compt.  1: aux-tank @ node no.  1
   -1    22    1    0 # sensor
# plant component   1:aux-tank @ node no.  1
   -1    22    1    0  # actuator
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 40.000 50.000 1000.000 0.000 0.000 0.000
