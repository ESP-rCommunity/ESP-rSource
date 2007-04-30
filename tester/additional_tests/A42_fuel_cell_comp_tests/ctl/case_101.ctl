control for CCHT house
* Building
no descrip
   4  # No. of functions
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    3 # No. day types
    1  120  # valid Fri  1 Jan - Fri 30 Apr
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  0.000 0.000 0.000 0.000 0.000 100.000 0.000
  121  273  # valid Sat  1 May - Thu 30 Sep
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  0.000 0.000 0.000 0.000 0.000 100.000 0.000
  274  365  # valid Fri  1 Oct - Fri 31 Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  0.000 0.000 0.000 0.000 20.000 100.000 0.000
* Control function
# senses dry bulb temperature in main_second.
    2    0    0    0  # sensor data
# actuates the air point in main_second.
    2    0    0  # actuator data
    3 # No. day types
    1  120  # valid Fri  1 Jan - Fri 30 Apr
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  6.000 2.000 1.000 99000.000 99000.000 10.000 1.000
  121  273  # valid Sat  1 May - Thu 30 Sep
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  0.000 0.000 0.000 0.000 0.000 100.000 0.000
  274  365  # valid Fri  1 Oct - Fri 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  6.000 2.000 1.000 99000.000 99000.000 10.000 1.000
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  20000.000 0.000 20000.000 0.000 50.000 50.100 0.000
# Function:Zone links
 4,3,1,1
* Plant
no descrip
   4  # No. of loops
* Control loops
# senses var in compt.  2:hot-water-tank @ node no.  1
   -1    2    1    0  # sensor 
# plant component   2:hot-water-tank @ node no.  2
   -1    2    2    0  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 50.000 60.000 1.000 0.000 0.000 0.000
* Control loops
# senses dry bulb temperature in main_second.
    2    0    0    0  # sensor 
# plant component   5:coil-fan @ node no.  1
   -1    5    1    0  # actuator 
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
  1.000 10.000 50.000 0.000 0.000 0.000 0.000
  274  365  # valid Fri  1 Oct - Fri 31 Dec
     1  # No. of periods in day
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 19.500 20.500 0.310 0.000 0.000 0.000
* Control loops
# senses dry bulb temperature in main_second.
    2    0    0    0  # sensor 
# plant component   4:coil-pump @ node no.  1
   -1    4    1    0  # actuator 
    3 # No. day types
    1  120  # valid Fri  1 Jan - Fri 30 Apr
     1  # No. of periods in day
    0    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 19.500 20.500 0.000 0.000 0.000 0.000
  121  273  # valid Sat  1 May - Thu 30 Sep
     1  # No. of periods in day
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 10.000 50.000 0.000 0.000 0.000 0.000
  274  365  # valid Fri  1 Oct - Fri 31 Dec
     1  # No. of periods in day
    0    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 19.500 20.500 0.000 0.000 0.000 0.000
* Control loops
# senses var in compt.  2:hot-water-tank @ node no.  1
   -1    2    1    0  # sensor 
# plant component   2:hot-water-tank @ node no.  2
   -1    2    2    0  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
   12   10   0.000  # ctl type, law (Cogen. controller), start @
     18.  # No. of data items
  -1.000 2.000 2.000 2.000 -1.000 -1.000 1.000 365.000 2.000 62.000 72.000 0.500 -10.000 -10.000 -10.000 -10.000 -10.000 0.000
