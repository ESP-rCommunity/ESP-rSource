proj cntrl  # overall descr 
* Building
no descrip  # bld descr 
   3  # No. of functions
* Control function
# measures dry bulb temperature in office_fl1
    2    0    0    0  # sensor data
# actuates the air point in zone. 2
    2    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  6.000 1.000 1.000 100000.000 0.000 15.000 1.000
* Control function
# measures dry bulb temperature in office_fl2
    3    0    0    0  # sensor data
# actuates the air point in zone. 3
    3    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  7.000 1.000 1.000 100000.000 0.000 15.000 1.000
* Control function
# measures dry bulb temperature in office_fl3
    4    0    0    0  # sensor data
# actuates the air point in zone. 4
    4    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  8.000 1.000 1.000 100000.000 0.000 16.000 1.000
# Function:Zone links
  0  1  2  3  0  0  0  0
* Plant
no descrip  # plant descr 
   5  # No. of loops
* Control loops
# measures dry bulb temperature in zone. 2
    2    0    0  # sensor 
# plant component  9
   -1    9    1  # actuator 
    0 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     3  # No. of periods in day
    0    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.000 10000.000 0.000 19.000 2.000 0.000 0.000
    0    0  18.000  # ctl type, law (period off), start @
      0.  # No. of data items
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
* Control loops
# measures dry bulb temperature in zone. 3
    3    0    0  # sensor 
# plant component 10
   -1   10    1  # actuator 
    0 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     3  # No. of periods in day
    0    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.000 10000.000 0.000 19.000 2.000 0.000 0.000
    0    0  18.000  # ctl type, law (period off), start @
      0.  # No. of data items
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
* Control loops
# measures dry bulb temperature in zone. 4
    4    0    0  # sensor 
# plant component 11
   -1   11    1  # actuator 
    0 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     3  # No. of periods in day
    0    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.000 10000.000 0.000 19.000 2.000 0.000 0.000
    0    0  18.000  # ctl type, law (period off), start @
      0.  # No. of data items
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
* Control loops
# measures dry bulb temperature in zone. 3
    3    0    0  # sensor 
# plant component  5
   -1    5    1  # actuator 
    0 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     3  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.000 20000.000 0.000 14.000 2.000 0.000 0.000
    0    1   7.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.000 20000.000 0.000 19.000 2.000 0.000 0.000
    0    1  18.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.000 20000.000 0.000 14.000 2.000 0.000 0.000
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     3  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.000 20000.000 0.000 14.000 2.000 0.000 0.000
    0    1   7.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.000 20000.000 0.000 14.000 2.000 0.000 0.000
    0    1  18.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.000 20000.000 0.000 14.000 2.000 0.000 0.000
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     3  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.000 20000.000 0.000 14.000 2.000 0.000 0.000
    0    1   7.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.000 20000.000 0.000 14.000 2.000 0.000 0.000
    0    1  18.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.000 20000.000 0.000 14.000 2.000 0.000 0.000
* Control loops
# measures compt. varbl 1 in compt.-1
   -1    1    1  # sensor 
# plant component  2
   -1    2    1  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.000 5000.000 0.000 9.000 2.000 0.000 0.000
* Mass Flow
no descrip  # flow descr 
   1  # No. of controls
* Control mass
# measures  mass flow node or connection.
   -4    0    0    0  # sensor data
# actuates mass flow component:   4 fan
   -4    4    2  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
    1    0   6.500  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  23.000 -1.000
    1    0  18.500  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
pv4     sfan    fan     off2  
efan    exit    fan     off2  
