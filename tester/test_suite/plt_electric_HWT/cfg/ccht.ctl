control for CCHT house  # overall descr
* Building
no descrip  # bld descr 
   3  # No. of functions
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
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     3  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  20000.000 0.000 0.000 0.000 18.000 100.000 0.000
    0    1   8.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  20000.000 0.000 6000.000 0.000 21.000 25.000 0.000
    0    1  20.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  20000.000 0.000 0.000 0.000 18.000 100.000 0.000
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  3.000 2.000 5.000 99000.000 0.000 2.000 1.000
# Function:Zone links
 2,1,3,2,1,1
* Plant
no descrip  # plant descr 
   2  # No. of loops
* Control loops
# senses var in compt.  1:elec_tank@ node no.  1
   -1    1    1    0    0  # sensor 
# plant component   1:elec_tank @ node no.  1
   -1    1    1    0  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
   1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 58.000 60.000 1.000 0.000 0.000 0.000
* Control loops
# senses dry bulb temperature in mainfloor.
    3    0    0    0    0  # sensor 
# plant component   4:pump @ node no.  1
   -1    4    1    0  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    1    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.000 20.000 21.000 0.0005 0.000 0.000 0.000
