plant test
* Building
ctl test 9-10 July.
   2  # No. of functions
* Control function
# senses dry bulb temperature in reception.
    1    0    0    0  # sensor data
# actuates the air point in reception.
    1    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      5.  # No. of data items
  6.000 1.000 1.000 50000.000 50000.000
* Control function
# senses dry bulb temperature in office.
    2    0    0    0  # sensor data
# actuates the air point in office.
    2    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      5.  # No. of data items
  8.000 1.000 1.000 50000.000 50000.000
# Function:Zone links
 1,2,0
* Plant
ctl loops test valid for any day of the year
   2  # No. of loops
* Control loops
# senses dry bulb temperature in reception.
    1    0    0    0  # sensor 
# plant component   5:5 @ node no.  1
   -1    5    1    0  # actuator 
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
    1    2   0.000  # ctl type, law (PID flow control.), start @
      6.  # No. of data items
  3.000 0.020 0.000 26.000 1.000 1000.000
* Control loops
# senses dry bulb temperature in office.
    2    0    0    0  # sensor 
# plant component   7:7 @ node no.  1
   -1    7    1    0  # actuator 
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
      6.  # No. of data items
  -3.000 3000.000 0.000 13.000 1.000 900.000
