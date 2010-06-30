plant test
* Building
 ctl test with 2 periods of validity 6-9 July & 10-12 July
   2  # No. of functions
* Control function
# senses dry bulb temperature in reception.
    1    0    0    0  # sensor data
# actuates the air point in reception.
    1    0    0  # actuator data
    2 # No. day types
  187  190  # valid Thu-06-Jul - Sun-09-Jul
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  100000.000 0.000 100000.000 0.000 23.000 23.000
  191  194  # valid Mon-10-Jul - Thu-13-Jul
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      5.  # No. of data items
  6.000 1.000 1.000 15000.000 15000.000
* Control function
# senses dry bulb temperature in office.
    2    0    0    0  # sensor data
# actuates the air point in office.
    2    0    0  # actuator data
    2 # No. day types
  187  190  # valid Thu-06-Jul - Sun-09-Jul
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  100000.000 0.000 100000.000 0.000 11.000 11.000
  191  194  # valid Mon-10-Jul - Thu-13-Jul
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      5.  # No. of data items
  8.000 1.000 1.000 15000.000 15000.000
# Function:Zone links
 1,2,0
* Plant
ctl loops for 6-9 July and 10-12 July
   2  # No. of loops
* Control loops
# senses dry bulb temperature in reception.
    1    0    0    0    0  # sensor 
# plant component   5:5 @ node no.  1
   -1    5    1    0  # actuator 
    2 # No. day types
  187  190  # valid Thu-06-Jul - Sun-09-Jul
     1  # No. of periods in day
    1    2   0.000  # ctl type, law (PID flow control.), start @
      7.  # No. of data items
  4.00000 0.00000 0.00100 26.00000 1.00000 1000.00000 1.00000
  191  194  # valid Mon-10-Jul - Thu-13-Jul
     1  # No. of periods in day
    1    2   0.000  # ctl type, law (PID flow control.), start @
      7.  # No. of data items
  4.00000 0.01000 0.00100 26.00000 1.00000 1000.00000 1.00000
* Control loops
# senses dry bulb temperature in office.
    2    0    0    0    0  # sensor 
# plant component   7:7 @ node no.  1
   -1    7    1    0  # actuator 
    2 # No. day types
  187  190  # valid Thu-06-Jul - Sun-09-Jul
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  -4.00000 0.00000 0.00000 13.00000 1.00000 5.00000 1.00000
  191  194  # valid Mon-10-Jul - Thu-13-Jul
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  -4.00000 920.00000 0.00000 13.00000 1.00000 5.00000 1.00000
