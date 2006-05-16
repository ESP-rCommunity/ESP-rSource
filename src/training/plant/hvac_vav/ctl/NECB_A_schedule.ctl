OA control  # overall descr 
* Building
ideal  # bld descr 
   3  # No. of functions
* Control function
# measures the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     4  # No. of periods in day
    0    1   0.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  20600.000 0.000 9300.000 0.000 18.000 100.000
    0    1   5.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  20600.000 0.000 9300.000 0.000 20.000 24.000
    0    1   6.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  20600.000 0.000 9300.000 0.000 22.000 24.000
    0    1  20.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  20600.000 0.000 9300.000 0.000 18.000 100.000
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  20600.000 0.000 0.000 0.000 18.000 100.000
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  20600.000 0.000 0.000 0.000 18.000 100.000
* Control function
# measures dry bulb temperature in west
    1    0    0    0  # sensor data
# actuates the air point in zone. 1
    1    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  5.000 1.000 1.000 99000.000 99000.000 6.000 1.000
* Control function
# measures dry bulb temperature in east
    2    0    0    0  # sensor data
# actuates the air point in zone. 2
    2    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  4.000 1.000 1.000 99000.000 99000.000 7.000 1.000
# Function:Zone links
   2  3
