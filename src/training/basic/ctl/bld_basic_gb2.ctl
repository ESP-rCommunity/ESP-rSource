simple building  # overall descr 
* Building
convective heating  # bld descr 
   2  # No. of functions
* Control function
# measures dry bulb temperature in reception
    1    0    0    0  # sensor data
# actuates the air point in zone. 1
    1    0    0  # actuator data
    0 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     4  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  1000.000 0.000 0.000 0.000 20.000 100.000
    0    1   9.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  1000.000 0.000 0.000 0.000 20.000 100.000
    0    2  18.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function
# measures dry bulb temperature in office
    2    0    0    0  # sensor data
# actuates the air point in zone. 2
    2    0    0  # actuator data
    0 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     4  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  1000.000 0.000 0.000 0.000 20.000 100.000
    0    1   9.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  1000.000 0.000 0.000 0.000 20.000 100.000
    0    2  18.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
# Function:Zone links
  1  2  0
* System
global demand limiting                                           
   1
* Global control 
   -3    0    0
    0    0    0
     1
    1  365
     1
    0    2   0.000
     4.0
# heating capacity; cooling capacity; max number shed; sequence shed
  1000.000 0.000 1.000 1
