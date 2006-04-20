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
global phasing                                                   
   1
* Global control 
   -3    0    0
    0    0    0
     1
    1  365
     1
    0    3   0.000
     3.0
# The following line gives the sequence:
# number of zones sequenced (=2); sequence (2 then 1)
  2  2  1
