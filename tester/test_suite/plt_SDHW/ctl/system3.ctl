no overall control description supplied
* Building
no zone control description supplied
   1  # No. of functions
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
  100000.000 0.000 0.000 0.000 21.000 100.000 0.000
# Function:Zone links
 1,0,1,1,1,0
* Plant
no plant control description supplied
   2  # No. of loops
* Control loops
# senses var in compt.  3:dhw_tank @ node no.  1
   -1    3    1    0    0  # sensor 
# plant component   3:dhw_tank @ node no.  1
   -1    3    1    0  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 54.00000 56.00000 1.00000 0.00000 0.00000 0.00000
* Control loops
# sen var diff bet compt.  1:solar_collector @ node  1and compt  4:solar_t
   -1    1    1    4    1  # sensor 
# plant component   2:collector_pumpr @ node no.  1
   -1    2    1    0  # actuator 
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
   24    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 1.00000 5.55000 0.00000 0.0000380 0.00000 0.00000
