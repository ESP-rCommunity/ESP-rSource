simple building  # overall descr 
* Building
convective heating  # bld descr 
   1  # No. of functions
* Control function
# measures the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     4  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  1000.000 0.000 1000.000 0.000 20.000 100.000
    0    1   9.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  1000.000 0.000 1000.000 0.000 20.000 100.000
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
  1  1  0
* Mass Flow
window control  # flow descr 
   1  # No. of controls
* Control mass
# measures  mass flow node or connection.
   -4    0    0    0  # sensor data
# actuates mass flow component:   6 window
   -4    6    2  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on / off), start @
      1.  # No. of data items
  100.000
    1    0   7.999  # ctl type (dry bulb > flow), law (on / off), start @
      1.  # No. of data items
  20.000
    1    0  17.000  # ctl type (dry bulb > flow), law (on / off), start @
      1.  # No. of data items
  100.000
south   recep   window  recep 
west    offic   window  offic 
