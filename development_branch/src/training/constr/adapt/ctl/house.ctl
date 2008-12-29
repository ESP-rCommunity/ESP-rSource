proj cntrl  # overall descr 
* Building
no descrip  # bld descr 
   2  # No. of functions
* Control function
# measures dry bulb temperature in liv_room
    5    0    0    0  # sensor data
# actuates the air point in zone. 5
    5    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     4  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   6.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  10000.000 0.000 0.000 0.000 21.000 100.000
    0    2   9.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1  17.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  10000.000 0.000 0.000 0.000 21.000 100.000
* Control function
# measures dry bulb temperature in hall_2
    9    0    0    0  # sensor data
# actuates the air point in zone. 9
    9    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     4  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   6.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  10000.000 0.000 0.000 0.000 21.000 100.000
    0    2   9.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1  20.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  10000.000 0.000 0.000 0.000 21.000 100.000
# Function:Zone links
  0  0  0  0  1  0  0  0  2  0  0  0
* Mass Flow
no descrip  # flow descr 
   1  # No. of controls
* Control mass
# measures  mass flow node or connection.
   -4    0    0    0  # sensor data
# actuates mass flow component:   5 windw
   -4    5    6  # actuator data
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  23.000 1.000
kitch   n_vert  windw   kitch 
bed1    n_vert  windw   bed1  
bathm   n_vert  windw   bathm 
livng   s_vert  windw   livng 
bed2    s_vert  windw   bed2  
bed3    s_vert  windw   bed3  
