proj cntrl
* Mass Flow
no descrip
   2  # No. of controls
* Control mass    1
# sensor node(s) not yet defined
   -4    0    0    0  # sensor data
# actuates flow component:   5 duct
   -4    5    3  # actuator data
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekday     
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      2.  # No. of data items
  100.00000 1.00000
    1    0   7.000  # ctl type (dry bulb > flow), law (on/off setpoint 20.00 direct action ON fraction 0.000.), starting @
      2.  # No. of data items
  20.00000 1.00000
    1    0  19.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      2.  # No. of data items
  100.00000 1.00000
liv           s_ext         win2          liv         
kitch         n_ext         win2          kitch       
bed1          s_ext         win2          bed1        
* Control mass    2
# sensor node(s) not yet defined
   -4    0    0    0  # sensor data
# actuates flow component:   7 fan
   -4    7    1  # actuator data
    1  # all daytypes
    1  365  # valid Sat-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekday     
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      2.  # No. of data items
  100.00000 1.00000
    1    0   9.000  # ctl type (dry bulb > flow), law (on/off setpoint 0.00 direct action ON fraction 0.000.), starting @
      2.  # No. of data items
  0.00000 1.00000
    1    0  17.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      2.  # No. of data items
  100.00000 1.00000
coll          hal_up        fan           hal_up      
