radiant panel heating via injecting 1kW into each of the two radiant panel zones. Control has 70C in zone to get 21C in the occupied spaces
* Building
The aux sensor control logic only works for heating. If room details change it may need to be re-tuned.
   2  # No. of functions
* Control function    1
# senses dry bulb temperature in rad_400.
    5    0    0    0  # sensor data
# actuates the air point in rad_400.
    5    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: weekdays    
    0   19   0.000  # ctl type, law (senses dry bulb temperature in one_bed_400.), start @
     13.  # No. of data items
  800.000 0.000 0.000 0.000 74.000 100.000 1.000 1.000 0.000 0.000 0.000 21.000 100.000
* Control function    2
# senses dry bulb temperature in rad_600.
    6    0    0    0  # sensor data
# actuates the air point in rad_600.
    6    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: weekdays    
    0   19   0.000  # ctl type, law (senses dry bulb temperature in one_bed_400.), start @
     13.  # No. of data items
  1000.000 0.000 0.000 0.000 74.000 100.000 1.000 1.000 0.000 0.000 0.000 21.000 100.000
# Function:Zone links
 0,0,0,0,1,2
