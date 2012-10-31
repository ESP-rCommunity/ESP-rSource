simple building
* Building
convective heating
   1  # No. of functions
* Control function    1
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # day types follow calendar  3
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     4  # No. of periods in day: weekday     
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  1000.000 0.000 1000.000 0.000 20.000 100.000
    0    1   9.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  1000.000 0.000 1000.000 0.000 20.000 100.000
    0    2  18.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: saturday    
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: sunday      
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
# Function:Zone links
 1,1,0
* Mass Flow
window control opened if over 20C from 8h00-17h00 otherwise closed
   2  # No. of controls
* Control mass    1
# senses node (6) recep
   -4    6    0    0  # sensor data
# actuates flow connection:   8 south - recep via window
   -3    8    0  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekday     
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.00000 1.00000 1.00000
    1    0   7.999  # ctl type (dry bulb > flow), law (on/off setpoint 20.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  20.00000 1.00000 1.00000
    1    0  17.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.00000 1.00000 1.00000
* Control mass    2
# senses node (7) offic
   -4    7    0    0  # sensor data
# actuates flow connection:   9 west - offic via window
   -3    9    0  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekday     
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.00000 1.00000 1.00000
    1    0   7.999  # ctl type (dry bulb > flow), law (on/off setpoint 20.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  20.00000 1.00000 1.00000
    1    0  17.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.00000 1.00000 1.00000
