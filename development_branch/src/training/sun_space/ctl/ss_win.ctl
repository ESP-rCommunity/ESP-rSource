winter_ht
* Building
winter heating to 19C in occupied spaces
   2  # No. of functions
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  2000.000 0.000 0.000 0.000 16.000 50.000
    0    1   6.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  3000.000 0.000 0.000 0.000 19.000 100.000 0.000
    0    1  22.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  2000.000 0.000 0.000 0.000 16.000 50.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  2000.000 0.000 0.000 0.000 16.000 50.000
    0    1   8.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  3000.000 0.000 0.000 0.000 19.000 100.000 0.000
    0    1  23.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  2000.000 0.000 0.000 0.000 16.000 50.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  2000.000 0.000 0.000 0.000 16.000 50.000
    0    1   8.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  3000.000 0.000 0.000 0.000 19.000 100.000 0.000
    0    1  22.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  2000.000 0.000 0.000 0.000 16.000 50.000
# Function:Zone links
 2,0,2,2,2,2,0,0,0,0
* Mass Flow
kitchen and bath extracts run 7-8am and 17-18h
   2  # No. of controls
* Control mass
# senses node (3) bath
   -4    3    0    0  # sensor data
# actuates flow connection:  18 bath - north via bathex
   -3   18    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     5  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 30.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  30.00000 1.00000 1.00000
    1    0   7.000  # ctl type (dry bulb > flow), law (on/off setpoint 0.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  0.00000 1.00000 1.00000
    1    0   9.000  # ctl type (dry bulb > flow), law (on/off setpoint 30.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  30.00000 1.00000 1.00000
    1    0  17.000  # ctl type (dry bulb > flow), law (on/off setpoint 10.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  10.00000 1.00000 1.00000
    1    0  19.000  # ctl type (dry bulb > flow), law (on/off setpoint 30.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  30.00000 1.00000 1.00000
* Control mass
# senses node (1) liv
   -4    1    0    0  # sensor data
# actuates flow connection:  17 liv - north via kitex
   -3   17    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     5  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 30.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  30.00000 1.00000 1.00000
    1    0   7.000  # ctl type (dry bulb > flow), law (on/off setpoint 0.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  0.00000 1.00000 1.00000
    1    0   8.000  # ctl type (dry bulb > flow), law (on/off setpoint 30.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  30.00000 1.00000 1.00000
    1    0  17.000  # ctl type (dry bulb > flow), law (on/off setpoint 0.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  0.00000 1.00000 1.00000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 30.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  30.00000 1.00000 1.00000
