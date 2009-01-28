zone control tempers inlet air from plant to manager_a & b to 18degC
* Building
strictly controls plant and plant-B temperature to 16degC
   1  # No. of functions
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   6.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  99999.000 0.000 99999.000 0.000 16.000 16.100 0.000
    0    2  18.000  # ctl type, law (free floating), start @
      0.  # No. of data items
# Function:Zone links
 0,0,0,1,1
* Mass Flow
Windows open and fan shuts down for manager_a when ambient temp is between 10 and 25 degC and room temp is more than 21degC. Manager_b windows closed via controller, mixing box used during office hours.
   6  # No. of controls
* Control mass
# senses node (1) manager_a
   -4    1    0    0  # sensor data
# actuates flow connection:   1 man_alow - manager_a via low_win
   -3    1    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      3.  # No. of data items
  100.000 1.000 0.000
    1    3   8.000  # ctl type (dry bulb > flow), law (multi-sensor: normally closed with 3 sensors: For sensor 1 ambient T setpoint 25.00 inverse action AND sensor 2 ambient T setpoint 10.00 direct action AND sensor 3 sense node manager_a setpoint 21.00 direct action.), starting @
     26.  # No. of data items
  0.000 3.000 -3.000 0.000 0.000 0.000 25.000 -1.000 0.000 1.000 -3.000 0.000 0.000 0.000 10.000 1.000 0.000 1.000
  -4.000 1.000 0.000 0.000 21.000 1.000 0.000 1.000
    1    0  17.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      3.  # No. of data items
  100.000 1.000 0.000
* Control mass
# senses node (1) manager_a
   -4    1    0    0  # sensor data
# actuates flow connection:   2 man_ahi - manager_a via high_win
   -3    2    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      3.  # No. of data items
  100.000 1.000 0.000
    1    3   8.000  # ctl type (dry bulb > flow), law (multi-sensor: normally closed with 3 sensors: For sensor 1 ambient T setpoint 25.00 inverse action AND sensor 2 ambient T setpoint 10.00 direct action AND sensor 3 sense node manager_a setpoint 21.00 direct action.), starting @
     26.  # No. of data items
  0.000 3.000 -3.000 0.000 0.000 0.000 25.000 -1.000 0.000 1.000 -3.000 0.000 0.000 0.000 10.000 1.000 0.000 1.000
  -4.000 1.000 0.000 0.000 21.000 1.000 0.000 1.000
    1    0  17.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      3.  # No. of data items
  100.000 1.000 0.000
* Control mass
# senses node (1) manager_a
   -4    1    0    0  # sensor data
# actuates flow connection:   9 plant - manager_a via fan
   -3    9    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      3.  # No. of data items
  100.000 1.000 0.000
    1    3   8.000  # ctl type (dry bulb > flow), law (multi-sensor: normally open with 3 sensors: For sensor 1 ambient T setpoint 25.00 inverse action AND sensor 2 ambient T setpoint 10.00 direct action AND sensor 3 sense node manager_a setpoint 21.00 direct action.), starting @
     26.  # No. of data items
  1.000 3.000 -3.000 0.000 0.000 0.000 25.000 -1.000 0.000 1.000 -3.000 0.000 0.000 0.000 10.000 1.000 0.000 1.000
  -4.000 1.000 0.000 0.000 21.000 1.000 0.000 1.000
    1    0  17.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      3.  # No. of data items
  100.000 1.000 0.000
* Control mass
# senses node (1) manager_a
   -4    1    0    0  # sensor data
# actuates flow connection:   3 man_blow - manager_b via low_win
   -3    3    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      3.  # No. of data items
  100.000 1.000 0.000
* Control mass
# senses node (1) manager_a
   -4    1    0    0  # sensor data
# actuates flow connection:   4 man_bhi - manager_b via high_win
   -3    4    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      3.  # No. of data items
  100.000 1.000 0.000
* Control mass
# senses node (1) manager_a
   -4    1    0    0  # sensor data
# actuates flow connection:  10 plant-B - manager_b via fan
   -3   10    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      3.  # No. of data items
  100.000 1.000 0.000
    1    0   8.000  # ctl type (dry bulb > flow), law (on/off setpoint -100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  -100.000 1.000 1.000
    1    0  17.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      3.  # No. of data items
  100.000 1.000 0.000
