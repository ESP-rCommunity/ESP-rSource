Ideal control for dual office model. Weekdays normal office hours, saturday reduced occupied hours, sunday stand-by only. One person per office, 100W lighting and 150W small power.
* Building
this is a base case set of assumptions
   1  # No. of functions
* Control function    1
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1000.000 0.000 0300.000 0.000 15.000 25.000 0.000
    0    1   6.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1000.000 0.000 0300.000 0.000 19.000 24.000 0.000
    0    1  18.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1000.000 0.000 0300.000 0.000 15.000 25.000 0.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: saturday    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1000.000 0.000 0300.000 0.000 15.000 25.000 0.000
    0    1   9.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1000.000 0.000 0300.000 0.000 19.000 24.000 0.000
    0    2  17.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: sunday      
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1000.000 0.000 0300.000 0.000 10.000 30.000 0.000
# Function:Zone links
 1,1,1
* Mass Flow
no flow control description supplied
   6  # No. of controls
* Control mass    1
# senses node (1) manager_a
   -4    1    0    0  # sensor data
# actuates flow connection:   8 corid_left - manager_a via fan_8lps
   -3    8    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekdays    
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
    1    0   8.500  # ctl type (dry bulb > flow), law (on/off setpoint -50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  -50.00000 1.00000 1.00000
    1    0  17.500  # ctl type (dry bulb > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: saturday    
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: sunday      
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
* Control mass    2
# senses node (2) manager_b
   -4    2    0    0  # sensor data
# actuates flow connection:   9 corid_left - manager_b via fan_8lps
   -3    9    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekdays    
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
    1    0   8.500  # ctl type (dry bulb > flow), law (on/off setpoint -50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  -50.00000 1.00000 1.00000
    1    0  17.500  # ctl type (dry bulb > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: saturday    
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: sunday      
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
* Control mass    3
# senses node (1) manager_a
   -4	 1    0    0  # sensor data
# actuates flow connection:  10 corid_left - manager_a via fan_8lps
   -3	10    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekdays    
    1	 0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
    1	 0   8.500  # ctl type (dry bulb > flow), law (on/off setpoint -50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  26.00000 1.00000 1.00000
    1	 0  17.500  # ctl type (dry bulb > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: saturday    
    1	 0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: sunday      
    1	 0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
* Control mass    4
# senses node (2) manager_b
   -4    2    0    0  # sensor data
# actuates flow connection:   3 man_blow - manager_b via crack
   -3    3    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekdays    
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
    1    0   8.500  # ctl type (dry bulb > flow), law (on/off setpoint -50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
   26.00000 1.00000 1.00000
    1    0  17.500  # ctl type (dry bulb > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: saturday    
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: sunday      
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
* Control mass    5
# senses node (2) manager_b, contaminant: CO2
   -4    2    1    2  # sensor data
# actuates flow connection:   4 man_bhi - manager_b via crack
   -3    4    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekdays    
   41    0   0.000  # ctl type (contam conc > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
   41    0   8.500  # ctl type (contam conc > flow), law (on/off setpoint 2.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  1.30000 1.00000 1.00000
   41    0  17.500  # ctl type (contam conc > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: saturday    
   41    0   0.000  # ctl type (contam conc > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: sunday      
   41    0   0.000  # ctl type (contam conc > flow), law (on/off setpoint 50.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  50.00000 1.00000 1.00000
* Control mass    6
# senses node (2) manager_b
   -4    2    0    0  # sensor data
# actuates flow connection:  11 corid_left - manager_b via fan_8lps
   -3   11    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: weekdays    
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 0.00 direct action ON fraction 0.000.), starting @
      2.  # No. of data items
  0.00000 1.00000
* System
no global control description supplied
   1 # number of control loops defined
* Global control 
    0    0    0 # sensor data
    0    0    0 # actuator data
     0 # number of day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: individual  
    5    5   0.000  # ctl type (Global multisensor), law (Currently sensing mass flow loop # 4 OR mass flow loo), starting @
     7.0
  1.000 4.000 2.000 1.000 5.000 1.000 6.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: individual  
    0    2   0.000  # ctl type (undefined law), law (not yet coded for global controls.                   ), starting @
     0.0
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: individual  
    0    2   0.000  # ctl type (undefined law), law (not yet coded for global controls.                   ), starting @
     0.0
