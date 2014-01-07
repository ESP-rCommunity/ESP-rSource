Solar model
* Building
Solar collector plant coupled to a building.
   3  # No. of functions
* Control function    1
# senses dry bulb temperature in direct_gain.
    1    0    0    0  # sensor data
# actuates the air point in direct_gain.
    1    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekday     
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    6   8.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  6.000 1.000 2.000 700.000 0.000 0.000 0.000
    0    2  17.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function    2
# senses dry bulb temperature in rad_test.
    2    0    0    0  # sensor data
# actuates the air point in rad_test.
    2    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: weekday     
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  3.000 1.000 2.000 10000.000 0.000 0.000 0.000
* Control function    3
# senses dry bulb temperature in heatexch.
    3    0    0    0  # sensor data
# actuates the air point in heatexch.
    3    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: weekday     
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  5.000 2.000 1.000 10000.000 0.000 0.000 0.000
# Function:Zone links
 1,2,3,0,0,0,0,0,0
* Plant
Flat plate solar collector
   3  # No. of loops
* Control loops    1
# senses var in compt.  2:collector @ node no.  1
   -1    2    1    0    0  # sensor 
# plant component   1:pump @ node no.  1
   -1    1    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: weekday     
    1    2   0.000  # ctl type, law (PID flow control.), start @
      9.  # No. of data items
  -1.00000 0.00006 0.00000 35.00000 1.00000 0.00000 0.00000 0.00000 0.00000
* Control loops    2
# senses var in compt.  5:heating_coil @ node no.  2
   -1    5    2    0    0  # sensor 
# plant component   4:fan @ node no.  1
   -1    4    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: weekday     
    1    2   0.000  # ctl type, law (PID flow control.), start @
      9.  # No. of data items
  -1.00000 0.06000 0.00000 18.00000 1.00000 0.00000 0.00000 0.00000 0.00000
* Control loops    3
# senses dry bulb temperature in direct_gain.
    1    0    0    0    0  # sensor 
# plant component   6:oil_radiator @ node no.  1
   -1    6    1    0  # actuator 
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekday     
    0    1   0.000  # ctl type, law (PID flux control.), start @
      9.  # No. of data items
  1.00000 0.00000 0.00000 5.00000 1.00000 0.00000 0.00000 0.00000 0.00000
    0    1   8.000  # ctl type, law (PID flux control.), start @
      9.  # No. of data items
  1.00000 700.00000 0.00000 19.00000 1.00000 0.00000 0.00000 0.00000 0.00000
    0    1  17.000  # ctl type, law (PID flux control.), start @
      9.  # No. of data items
  1.00000 0.00000 0.00000 5.00000 1.00000 0.00000 0.00000 0.00000 0.00000
* Mass Flow
mass_flow ctl
   3  # No. of controls
* Control mass    1
# sensor node(s) not yet defined
   -4    0    0    0  # sensor data
# actuates flow component:   4 op_win
   -4    4    5  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekday     
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.00000 1.00000 1.00000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 23.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  23.00000 1.00000 1.00000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.00000 1.00000 1.00000
south         dirg          op_win        dirg        
passg         north         op_win        passg       
south         rad           op_win        rad         
south         exch          op_win        exch        
south         mixed         op_win        mixed       
* Control mass    2
# senses node (12) air_r
   -4   12    0    0  # sensor data
# actuates flow connection:  22 air_r - mixed via blower
   -3   22    0  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekday     
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.00000 1.00000 1.00000
    1    0   7.000  # ctl type (dry bulb > flow), law (on/off setpoint 26.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  26.00000 1.00000 1.00000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.00000 1.00000 1.00000
* Control mass    3
# sensor node(s) not yet defined
   -4    0    0    0  # sensor data
# actuates flow component:   5 extr
   -4    5    2  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     2  # No. of periods in day: weekday     
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.00000 1.00000 1.00000
    1    0   7.000  # ctl type (dry bulb > flow), law (on/off setpoint 22.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  22.00000 1.00000 1.00000
mixed         roof          extr          mixed       
exch          north         extr          exch        
