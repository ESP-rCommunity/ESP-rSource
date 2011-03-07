simple building
* Building
convective heating via system components with PID controller and bypass
   1  # No. of functions
* Control function
# senses dry bulb temperature in reception.
    1    0    0    0  # sensor data
# actuates the air point in reception.
    1    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  4.000 1.000 1.000 5000.000 1000.000 5.000 1.000
# Function:Zone links
 1,0,0
* Plant
implements a component heater over 3 periods
   1  # No. of loops
* Control loops
# senses dry bulb temperature in reception.
    1    0    0    0  # sensor 
# plant component   3:heating_coil @ node no.  1
   -1    3    1    0  # actuator 
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     3  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
      9.  # No. of data items
  1.000 7000.000 0.000 14.000 8.000 1.000 1080.000 1.000 70.000
    0    1   7.000  # ctl type, law (PID flux control.), start @
      9.  # No. of data items
  1.000 10000.000 0.000 21.000 8.000 1.000 1080.000 1.000 70.000
    0    1  18.000  # ctl type, law (PID flux control.), start @
      9.  # No. of data items
  1.000 7000.000 0.000 14.000 8.000 1.000 1080.000 1.000 70.000
* Mass Flow
implements a damper via control of two separate paths
   3  # No. of controls
* Control mass
# senses node (  0) 
   -4    0    0    0  # sensor data
# actuates flow component:   9 damper2
   -4    9    1  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
   24    1   0.000  # ctl type (delt T > flow), law (proportional + hysteresis), start @
      5.  # No. of data items
  0.000 10.000 20.000 90.000 0.000
exhaust       mixing        damper2       recep         north       
* Control mass
# senses node (  0) 
   -4    0    0    0  # sensor data
# actuates flow component:   8 damper1
   -4    8    1  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
   24    1   0.000  # ctl type (delt T > flow), law (proportional + hysteresis), start @
      5.  # No. of data items
  0.000 90.000 20.000 10.000 0.000
north         mixing        damper1       recep         north       
* Control mass
# senses node (  0) 
   -4    0    0    0  # sensor data
# actuates flow component:  10 damper3
   -4   10    1  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
   24    1   0.000  # ctl type (delt T > flow), law (proportional + hysteresis), start @
      5.  # No. of data items
  0.000 10.000 20.000 90.000 0.000
exhaust       north         damper3       recep         north       
