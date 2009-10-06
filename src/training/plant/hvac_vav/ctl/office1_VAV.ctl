proj cntrl
* Building
no descrip
   2  # No. of functions
* Control function
# senses dry bulb temperature in west.
    1    0    0    0  # sensor data
# actuates the air point in west.
    1    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  6.000 1.000 1.000 99000.000 99000.000 7.000 1.000
* Control function
# senses dry bulb temperature in east.
    2    0    0    0  # sensor data
# actuates the air point in east.
    2    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  5.000 1.000 1.000 99000.000 99000.000 8.000 1.000
# Function:Zone links
 1,2
* Plant
no descrip
   1  # No. of loops
* Control loops
# senses var in compt.  4:duct_s @ node no.  1
   -1    4    1    0    0  # sensor 
# plant component   3:cooling_coil @ node no.  1
   -1    3    1    0  # actuator 
    1 # No. day types
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     3  # No. of periods in day
    1    2   0.000  # ctl type, law (PID flow control.), start @
      7.  # No. of data items
  -1.00000 0.00100 0.00000 17.00000 3.00000 0.00000 0.00000
    1    2   7.000  # ctl type, law (PID flow control.), start @
      7.  # No. of data items
  -1.00000 0.25000 0.00000 13.00000 1.00000 0.00000 0.00000
    1    2  18.000  # ctl type, law (PID flow control.), start @
      7.  # No. of data items
  -1.00000 0.00100 0.00000 17.00000 3.00000 0.00000 0.00000
* Mass Flow
flow control on damper (twice) and fan (twice)
   4  # No. of controls
* Control mass
# senses node (1) west
   -4    1    0    0  # sensor data
# actuates flow connection:   8 reheat_w - damp_w via damper
   -3    8    0  # actuator data
    1 # No. day types
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     3  # No. of periods in day
    1    1   0.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 18.00 valve lower limit (%)10.0 signal upper limit 27.00 valve upper limit (%)100.0 dS 0.00.), starting @
      5.  # No. of data items
  18.00000 10.00000 27.00000 100.00000 0.00000
    1    1   7.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 18.00 valve lower limit (%)10.0 signal upper limit 21.00 valve upper limit (%)100.0 dS 0.00.), starting @
      5.  # No. of data items
  18.00000 10.00000 21.00000 100.00000 0.00000
    1    1  18.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 18.00 valve lower limit (%)10.0 signal upper limit 27.00 valve upper limit (%)100.0 dS 0.00.), starting @
      5.  # No. of data items
  18.00000 10.00000 27.00000 100.00000 0.00000
* Control mass
# senses node (12) junc_e
   -4   12    0    0  # sensor data
# actuates flow connection:   5 duct_in - fan_s via fan
   -3    5    0  # actuator data
    1 # No. day types
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     3  # No. of periods in day
    1    1   0.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 18.00 valve lower limit (%)10.0 signal upper limit 27.00 valve upper limit (%)100.0 dS 0.00.), starting @
      5.  # No. of data items
  18.00000 10.00000 27.00000 100.00000 0.00000
    1    1   7.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 18.00 valve lower limit (%)10.0 signal upper limit 21.00 valve upper limit (%)100.0 dS 0.00.), starting @
      5.  # No. of data items
  18.00000 10.00000 21.00000 100.00000 0.00000
    1    1  18.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 18.00 valve lower limit (%)10.0 signal upper limit 27.00 valve upper limit (%)100.0 dS 0.00.), starting @
      5.  # No. of data items
  18.00000 10.00000 27.00000 100.00000 0.00000
* Control mass
# senses node (2) east
   -4    2    0    0  # sensor data
# actuates flow connection:  10 reheat_e - damp_e via damper
   -3   10    0  # actuator data
    1 # No. day types
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     3  # No. of periods in day
    1    1   0.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 18.00 valve lower limit (%)10.0 signal upper limit 27.00 valve upper limit (%)100.0 dS 0.00.), starting @
      5.  # No. of data items
  18.00000 10.00000 27.00000 100.00000 0.00000
    1    1   7.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 18.00 valve lower limit (%)10.0 signal upper limit 21.00 valve upper limit (%)100.0 dS 0.00.), starting @
      5.  # No. of data items
  18.00000 10.00000 21.00000 100.00000 0.00000
    1    1  18.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 18.00 valve lower limit (%)10.0 signal upper limit 27.00 valve upper limit (%)100.0 dS 0.00.), starting @
      5.  # No. of data items
  18.00000 10.00000 27.00000 100.00000 0.00000
* Control mass
# senses node (12) junc_e
   -4   12    0    0  # sensor data
# actuates flow connection:  15 junc_e - fan_e via fan
   -3   15    0  # actuator data
    1 # No. day types
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     3  # No. of periods in day
    1    1   0.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 18.00 valve lower limit (%)10.0 signal upper limit 27.00 valve upper limit (%)100.0 dS 0.00.), starting @
      5.  # No. of data items
  18.00000 10.00000 27.00000 100.00000 0.00000
    1    1   7.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 18.00 valve lower limit (%)10.0 signal upper limit 21.00 valve upper limit (%)100.0 dS 0.00.), starting @
      5.  # No. of data items
  18.00000 10.00000 21.00000 100.00000 0.00000
    1    1  18.000  # ctl type (dry bulb > flow), law (proportional + hysteresis: signal lower limit 18.00 valve lower limit (%)10.0 signal upper limit 27.00 valve upper limit (%)100.0 dS 0.00.), starting @
      5.  # No. of data items
  18.00000 10.00000 27.00000 100.00000 0.00000
