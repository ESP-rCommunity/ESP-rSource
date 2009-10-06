Heating and exhaust ventilation system.
* Building
Ventilation system
   2  # No. of functions
* Control function
# senses dry bulb temperature in reception.
    1    0    0    0  # sensor data
# actuates the air point in reception.
    1    0    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  10.000 1.000 1.000 10000.000 10000.000 9.000 1.000
* Control function
# senses dry bulb temperature in office.
    2    0    0    0  # sensor data
# actuates the air point in office.
    2    0    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  10.000 1.000 1.000 10000.000 10000.000 9.000 1.000
# Function:Zone links
 1,2,0
* Plant
plt ctrol
   1  # No. of loops
* Control loops
# senses dry bulb temperature in reception.
    1    0    0    0    0  # sensor 
# plant component   9:heat_coil @ node no.  1
   -1    9    1    0  # actuator 
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
      9.  # No. of data items
  1.00000 4000.00000 0.00000 20.00000 2.00000 0.00000 0.00000 0.00000 0.00000
