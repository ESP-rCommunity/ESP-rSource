living room control of wch radiator - one day type applicable all year
* Building
Example of building control via wet central heating.  For more informati see the wch.log file in the cfg folder.
   1  # No. of functions
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sat  1 Jan - Sun 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      5.  # No. of data items
  3.000 1.000 2.000 10000.000 10000.000
# Function:Zone links
 1
* Plant
plt ctrl reg
   2  # No. of loops
* Control loops
# senses var in compt.  6:thermostat @ node no.  1
   -1    6    1    0  # sensor 
# plant component   1:boiler @ node no.  1
   -1    1    1    0  # actuator 
    1 # No. day types
    1  365  # valid Sat  1 Jan - Sun 31 Dec
     1  # No. of periods in day
   12    3   0.000  # ctl type, law (Prop numerical ctrl.), start @
      5.  # No. of data items
  0.000 1.000 22.300 22.300 0.500
* Control loops
# senses var in compt.  1:boiler @ node no.  1
   -1    1    1    0  # sensor 
# plant component   6:thermostat @ node no.  1
   -1    6    1    0  # actuator 
    1 # No. day types
    1  365  # valid Sat  1 Jan - Sun 31 Dec
     1  # No. of periods in day
   16    3   0.000  # ctl type, law (Prop numerical ctrl.), start @
      5.  # No. of data items
  0.200 0.000 1.000 0.000 0.000
