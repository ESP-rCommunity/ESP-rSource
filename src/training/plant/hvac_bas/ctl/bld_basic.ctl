simple building  # overall descr 
* Building
convective heating, ideal control  # bld descr 
   1  # No. of functions
* Control function
# measures dry bulb temperature in reception
    1    0    0    0  # sensor data
# actuates the air point in zone. 1
    1    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  4.000 1.000 1.000 99000.000 99000.000 5.000 1.000
# Function:Zone links
   1  0  0
* Plant
no descrip  # plant descr 
   1  # No. of loops
* Control loops
# measures var in compt.  5:extract_duct @ node no.  1
   -1    5    1  # sensor 
# plant component   3:heating_coil @ node no.  1
   -1    3    1  # actuator 
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     3  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
     9.  # No. of data items
  1.000 15000.000 0.000 14.000 6.000 1.000 1080.000 1.000 270.000
    0    1   7.000  # ctl type, law (PID flux control.), start @
      9.  # No. of data items
  1.000 15000.000 0.000 21.000 6.000 1.000 1080.000 1.000 270.000
    0    1  18.000  # ctl type, law (PID flux control.), start @
     9.  # No. of data items
  1.000 15000.000 0.000 14.000 6.000 1.000 1080.000 1.000 270.000
