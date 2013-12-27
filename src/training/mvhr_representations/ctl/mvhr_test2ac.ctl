loop one for offices loop two for zero
* Building
ideal heating to 20C and cooling to 20.2C all hours of all days
   3  # No. of functions
* Control function    1
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # day types follow calendar  3
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2000.000 0.000 200.000 0.000 20.000 20.200 0.000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: saturday    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2000.000 0.000 200.000 0.000 20.000 20.200 0.000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: sunday      
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2000.000 0.000 200.000 0.000 20.000 20.200 0.000
* Control function    2
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # day types follow calendar  3
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  20000.000 0.000 20000.000 0.000 0.000 0.100 0.000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: saturday    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  20000.000 0.000 20000.000 0.000 0.000 0.100 0.000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: sunday      
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  20000.000 0.000 20000.000 0.000 0.000 0.100 0.000
* Control function    3
# senses dry bulb temperature in man_plt.
    4    0    0    0  # sensor data
# actuates the air point in man_plt.
    4    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: weekdays    
    0    6   0.000  # ctl type, law (flux zone/plant), start @
      7.  # No. of data items
  9.000 1.000 1.000 99000.000 99000.000 5.000 1.000
# Function:Zone links
 1,1,1,3,0,0,2,0,0,0,0,0
* Plant
Use PID at heater and cooling coil. 4C deadband, cooling capacity to match ideal
   2  # No. of loops
* Control loops    1
# senses dry bulb temperature in man_plt.
    4    0    0    0    0  # sensor 
# plant component   8:ht_coil @ node no.  1
   -1    8    1    0  # actuator 
    0 # day types follow calendar  3
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.00000 2000.00000 0.00000 20.00000 4.00000 900.00000 0.00000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: saturday    
    0    1   0.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.00000 2000.00000 0.00000 20.00000 4.00000 900.00000 0.00000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: sunday      
    0    1   0.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  1.00000 2000.00000 0.00000 20.00000 4.00000 900.00000 0.00000
* Control loops    2
# senses dry bulb temperature in man_plt.
    4    0    0    0    0  # sensor 
# plant component   9:cool_cl @ node no.  1
   -1    9    1    0  # actuator 
    0 # day types follow calendar  3
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  -1.00000 200.00000 0.00000 20.40000 4.00000 900.00000 0.00000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: saturday    
    0    1   0.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  -1.00000 200.00000 0.00000 20.40000 4.00000 900.00000 0.00000
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day: sunday      
    0    1   0.000  # ctl type, law (PID flux control.), start @
      7.  # No. of data items
  -1.00000 200.00000 0.00000 20.40000 4.00000 900.00000 0.00000
