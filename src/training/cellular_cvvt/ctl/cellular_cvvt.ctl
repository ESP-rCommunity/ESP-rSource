Approximation of CV HVAC with variable supply temperature (10C cooling 35C heating supply at an flow rate equivalent to 5ach. Loop 2 is for office loop 3 for corididor. 1 person per office, 100W lights and. 150W small power.
* Building
this is a variant of the base case set of assumptions
   3  # No. of functions
* Control function    1
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # day types follow calendar  3
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 15.000 26.000 0.000
    0    1   6.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 19.000 24.000 0.000
    0    1  18.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 15.000 26.000 0.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: saturday    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 15.000 26.000 0.000
    0    1   9.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 19.000 24.000 0.000
    0    2  17.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: sunday      
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 10.000 30.000 0.000
* Control function    2
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekdays    
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    8   7.000  # ctl type, law (variable supply), start @
      6.  # No. of data items
  35.000 10.000 0.056 18.000 24.000 0.000
    0    2  19.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function    3
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekdays    
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    8   7.000  # ctl type, law (variable supply), start @
      6.  # No. of data items
  35.000 10.000 0.056 18.000 24.000 0.000
    0    2  19.000  # ctl type, law (free floating), start @
      0.  # No. of data items
# Function:Zone links
 2,2,3
