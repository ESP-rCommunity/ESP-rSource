simple building
* Building
match zone temperatures (loop 2) with 18C fixed 9h-18h (loop 1)
   2  # No. of functions
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  10000.000 0.000 10000.000 0.000 10.000 10.000
    0    1   9.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  10000.000 0.000 10000.000 0.000 18.000 18.000
    0    1  18.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  10000.000 0.000 10000.000 0.000 10.000 10.000
* Control function
# senses dry bulb temperature in roof_space.
    3    0    0    0  # sensor data
# actuates the air point in roof_space.
    3    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    0   11   0.000  # ctl type, law (match temp - ideal), start @
     16.  # No. of data items
  10000.000 0.000 10000.000 0.000 2.000 3.000 -3.000 0.000 0.000 0.000 1.000 0.000 0.000 0.000 1.000 0.000
# Function:Zone links
 1,1,2
