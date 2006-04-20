simple building  # overall descr 
* Building
match zone temperatures (loop 2), control to 18C with 10kW h/c (loop 1) 
   2  # No. of functions
* Control function
# measures the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     3  # No. of periods in day
    0    1   0.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  10000.000 0.000 10000.000 0.000 10.000 10.000
    0    1   9.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  10000.000 0.000 10000.000 0.000 18.000 18.000
    0    1  18.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  10000.000 0.000 10000.000 0.000 10.000 10.000
* Control function
# measures dry bulb temperature in roof_space
    3    0    0    0  # sensor data
# actuates the air point in zone. 3
    3    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
    0   11   0.000  # ctl type, law (match temp - ideal), start @
     10.  # No. of data items
  10000.000 0.000 10000.000 0.000 1.000 1.000 -3.000 0.000 0.000 0.000
# Function:Zone links
  1  1  2
