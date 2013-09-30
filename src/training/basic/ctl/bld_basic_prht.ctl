simple building
* Building
morning pre-heating
   1  # No. of functions
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0    4   0.000  # ctl type, law (fixed inject/extract), start @
      4.  # No. of data items
  1000.000 0.000 18.000 100.000
    0    1   9.000  # ctl type, law (basic control), start @
      6.  # No. of data items
  1000.000 0.000 0.000 0.000 21.000 100.000
    0    2  18.000  # ctl type, law (free floating), start @
      0.  # No. of data items
# Function:Zone links
 1,1,0
