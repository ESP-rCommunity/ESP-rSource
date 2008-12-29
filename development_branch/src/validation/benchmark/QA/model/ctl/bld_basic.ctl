simple building  # overall descr 
* Building
convective heating, ideal control + cooling to 24C # bld descr 
   1  # No. of functions
* Control function
# measures the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     4  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  1000.000 0.000 1000.000 0.000 20.000 24.000
    0    1   9.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  1000.000 0.000 1000.000 0.000 20.000 24.000
    0    2  18.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
# Function:Zone links
  1  1  0
