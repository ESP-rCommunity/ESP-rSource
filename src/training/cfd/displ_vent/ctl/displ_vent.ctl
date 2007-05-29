proj cntrl
* Building
control in the reception (note zero heating capacity). Cooling to 25C during the day, free float at other times.
   1  # No. of functions
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     3  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   7.990  # ctl type, law (basic control), start @
      7.  # No. of data items
  0.000 0.000 9000.000 0.000 20.000 25.000 0.000
    0    2  17.100  # ctl type, law (free floating), start @
      0.  # No. of data items
# Function:Zone links
 1,0,0
