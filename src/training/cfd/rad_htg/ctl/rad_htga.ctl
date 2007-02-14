rad ctl
* Building
radiator ctl
   1  # No. of functions
* Control function
# senses dry bulb temperature in room.
    1    0    0    0  # sensor data
# actuates at inside face of radiator in room.
    1    7    0  # actuator data
    1 # No. day types
    1  365  # valid Fri-01-Jan - Fri-31-Dec
     3  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    5   7.990  # ctl type, law (basic proportional), start @
     11.  # No. of data items
  4.000 4000.000 2000.000 18.000 2.000 0.000 0.000 0.000 0.000 200.000 0.000
    0    2  16.990  # ctl type, law (free floating), start @
      0.  # No. of data items
# Function:Zone links
 1,0,0,0,0
