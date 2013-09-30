Represents an initial optimal start controller (4 AM initial guess) to reach 20C +- 1C at 8h00 via a 300W heating input. Cooling not enabled. Loop 1 is for manager_a and manager_b and loop 2 for the corridor.
* Building
It has been calibrated to work with a rewind from 4AM to 2AM at 20 time steps per hour. A basic control is used after ideal startup.
   2  # No. of functions
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0   15   0.000  # ctl type, law (opt. start logic ctl), start @
      7.  # No. of data items
  300.000 20.000 1.000 8.000 0.500 1.000 4.000
    0    1   8.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1000.000 0.000 1000.000 0.000 20.000 24.000 0.000
    0    2  19.000  # ctl type, law (free floating), start @
      0.  # No. of data items
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  1500.000 0.000 1500.000 0.000 18.000 26.000 0.000
    0    2  19.000  # ctl type, law (free floating), start @
      0.  # No. of data items
# Function:Zone links
 1,1,2
