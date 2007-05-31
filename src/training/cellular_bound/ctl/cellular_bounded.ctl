Ideal control for dual office model. Weekdays normal office hours, saturday reduced occupied hours, sunday stand-by only. One person per office, 100W lighting and 150W small power. Tighter setpoints for manager_b (so the mean control in boundary_up
* Building
The floor_below zone is controlled to the temperature of the suspended ceiling zone (to act as a boundary). The boundary_up zone is controlled to the mean temperature of manager_a and manager_b.
   4  # No. of functions
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
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
     3  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 15.000 26.000 0.000
    0    1   9.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 19.000 24.000 0.000
    0    2  17.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 10.000 30.000 0.000
* Control function
# senses dry bulb temperature in floor_below.
    4    0    0    0  # sensor data
# actuates the air point in floor_below.
    4    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    0   11   0.000  # ctl type, law (senses dry bulb temperature in ceiling_abv.), start @
     12.  # No. of data items
  2000.000 0.000 2000.000 0.000 1.000 3.000 5.000 0.000 0.000 0.000 1.000 0.000
* Control function
# senses dry bulb temperature in boundary_up.
    6    0    0    0  # sensor data
# actuates the air point in boundary_up.
    6    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    0   11   0.000  # ctl type, law (senses dry bulb temperature in manager_a.), start @
     16.  # No. of data items
  2000.000 0.000 2000.000 0.000 2.000 3.000 1.000 0.000 0.000 0.000 2.000 0.000 0.000 0.000 1.000 0.000
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 16.000 26.000 0.000
    0    1   6.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 20.000 23.000 0.000
    0    1  18.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 16.000 26.000 0.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 15.000 26.000 0.000
    0    1   9.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 19.000 24.000 0.000
    0    2  17.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 10.000 30.000 0.000
# Function:Zone links
 1,4,1,2,0,3
