Ideal control for dual office model. Weekdays normal office hours, saturday reduced occupied hours, sunday stand-by only. One person per office, 100W lighting and 150W small power.
* Building
this is a base case set of assumptions
   1  # No. of functions
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
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
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 15.000 26.000 0.000
    0    1   9.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 19.000 24.000 0.000
    0    2  17.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 10.000 30.000 0.000
# Function:Zone links
 1,1,1,0
* Mass Flow
no flow control description supplied
   4  # No. of controls
* Control mass
# senses node (1) manager_a
   -4    1    0    0  # sensor data
# actuates flow connection:   1 man_alow - manager_a via low_win
   -3    1    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    1    2   0.000  # ctl type (dry bulb > flow), law (low/default/mid/hi), start @
      6.  # No. of data items
  19.100 21.000 26.000 0.100 1.500 0.100
* Control mass
# senses node (1) manager_a
   -4    1    0    0  # sensor data
# actuates flow connection:   2 man_ahi - manager_a via high_win
   -3    2    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    1    2   0.000  # ctl type (dry bulb > flow), law (low/default/mid/hi), start @
      6.  # No. of data items
  19.100 21.000 26.000 0.100 1.500 0.100
* Control mass
# senses node (2) manager_b
   -4    2    0    0  # sensor data
# actuates flow connection:   3 man_blow - manager_b via low_win
   -3    3    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    1    2   0.000  # ctl type (dry bulb > flow), law (low/default/mid/hi), start @
      6.  # No. of data items
  19.100 21.000 26.000 0.100 1.500 0.100
* Control mass
# senses node (2) manager_b
   -4    2    0    0  # sensor data
# actuates flow connection:   4 man_bhi - manager_b via high_win
   -3    4    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    1    2   0.000  # ctl type (dry bulb > flow), law (low/default/mid/hi), start @
      6.  # No. of data items
  19.100 21.000 26.000 0.100 1.500 0.100
