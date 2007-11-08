Ideal floor heating control for dual office model. Weekdays normal office hours, saturday and sunday reduced occupied hours. One person per office, 100W lighting and 150W small power.
* Building
Heat injected in floor zone with high hc coef, controlled via multi-sensor inject in floor (heat to ~30C) to heat manager_a to ~21C. Cooling (summer) via chilled ceiling (cooled to ~16C) to keep zone ~26C.
   3  # No. of functions
* Control function
# senses dry bulb temperature in manager_a.
    1    0    0    0  # sensor data
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
# senses dry bulb temperature in manager_a.
    1    0    0    0  # sensor data
# actuates the air point in floor.
    4    0    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0   19   0.000  # ctl type, law (senses dry bulb temperature in manager_a.), start @
     13.  # No. of data items
  2000.000 0.000 0.000 0.000 35.000 100.000 1.000 1.000 0.000 0.000 0.000 21.000 100.000
    0   19   6.000  # ctl type, law (senses dry bulb temperature in manager_a.), start @
     13.  # No. of data items
  2000.000 0.000 0.000 0.000 30.000 100.000 1.000 1.000 0.000 0.000 0.000 21.000 100.000
    0   19  18.000  # ctl type, law (senses dry bulb temperature in manager_a.), start @
     13.  # No. of data items
  2000.000 0.000 0.000 0.000 28.000 100.000 1.000 1.000 0.000 0.000 0.000 18.000 100.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0   19   0.000  # ctl type, law (senses dry bulb temperature in manager_a.), start @
     13.  # No. of data items
  2000.000 0.000 0.000 0.000 28.000 100.000 1.000 1.000 0.000 0.000 0.000 18.000 100.000
    0   19   8.000  # ctl type, law (senses dry bulb temperature in manager_a.), start @
     13.  # No. of data items
  2000.000 0.000 0.000 0.000 35.000 100.000 1.000 1.000 0.000 0.000 0.000 21.000 100.000
    0    2  17.000  # ctl type, law (free floating), start @
     13.  # No. of data items
  2000.000 0.000 0.000 0.000 28.000 100.000 1.000 1.000 0.000 0.000 0.000 18.000 100.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0   19   0.000  # ctl type, law (senses dry bulb temperature in manager_a.), start @
     13.  # No. of data items
  2000.000 0.000 0.000 0.000 28.000 100.000 1.000 1.000 0.000 0.000 0.000 18.000 100.000
    0   19   8.000  # ctl type, law (senses dry bulb temperature in manager_a.), start @
     13.  # No. of data items
  2000.000 0.000 0.000 0.000 35.000 100.000 1.000 1.000 0.000 0.000 0.000 21.000 100.000
    0    2  17.000  # ctl type, law (free floating), start @
     13.  # No. of data items
  2000.000 0.000 0.000 0.000 28.000 100.000 1.000 1.000 0.000 0.000 0.000 18.000 100.000
* Control function
# senses dry bulb temperature in ceiling_slb.
    5    0    0    0  # sensor data
# actuates the air point in ceiling_slb.
    5    0    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0   19   0.000  # ctl type, law (senses dry bulb temperature in manager_a.), start @
     13.  # No. of data items
  0.000 0.000 2000.000 0.000 0.000 16.000 1.000 1.000 0.000 0.000 0.000 0.000 26.000
    0   19   6.000  # ctl type, law (senses dry bulb temperature in manager_a.), start @
     13.  # No. of data items
  0.000 0.000 2000.000 0.000 0.000 16.000 1.000 1.000 0.000 0.000 0.000 0.000 26.000
    0   19  18.000  # ctl type, law (senses dry bulb temperature in manager_a.), start @
     13.  # No. of data items
  0.000 0.000 2000.000 0.000 0.000 18.000 1.000 1.000 0.000 0.000 0.000 0.000 28.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0   19   0.000  # ctl type, law (senses dry bulb temperature in manager_a.), start @
     13.  # No. of data items
  0.000 0.000 2000.000 0.000 0.000 16.000 1.000 1.000 0.000 0.000 0.000 0.000 26.000
    0   19   6.000  # ctl type, law (senses dry bulb temperature in manager_a.), start @
     13.  # No. of data items
  0.000 0.000 2000.000 0.000 0.000 16.000 1.000 1.000 0.000 0.000 0.000 0.000 26.000
    0   19  18.000  # ctl type, law (senses dry bulb temperature in manager_a.), start @
     13.  # No. of data items
  0.000 0.000 2000.000 0.000 0.000 18.000 1.000 1.000 0.000 0.000 0.000 0.000 28.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0   19   0.000  # ctl type, law (senses dry bulb temperature in manager_a.), start @
     13.  # No. of data items
  0.000 0.000 2000.000 0.000 0.000 16.000 1.000 1.000 0.000 0.000 0.000 0.000 26.000
    0   19   6.000  # ctl type, law (senses dry bulb temperature in manager_a.), start @
     13.  # No. of data items
  0.000 0.000 2000.000 0.000 0.000 16.000 1.000 1.000 0.000 0.000 0.000 0.000 26.000
    0   19  18.000  # ctl type, law (senses dry bulb temperature in manager_a.), start @
     13.  # No. of data items
  0.000 0.000 2000.000 0.000 0.000 18.000 1.000 1.000 0.000 0.000 0.000 0.000 28.000
# Function:Zone links
 0,0,0,2,3
