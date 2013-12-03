Represents a master-slave control scheme where manager_a has the thermostat and manager_b and the corridor are fixed capacity slaves. Heating to 18C cooling 24C. (note how the rooms perform differently).
* Building
For testing an all-to-common control issue - rough control in slave zones. Capacity 1.12kW heating in total and 1.7kW cooling in total. Slave capacity via observation of demands in the base case model for seasonal assessments.
   3  # No. of functions
* Control function
# senses dry bulb temperature in manager_a.
    1    0    0    0  # sensor data
# actuates the air point in manager_a.
    1    0    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  540.000 0.000 1200.000 0.000 15.000 26.000 0.000
    0    1   6.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  540.000 0.000 1200.000 0.000 19.000 24.000 0.000
    0    1  18.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  540.000 0.000 1200.000 0.000 15.000 26.000 0.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  540.000 0.000 1200.000 0.000 15.000 26.000 0.000
    0    1   9.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  540.000 0.000 1200.000 0.000 19.000 24.000 0.000
    0    2  17.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  540.000 0.000 1200.000 0.000 10.000 30.000 0.000
* Control function
# senses dry bulb temperature in manager_a.
    1    0    0    0  # sensor data
# actuates the air point in manager_b.
    2    0    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0   21   0.000  # ctl type, law (slave capacity controller), start @
      3.  # No. of data items
  1.000 530.000 1200.000
    0   21   6.000  # ctl type, law (slave capacity controller), start @
      3.  # No. of data items
  1.000 530.000 1200.000
    0   21  18.000  # ctl type, law (slave capacity controller), start @
      3.  # No. of data items
  1.000 530.000 1200.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0   21   0.000  # ctl type, law (slave capacity controller), start @
      3.  # No. of data items
  1.000 530.000 1200.000
    0   21   9.000  # ctl type, law (slave capacity controller), start @
      3.  # No. of data items
  1.000 530.000 1200.000
    0    2  17.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    0   21   0.000  # ctl type, law (slave capacity controller), start @
      3.  # No. of data items
  1.000 530.000 1200.000
* Control function
# senses dry bulb temperature in manager_a.
    1    0    0    0  # sensor data
# actuates the air point in corridor.
    3    0    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0   21   0.000  # ctl type, law (slave capacity controller), start @
      3.  # No. of data items
  1.000 100.000 370.000
    0   21   6.000  # ctl type, law (slave capacity controller), start @
      3.  # No. of data items
  1.000 100.000 370.000
    0   21  18.000  # ctl type, law (slave capacity controller), start @
      3.  # No. of data items
  1.000 100.000 370.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0   21   0.000  # ctl type, law (slave capacity controller), start @
      3.  # No. of data items
  1.000 100.000 370.000
    0   21   9.000  # ctl type, law (slave capacity controller), start @
      3.  # No. of data items
  1.000 100.000 370.000
    0    2  17.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    0   21   0.000  # ctl type, law (slave capacity controller), start @
      3.  # No. of data items
  1.000 100.000 370.000
# Function:Zone links
 1,2,3
