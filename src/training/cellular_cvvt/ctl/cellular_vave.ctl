Simple VAV HVAC reverting to CAV with reheat for heating op. 11C supply equivalent of 20ach=0.2222 and min 5 ach in room (0.056m3/s) and CAV 0.056 m3/s. 700W reheat for desired room t of 20C.
* Building
this model is for testing purposes - the VAV representation is evolving. Note simulation trace must be used to track reheat and air flow rates..
   2  # No. of functions
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0   22   6.000  # ctl type, law (VAV and CAV air based), start @
      6.  # No. of data items
  700.000 11.000 20.000 0.222 0.056 0.000
    0    2  20.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0   22   6.000  # ctl type, law (VAV and CAV air based), start @
      6.  # No. of data items
  700.000 11.000 20.000 0.222 0.056 0.000
    0    2  20.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 10.000 30.000 0.000
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
    0    8   7.000  # ctl type, law (variable supply), start @
      6.  # No. of data items
  35.000 10.000 0.056 18.000 24.000 0.000
    0    2  19.000  # ctl type, law (free floating), start @
      0.  # No. of data items
# Function:Zone links
 1,1,2
