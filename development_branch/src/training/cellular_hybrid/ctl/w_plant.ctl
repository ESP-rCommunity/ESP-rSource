no overall control description supplied
* Building
no zone control description supplied
   1  # No. of functions
* Control function
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   6.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  99999.000 0.000 99999.000 0.000 19.000 19.100 0.000
    0    2  18.000  # ctl type, law (free floating), start @
      0.  # No. of data items
# Function:Zone links
 0,0,0,1
* Mass Flow
no flow control description supplied
  18  # No. of controls
* Control mass
# senses ambient dry bulb temperature.
   -3    0    0    0  # sensor data
# actuates flow connection:   3 plant - 1inlet via fan
   -3    3    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day
    0    0   0.000  # ctl type (outside ambient > flow), law (on / off), start @
      3.  # No. of data items
  100.000 1.000 1.000
    0    0   8.000  # ctl type (outside ambient > flow), law (on / off), start @
      2.  # No. of data items
  -100.000 1.000
    0    0  18.000  # ctl type (outside ambient > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
* Control mass
# senses ambient dry bulb temperature.
   -3    0    0    0  # sensor data
# actuates flow connection:   1 plant - manager_b via fan
   -3    1    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day
    0    0   0.000  # ctl type (outside ambient > flow), law (on / off), start @
      3.  # No. of data items
  100.000 1.000 1.000
    0    0   8.000  # ctl type (outside ambient > flow), law (on / off), start @
      2.  # No. of data items
  -100.000 1.000
    0    0  18.000  # ctl type (outside ambient > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
* Control mass
# senses ambient dry bulb temperature.
   -3    0    0    0  # sensor data
# actuates flow connection:   2 manager_b - plant via grill
   -3    2    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day
    0    0   0.000  # ctl type (outside ambient > flow), law (on / off), start @
      3.  # No. of data items
  100.000 1.000 1.000
    0    0   8.000  # ctl type (outside ambient > flow), law (on / off), start @
      2.  # No. of data items
  -100.000 1.000
    0    0  18.000  # ctl type (outside ambient > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
* Control mass
# senses ambient dry bulb temperature.
   -3    0    0    0  # sensor data
# actuates flow connection:   7 manager_a - 1return via big_open
   -3    7    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day
    0    0   0.000  # ctl type (outside ambient > flow), law (on / off), start @
      3.  # No. of data items
  100.000 1.000 1.000
    0    0   8.000  # ctl type (outside ambient > flow), law (on / off), start @
      2.  # No. of data items
  -100.000 1.000
    0    0  18.000  # ctl type (outside ambient > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
* Control mass
# senses ambient dry bulb temperature.
   -3    0    0    0  # sensor data
# actuates flow connection:  11 manager_a - 1winlow via big_open
   -3   11    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day
    0    0   0.000  # ctl type (outside ambient > flow), law (on / off), start @
      3.  # No. of data items
  100.000 1.000 1.000
    0    0   8.000  # ctl type (outside ambient > flow), law (on / off), start @
      2.  # No. of data items
  -100.000 1.000
    0    0  18.000  # ctl type (outside ambient > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
* Control mass
# senses ambient dry bulb temperature.
   -3    0    0    0  # sensor data
# actuates flow connection:  15 manager_a - 1winhigh via big_open
   -3   15    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day
    0    0   0.000  # ctl type (outside ambient > flow), law (on / off), start @
      3.  # No. of data items
  100.000 1.000 1.000
    0    0   8.000  # ctl type (outside ambient > flow), law (on / off), start @
      2.  # No. of data items
  -100.000 1.000
    0    0  18.000  # ctl type (outside ambient > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
* Control mass
# senses ambient dry bulb temperature.
   -3    0    0    0  # sensor data
# actuates flow connection:   4 1inlet - 2inlet via big_open
   -3    4    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    0   0.000  # ctl type (outside ambient > flow), law (on / off), start @
      3.  # No. of data items
  21.000 -1.000 1.000
* Control mass
# senses ambient dry bulb temperature.
   -3    0    0    0  # sensor data
# actuates flow connection:   8 1return - 2return via big_open
   -3    8    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    0   0.000  # ctl type (outside ambient > flow), law (on / off), start @
      3.  # No. of data items
  21.000 -1.000 1.000
* Control mass
# senses ambient dry bulb temperature.
   -3    0    0    0  # sensor data
# actuates flow connection:  12 1winlow - 2winlow via big_open
   -3   12    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    0   0.000  # ctl type (outside ambient > flow), law (on / off), start @
      3.  # No. of data items
  21.000 1.000 1.000
* Control mass
# senses ambient dry bulb temperature.
   -3    0    0    0  # sensor data
# actuates flow connection:  16 1winhigh - 2winhigh via big_open
   -3   16    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    0   0.000  # ctl type (outside ambient > flow), law (on / off), start @
      3.  # No. of data items
  21.000 1.000 1.000
* Control mass
# senses ambient dry bulb temperature.
   -3    0    0    0  # sensor data
# actuates flow connection:   5 2inlet - 3inlet via big_open
   -3    5    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    0   0.000  # ctl type (outside ambient > flow), law (on / off), start @
      3.  # No. of data items
  10.000 1.000 1.000
* Control mass
# senses ambient dry bulb temperature.
   -3    0    0    0  # sensor data
# actuates flow connection:   9 2return - 3return via big_open
   -3    9    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    0   0.000  # ctl type (outside ambient > flow), law (on / off), start @
      3.  # No. of data items
  10.000 1.000 1.000
* Control mass
# senses ambient dry bulb temperature.
   -3    0    0    0  # sensor data
# actuates flow connection:  13 2winlow - 3winlow via big_open
   -3   13    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    0   0.000  # ctl type (outside ambient > flow), law (on / off), start @
      3.  # No. of data items
  10.000 -1.000 1.000
* Control mass
# senses ambient dry bulb temperature.
   -3    0    0    0  # sensor data
# actuates flow connection:  17 2winhigh - 3winhigh via big_open
   -3   17    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    0    0   0.000  # ctl type (outside ambient > flow), law (on / off), start @
      3.  # No. of data items
  10.000 -1.000 1.000
* Control mass
# senses node (1) manager_a
   -4    1    0    0  # sensor data
# actuates flow connection:   6 3inlet - manager_a via big_open
   -3    6    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on / off), start @
      3.  # No. of data items
  21.000 1.000 1.000
* Control mass
# senses node (1) manager_a
   -4    1    0    0  # sensor data
# actuates flow connection:  10 3return - plant via grill
   -3   10    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on / off), start @
      3.  # No. of data items
  21.000 1.000 1.000
* Control mass
# senses node (1) manager_a
   -4    1    0    0  # sensor data
# actuates flow connection:  14 3winlow - man_alow via low_win
   -3   14    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on / off), start @
      3.  # No. of data items
  21.000 -1.000 1.000
* Control mass
# senses node (1) manager_a
   -4    1    0    0  # sensor data
# actuates flow connection:  18 3winhigh - man_ahi via high_win
   -3   18    0  # actuator data
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on / off), start @
      3.  # No. of data items
  21.000 -1.000 1.000
