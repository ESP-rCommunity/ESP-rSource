control facade windows to closed 0h00-8h00 and 18h00-24h00. During day open if zone is > 21C.  Doors between entry and corridor to units are open during office hours on weekdays only.
* Mass Flow
mass_flow control is applied to each connection that has either ctdoor or ctwina to match the facade or inner door control logic. Made by setting up one of each type of control and the copy and re-assucng the sensor/actuator point.
  23  # No. of controls
* Control mass
# senses node (7) unit_a
   -4    7    0    0  # sensor data
# actuates flow connection:  41 unit_a - north via ctwina
   -3   41    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 21.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  21.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
* Control mass
# senses node (5) ent_g
   -4    5    0    0  # sensor data
# actuates flow connection:  56 ent_g - west via ctdoor
   -3   56    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 10.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  10.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      1.  # No. of data items
  100.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      1.  # No. of data items
  100.000
* Control mass
# senses node (7) unit_a
   -4    7    0    0  # sensor data
# actuates flow connection:  42 unit_a - east via ctwina
   -3   42    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 21.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  21.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
* Control mass
# senses node (7) unit_a
   -4    7    0    0  # sensor data
# actuates flow connection:  43 unit_a - west via ctwina
   -3   43    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 21.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  21.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
* Control mass
# senses node (8) unit_b
   -4    8    0    0  # sensor data
# actuates flow connection:  44 unit_b - east via ctwina
   -3   44    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 21.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  21.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
* Control mass
# senses node (9) unt_cd
   -4    9    0    0  # sensor data
# actuates flow connection:  45 unt_cd - east via ctwina
   -3   45    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 21.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  21.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
* Control mass
# senses node (10) unit_e
   -4   10    0    0  # sensor data
# actuates flow connection:  46 unit_e - south via ctwina
   -3   46    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 21.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  21.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
* Control mass
# senses node (10) unit_e
   -4   10    0    0  # sensor data
# actuates flow connection:  47 unit_e - east via ctwina
   -3   47    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 21.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  21.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
* Control mass
# senses node (10) unit_e
   -4   10    0    0  # sensor data
# actuates flow connection:  48 unit_e - west via ctwina
   -3   48    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 21.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  21.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
* Control mass
# senses node (11) unit_f
   -4   11    0    0  # sensor data
# actuates flow connection:  49 unit_f - north via ctwina
   -3   49    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 21.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  21.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
* Control mass
# senses node (11) unit_f
   -4   11    0    0  # sensor data
# actuates flow connection:  50 unit_f - east via ctwina
   -3   50    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 21.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  21.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
* Control mass
# senses node (12) unit_g
   -4   12    0    0  # sensor data
# actuates flow connection:  51 unit_g - east via ctwina
   -3   51    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 21.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  21.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
* Control mass
# senses node (13) unt_hi
   -4   13    0    0  # sensor data
# actuates flow connection:  52 unt_hi - east via ctwina
   -3   52    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 21.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  21.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
* Control mass
# senses node (14) unit_j
   -4   14    0    0  # sensor data
# actuates flow connection:  53 unit_j - south via ctwina
   -3   53    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 21.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  21.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
* Control mass
# senses node (14) unit_j
   -4   14    0    0  # sensor data
# actuates flow connection:  54 unit_j - east via ctwina
   -3   54    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 21.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  21.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
* Control mass
# senses node (14) unit_j
   -4   14    0    0  # sensor data
# actuates flow connection:  55 unit_j - west via ctwina
   -3   55    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 21.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  21.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
* Control mass
# senses node (14) unit_j
   -4   14    0    0  # sensor data
# actuates flow connection:  57 unit_j - ent_g via ctdoor
   -3   57    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 10.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  10.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      1.  # No. of data items
  100.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      1.  # No. of data items
  100.000
* Control mass
# senses node (11) unit_f
   -4   11    0    0  # sensor data
# actuates flow connection:  58 unit_f - ent_g via ctdoor
   -3   58    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 10.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  10.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      1.  # No. of data items
  100.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      1.  # No. of data items
  100.000
* Control mass
# senses node (13) unt_hi
   -4   13    0    0  # sensor data
# actuates flow connection:  59 unt_hi - ent_g via ctdoor
   -3   59    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 10.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  10.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      1.  # No. of data items
  100.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      1.  # No. of data items
  100.000
* Control mass
# senses node (7) unit_a
   -4    7    0    0  # sensor data
# actuates flow connection:  60 unit_a - corid1 via ctdoor
   -3   60    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 10.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  10.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      1.  # No. of data items
  100.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      1.  # No. of data items
  100.000
* Control mass
# senses node (10) unit_e
   -4   10    0    0  # sensor data
# actuates flow connection:  61 unit_e - corid1 via ctdoor
   -3   61    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 10.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  10.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      1.  # No. of data items
  100.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      1.  # No. of data items
  100.000
* Control mass
# senses node (12) unit_g
   -4   12    0    0  # sensor data
# actuates flow connection:  62 unit_g - ent_g via ctdoor
   -3   62    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 10.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  10.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      1.  # No. of data items
  100.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      1.  # No. of data items
  100.000
* Control mass
# senses node (8) unit_b
   -4    8    0    0  # sensor data
# actuates flow connection:  63 unit_b - corid1 via ctdoor
   -3   63    0  # actuator data
    0 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1    0   7.990  # ctl type (dry bulb > flow), law (on/off setpoint 10.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  10.000 1.000 1.000
    1    0  18.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 1.000.), starting @
      3.  # No. of data items
  100.000 1.000 1.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      1.  # No. of data items
  100.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on/off setpoint 100.00 direct action ON fraction 0.000.), starting @
      1.  # No. of data items
  100.000
