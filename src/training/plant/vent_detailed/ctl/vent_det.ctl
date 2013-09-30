Heating and exhaust ventilation system.
* Plant
zone_b (imaginary) is heated to 20C, throt range 3C via PID controller with 6kW capacity. Senses zone_b, actuates the heater component all hrs.
   1  # No. of loops
* Control loops
# senses var in compt. 11:zone_b @ node no.  2
   -1   11    2    0    0  # sensor 
# plant component   9:heater @ node no.  1
   -1    9    1    0  # actuator 
    1 # No. day types
    1  365  # valid Sat-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    0    1   0.000  # ctl type, law (PID flux control.), start @
      9.  # No. of data items
  1.00000 6000.00000 0.00000 20.00000 3.00000 0.00000 0.00000 0.00000 0.00000
