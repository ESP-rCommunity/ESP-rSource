no overall control description supplied
* Plant
no plant control description supplied
   3  # No. of loops
* Control loops
# senses var in compt.  3:dhw_tank @ node no.  1
   -1    3    1    0    0  # sensor
# plant component   3:dhw_tank @ node no.  1
   -1    3    1    0  # actuator
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
   12    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 54.00000 56.00000 1.00000 0.00000 0.00000 0.00000
* Control loops
# sen var diff bet compt.  1:solar_collector @ node  1and compt  4:solar_t
   -1    1    1    4    1  # sensor
# plant component   2:collector_pumpr @ node no.  1
   -1    2    1    0  # actuator
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
   24    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 1.00000 5.55000 0.00000 0.0000380 0.00000 0.00000
* Control loops
# sen var diff bet compt.  1:solar_collector @ node  1and compt  4:solar_t
   -1    1    1    4    1  # sensor
# plant component   6:tank_pump @ node no.  1
   -1    6    1    0  # actuator
    1 # No. day types
    1  365  # valid Fri  1 Jan - Fri 31 Dec
     1  # No. of periods in day
   24    8   0.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 1.00000 5.55000 0.00000 0.0000380 0.00000 0.00000
