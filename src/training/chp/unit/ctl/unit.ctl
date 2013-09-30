proj cntrl
* Plant
no descrip
   2  # No. of loops
* Control loops
# senses var in compt.  7:DHW-converge @ node no.  1
   -1    7    1    0    0  # sensor 
# plant component   1:CHP-unit @ node no.  1
   -1    1    1    0  # actuator 
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     3  # No. of periods in day
   12    0   0.000  # ctl type, law (period off), start @
      0.  # No. of data items
   12    8   7.000  # ctl type, law (On-Off control.), start @
      7.  # No. of data items
  1.00000 90.00000 80.00000 15000.00000 0.00000 0.00000 0.00000
   12    0  22.000  # ctl type, law (period off), start @
      0.  # No. of data items
* Control loops
# senses var in compt.  2:calorifier @ node no.  1
   -1    2    1    0    0  # sensor 
# plant component   5:diverting_valve @ node no.  1
   -1    5    1    0  # actuator 
    1 # No. day types
    1  365  # valid Mon-01-Jan - Mon-31-Dec
     1  # No. of periods in day
   18    5   0.000  # ctl type, law (Prop`l damper ctl.), start @
      6.  # No. of data items
  1.00000 0.10000 80.00000 70.00000 11.00000 3.00000
