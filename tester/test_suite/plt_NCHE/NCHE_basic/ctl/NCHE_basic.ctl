*CONTROL
*cdoc no overall control description supplied
*plant
*pdoc no plant control description supplied
   1                        # number of loops
*loop   1 plant_loop_01
   -1    1    1    0    0  # senses var in compt.  1:StratTank @ node no.  1
   -1    3    1    0        # actuates plant component   3:CollPump @ node no.  1
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    1    8   0.000   7.     # ctl type, law (On-Off control.), start @, data items
  1.00000 -40.00000 -10.00000 0.00000 0.00002 0.00000 0.00000  # OnOff std mode 1.0 on setpt -40.00C off setpt -10.00C output @ hi 0.000 output @ lo 0.000 sensor lag 0.0s actuator lag 0.0s.
