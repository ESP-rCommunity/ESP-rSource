*CONTROL
*cdoc control for CCHT house
*plant
*pdoc no descrip
   4                        # number of loops
*loop   1 water temp ctl
   -1    1    1    0    0  # senses var in compt.  1:Flow_source @ node no.  1
   -1    1    1    0        # actuates plant component   1:Flow_source @ node no.  1
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    0   12   0.000   2.     # ctl type, law (boundary control), start @, data items
2.00000 1.00000             # BC column and interpolation method.
*loop   2 plant_loop_02
   -1    1    1    0    0  # senses var in compt.  1:Flow_source @ node no.  1
   -1    1    2    0        # actuates plant component   1:Flow_source @ node no.  2
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    0   12   0.000   2.     # ctl type, law (boundary control), start @, data items
3.00000 1.00000             # bc column and interpolation method.
*loop   3 plant_loop_03
   -1    1    1    0    0  # senses var in compt.  1:Flow_source @ node no.  1
   -1    2    1    0        # actuates plant component   2:CHP_Unit @ node no.  1
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    0   12   0.000   2.     # ctl type, law (boundary control), start @, data items
4.00000 1.00000             # bc column and interpolation method.
*loop   4 plant_loop_04
   -1    1    1    0    0  # senses var in compt.  1:Flow_source @ node no.  1
   -1    2    2    0        # actuates plant component   2:CHP_Unit @ node no.  2
    1                       # all day types have same control
    1  365    1             # valid Fri-01-Jan - Fri-31-Dec, periods in weekday
    0   12   0.000   2.     # ctl type, law (boundary control), start @, data items
5.00000 1.00000             # bc column and interpolation method.
