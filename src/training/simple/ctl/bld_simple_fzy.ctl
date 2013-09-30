bld_simple_fzy.ctl
* Building
convective heating with fuzzy control
   1  # No. of functions
* Control function
# senses dry bulb temperature in reception.
    1    0    0    0  # sensor data
# actuates the air point in reception.
    1    0    0  # actuator data
    1 # No. day types
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day
    0   17   0.000  # ctl type, law (fuzzy logic PI-PD ctl), start @
      7.  # No. of data items
  1.000 1.000 21.000 2.000 1.000 1.000 0.900
# Function:Zone links
 1
