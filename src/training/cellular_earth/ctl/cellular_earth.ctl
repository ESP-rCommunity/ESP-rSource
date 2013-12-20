Ideal control for dual office model. Weekdays normal office hours, saturday reduced occupied hours, sunday stand-by only. One person per office, 100W lighting and 150W small power.
* Building
this is a base case set of assumptions
   1  # No. of functions
* Control function    1
# senses the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates air point of the current zone
    0    0    0  # actuator data
    0 # day types follow calendar  3
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: weekdays    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 15.000 26.000 0.000
    0    1   6.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 19.000 24.000 0.000
    0    1  18.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 15.000 26.000 0.000
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     3  # No. of periods in day: saturday    
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 15.000 26.000 0.000
    0    1   9.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 19.000 24.000 0.000
    0    2  17.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: sunday      
    0    1   0.000  # ctl type, law (basic control), start @
      7.  # No. of data items
  2500.000 0.000 2500.000 0.000 10.000 30.000 0.000
# Function:Zone links
 1,1,1,0,0,0
* Mass Flow
Range based flow control for extract fans. Low flow if heating or n cooling is on. 4ach equiv for normal operation, 1.5x if over 22C
   2  # No. of controls
* Control mass    1
# senses node (1) manager_a
   -4    1    0    0  # sensor data
# actuates flow connection:   9 manager_a - extr_point via extra_4ach
   -3    9    0  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: weekdays    
    1    2   0.000  # ctl type (dry bulb > flow), law (range setpoints: low 19.50 mid 22.00 high 23.90 actuation ranges: low (< low sp) 0.10 mid (>mid sp) 1.50 high (>high sp) 0.10.), starting @
      6.  # No. of data items
  19.50000 22.00000 23.90000 0.10000 1.50000 0.10000
* Control mass    2
# senses node (2) manager_b
   -4    2    0    0  # sensor data
# actuates flow connection:  10 manager_b - extr_point via extra_4ach
   -3   10    0  # actuator data
    1  # all daytypes
    1  365  # valid Sun-01-Jan - Sun-31-Dec
     1  # No. of periods in day: weekdays    
    1    2   0.000  # ctl type (dry bulb > flow), law (range setpoints: low 19.50 mid 22.00 high 23.90 actuation ranges: low (< low sp) 0.10 mid (>mid sp) 1.50 high (>high sp) 0.10.), starting @
      6.  # No. of data items
  19.50000 22.00000 23.90000 0.10000 1.50000 0.10000
