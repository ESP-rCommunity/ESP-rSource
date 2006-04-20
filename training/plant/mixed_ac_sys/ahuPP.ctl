AHU system
* Building
AHU plant coupled to zone 1
   1
* Control function
    1    0    0    0
    1    0    0
     1
    1  365
     1
    0    6   0.000
     7.0
   12.00000   5.00000   1.00000  100000.00  100000.0  13  3
  1 0 0
* Plant
Single zone AHU.
  4
* Control loops
   -1   5   3  0
   -1   4   1  
      1
  1  365
     3
  0  0  0
     0
  0  1  7
   7.0
# pid md, qmax, qmin,   sp,  thro, ia, da, 
   1.0   3500.   0.0    5.0  2.0   0   0  
  0  0  18
     0
* Control loops
   -1   7   3  0
   -1   6   1  
      1
  1  365
     3
  11  0  0
     0
  11  2  7
      7
# pid md,  qmax,  qmin,    sp,   thro, ia, da 
  1.0     0.005  0.00005    50.0   12.0   0   0
  11  0  18
     0
* Control loops
   -1  13  3
   -1  10  1
      1
  1  365
     3
  0  0  0
     0
  0  1  7
    7.0
# pid md, qmax, qmin,    sp,  thro, ia, da
 1.0     5000.0  0.0    22.5    1.0   0   0 
  0  0  18
     0
* Control loops
   -1  13   3  0
   -1   8   1  
      1
  1  365
     3
  0  0  0
     0
  10  1  7
      7
# pid md, qmax, qmin,    sp,   thro, ia, da 
 -1.0     1600.0  0.0    50.0    10.0   0   0
  0  0  18
     0
