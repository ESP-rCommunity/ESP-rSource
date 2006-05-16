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
   11.00000   1.00000   1.00000  100000.00  100000.0  13  1
  1 0 0
* Plant
Single zone AHU.
  4
* Control loops
   -1   13   1  0
   -1   4   1  
      1
  1  365
     3
  0  0  0
     0
  10  1  7
   7.0
# pid md, qmax, qmin,   sp,  thro, ia, da, 
   1.0   3500.   0.0    50     12.0   0   0  
  0  0  18
     0
* Control loops
   -1   7   1  0
   -1   6   1  
      1
  1  365
     3
  11  0  0
     0
  11  2  7
      7
# pid md,  qmax,  qmin,    sp,   thro, ia, da 
  1.0     0.005  0.001    50.0   12.0   0   0
  11  0  18
     0
* Control loops
   -1  13  1
   -1  10  1
      1
  1  365
     3
  0  0  0
     0
  0  1  7
    8.0
# pid md, qmax, qmin,    sp,  thro, iafg, iat dafg 
 1.0     5000.0  0.0    19.0    2.0   1   900  0  
  0  0  18
     0
* Control loops
   -1   9   1  0
   -1   8   1  
      1
  1  365
     3
  11  0  0
     0
  10  1  7
      7
# pid md, qmax, qmin,    sp,   thro, ia, da 
 -1.0     800.0  0.0    19.0     2.0   0   0
  11  0  18
     0
