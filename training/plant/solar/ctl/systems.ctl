Solar model
* Building
Solar collector plant coupled to a building.
   3
* Control function
    1    0    0    0
    1    0    0
     1
    1  365
     3
    0    2   0.000
     0.0
    0    6   8.000
     7.0
   6.00000   1.00000   2.00000  700.00  0.0  0.0  0.0
    0    2  17.000
     0.0
* Control function
    2    0    0    0
    2    0    0
     1
    1  365
     1
    0    6   0.
     7.0
   3.00000   1.00000   2.00000  10000.00  0.0  0.0  0.0
* Control function
    3    0    0    0
    3    0    0
     1
    1  365
     1
    0    6   0.
     7.0
   5.00000   2.00000   1.00000  10000.00  0.0  0.0  0.0
  1 2 3 0 0 0 0 0 0
* Plant
Flat plate solar collector
   3
* Control loops
    -1    2    1    0
   -1    1    1
     1
    1  365
     1
    1   2   0.0
    9.0
-1.0  0.000055 0.0000001  35.0   1.0  0.0  0.0  0.0  0.0  
* Control loops
    -1    5    2    0
   -1    4    1
     1
    1  365
     1
    1   2   0.0
    9.0
-1.0 0.06  0.0  18.0   1.0  0.0  0.0  0.0  0.0  
* Control loops
    1    0    0    0
   -1    6    1
     1
    1  365
     3
    0    1   0.000
     9.0
    1.0 0.0  0.0   5.0   1.0  0.0  0.0  0.0  0.0 
    0    1   8.000
     9.0
    1.0  700.0  0.0   19.0   1.0  0.0  0.0  0.0  0.0 
    0    1  17.000
     9.0
    1.0  0.0  0.0   5.0   1.0  0.0  0.0  0.0  0.0  
* Mass Flow
mass_flow ctl
   3
* Control function
   -4    0     0    
   -4    4     5    
     1
    1  365
     3
    1    0   0.000
      1.0
100.0
    1    0   7.990
      1.0
23.0
    1    0   18.000
      1.0
100.0
 south      dirg       op_win   dirg  
 passg      north      op_win   passg     
 south      rad        op_win   rad     
 south      exch       op_win   exch    
 south      mixed      op_win   mixed
* Control function
   -4    12    0    
   -3    22    0    
     1
    1  365
     3
    1    0   0.000
      1.0
100.0
    1    0   7.000
      1.0
26.0
    1    0   18.000
      1.0
100.0
* Control function
   -4    0     0    
   -4    5     2    
     1
    1  365
     2
    1    0   0.000
      1.0
100.0
    1    0   7.000
      1.0
22.0
 mixed      roof      extr     mixed        
 exch       north     extr     exch
