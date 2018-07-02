/*

 GenOpt command file for ESP-r optimisation

*/
Vary{
   Parameter{   // offy1
     Name    =     offy1;
     Ini     =         3;
     Values  =   "5, 6, 7";
     Type   =       SET;
   }
   Parameter{    // offy2
     Name    =      offy2;
     Ini     =          3;
     Values  =     "7, 8, 9";
     Type    =        SET;
   }
   Parameter{    // offz1
     Name    =    offz1;
     Min     =   1.0;
     Ini     =   1.25;
     Max     =   1.55;
     Step    =   0.1;
   }
   Parameter{    // offz2
     Name    =     offz2;
     Min     =     1.65;
     Ini     =     2.0;
     Max     =     2.2;
     Step    =     0.1;
   }
   Parameter{    // recx1
    Name    =     recx1;
    Ini     =      2;
    Values  =    "2, 3, 4, 5, 6, 7";
    Type    =    SET;
   }
   Parameter{   // recx2
    Name   =	recx2;
    Ini	   =	   2;
    Values =    "8, 9";
    Type   =     SET;
   }
   Parameter{   //recy1
    Name  =     recy1;
    Ini   =         2;
    Values=      "1, 2";
    Type  =       SET;
   } 
   Parameter{  //recy2
     Name   =  recy2;
     Ini    =      2;
     Values =    "3, 4";
     Type   =    SET;
   }
   Parameter{   //recz1
    Name  =     recz1;
    Min   =      1.0;
    Ini   =      1.25;
    Max   =      1.55;
    Step  =      0.1;
   }
   Parameter{   //recz2
     Name  =    recz2;
     Min   =     1.65;
     Ini   =     2.0;
     Max   =     2.2;
     Step  =     0.1;
   }
   Parameter{   //recz3
     Name  =   recz3;
     Min   =     1.0;
     Ini   =     1.25;
     Max   =     1.55;
     Step  =     0.1;
   }
   Parameter{   //recz4
     Name  =   recz4;
     Min   =    1.65;
     Ini   =    2.0;
     Max   =    2.2;
     Step  =    0.1;
   }
}

OptimizationSettings{
  MaxIte =  1000;
  MaxEqualResults =  25;
  WriteStepNumber = true;
}

Algorithm{
  Main = GPSPSOCCHJ;
  NeighborhoodTopology = vonNeumann;
  NeighborhoodSize = 5;
  NumberOfParticle = 5;
  NumberOfGeneration = 30;
  Seed = 1;
  CognitiveAcceleration = 2.8;
  SocialAcceleration = 1.3;
  MaxVelocityGainContinuous = 0.5;
  MaxVelocityDiscrete = 4;
  ConstrictionGain = 0.5;
  MeshSizeDivider = 2;
  InitialMeshSizeExponent = 1;
  MeshSizeExponentIncrement = 1;
  NumberOfStepReduction = 4;
}
