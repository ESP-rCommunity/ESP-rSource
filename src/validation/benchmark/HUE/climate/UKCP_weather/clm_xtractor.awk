# File to extract ESP-r specific climate infromation from UKCIP09
# Weather Generator generated weather files

BEGIN{
  i=0
  j=1
}
{
  if ($1 == 3001){
    if (i%24 == 0){
      if      (j <=  31){k= 1;l=j    }
      else if (j <=  59){k= 2;l=j-31 }
      else if (j <=  90){k= 3;l=j-59 }
      else if (j <= 120){k= 4;l=j-90 }
      else if (j <= 151){k= 5;l=j-120}
      else if (j <= 181){k= 6;l=j-151}
      else if (j <= 212){k= 7;l=j-181}
      else if (j <= 243){k= 8;l=j-212}
      else if (j <= 273){k= 9;l=j-243}
      else if (j <= 304){k=10;l=j-273}
      else if (j <= 334){k=11;l=j-304}
      else              {k=12;l=j-334}
      print "* day "l" month "k;
      j++ ;
    }
    printf ("%3.0f %3.0f %3.0f 0 0 %3.0f\n",$10,$6*10,$11,$8*100) ;
    i++ ;
  }
}
END{
}
