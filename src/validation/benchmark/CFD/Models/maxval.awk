BEGIN{print "max           average      record"}
{
if (NR == 1) 
  {
  for (i=1;i<=NF;i++) 
    {
    max[i]=$i
    average[i]=$i
    record[i]=NR
    }
  }
else
  {
  for (i=1;i<=NF;i++) 
    {
    if ($i>max[i])
      {
      max[i]=$i
      average[i]=average[i]+$i
      record[i]=NR
      }
    }
  }
}
END{for (i=1;i<=NF;i++) printf("%e  %e %6d\n", max[i], average[i]/NR, record[i]+2)}
  
