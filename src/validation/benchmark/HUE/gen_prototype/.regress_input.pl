#!/usr/bin/perl
## SAPP 2009 Project File
## Script to generate raw results database

## Open file
open(my $in,  "<",  "@ARGV[0]")  
or die "Can't open results.file: $!" ;

## Initialise output strings 
my @stoutname = ("null","null") ;
my @stoutheat = ("0","1","2","3","4","5","6","7","8","9","10","11") ;
my @stoutcool = ("0","1","2","3","4","5","6","7","8","9","10","11") ;
my $substring = "null" ;

## Initialise counter
my $kounter = 0 ;

## Read file one line at a time
while (<$in>) 
{
  $kounter++ ;
  my $rem = $kounter % 26 - 1 ;
  if ($rem == -1)
  {
    $rem = 25 ;
  }
  my $line = $_ ;
  chomp($line) ; # remove trailing \n
  ## Uncomment following line to get complete line
  #  print "no= $kounter , rem= $rem , ;line= $line \n" ;
  if ($rem <= 1)
  {
    $stoutname[$rem] = $line ;
    if ($rem == 0)
    {
      ## Remove extension from cfg file name
      $stoutname[$rem] = substr ($line , 0 , length($line) - 4) ;
    }
    print "@stoutname " if ($rem == 1) ;
  }
  elsif ($rem <= 13)
  {
    $substring = substr ($line , 24 , 6);
    $stoutheat[$rem-2] = $substring ;
    print "@stoutheat " if ($rem == 13);
  }
  else
  {
    $substring = substr ($line , 24 , 6);
    $stoutcool[$rem-14] = $substring ;
    print "@stoutcool \n" if ($rem == 25);
  }
}
