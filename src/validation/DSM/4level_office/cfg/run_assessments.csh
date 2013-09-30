#!/bin/csh
echo 'Runs assessments for each model and generates an IPV'
echo "invoke with the command weekly or seasonal or help"
echo "script needs to be edited to set the correct ecm"
set action = $argv[1]
if ( $action == "weekly" ) then
  echo "about to run weekly asssessments..."
  set ecm = '_bc'
# set ecm = '_bc'
else if ( $action == "seasonal" ) then
  echo "about to run seasonal assessments..."
  set ecm = '_bcX'
# set ecm = '_bcX'
else if ( $action == "help" ) then
  echo "takes the following command options:
  echo " weekly (fast runs which only approximate peak demands)
  echo " seasonal(runs all days in each season - takes longer)
  echo " help (this message)
else
  echo "command was neither weekly or seasonal or help"
  exit 1
endif

# loop through each of the sites...
#  foreach i (bur hou el_ alb dul bal chi boi fai mem mia pho)
  foreach i (bur el_ alb dul bal boi fai mem mia pho)
#  foreach i (hou)
    if( $i == 'bur')then
      set C='Burlington'
      set D='office'$ecm'bur.cfg' 
    endif

#Houston
    if( $i == 'hou')then
      set C='Houston'
      set D='office'$ecm'hou.cfg' 
    endif

#El Paso
    if( $i == 'el_')then
      set C='El_Paso'
      set D='office'$ecm'el_.cfg' 
    endif

#Albuquerque
    if( $i == 'alb')then
      set C='Albuquerque'
      set D='office'$ecm'alb.cfg' 
    endif

#Duluth
    if( $i == 'dul')then
      set C='Duluth'
      set D='office'$ecm'dul.cfg' 
    endif

#Baltimore
    if( $i == 'bal')then
      set C='Baltimore'
      set D='office'$ecm'bal.cfg' 
    endif

#Chicago
    if( $i == 'chi')then
      set C='Chicago'
      set D='office'$ecm'chi.cfg' 
    endif

#Boise
    if( $i == 'boi')then
      set C='Boise'
      set D='office'$ecm'boi.cfg' 
    endif

#Fairbanks
    if( $i == 'fai')then
      set C='Fairbanks'
      set D='office'$ecm'fai.cfg' 
    endif

#Memphis
    if( $i == 'mem')then
      set C='Memphis'
      set D='office'$ecm'mem.cfg' 
    endif

#Miami
    if( $i == 'mia')then
      set C='Miami'
      set D='office'$ecm'mia.cfg' 
    endif

#Phoenix AZ
    if( $i == 'pho')then
      set C='Phoenix'
      set D='office'$ecm'pho.cfg' 
    endif

    set E=$C' assessment'
    set F='office'$ecm$i'*.mfr'
    set G='office'$ecm$i'*.res'
    set H='office'$ecm$i'_win1.res'
    set I='office'$ecm$i'_spr.res'
    set K='office'$ecm$i'_sum.res'
    set L='office'$ecm$i'_aut.res'
    set M='office'$ecm$i'_win2.res'
    echo "running for "$C

rm -f $F
rm -f $G

time prj -mode text -file $D << XYZ
m    # simulate
p    # simulation
t    # do IPV
a    # run require simulations
b    # silent
t    # do IPV
b    # extract reports
b    # silent running
c    # all of them
-
-
-
XYZ

echo $H
echo $I
echo $K
echo $L
echo $M
sleep 10s

./extract $H /opt/esru/esp-r/bin
./extract $I /opt/esru/esp-r/bin
./extract $K /opt/esru/esp-r/bin
./extract $L /opt/esru/esp-r/bin
./extract $M /opt/esru/esp-r/bin

rm -f *.data.par

 set N='office'$ecm$i'_win1.res.data'
 set O='office'$ecm$i'_spr.res.data'
 set P='office'$ecm$i'_sum.res.data'
 set Q='office'$ecm$i'_aut.res.data'
 set R='office'$ecm$i'_win2.res.data'
 set S='office'$ecm'_'$i'.stats'
more $N $O $P $Q $R > $S
echo $N
echo $O
echo $P
echo $Q
echo $R
echo $S

# uncomment the next lines if results files no longer needed.
# rm -f $H
rm -f $I
rm -f $K
rm -f $L
rm -f $M
rm -f *.res.data

  end
endif
