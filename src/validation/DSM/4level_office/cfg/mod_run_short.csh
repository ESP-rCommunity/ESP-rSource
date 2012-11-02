#!/bin/csh
echo 'Script to run office base case weekly assessments'
echo 'and generate IPVs'
echo ' '
echo 'For each weekly configuration file...'
  foreach i (bur hou el_ alb dul bal chi boi fai mem mia pho)
#  foreach i (chi )
#Burlington
    if( $i == 'bur')then
      set C='Burlington'
      set D='office_bcbur.cfg' 
    endif

#Houston
    if( $i == 'hou')then
      set C='Houston'
      set D='office_bchou.cfg' 
    endif

#El Paso
    if( $i == 'el_')then
      set C='El_Paso'
      set D='office_bcel_.cfg' 
    endif

#Albuquerque
    if( $i == 'alb')then
      set C='Albuquerque'
      set D='office_bcalb.cfg' 
    endif

#Duluth
    if( $i == 'dul')then
      set C='Duluth'
      set D='office_bcdul.cfg' 
    endif

#Baltimore
    if( $i == 'bal')then
      set C='Baltimore'
      set D='office_bcbal.cfg' 
    endif

#Chicago
    if( $i == 'chi')then
      set C='Chicago'
      set D='office_bcchi.cfg' 
    endif

#Boise
    if( $i == 'boi')then
      set C='Boise'
      set D='office_bcboi.cfg' 
    endif

#Fairbanks
    if( $i == 'fai')then
      set C='Fairbanks'
      set D='office_bcfai.cfg' 
    endif

#Memphis
    if( $i == 'mem')then
      set C='Memphis'
      set D='office_bcmem.cfg' 
    endif

#Miami
    if( $i == 'mia')then
      set C='Miami'
      set D='office_bcmia.cfg' 
    endif

#Phoenix AZ
    if( $i == 'pho')then
      set C='Phoenix'
      set D='office_bcpho.cfg' 
    endif

    set E='weekly '$C' assessment'
    set F='office_bc_*'$i'.mfr'
    set G='office_bc_*'$i'.res'
    set H='office_bc_win1'$i'.res'
    set I='office_bc_spr'$i'.res'
    set K='office_bc_sum'$i'.res'
    set L='office_bc_aut'$i'.res'
    set M='office_bc_win2'$i'.res'
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

./extract $H ~/esru/esp-r/bin
./extract $I ~/esru/esp-r/bin
./extract $K ~/esru/esp-r/bin
./extract $L ~/esru/esp-r/bin
./extract $M ~/esru/esp-r/bin

rm -f *.data.par

 set N='office_bc_win1'$i'.res.data'
 set O='office_bc_spr'$i'.res.data'
 set P='office_bc_sum'$i'.res.data'
 set Q='office_bc_aut'$i'.res.data'
 set R='office_bc_win2'$i'.res.data'
 set S='office_bc_'$i'.stats'
more $N $O $P $Q $R > $S

# uncomment the next lines if results files no longer needed.
rm -f $H
rm -f $I
rm -f $K
rm -f $L
rm -f $M

  end
endif
