#!/bin/csh
echo 'Script to run office base case seasonal assessments'
echo 'and generate IPVs'
echo ' '
echo 'For each seaspma; configuration file...'
 foreach i (bur hou el_ alb dul bal chi boi fai mem mia pho)
#   foreach i (fai )
#Burlington
    if( $i == 'bur')then
      set C='Burlington'
      set D='office_bcXbur.cfg' 
    endif

#Houston
    if( $i == 'hou')then
      set C='Houston'
      set D='office_bcXhou.cfg' 
    endif

#El Paso
    if( $i == 'el_')then
      set C='El_Paso'
      set D='office_bcXel_.cfg' 
    endif

#Albuquerque
    if( $i == 'alb')then
      set C='Albuquerque'
      set D='office_bcXalb.cfg' 
    endif

#Duluth
    if( $i == 'dul')then
      set C='Duluth'
      set D='office_bcXdul.cfg' 
    endif

#Baltimore
    if( $i == 'bal')then
      set C='Baltimore'
      set D='office_bcXbal.cfg' 
    endif

#Chicago
    if( $i == 'chi')then
      set C='Chicago'
      set D='office_bcXchi.cfg' 
    endif

#Boise
    if( $i == 'boi')then
      set C='Boise'
      set D='office_bcXboi.cfg' 
    endif

#Fairbanks
    if( $i == 'fai')then
      set C='Fairbanks'
      set D='office_bcXfai.cfg' 
    endif

#Memphis
    if( $i == 'mem')then
      set C='Memphis'
      set D='office_bcXmem.cfg' 
    endif

#Miami
    if( $i == 'mia')then
      set C='Miami'
      set D='office_bcXmia.cfg' 
    endif

#Phoenix AZ
    if( $i == 'pho')then
      set C='Phoenix'
      set D='office_bcXpho.cfg' 
    endif

    set E='weekly '$C' assessment'
    set F='office_bcX_*'$i'.mfr'
    set G='office_bcX_*'$i'.res'
    set H='office_bcX_win1'$i'.res'
    set I='office_bcX_spr'$i'.res'
    set K='office_bcX_sum'$i'.res'
    set L='office_bcX_aut'$i'.res'
    set M='office_bcX_win2'$i'.res'
    echo "running seasonal for "$C

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

##./extract $H ~/esru/esp-r/bin
##./extract $I ~/esru/esp-r/bin
##./extract $K ~/esru/esp-r/bin
##./extract $L ~/esru/esp-r/bin
##./extract $M ~/esru/esp-r/bin

rm -f *.data.par

## set N='office_bcX_win1'$i'.res.data'
## set O='office_bcX_spr'$i'.res.data'
## set P='office_bcX_sum'$i'.res.data'
## set Q='office_bcX_aut'$i'.res.data'
## set R='office_bcX_win2'$i'.res.data'
## set S='office_bcX_'$i'.stats'
##more $N $O $P $Q $R > $S

# uncomment the next lines if results files no longer needed.
rm -f $H
rm -f $I
rm -f $K
rm -f $L
rm -f $M

  end
endif
