#!/bin/csh -fb
#
set OS = "Sun"
echo "Operating system?"
echo " (1) Sun"
echo " (2) Linux"
@ A=0
while ( $A<1 || $A>2 )
set A = $<
end
if ( $A == "2" ) then
  set OS = "Linux"
endif
echo "Use default version of CFD module (Y/N)?"
set A = $<
if ( $A == "Y" || $A == "y" ) then
  set DFS = "dfs"
  echo "Using:"
  which dfs
else
  echo "Search for all possible options (Y/N)?"
  echo "(This can take a few minutes)"
  set A = $<
  if ( $A == "Y" || $A == "y" ) then
    echo "Working... please be patient."
    rm -f modules$$
    find / -name "dfs" -print >& modules$$
    rm -f modules$$-
    grep -v "cannot read dir" modules$$ > modules$$-
    set CHOICE = (`file -f modules$$- | grep executable | cut -d : -f 1`)
    rm -f modules$$ modules$$-
    @ i = 0
    while ($i < $#CHOICE )
      @ i ++
      echo " ("$i")" $CHOICE[$i]
    end
    @ A = 0
    echo "Which version to use?"
    while ( $A < 1 || $A > $#CHOICE )
      @ A = $<
    end
    set DFS = $CHOICE[$A]
    echo $DFS
  else
    echo "Version of CFD module to use (including full path)?"
    set A = $<
    @ OK = `file $A | grep -ic "No such file"`
    if ( $OK == 0 ) then
      set DFS = $A
      echo $DFS
    else
      echo $A " does not exist! "
      echo "...using default: "
      set DFS = "dfs"
      which dfs
    endif
  endif
endif
echo "Which tests?"
echo " (1) Fast (23 simulations; run time: ~15 minutes & <2hrs on a slow computer)"
echo " (2) Slow (3 simulations; run time: 12x time for fast tests)"
echo " (3) All (26 simulations)"
@ A=0
while ( $A<1 || $A>3 )
set A = $<
end
@ test=0
#
# Fast converging tests
#
if ( $A == 1 || $A == 3 ) then
  foreach i (basic block block2 block3 blockc blockh \
             blocklma basiclma co2_so hum_so lma sso \
             tm1 porous tm2_so tst33vol)
    @ test ++
    echo "Test "$test": "$i".dfd with the default solver."
    cd Models; ./dfs_basic.sh $DFS $i; cd ..
  end
endif
#
# Fast bi-cg solver tests
#
if ( $A == 1 || $A == 3 ) then
  foreach i (basicsol)
    @ test ++
    echo "Test "$test": "$i".dfd with the bi-cg solver."
    cd Models; ./dfs_bi-cg.sh $DFS $i; cd ..
  end
endif
#
# Fast ggdh solver tests
#
if ( $A == 1 || $A == 3 ) then
  foreach i (displ)
    @ test ++
    echo "Test "$test": "$i".dfd with the ggdh solver."
    cd Models; ./dfs_ggdh_not_conv.sh $DFS $i; cd ..
  end
endif
#
# Fast not converging tests
#
if ( $A == 1 || $A == 3 ) then
  foreach i (displ displ_si tm1_so)
    @ test ++
    echo "Test "$test": "$i".dfd with the default solver."
    cd Models; ./dfs_not_conv.sh $DFS $i; cd ..
  end
endif
#
# OS dependent tests
#
if ( $A == 1 || $A == 3 ) then
  foreach i (tm1a tm1b)
    @ test ++
    echo "Test "$test": "$i".dfd with the default solver."
    cd Models; ./dfs_basic.sh $DFS $i; cd ..
  end
endif
#
# Slow converging tests
#
if ( $A == 2 || $A == 3 ) then
  foreach i (blockt)
    @ test ++
    echo "Test "$test": "$i".dfd with the default solver."
    cd Models; ./dfs_basic.sh $DFS $i; cd ..
  end
endif
#
# Slow not converging tests
#
if ( $A == 2 || $A == 3 ) then
  foreach i (bi-cg)
    @ test ++
    echo "Test "$test": "$i".dfd with the default solver."
    cd Models; ./dfs_basic.sh $DFS $i; cd ..
  end
endif
#
# Slow ggdh solver tests
#
if ( $A == 2 || $A == 3 ) then
  foreach i (bi-cg)
    @ test ++
    echo "Test "$test": "$i".dfd with the bi-cg solver."
    cd Models; ./dfs_bi-cg.sh $DFS $i; cd ..
  end
endif
