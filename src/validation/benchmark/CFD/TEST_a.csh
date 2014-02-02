#!/bin/csh -fb
# version only using csh for runs
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
  foreach i (basic.dfd block.dfd block2.dfd block3.dfd blockc.dfd blockh.dfd \
             blocklma.dfd basiclma.dfd co2_so.dfd hum_so.dfd lma.dfd sso.dfd \
             tm1.dfd porous.dfd tm2_so.dfd tst33vol.dfd)
    @ test ++
    echo "Test "$test": "$i" with the default solver."
    cd Models; ./dfs_basic.csh $DFS $i; cd ..
  end
endif
#
# Fast bi-cg solver tests
#
if ( $A == 1 || $A == 3 ) then
  foreach i (basicsol.dfd)
    @ test ++
    echo "Test "$test": "$i" with the bi-cg solver."
    cd Models; ./dfs_bi-cg.csh $DFS $i; cd ..
  end
endif
#
# Fast ggdh solver tests
#
if ( $A == 1 || $A == 3 ) then
  foreach i (displ.dfd)
    @ test ++
    echo "Test "$test": "$i" with the ggdh solver."
    cd Models; ./dfs_ggdh_not_conv.csh $DFS $i; cd ..
  end
endif
#
# Fast not converging tests
#
if ( $A == 1 || $A == 3 ) then
  foreach i (displ.dfd displ_si.dfd tm1_so.dfd)
    @ test ++
    echo "Test "$test": "$i" with the default solver."
    cd Models; ./dfs_not_conv.csh $DFS $i; cd ..
  end
endif
#
# OS dependent tests
#
if ( $A == 1 || $A == 3 ) then
  foreach i (tm1a.dfd tm1b.dfd)
    @ test ++
    echo "Test "$test": "$i" with the default solver."
    cd Models; ./dfs_basic.csh $DFS $i; cd ..
  end
endif
#
# Slow converging tests
#
if ( $A == 2 || $A == 3 ) then
  foreach i (blockt.dfd)
    @ test ++
    echo "Test "$test": "$i" with the default solver."
    cd Models; ./dfs_basic.csh $DFS $i; cd ..
  end
endif
#
# Slow not converging tests
#
if ( $A == 2 || $A == 3 ) then
  foreach i (bi-cg.dfd)
    @ test ++
    echo "Test "$test": "$i" with the default solver."
    cd Models; ./dfs_basic.csh $DFS $i; cd ..
  end
endif
#
# Slow ggdh solver tests
#
if ( $A == 2 || $A == 3 ) then
  foreach i (bi-cg.dfd)
    @ test ++
    echo "Test "$test": "$i" with the bi-cg solver."
    cd Models; ./dfs_bi-cg.csh $DFS $i; cd ..
  end
endif
