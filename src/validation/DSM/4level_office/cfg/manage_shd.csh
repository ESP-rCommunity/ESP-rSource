#!/bin/csh
echo "invoke with the command make or clear or qa or help"
echo "script needs to be edited to set the correct ecm"
set action = $argv[1]
if ( $action == "make" ) then
  echo "about to create shading files for the model..."
else if ( $action == "clear" ) then
  echo "about to replace the current shading files with placeholders..."
else if ( $action == "qa" ) then
  echo "about to generate a QA report for each model..."
else if ( $action == "ipv" ) then
  echo "about to freshen each model after change in ipv file..."
else if ( $action == "help" ) then
  echo "takes the following command options:
  echo " make (to make the shading files for all of the models)
  echo " clear (to clear current shading files and replace with a placeholder file)
  echo " help (this message)
else
  echo "command was neither make or clear"
  exit 1
endif

# comment out one or these to get the correct variant
set ecm = '_bc'
# set ecm = '_bcX'

foreach i (alb bur hou el_ dul bal chi boi fai mem mia pho)
#  foreach i (alb )
#Burlington
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
    
    if ( $action == "make" ) then
      echo "about to create shading files for the model..."
      echo "running for "$C

prj -mode text -file $D << XYZ
m
c
f
*
a
a
-
-
-
XYZ
    else if ( $action == "qa" ) then

prj -mode text -file $D << XYZ
m
r
g
*
-
>

!
-
-
-
XYZ
    else if ( $action == "clear" ) then
      foreach j ( office_2_sth office_e_0 office_e_1 office_e2e office_e_3 office_w_0 office_w_1 office_w2w office_w_3 )
        set F='../zones/'$j$i'.shd'
        echo "found shading for "$F
        echo placeholder > $F
      end
    else if ( $action == "ipv" ) then

prj -mode text -file $D << XYZ
m
b
o

b

-

-
-
-
XYZ
      echo "about to freshen each model after change in ipv file..."
    endif
  end
endif
