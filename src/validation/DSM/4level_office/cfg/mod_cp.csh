#!/bin/csh
echo 'Script to copy *.cfg files changing climate, ground temperature'
echo 'profiles and integrated performance view'
echo ' '
echo 'Enter base case configuration file name'
set A=$<
  foreach i (bur hou el_ alb dul bal chi boi fai mem mia pho)
#  foreach i (bur )
#Burlington
    if( $i == 'bur')then
      set C='Burlington'
      set D='i' 
      set E='44.47'
      set F='1.85'
      set G='2001'
      set H='16.0 16.0 16.05 16.3 17.38 19.4 '
      set I='22.1 20.5 17.34 16.15 16.02 16.0 '
      set P=1
      echo "running for "$C
    endif

#Houston
    if( $i == 'hou')then
      set C='Houston'
      set D='w' 
      set E='29.98'
      set F='5.37'
      set G='2001'
      set H='16.8 16.8 17.3 18.34 20.5 23.98 '
      set I='25.64 24.38 21.85 19.1 17.65 16.67'
      echo "running for "$C
      set P=2
    endif

#El Paso
    if( $i == 'el_')then
      set C='El_Paso'
      set D='p' 
      set E='31.80'
      set F='1.4'
      set G='2001'
      set H='16.25 16.58 17.09 18.05 20.21 24.7 '
      set I='25.9 23.94 21.1 18.4 16.7 16.2'
      echo "running for "$C
      set P=1
    endif

#Albuquerque
    if( $i == 'alb')then
      set C='Albuquerque'
      set D='b' 
      set E='35.05'
      set F='1.62'
      set G='2001'
      set H='16.07 16.17 16.29 16.87 18.9 22.2 '
      set I='24.6 22.66 19.68 17.1 16.2 16.0'
      echo "running for "$C
      set P=1
    endif

#Duluth
    if( $i == 'dul')then
      set C='Duluth'
      set D='o' 
      set E='46.83'
      set F='2.18'
      set G='2001'
      set H='16.0 16.0 16.0 16.04 16.29 18.3 '
      set I='20.99 18.97 16.54 16.1 16.0 16.0'
      echo "running for "$C
      set P=1
    endif

#Baltimore
    if( $i == 'bal')then
      set C='Baltimore'
      set D='c' 
      set E='39.18'
      set F='1.67'
      set G='2001'
      set H='16.0 16.0 16.2 16.37 17.5 21.9 '
      set I='23.9 22.9 19.2 17.1 16.2 16.0'
      echo "running for "$C
      set P=1
    endif

#Chicago
    if( $i == 'chi')then
      set C='Chicago'
      set D='k' 
      set E='41.78'
      set F='2.25'
      set G='2001'
      set H='16.0 16.0 16.05 16.2 17.5 21.8 '
      set I='24.1 22.22 18.99 16.6 16.1 16.0'
      set P=1
      echo "running for "$C
    endif

#Boise
    if( $i == 'boi')then
      set C='Boise'
      set D='g' 
      set E='43.57'
      set F='11.22'
      set G='2001'
      set H='16.0 16.0 16.05 16.3 17.5 20.1 '
      set I='24.3 21.7 18.4 16.6 16.1 16.0'
      echo "running for "$C
      set P=1
    endif

#Fairbanks
    if( $i == 'fai')then
      set C='Fairbanks'
      set D='r' 
      set E='64.82'
      set F='12.87'
      set G='2001'
      set H='16.0 16.0 16.0 16.01 16.2 18.3 '
      set I='20.01 17.5 16.08 16.6 16.0 16.0'
      echo "running for "$C
      set P=2
    endif

#Memphis
    if( $i == 'mem')then
      set C='Memphis'
      set D='a' 
      set E='35.05'
      set F='0.02'
      set G='2001'
      set H='16.15 16.08 16.56 17.39 19.8 23.5 '
      set I='25.2 24.15 20.64 17.93 16.45 16.1'
      echo "running for "$C
      set P=2
    endif

#Miami
    if( $i == 'mia')then
      set C='Miami'
      set D='b' 
      set E='25.80'
      set F='5.27'
      set G='2001'
      set H='18.15 18.47 18.69 19.82 21.18 24.01 '
      set I='25.29 24.43 22.4 20.34 19.4 18.1    '
      echo "running for "$C
      set P=2
    endif

#Phoenix AZ
    if( $i == 'pho')then
      set C='Phoenix'
      set D='h' 
      set E='33.43'
      set F='7.02'
      set G='2001'
      set H='16.5 16.7 18.3 19.5 21.9 27.1 '
      set I='28.5 27.28 24.4 20.03 17.92 16.6 '
      echo "running for "$C
      set P=3
    endif


time prj -mode text -file $A << XYZ
j 
a 
$i
office building for $C 
f
b
0
c
$P
$D
a
n
n
$E $F
n
$G
y
l
a
1
$H 
$I 
y
q
office building for $C
version with weekly data
weekly data for $C
3
c
a
5
b
17.0
21.0
a
1. 1. 1.
a
a
a
a
-
 
n
a
-
XYZ
  end
endif


# Climate list appears as under, this should be updated whenever file
# esp-r/climate/climatelist is updated
# 	page	option (variables P and D in the above script)
#bur 	1	i
#hou 	2	w
#el_ 	1	p
#alb 	1	b
#dul 	1	o
#bal 	1	c
#chi 	1	k
#boi 	1	g
#fai	2	r
#mem	2	a
#mia	2	b
#pho	3	h
