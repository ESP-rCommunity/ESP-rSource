case 195:

- geometry - as specified
- Controls - heating and cooling power 10kW (unlimited); heating and cooling
  setpoints: 20degC (table 1-13)
- Location details OK: longitude is 104.9, therefore longitude difference
  from meridian of 105degW is +0.1
- Exposure set for isolated rural site (index 6) - note that DMU files
  were set at typical rural site (index 3) although report specified
  that it was isolated unobstructed site.
- Infiltration all zero
- Internal gains all zero
- Constructions (checked against table 1.15)
   - emissiv. and absorpt for int and ext surfaces all 0.1 (table 1-13)				
- Viewfactors calculated explicitly

Results (all energy in kWh)
-----------
timesteps    	 heating energy (hrs)   cooling energy(hrs)
------------------------------------------------
 1 ts/hour		4195 (6775)		435 (1985)
 4 ts/hour		4165 (6806)		416 (1955)
 6 ts/hour		4161 (6804)		414 (1956)
 4 ts/hour(save 4)	4165 (6806)		416 (1955)
 DM (4ts/hour)		4167			414

** Therefore all simulations to be done at 4 timesteps/hour:


Date:		heating energy    cooling energy (all energy in kWh)
------------------------------------------------
 28/7/04	4165			416


Results (all loads in kW)
-----------
timesteps       Peak heating load               Peak cooling load
---------------------------------------------------------------------------
 4 ts/hour	 2.01 (4 Jan@01h52)     	0.66 (26 Jul@15h07)


Sensitivity
----------- 
heating energy  diff   cooling energy  diff	simulation
--------------------------------------------------------
  4155		-10	423		+7 	4 ts/hour; exposure type 3
  4165		 0	416		 0	non-explicit viewfactors (default)



Diff 200-195
Date:		heating energy    cooling energy
------------------------------------------------
 28/7/04	 1085		 	 158
 DM (1993)	 1085			 156


Diff 200-195
Date:	       Peak Heating          Peak cooling
--------------------------------------------------------
 28/7/04	  0.67		 	  0.21
 DM (1993)	  0.647			  0.212


