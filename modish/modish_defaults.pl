# Configuration file for modish, version 0.325.

@defaults = ( [ 2, 2 ], 5, 1, 7, 0.01 );

### The line above means: ( [ resolution_x, resolution_y ], $dirvectorsnum, $bounceambnum, $bouncemaxnum, $distgrid, $threshold )
### resolution_x, resolution_y are the gridding values
### The value "$dirvectorsnum" controls the numbers of direction vectors that are used for
### computing the irradiances at each point of each grid over the surfaces. The values that currently
### can be chosen are 1, 5 and 9. When the points are more than 1, they are evenly distributed
### on a hemisphere following a geodetic pattern.
### $bounceambnum are the number of the bounces of the diffuse light which are taken into account.
# $bouncemaxnum are the number of the bounces of direct light which are taken into account.
# $distgrid is the distance of the grid in meters outside the surfaces which are taken into account.
# TO TAKE INTO ACCOUNT WELL THE REFLECTIONS FROM GROUND, ONE NEEDS MORE THAN ONE DIRECTION VECTORS. AT LEAST 5.


#@calcprocedures = ( "diluted", "gensky", "alldiff", "radical" );
#@calcprocedures = ( "diluted", "gendaylit", "composite", "alldiff" );
# If @calcprocedure is unspedified, the program defaults to:
@calcprocedures = ( "diluted", "gensky", "composite", "groundreflections", "alldiff" ) ;
# The advice is to let @calcprocedures unspecified and get those default settings, or to go with this other setting:
# @calcprocedures = ( "diluted", "gensky", "radical", "alldiff" ), which is for calculating the shading factors from scratch.
# Quick description of the available calculation options:
# The best groups of settings of @calcprocedures for calculating the shading factors are likely to be
# the following ones in most cases:
# 1)
# @calcprocedures = ( "diluted", "gendaylit", "alldiff", "composite" ); ## TO USE THE IRRADIANCE RATIOS BETWEEN A MODEL WITH BLACK OBSTRUCTIONS AND A MODEL WITH REFLECTIVE OBSTRUCTIONS FOR MODIFYING THE SHADING FACTORS CALCULATED BY THE ISH MODULE OF ESP-R. AND ALSO, FOR USING A PEREZ SKY ("gendaylit"). "diluted" ENTAILS THAT THE SURFACES OF THE MODEL ARE REFLECTIVE. THESE SETTINGS ARE THE ONES USED BY DEFAULT, IF NOTHING IS SPECIFIED IN @calcprocedures. TO GET A CIE SKY INSTEAD OF A PEREZ SKY, "gensky" has to be used in place of "gendaylit".
# 2)
# @calcprocedures = ( "diluted", "gendaylit", "alldiff", "noreflections" ); # FOR MODIFYING THE SHADING FACTORS CALCULATED BY THE ISH MODULE SO AS TO TAKE INTO ACCOUNT THE SHADING EFFECT OF OBSTRUCTIONS ON REFLECTIONS FROM THE GROUND.
# 3)
# @calcprocedures = ( "diluted", "gendaylit", "alldiff", "composite", "groundreflections" ); ## LIKE POINT 1, BUT NOT TAKING INTO ACCOUNT THE SHADING EFFECT OF OBSTRUCTIONS ON THE REFLECTIONS FROM THE GROUND.
# 4)
# @calcprocedures = ( "diluted", "gendaylit", "alldiff", "radical" ); ## TO CALCULATE THE SHADING FACTORS FROM SCRATCH, WITHOUT TAKING INTO ACCOUNT THE ONES CALCULATED BY ISH, "DIRECTING" THE CONSEQUENCES OF ALL THE VARIATIONS FROM THE EXPECTED DIRECT SHADING FACTORS DUE TO DIRECT UNREFLECTED RADIATION INTO THE DIFFUSE SHADING FACTORS.
# 5)
# @calcprocedures = ( "diluted", "gendaylit", "radical" ); ## TO CALCULATE THE SHADING FACTORS FROM SCRATCH, WITHOUT TAKING INTO ACCOUNT THE ONES CALCULATED BY ISH, "DIRECTING" THE CONSEQUENCES OF ALL THE VARIATIONS FROM THE EXPECTED DIRECT SHADING FACTORS DUE TO DIRECT UNREFLECTED RADIATION INTO THE DIRECT SHADING FACTORS.
# 6) 
# @calcprocedures = ( "diluted", "getweather", "getsimple", "alldiff", "composite" ); # LIKE POINT 1, BUT ON THE BASIS OF PEREZ SKIES.
# 7) 
# @calcprocedures = ( "diluted", "getweather", "getsimple", "alldiff", "composite" ); # LIKE POINT 1, BUT ON THE BASIS OF PEREZ SKIES CALCULATED FROM THE AVERAGE OF WEATHER DATA. TAKE CARE.
# Explanations follow.
# "diluted" means that the two models from which the shading ratios are derived
# are going to be the following: 
# 1)
# a) a model in which all the surfaces are reflective,
# excepted the obstructions, which are black;
# b) a model in which everything is reflective.
# 2) 
# if "complete" is specified, the two models from which the shading ratios 
# are derived are going to be the following:
# a) a model in which everything is black, and
# b) a model in which all the surfaces are black, excepted the obstructions,
# which are reflective. The settings "diluted" and "complete" are alternatives.
# With "aldiff" the program "directs" the consequences of all the variations from the expected direct shading factors due to direct unreflected radiation into the difffuse shading factors. The lack of "alldiff" let them be sent into the direct shading factors.
# The settings "plain" with "alldiff" activates the simplest and most robust calculation method, with which the diffuse irradiance ratios are calculated on the basis of the total irradiances. This method is very cautious and systematically slightly overestimates shading factors and, by consequence, underestimates solar gains. Its main utility is as a benchmark for the other methods. It the shading factors produced by another method are lower than the shading factors produced by this method, there might be something wrong with the method or in the scene model.
# If "gensky" is specified, the irradiances are calculated using the gensky program 
# of Radiance, entailing the use of the CIE standard skies, for both the diffuse and direct
# calculations, and the result is sensible to the setting of sky condition for each month (below: 
# clear, cloudy, or overcast).
# If "gendaylit" is specified, the irradiances are calculated using the gendaylit program 
# of Radiance, entailing the use of the Perez sky model for the diffuse
# calculations and the direct ones. If the "getweather" setting is not specified, 
# the direct calculations are performed by the means of gensky. If "getweather" is specified,
# the both the direct and the diffuse calculations are used with gendaylit by the means
# of averages of the weather data about direct normal and horizontal diffuse irradiances.
# For the setting "gendaylit" to work, it has to be specified together with the "altdiff" setting.
# With the setting "keepdirshdf", only the diffuse shading factors get recalculated.
# The setting "getweather" used with "gendaylit" ("it can't be used without it) 
# makes possible that the average radiation values of the weather data are utilized 
# when calling gendaylit. 
# The option "getsimple" used with "getweather" (it can't be used without it) 
# determines the fact that the proportion of direct to diffuse radiation 
# is determined directly from the shading data and overriding the other methods
# for defining that ratio.
# The materials used in the obstructions should be not shared
# by objects which are not obstructions. If necessary, to obtain that,
# some materials may have to be suitably duplicated and renamed.
# The setting "alldiff", "plain" is a simplified version of "aldiff"
# with a slighly quicker and rougher manner of separating diffuse radiation
# from the total one. It computer simply direct radiation and diffuse radiation 
# together as regards the diffuse shading factors. This setting
# is cautious and tends to underestimate the diffuse shading factors.
# By specifying in @calcprocedure items of the kind "light/infrared-ratio:materialname:ratio"
# (for example: "light/infrared-ratio:gypsum:1.2" ) it is possible to model
# obstruction material which are selective in reflection - i.e. having different
# reflectivities in the range of light and solar infrared.
# The order in which the settings in @calcprocedures are specified is not meaningful for the program.

@specularratios = (  );

#@specularratios = ( "reflector:mirror" );
#@specularratios = ( "reflector:0.03:0.05" );
# Here values of the kind "construction:specularratio:roughnessvalue"
# may be specified. For example, "reflector:0.03:0.05".
# The textual element ("reflector") is the name
# of a construction. The first number is the specular ratio
# for that construction. The second number is the roughness value.
# Specifying those values here makes possible
# to override the values specified in a Radiance database.
# (for example, the "0"s that may be in the database
# by defaul as regards specular ratios and roughness values).
# As an alternative, a material can be declared to be of the "mirror" type.
# This is done by specifying a value "construction:mirror".
# For example: reflector:mirror (see Radiance documentation
# about the properties of the "mirror" material type).

%skycondition = ( 1=> "clear", 2=> "clear", 3=> "clear", 4=> "clear", 5=> "clear", 6=> "clear", 7=> "clear", 8=> "clear", 9=> "clear", 10=> "clear", 11=> "clear", 12=> "clear" );
# PREVAILING CONDITION OF THE SKY FOR EACH MONTH, EXPRESSED WITH ITS NUMBER, IN THE CASE IN WHICH
# CIE SKIES (WITH GENSKY) ARE UTILIZED.
# THE OPTIONS ARE: "clear", "cloudy" and "overcast".
# IF NO VALUE IS SPECIFIED, THE DEFAULT IS "clear".

$parproc = 6; # NUMBER OF PARALLEL PROCESSORS. NOT MANDATORY. FOR PARALLEL CALCULATIONS IN RADIANCE.

#@boundbox = ( -20, 40, -20, 40, -20, 40 );
# THE BOUNDING BOX OF THE RADIANCE SCENE.

$add = " -ad 512 -aa 0.5 -dc .25 -dr 2 -ss 1 -st .05 -ds .04 -dt .02 -bv ";
# THIS ADDS OPTIONS TO THE CALLS TO RADIANCE (SEE "man rtrace"), MODIFYING THE DEFAULTS GIVEN BY ESP-r.
# IF THE -ad OPTION IS NOT SPECIFIED, IT DEFAULTS TO 1024. IF ONE ARE TAKING INTO ACCOUNT MORE THAN 1 DIFFUSE BOUNCE, IT IS BETTER TO REDUCE IT (FOR EXAMPLE, TO 512) FOR NOT SLOWING DOWN TOO MUCH THE COMPUTATIONS. -ad 512



