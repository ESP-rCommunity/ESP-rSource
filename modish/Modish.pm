#package Sim::OPT::Modish;
#NOTE: TO USE THE PROGRAM AS A SCRIPT, THE LINE ABOVE SHOULD BE ERASED OR TURNED INTO A COMMENT.
#!/usr/bin/perl
# Modish
$VERSION = '0.4';
# Author: Gian Luca Brunetti, Politecnico di Milano - gianluca.brunetti@polimi.it.
# An intermediate version of the subroutine createconstrdbfile has been modified by ESRU (2038),
# University of Strathclyde, Glasgow.
# All rights reserved, 2015-22.
# This is free software.  You can redistribute it and/or modify it under the terms of the
# GNU General Public License, version 3, as published by the Free Software Foundation.

# In version 0.287: added the possibility to decouple the diffuse resolution from the direct resolution.
# In version 0.289 (13.03.2020): added the ability to work with ESP-r versions more recent than 13.3.2. Last version tested: 13.3.8.
# In version 0.291 (03.08.2020): added the ability to work with ESP-r versions 13.3.10;
# In version 0.301 (04.08.2020): added the ability of being called from ESP-r for monthly inquiries or embedded, daily inquiries.
# In version 0.319 (02.09.2020): speedup modifications for the embedded mode, modification of the createconstrdbfile and creatematdbfiles subroutines.
# In versions 0.321 to 0.325 (17.10.2020): bug fixes.
# In versions 0.4 (20.12.2021): adapted code to changes in the e2r interaction;
# reintroduced the possibility of non-embedded use; added the possibility of choosing which zones and surfaces to operate on.

use v5.14;
use Exporter;
use vars qw( $VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS );
use Math::Trig;
use List::Util qw[ min max reduce shuffle any];
use List::MoreUtils qw(uniq);
use List::AllUtils qw(sum);
use Statistics::Basic qw(:all);
use Data::Dump qw(dump);
use Regexp::Common;
use Vector::Object3D::Polygon;
use Math::Polygon::Tree;
use Storable qw(store retrieve dclone);
use feature 'say';
no strict;
no warnings;

our @ISA = qw(Exporter);
our @EXPORT = qw( modish );

$ABSTRACT = 'Modish is a program for modifying the shading factors in the ISH (shading and insolation) files of the ESP-r building performance simulation suite in order to make it take into account the reflections from obstructions.';

# Modish is a program for modifying the shading factors in the ISH (shading and insolation) files of the ESP-r building performance simulation suite in order to make it take into account the solar reflections from obstructions.
# The program, more precisely, brings into account the reflective effect of solar obstructions on solar gains in the ESP-r building models on the basis of irradiance ratios. Those ratios are obtained combining the direct radiation on a surface, calculated by the means of ESP-r and by the means of a raytracer (Radiance), and the total radiation on the same surface calculated by the means of the raytracer. Using proportions, the values of the total radiation to be input to ESP-r, and from it, the modifications to the shading coefficients needed to obtain that, are calculated.
#
# How the program works
# The effect of solar reflections is taken into account at each hour on the basis of the ratios between the irradiances measured at the models' surfaces in two transitional, fictious model derived from the primary model.
# The procedure in question has to variants, at the user's choice.

# In the first variant of the procedure, the irradiances calculated by the means of Radiance can be derived from the following two alternative sets of models (the choice between them has to be done in the configuration file "modish_defaults.pl"):
#
# 1)
# a) a model in which all the surfaces are reflective, excepted the obstructions, which are black;
# b) a model in which everything is reflective.
#
# 2)
# a) a model in which everything is black;
# b) a model in which all the surfaces are black, excepted the obstructions, which are reflective.
#
# The value given by 1 minus the irradiance ratios gives the diffuse shading factors that are put in the ISH file of the ESP-r model in place of the original values. Solution 1) is the most correct one, but there are cases in which the results of the two solutions converge.
# The value given by 1 minus the irradiance ratios gives the diffuse shading factors that are put in the ISH file of the ESP-r model in place of the original values. Solution 1) is the most correct one, but there are cases in which the results of the two solutions converge.

# In the other variant of the procedure, the irradiances calculated by the means of Radiance can be derived from the following two alternative sets of models:
# 3)
# a) a model in which all the surfaces are reflective, and the obstruction are absent;
# b) a model in which everything is reflective, and the obstructions are present.
# In the case of this variant, the "new" shading factors are not calculated as a correction of the original ESP-r's ISH shading factors, but with the only means of Radiance, again, on the basis of irradiance ratios.
#
# Variant 3) can be combined with variant 1) OR 2).

# The original ISH's ".shda" files are not substituted. Two new files are added in the "zone" folder of the ESP-r model: the ".mod.shda" file is usable by ESP-r. It features the newly calculated shading factors; the ".report.shda" file lists the original shading factors and, at the bottom, the irradiance ratios from which the new shading factors in the ".mod.shda" file have been derived. Note that when the radiation on a surface is increased, instead of decreased, as an effect of reflections on obstructions, the shading factor are negative.
#
# To launch Modish the following command has to be issued:
#
# perl ./Modish.pm PATH_TO_THE_ESP-r_CONFIGURATION_FILE.cfg
#
# For example:
#
# perl ./Modish.pm PATH_TO_THE_ESP-r_CONFIGURATION_FILE.cfg

# More manners for launching ESP-r are presented in the help at the bottom of this page.
#
# In calculating the irradiance ratios, the program defaults to the following settings: diffuse reflections: 1 ; direct reflections: 7; surface grid: 2 x 2; direction vectors for each surface: 1 ; distance from the surface for calculating the irradiances: 0.01 (metres); ratio of the of the original shading factor to the "new" shading factor under which the new shading factor is used to substitute the original one in the ".shda" file. If this value is 0, it is inactive, there is no threshold.
# These defaults are a compromise between quality and speed. They can be overridden by preparing a "modish_defaults.pl" file and placing it in the same directory from which modish is called.
#
# The content of a configuration file for the present defaults, for example, would be constituted by the following line (note that after it a line of comment follows):
#
# @defaults = ( [  2, 2 ], 1, 1, 7, 0.01, 1 );### i.e ( [ resolution_x, resolution_y ], $dirvectorsnum, $bounceambnum, $bouncemaxnum, $distgrid )
#
# The value "$dirvectorsnum" controls the numbers of direction vectors that are used for computing the irradiances at each point of each grid over the surfaces. The values that currently can be chosen are 1, 5 and 17. When the points are more than 1, they are evenly distributed on a hemisphere following a precalculated geodetic pattern.
# Modish works with no limitation of number of surfaces, it can be used with complex geometries and takes into account parent and child surfaces.
#
# For the program to work correctly, the materials, construction and optics databases
# must be local to the ESP-r model.
# Note that in the present version of the program, the materials used for obstructions should be not shared
# by objects which are not reflective obstructions. If it is, that material should be duplicated with a different name in the ESP-r material database and be given a different name, so that one variant of the material can be assigned to the obstructions, and another to the zones surfaces.
#
# The value @calcprocedures in a configuration file controls the algorithmic variant that is used in the calculations.
# If @calcprocedures is set to "diluted", the algorithmic strategy 1 listed above will be used.
# @calcprocedures = ( "diluted" );
# Otherwise, the algorithmic strategy 2 will be used.
# If @calcprocedures is set to "radical", strategy 3 will be used. Together with the option "radical", the options "diluted" or "complete" can be specified.
# If calcprocedures is set to "keepdirshdf", the direct shading factors calculated by ESP-r's ISH will be kept.
# Experience suggests that the most appropriate settings are:
# @calcprocedures = ( "diluted", "radical" );
# and
# @calcprocedures = ( "diluted" );
#
# In a configuration file, values of specular ratio and rougnness can be specified
# ovverriding the values specified in the Radiance material database.
# To obtain that, values of the kind "construction:specularratio:roughnessvalue"
# should be specified in the configuration file, with, for example:
# < @specularratios = ( "polishedmetal:0.03:0.05" );
# like shown in the example configuration case listed at the bottom of this writing.
# This will enable the program to take into account the specular reflective components.
#
# Another manner for specifying a complete (ideal) specular reflection for a material
# in a configuration file, of the kind that can be obtained by settings
# @specularratios = ( "constructiontype:1:0" );
# is that of giving to the material the "mirror" property, in the following manner:
# @specularratios = ( "constructiontype:mirror" );
#
# Considerations on the efficiency of the program.
# The speed of the program largely depends on the number of times that the Radiance raytracer is called, which in turn depends on the resolution of the grid on the external surface which is being considered.
#
# One drawback of the procedure in question from the viewpoint of execution speed may seem to be that the number of calls to the raytracer is double the number of the grid points defined on the considered external surface(s) for taking into account the solar reflections from obstructions. But another implication of this strategy is that it makes possible to decouple the gridding resolution on the considered external surface(s) regarding the effect of direct and diffuse reflections from obstruction from those on: (a) the considered external surface(s), for what is aimed to calculating direct radiation; (b) the internal surfaces, as regards the insolation. This makes possible to adopt a low gridding resolution for the considered external surface(s) relative to the diffuse and specular solar reflections from obstructions while adopting a higher resolution for (a) and (b). Which entails that the calculations regarding the direct radiation, which are likely to be the most important quantitatively for determining the solar gains in the thermal zones, and which are much quicker to calculate than the ones performed by the raytracer (which are necessary for determining the amount of solar radiation reflected from obstructions) can be carried out with a higher resolution than those involved in the calculations of the raytracer, so as to avoid to slow down the calculations themselves by a considerable amount. The amount of computations spared in the described manner may be significant, because the gridding entailed in the calculations not requiring the raytracer is commonly in the order of tens (for example, 20 x 20), whilst a gridding suitable for the use of a raytracer in this kind of operation is commonly in the order of units (for example, 2 x 2).
#
# The alternative to this strategy would be that of calculating all the solar radiation explicitly by defining one only gridding density for each surface; one only for all the radiation components entailed: the direct one, the diffuse one, and the one (diffuse and specular) reflected from obstruction. But this would require a gridding resolution of compromise between the components. For this reason, the calculation efficiency of the Modish procedure is likely to be most of the times not lower, but rather higher, than the alternative one entirely relying on calls to a raytracer.
#
# Modish should work with Linux and the Mac.
#
#
######### EXAMPLE OF CONFIGURATION FILE TO BE NAMED "modish_defaults.pl", ###################
######### TO BE PLACED IN THE DIRECTORY FROM WHICH MODISH IS CALLED, ########################
######### TOGETHER WITH "fix.pl" AND "perlfix.pl". ##########################################
######### THE FIRST "#" DIGIT OF EACH LINE MUST BE UNCHECKED FOR IT TO WORK. ################



# Configuration file for modish, version 0.241.
#
#@defaults = ( [ 2, 2 ], 1, 1, 7, 0.01 );
#
### The line above means: ( [ resolution_x, resolution_y ], $dirvectorsnum, $bounceambnum, $bouncemaxnum, $distgrid, $threshold )
### resolution_x, resolution_y are the gridding values
### The value "$dirvectorsnum" controls the numbers of direction vectors that are used for
### computing the irradiances at each point of each grid over the surfaces. The values that currently
### can be chosen are 1, 5 and 9. When the points are more than 1, they are evenly distributed
### on a hemisphere following a geodetic pattern.
### $bounceambnum are the number of the bounces of the diffuse light which are taken into account.
### $bouncemaxnum are the number of the bounces of direct light which are taken into account.
### $distgrid is the distance of the grid in meters outside the surfaces which are taken into account.
### TO TAKE INTO ACCOUNT WELL THE REFLECTIONS FROM GROUND, ONE NEEDS MORE THAN ONE DIRECTION VECTORS. AT LEAST 5.

###IT IS ALSO POSSIBLE TO DECOUPLE THE RESOLUTION FOR THE DIFFUSE AND TOTAL RADIATION CALCULATIONS FROM THE RESOLUTION
###FOR THE DIRECT RADIATION CALCULATIONS. TO DO THAT, THE @defaults SHOULD BE SPECIFIED LIKE HERE BELOW:
###@defaults = ( [ [ 2, 2 ], [ 10, 10 ]  ], 1, 1, 7, 0.01 );
###THE LINE ABOVE MEANS: ( [ [ diffuse_resolution_x, diffuse_resolution_y ], [ direct_resolution_x, direct_resolution_y ] ], ETC.
#
##@calcprocedures = ( "diluted", "gensky", "alldiff", "radical" );
#@calcprocedures = ( "diluted", "gendaylit", "composite", "alldiff" );
## If @calcprocedure is unspedified, the program defaults to:
## @calcprocedures = ( "diluted", "gensky", "composite", "alldiff" );
## The advice is to let @calcprocedures unspecified and get those default settings, or to go with this other setting:
## @calcprocedures = ( "diluted", "gensky", "radical", "alldiff" ), which is for calculating the shading factors from scratch.
## Quick description of the available calculation options:
## The best groups of settings of @calcprocedures for calculating the shading factors are likely to be
## the following ones in most cases:
## 1)
## @calcprocedures = ( "diluted", "gendaylit", "alldiff", "composite" ); #### TO USE THE IRRADIANCE RATIOS BETWEEN A MODEL WITH BLACK OBSTRUCTIONS AND A MODEL WITH REFLECTIVE OBSTRUCTIONS FOR MODIFYING THE SHADING FACTORS CALCULATED BY THE ISH MODULE OF ESP-R. AND ALSO, FOR USING A PEREZ SKY ("gendaylit"). "diluted" ENTAILS THAT THE SURFACES OF THE MODEL ARE REFLECTIVE. THESE SETTINGS ARE THE ONES USED BY DEFAULT, IF NOTHING IS SPECIFIED IN @calcprocedures. TO GET A CIE SKY INSTEAD OF A PEREZ SKY, "gensky" has to be used in place of "gendaylit".
## 2)
## @calcprocedures = ( "diluted", "gendaylit", "alldiff", "noreflections" ); ## FOR MODIFYING THE SHADING FACTORS CALCULATED BY THE ISH MODULE SO AS TO TAKE INTO ACCOUNT THE SHADING EFFECT OF OBSTRUCTIONS ON REFLECTIONS FROM THE GROUND.
## 3)
## @calcprocedures = ( "diluted", "gendaylit", "alldiff", "composite", "groundreflections" ); #### LIKE POINT 1, BUT NOT TAKING INTO ACCOUNT THE SHADING EFFECT OF OBSTRUCTIONS ON THE REFLECTIONS FROM THE GROUND.
## 4)
## @calcprocedures = ( "diluted", "gendaylit", "alldiff", "radical" ); #### TO CALCULATE THE SHADING FACTORS FROM SCRATCH, WITHOUT TAKING INTO ACCOUNT THE ONES CALCULATED BY ISH, "DIRECTING" THE CONSEQUENCES OF ALL THE VARIATIONS FROM THE EXPECTED DIRECT SHADING FACTORS DUE TO DIRECT UNREFLECTED RADIATION INTO THE DIFFUSE SHADING FACTORS.
## 5)
## @calcprocedures = ( "diluted", "gendaylit", "radical" ); ### TO CALCULATE THE SHADING FACTORS FROM SCRATCH, WITHOUT TAKING INTO ACCOUNT THE ONES CALCULATED BY ISH, "DIRECTING" THE CONSEQUENCES OF ALL THE VARIATIONS FROM THE EXPECTED DIRECT SHADING FACTORS DUE TO DIRECT UNREFLECTED RADIATION INTO THE DIRECT SHADING FACTORS.
## 6)
## @calcprocedures = ( "diluted", "getweather", "getsimple", "alldiff", "composite" ); ## LIKE POINT 1, BUT ON THE BASIS OF PEREZ SKIES.
## 7)
## @calcprocedures = ( "diluted", "getweather", "getsimple", "alldiff", "composite" ); ## LIKE POINT 1, BUT ON THE BASIS OF PEREZ SKIES CALCULATED FROM THE AVERAGE OF WEATHER DATA. TAKE CARE.
## Explanations follow.
## "diluted" means that the two models from which the shading ratios are derived
## are going to be the following:
## 1)
## a) a model in which all the surfaces are reflective,
## excepted the obstructions, which are black;
## b) a model in which everything is reflective.
## 2)
## if "complete" is specified, the two models from which the shading ratios
## are derived are going to be the following:
## a) a model in which everything is black, and
## b) a model in which all the surfaces are black, excepted the obstructions,
## which are reflective. The settings "diluted" and "complete" are alternatives.
## With "aldiff" the program "directs" the consequences of all the variations from the expected direct shading factors due to direct unreflected radiation into the difffuse shading factors. The lack of "alldiff" let them be sent into the direct shading factors.
## The settings "plain" with "alldiff" activates the simplest and most robust calculation method, with which the diffuse irradiance ratios are calculated on the basis of the total irradiances. This method is very cautious and systematically slightly overestimates shading factors and, by consequence, underestimates solar gains. Its main utility is as a benchmark for the other methods. It the shading factors produced by another method are lower than the shading factors produced by this method, there might be something wrong with the method or in the scene model.
## If "gensky" is specified, the irradiances are calculated using the gensky program
## of Radiance, entailing the use of the CIE standard skies, for both the diffuse and direct
## calculations, and the result is sensible to the setting of sky condition for each month (below:
## clear, cloudy, or overcast).
## If "gendaylit" is specified, the irradiances are calculated using the gendaylit program
## of Radiance, entailing the use of the Perez sky model for the diffuse
## calculations and the direct ones. If the "getweather" setting is not specified,
## the direct calculations are performed by the means of gensky. If "getweather" is specified,
## the both the direct and the diffuse calculations are used with gendaylit by the means
## of averages of the weather data about direct normal and horizontal diffuse irradiances.
## For the setting "gendaylit" to work, it has to be specified together with the "altcalcdiff" setting.
## The setting "getweather" used with "gendaylit" ("it can't be used without it)
## makes possible that the average radiation values of the weather data are utilized
## when calling gendaylit.
## The option "getsimple" used with "getweather" (it can't be used without it)
## determines the fact that the proportion of direct to diffuse radiation
## is determined directly from the shading data and overriding the other methods
## for defining that ratio.
## The materials used in the obstructions should be not shared
## by objects which are not obstructions. If necessary, to obtain that,
## some materials may have to be suitably duplicated and renamed.
## The setting "alldiff", "plain" is a simplified version of "aldiff"
## with a slighly quicker and rougher manner of separating diffuse radiation
## from the total one. It computer simply direct radiation and diffuse radiation
## together as regards the diffuse shading factors. This setting
## is cautious and tends to underestimate the diffuse shading factors.
## By specifying in @calcprocedure items of the kind "light/infrared-ratio:materialname:ratio"
## (for example: "light/infrared-ratio:gypsum:1.2" ) it is possible to model
## obstruction material which are selective in reflection - i.e. having different
## reflectivities in the range of light and solar infrared.
## The order in which the settings in @calcprocedures are specified is not meaningful for the program.
#
#@specularratios = (  );
#
##@specularratios = ( "reflector:mirror" );
##@specularratios = ( "reflector:0.03:0.05" );
## Here values of the kind "construction:specularratio:roughnessvalue"
## may be specified. For example, "reflector:0.03:0.05".
## The textual element ("reflector") is the name
## of a construction. The first number is the specular ratio
## for that construction. The second number is the roughness value.
## Specifying those values here makes possible
## to override the values specified in a Radiance database.
## (for example, the "0"s that may be in the database
## by defaul as regards specular ratios and roughness values).
## As an alternative, a material can be declared to be of the "mirror" type.
## This is done by specifying a value "construction:mirror".
## For example: reflector:mirror (see Radiance documentation
## about the properties of the "mirror" material type).
#
#%skycondition = ( 1=> "clear", 2=> "clear", 3=> "clear", 4=> "clear", 5=> "clear", 6=> "clear", 7=> "clear", 8=> "clear", 9=> "clear", 10=> "clear", 11=> "clear", 12=> "clear" );
## PREVAILING CONDITION OF THE SKY FOR EACH MONTH, EXPRESSED WITH ITS NUMBER, IN THE CASE IN WHICH
## CIE SKIES (WITH GENSKY) ARE UTILIZED.
## THE OPTIONS ARE: "clear", "cloudy" and "overcast".
## IF NO VALUE IS SPECIFIED, THE DEFAULT IS "clear".
#
#$parproc = 6; ## NUMBER OF PARALLEL PROCESSORS. NOT MANDATORY. FOR PARALLEL CALCULATIONS IN RADIANCE.
#
#@boundbox = ( -20, 40, -20, 40, -20, 40 );
## THE BOUNDING BOX OF THE RADIANCE SCENE.
#
#$add = " -ad 512 -aa 0.5 -dc .25 -dr 2 -ss 1 -st .05 -ds .04 -dt .02 -bv ";
## THIS ADDS OPTIONS TO THE CALLS TO RADIANCE (SEE "man rtrace"), MODIFYING THE DEFAULTS GIVEN BY ESP-r.
## IF THE -ad OPTION IS NOT SPECIFIED, IT DEFAULTS TO 1024. IF ONE ARE TAKING INTO ACCOUNT MORE THAN 1 DIFFUSE BOUNCE, IT IS BETTER TO REDUCE IT (FOR EXAMPLE, TO 512) FOR NOT SLOWING DOWN TOO MUCH THE COMPUTATIONS. -ad 512


######### END OF EXAMPLE CONFIGURATION LINES ###############################################
############################################################################################

############################################################################################
######### BEGINNING OF MODISH ##############################################################


my $max_processes = $main::max_processes;
if ( not ( defined( $max_processes ) ) ) { $max_processes = 1; }

if ( ( "$^O" eq "MSWin32" ) or ( "$^O" eq "MSWin64" ) )
{
  say "\nSorry, this procedure works only on Linux and OSX." and die;
}

my ( @zoneshds, @winsdata );
my ( %surfslist, %shdfileslist, %obsinfo );

my %days_inmonths = ( Jan => 16, Feb => 15, Mar => 16, Apr => 16, May => 16, Jun => 16, Jul => 16, Aug => 16, Sep => 16, Oct => 16, Nov => 16, Dec => 16 );

my %monthsnum = ( Jan => 1, Feb => 2, Mar => 3, Apr => 4, May => 5, Jun => 6, Jul => 7, Aug => 8, Sep => 9, Oct => 10, Nov => 11, Dec => 12 );

sub getmonthname
{
  my $monthnum = shift;

  my %monthsnames = ( 1 => "Jan" , 2 => "Feb", 3 => "Mar", 4 => "Apr", 5 => "May", 6 => "Jun", 7 => "Jul", 8 => "Aug", 9 => "Sep", 10 => "Oct", 11 => "Nov", 12 => "Dec" );
  my $monthname = $monthsnames{ "$monthnum" };
  return $monthname;
}

sub getmonthnum
{
  my $monthname = shift;
  my %monthsnums = ( Jan => 1, Feb => 2, Mar => 3, Apr => 4, May => 5, Jun => 6, Jul => 7, Aug => 8, Sep => 9, Oct => 10, Nov => 11, Dec => 12 );
  my $monthnum = $monthsnums{ "$monthname" };
  return $monthnum;
}

sub getconffilenames
{  # THIS GETS THE CONSTRUCTION AND MATERIALS FILES FROM THE CFG FILE. IT IS CALLED BY sub createfictitious
  my ( $conffile, $path, $askedzonenum, $cfgfile ) = @_;
  open ( CONFFILE, "$conffile") or die;
  my @lines = <CONFFILE>;
  close CONFFILE;

  my ( $constrdbfile, $matdbfile );
  my @zonedata;
  my ( $zonepath, $netpath, $ctlpath, $aimpath, $radpath, $imgpath, $docpath, $dbspath, $hvacpath, $bsmpath,
      $matfile, $constfile, $long, $groundrefl, $lat, $longdiff );
  my %paths;

  $paths{rootconffile} = $cfgfile;

  my $semaphore = "no";
  my $countline = 0;
  foreach my $line (@lines)
  {
    my ($geofile, $constrfile, $shdfile, $zonenum_cfg );
    my @row = split(/\s+|,/, $line);

    if ( ( $line =~ /#/ ) and ( $line =~ /ite/ ) and ( $line =~ /exposure/ ) and ( $line =~ /ground/ ) and ( $line =~ /reflectivity/ ) )
    {
      my @minis;
      foreach my $mini ( @row )
      {
        unless ( ( $mini eq "" ) or ( $mini eq " " ) or ( $mini eq "  " ) or ( $mini eq "   " ) or ( $mini eq "    " ) )
        {
          push( @minis, $mini );
        }
      }
      $groundrefl = $minis[1];
    }

    if ( ( $line =~ /#/ ) and ( $line =~ /atitude/ ) and ( $line =~ /ongitude/ ) and ( $line =~ /iff/ ) )
    {
      my @minis;
      foreach my $mini ( @row )
      {
        unless ( ( $mini eq "" ) or ( $mini eq " " ) or ( $mini eq "  " ) or ( $mini eq "   " ) or ( $mini eq "    " ) )
        {
          push( @minis, $mini );
        }
      }
      $lat = $minis[0];
      $longdiff = $minis[1];
    }

    if ( $row[0] eq "*zonpth" )
    {
      $zonepath = $row[1];
      $zonepath =~ s/^\.//;
      $zonepath =~ s/^\.//;
      $zonepath =~ s/\/$//;
      if ( not ( $zonepath =~ /^\// ) )
      {
        $zonepath = "/" . $zonepath;
      }
      $zonepath = $path . $zonepath;
    }
    elsif ( $row[0] eq "*netpth" )
    {
      $netpath = $row[1];
      $netpath =~ s/^\.//;
      $netpath =~ s/^\.//;
      $netpath =~ s/\/$//;
      if ( not ( $netpath =~ /^\// ) )
      {
        $netpath = "/" . $netpath;
      }
      $netpath = $path . $netpath;
    }
    elsif ( $row[0] eq "*ctlpth" )
    {
      $ctlpath = $row[1];
      $ctlpath =~ s/^\.//;
      $ctlpath =~ s/^\.//;
      $ctlpath =~ s/\/$//;
      if ( not ( $ctlpath =~ /^\// ) )
      {
        $ctlpath = "/" . $ctlpath;
      }
      $ctlpath = $path . $ctlpath;
    }
    elsif ( $row[0] eq "*aimpth" )
    {
      $aimpath = $row[1];
      $aimpath =~ s/^\.//;
      $aimpath =~ s/^\.//;
      $aimpath =~ s/\/$//;
      if ( not ( $aimpath =~ /^\// ) )
      {
        $aimpath = "/" . $aimpath;
      }
      $aimpath = $path . $aimpath;
    }
    elsif ( $row[0] eq "*radpth" )
    {
      $radpath = $row[1];
      $radpath =~ s/^\.//;
      $radpath =~ s/^\.//;
      $radpath =~ s/\/$//;
      if ( not ( $radpath =~ /^\// ) )
      {
        $radpath = "/" . $radpath;
      }
      $radpath = $path . $radpath;
    }
    elsif ( $row[0] eq "*imgpth" )
    {
      $imgpath = $row[1];
      $imgpath =~ s/^\.//;
      $imgpath =~ s/^\.//;
      $imgpath =~ s/\/$//;
      if ( not ( $imgpath =~ /^\// ) )
      {
        $imgpath = "/" . $imgpath;
      }
      $imgpath = $path . $imgpath;
    }
    elsif ( $row[0] eq "*docpth" )
    {
      $docpath = $row[1];
      $docpath =~ s/^\.//;
      $docpath =~ s/^\.//;
      $docpath =~ s/\/$//;
      if ( not ( $docpath =~ /^\// ) )
      {
        $docpath = "/" . $docpath;
      }
      $docpath = $path . $docpath;
    }
    elsif ( $row[0] eq "*dbspth" )
    {
      $dbspath = $row[1];
      $dbspath =~ s/^\.//;
      $dbspath =~ s/^\.//;
      $dbspath =~ s/\/$//;
      if ( not ( $dbspath =~ /^\// ) )
      {
        $dbspath = "/" . $dbspath;
      }
      $dbspath = $path . $dbspath;
    }
    elsif ( $row[0] eq "*hvacpth" )
    {
      $hvacpath = $row[1];
      $hvacpath =~ s/^\.//;
      $hvacpath =~ s/^\.//;
      $hvacpath =~ s/\/$//;
      if ( not ( $hvacpath =~ /^\// ) )
      {
        $hvacpath = "/" . $hvacpath;
      }
      $hvacpath = $path . $hvacpath;
    }
    elsif ( $row[0] eq "*bsmpth" )
    {
      $bsmpath = $row[1];
      $bsmpath =~ s/^\.//;
      $bsmpath =~ s/^\.//;
      $bsmpath =~ s/\/$//;
      if ( not ( $bsmpath =~ /^\// ) )
      {
        $bsmpath = "/" . $bsmpath;
      }
      $bsmpath = $path . $bsmpath;
    }
    elsif ( $row[0] eq "*mat" )
    {
      $matdbfile = $row[1];
      $matdbfile =~ s/^\.//;
      $matdbfile =~ s/^\.//;
      if ( not ( $matdbfile =~ /^\// ) )
      {
        $matdbfile = "/" . $matdbfile;
      }
      $matdbfile = $path . $matdbfile;
    }
    elsif ( $row[0] eq "*mlc" )
    {
      $constrdbfile = $row[1];
      $constrdbfile =~ s/^\.//;
      $constrdbfile =~ s/^\.//;
      if ( not ( $constrdbfile =~ /^\// ) )
      {
        $constrdbfile = "/" . $constrdbfile;
      }
      $constrdbfile = $path . $constrdbfile;
    }
    elsif ( $row[0] eq "*clm" )
    {
      $clmfile = $row[1];
      $clmfile =~ s/^\.//;
      $clmfile =~ s/^\.//;
      if ( not ( $clmfile =~ /^\// ) )
      {
        $clmfile = "/" . $clmfile;
      }
      $clmfile = $path . $clmfile;
    }

    if ($row[0] eq "*zon")
    {
      $countzone++;
      my $zonenum = $row[1];
      if ( $zonenum eq $askedzonenum )
      {
        $semaphore = "yes";
        push ( @zonedata, $zonenum );
      }
    }

    if ( $semaphore eq "yes" )
    {
      if ($row[0] eq "*geo")
      {
        $geofile = $row[1];
        $geofile =~ s/^\.//;
        $geofile =~ s/^\.//;
        if ( not ( $geofile =~ /^\// ) )
        {
          $geofile = "/" . $geofile;
        }
        $geofile = $path . $geofile;
        push ( @zonedata, $geofile );
      }

      if ( $row[0] eq "*con" )
      {
        $constrfile = $row[1];
        $constrfile =~ s/^\.//;
        $constrfile =~ s/^\.//;
        if ( not ( $constrfile =~ /^\// ) )
        {
          $constrfile = "/" . $constrfile;
        }
        $constrfile = $path . $constrfile;
        push ( @zonedata, $constrfile );
      }

      if ( ( $row[0] eq "*isi") or ( $row[0] eq "*ise") )
      {
        $shdfile = $row[1];
        $shdfile =~ s/^\.//;
        $shdfile =~ s/^\.//;
        if ( not ( $shdfile =~ /^\// ) )
        {
          $shdfile = "/" . $shdfile;
        }
        $shdfile = $path . $shdfile;
        push ( @zonedata, $shdfile );
        $semaphore = "no";
      }
    }
  }

  my $clmfilea = $clmfile . ".a";
  say MONITOR "CLMFILEA $clmfilea";

  #if ( ( "getweather" ~~ @calcprocedures ) or ( "embedded" ~~ @calcprocedure ) )
  {
    if ( not ( -e $clmfilea ) )
    {# THE CALL DOES NOT WORK WITH MODELS FLATTENED INTO ONE ROOT DIRECTORY. FIX THIS.

    #if ( "embedded" ~~ @calcprocedures )
    #{
    #  defendshd( $shdfile );
    #}

`cd $path/cfg \n clm -file $paths{rootconffile} -mode script<<YYY
$clmfile
j
a

y
-
YYY
`;


print REPORT "cd $path/cfg \n clm -file $paths{rootconffile} -mode script<<YYY
$clmfile
j
a

y
-
YYY
";
      #if ( "embedded" ~~ @calcprocedures )
      #{
      #  restoreshd( $shdfile );
      #}
    }
  }



#  if ( $longitude eq "" )
#  {
#    open( CLMFILE, "$clmfilea");
#    my @clmlines = <CLMFILE>;
#    close CLMFILE;
#    my @elts = split( ",", $clmlines[0] );
#    my $long = $elts[6];
#    if ( $long eq "" )
#    {
#      $long = $longitude;
#    }
#  }

  my $standardmeridian = 0; ################################ WORKING, BUT UNELEGANT FOR REPORTS
  #if ( $long eq "" )
  #{
    $long = $standardmeridian + $longdiff; ################################ WORKING, BUT UNELEGANT FOR REPORTS
  #}

  my $clmavgs = $clmfilea . "_avgs";

  my $tmppath = $radpath;
  $tmppath =~ s/\/rad$/\/tmp/ ;

  $paths{tmppath} = $tmppath;
  $paths{zonepath} = $zonepath;
  $paths{netpath} = $netpath;
  $paths{ctlpath} = $ctlpath;
  $paths{aimpath} = $aimpath;
  $paths{radpath} = $radpath;
  $paths{imgpath} = $imgpath;
  $paths{docpath} = $docpath;
  $paths{dbspath} = $dbspath;
  $paths{hvacpath} = $hvacpath;
  $paths{bsmpath} = $bsmpath;
  $paths{matdbfile} = $matdbfile;
  $paths{constrdbfile} = $constrdbfile;
  $paths{conffile} = $conffile;
  $paths{lat} = $lat;
  $paths{longdiff} = $longdiff;
  $paths{long} = $long;
  $paths{groundrefl} = $groundrefl;
  $paths{standardmeridian} = $standardmeridian;
  $paths{clmfile} = $clmfile;
  $paths{clmfilea} = $clmfilea;
  $paths{clmavgs} = $clmavgs;

  my $cfgpath = $conffile;
  $cfgpath =~ s/\.cfg$// ;

  while ( not ( $cfgpath =~ /\/$/ ) )
  {
    $cfgpath =~ s/(\w+)$// ;
  }
  $cfgpath =~ s/\/$// ;

  $paths{cfgpath} = $cfgpath;

  return ( $constrdbfile, $matdbfile, \@zonedata, \@lines, \%paths );
}

sub createfictitiousfiles
{
  # THIS CREATES THE FILES FOR THE MODELS FEATURING FICTITIOUS QUALITIES AIMED TO THE MAIN Modish PROGRAM,
  # MODIFIES THE MATERIALS DB AS REQUESTED
  # _AND_ PREPARES THE CONFIGURATION FILES FOR THE FICTITIOUS MODELS
  my ($conffile, $path, $zonenum, $calcprocedures_ref, $cfgfile, $modishlock ) = @_;
  my $conffile_f1 = $conffile;
  my ($flaggeo, $flagconstrdb, $flagmatdb, $flagconstr);

  #my $cfgpath = $conffile;
  #$cfgpath =~ s/\/$cfgfile// ;
  my $cfgpath = $path . "/cfg";
  say MONITOR "\$cfgpath $cfgpath";

  $conffile_f1 =~ s/\.cfg/\_f1\.cfg/;
  my $conffile_f2 = $conffile;
  $conffile_f2 =~ s/\.cfg/\_f2\.cfg/;
  my $conffile_f3 = $conffile;
  $conffile_f3 =~ s/\.cfg/\_f3\.cfg/;
  my $conffile_f4 = $conffile;
  $conffile_f4 =~ s/\.cfg/\_f4\.cfg/;
  my $conffile_f5 = $conffile;
  $conffile_f5 =~ s/\.cfg/\_f5\.cfg/;
  my $conffile_f6 = $conffile;
  $conffile_f6 =~ s/\.cfg/\_f6\.cfg/;
  my $conffile_f7 = $conffile;
  $conffile_f7 =~ s/\.cfg/\_f7\.cfg/;
  my $conffile_f8 = $conffile;
  $conffile_f8 =~ s/\.cfg/\_f8\.cfg/;
  my $conffile_f9 = $conffile;
  $conffile_f9 =~ s/\.cfg/\_f9\.cfg/;
  my $conffile_f10 = $conffile;
  $conffile_f10 =~ s/\.cfg/\_f10\.cfg/;
  my $conffile_f11 = $conffile;
  $conffile_f11 =~ s/\.cfg/\_f11\.cfg/;

  my @calcprocedures = @{ $calcprocedures_ref };

  unless ( -e $modishlock )
  {
    open( CONFFILE, "$conffile" ) or die;
    my @lines = <CONFFILE>;
    close CONFFILE;

    my ( $localizeclm, $localizemat, $localizemlc );
    foreach my $line ( @lines )
    {
      if ( $line =~ /^\*stdclm/ )
      {
        $localizeclm = "yes";
      }
      if ( $line =~ /^\*stdmat/ )
      {
        $localizemat = "yes";
      }
      if ( $line =~ /^\*stdmlc/ )
      {
        $localizemlc = "yes";
      }
    }

    if ( $localizeclm eq "yes" )
    {
      my $outp = `cd $cfgpath \n prj -file $cfgfile -mode script<<YYY
b
a
e

n
-
y
s


-
YYY`;

      #say REPORT $outp;
      say REPORT "cd $cfgpath \n prj -file $cfgfile -mode script<<YYY
b
a
e

n
-
y
s


-
YYY";
    }

    if ( $localizemat eq "yes" )
    {
      my $outp = `cd $cfgpath \n prj -file $cfgfile -mode script<<YYY
b
c
d

-
-
y
s


-
YYY`;

      #say REPORT $outp;
      say REPORT "cd $cfgpath \n prj -file $cfgfile -mode script<<YYY
b
c
d

-
-
y
s


-
YYY";
    }

    if ( $localizemlc eq "yes" )
    {
      my $outp = `cd $cfgpath \n prj -file $cfgfile -mode script<<YYY
b
e
d


-
-
-
s


-
YYY`;

      #say REPORT $outp;
      say REPORT "cd $cfgpath \n prj -file $cfgfile -mode script<<YYY
b
e
d


-
-
-
s


-
YYY";
    }
  }

  unless ( -e $modishlock )
  {
    `cp -R -f $conffile $conffile_f1\n`;
    print REPORT "cp -R -f $conffile $conffile_f1\n";


    `cp -R -f $conffile $conffile_f2\n`;
    print REPORT "cp -R -f $conffile $conffile_f2\n";


    if ( scalar( @selectives ) > 0 )
    {
      `cp -R -f $conffile $conffile_f3\n`;
      `cp -R -f $conffile $conffile_f4\n`;
      print REPORT "cp -R -f $conffile $conffile_f3\n";
      print REPORT "cp -R -f $conffile $conffile_f4\n";
    }

    {
      `cp -R -f $conffile $conffile_f5\n`;
      print REPORT "cp -R -f $conffile $conffile_f5\n";
    }

    if ( ( ( "radical" ~~ @calcprocedures ) and not( "alldiff" ~~ @calcprocedures ) )
      or( ( "composite" ~~ @calcprocedures ) and not( "alldiff" ~~ @calcprocedures ) )
      or( "noreflections" ~~ @calcprocedures ) )
    {
      `cp -R -f $conffile $conffile_f6\n`;
      print REPORT "cp -R -f $conffile $conffile_f6\n";
    }

    if ( ( ( "radical" ~~ @calcprocedures ) and not( "alldiff" ~~ @calcprocedures ) )
      or( ( "composite" ~~ @calcprocedures ) and not( "groundreflections" ~~ @calcprocedures ) )
      or( "noreflections" ~~ @calcprocedures ) )
    {
      `cp -R -f $conffile $conffile_f7\n`;
      print REPORT "cp -R -f $conffile $conffile_f7\n";
    }

    if ( ( ( "composite" ~~ @calcprocedures ) and not( "groundreflections" ~~ @calcprocedures ) )
      or( "noreflections" ~~ @calcprocedures ) )
    {
      `cp -R -f $conffile $conffile_f8\n`;
      print REPORT "cp -R -f $conffile $conffile_f8\n";
    }

    if ( "something_used" ~~ @calcprocedures ) # CURRENTLY UNUSED
    {
      `cp -R -f $conffile $conffile_f9\n`;
      print REPORT "cp -R -f $conffile $conffile_f9\n";
    }

    if ( "something_used" ~~ @calcprocedures ) # CURRENTLY UNUSED
    {
      `cp -R -f $conffile $conffile_f10\n`;
      print REPORT "cp -R -f $conffile $conffile_f10\n";
    }

    if ( ( ( "composite" ~~ @calcprocedures ) and not( "groundreflections" ~~ @calcprocedures ) )
      or( "noreflections" ~~ @calcprocedures ) )
    {
      `cp -R -f $conffile $conffile_f11\n`;
      print REPORT "cp -R -f $conffile $conffile_f11\n";
    }
  }

  my ( $constrdbfile, $matdbfile, $zonedataref, $conflinesref, $paths_ref ) = getconffilenames( $conffile, $path, $zonenum, $cfgfile );
  my @zonedata = @$zonedataref;
  my $geofile = $zonedata[1];
  my @conflines0 = @$conflinesref;
  my @conflines = @{ dclone( \@conflines0 ) };
  my %paths = %{ $paths_ref }; ################################

  my $geofile_f = $geofile;
  $geofile_f =~ s/\.geo/_f\.geo/;

  my $geofile_f5 = $geofile;
  $geofile_f5 =~ s/\.geo/_f5\.geo/;
  my @geofiles;

  unless ( -e $geofile_f5 )
  {
    @geofiles = ( $geofile_f );
  }
  else
  {
    @geofiles = ( $geofile_f, $geofile_f5 );
  }
  say MONITOR "GEOFILES: " . dump ( @geofiles );

  #unless ( -e $modishlock )
  #{
  #  say MONITOR "\$matdbfile $matdbfile";
  #  open( MATDBFILE, "$matdbfile" ) or die;
  #  my @matdblines = <MATDBFILE>;
  #  close MATDBFILE;
  #  open( MATDBFILE, ">$matdbfile" ) or die;
  #  foreach my $matdbline ( @matdblines )
  #  {
  #    unless ( ( $matdbline =~ /^#/ ) or ( $matdbline =~ /^\*/ ) or ( $matdbline =~ /^Category/ ) )
  #    {
  #      my @elts = split( /,/, $matdbline );
  #      $elts[0] = sprintf("%.3f", $elts[0]);
  #      $matdbline = join( ',', @elts );
  #    }
  #    print MATDBFILE $matdbline;
  #  }
  #}

  my (@originals, @fictitia1, @fictitia2, @fictitia3, @fictitia4, @fictitia5, @fictitia6, @fictitia7, @fictitia8, @fictitia9,
      @fictitia10, @fictitia11 );

  push ( @originals, $constrdbfile);

  my $constrdbfile_f = $constrdbfile;
  $constrdbfile_f = $constrdbfile . "_f" ;
  push ( @fictitia1, $constrdbfile_f);
  push ( @fictitia2, $constrdbfile_f);

  @fictitia3 = @fictitia2;
  @fictitia4 = @fictitia2;
  @fictitia5 = @fictitia2;
  @fictitia6 = @fictitia2;
  @fictitia7 = @fictitia2;
  @fictitia8 = @fictitia2;
  @fictitia9 = @fictitia2;
  @fictitia10 = @fictitia2;
  @fictitia11 = @fictitia2;

  push ( @originals, $matdbfile);

  my $matdbfile_f1 = $matdbfile;
  $matdbfile_f1 = $matdbfile . "_f1";
  push ( @fictitia1, $matdbfile_f1 );

  my $matdbfile_f2 = $matdbfile;
  $matdbfile_f2 = $matdbfile . "_f2";
  push ( @fictitia2, $matdbfile_f2 );

  my $matdbfile_f3 = $matdbfile;
  $matdbfile_f3 = $matdbfile . "_f3";
  push ( @fictitia3, $matdbfile_f3 );

  my $matdbfile_f4 = $matdbfile;
  $matdbfile_f4 = $matdbfile . "_f4";
  push ( @fictitia4, $matdbfile_f4 );

  push ( @fictitia5, $matdbfile_f2 );

  my $matdbfile_f6 = $matdbfile;
  $matdbfile_f6 = $matdbfile . "_f6";
  push ( @fictitia6, $matdbfile_f6 );

  push ( @fictitia7, $matdbfile_f6 );

  push ( @fictitia8, $matdbfile_f6 );

  push ( @fictitia9, $matdbfile_f1 );

  push ( @fictitia10, $matdbfile_f1 );

  push ( @fictitia11, $matdbfile_f6 );


  my ( @tempbox_original, @tempbox_fictitia1, @tempbox_fictitia2,
    @tempbox_fictitia3, @tempbox_fictitia4, @tempbox_fictitia5, @tempbox_fictitia6, @tempbox_fictitia7,
      @tempbox_fictitia8, @tempbox_fictitia9, @tempbox_fictitia10, @tempbox_fictitia11 );

  my $geofile = $zonedata[1];
  push ( @tempbox_originals, $geofile );

  my $geofile_f = $geofile;
  $geofile_f =~ s/\.geo/_f\.geo/;

  my $geofile_f5 = $geofile;
  $geofile_f5 =~ s/\.geo/_f5\.geo/;

  push ( @tempbox_fictitia1, $geofile_f);
  push ( @tempbox_fictitia2, $geofile_f);
  push ( @tempbox_fictitia3, $geofile_f);
  push ( @tempbox_fictitia4, $geofile_f);
  push ( @tempbox_fictitia5, $geofile_f5);
  push ( @tempbox_fictitia6, $geofile_f);
  push ( @tempbox_fictitia7, $geofile_f5);
  push ( @tempbox_fictitia8, $geofile_f);
  push ( @tempbox_fictitia9, $geofile_f);
  push ( @tempbox_fictitia10, $geofile_f5);
  push ( @tempbox_fictitia11, $geofile_f5);

  my $constrfile = $zonedata[2];
  push ( @tempbox_originals, $constrfile );

  my $constrfile_f = $constrfile;
  $constrfile_f =~ s/\.con/_f\.con/;

  push ( @tempbox_fictitia1, $constrfile_f );
  push ( @tempbox_fictitia2, $constrfile_f );
  push ( @tempbox_fictitia3, $constrfile_f );
  push ( @tempbox_fictitia4, $constrfile_f );
  push ( @tempbox_fictitia5, $constrfile_f );
  push ( @tempbox_fictitia6, $constrfile_f );
  push ( @tempbox_fictitia7, $constrfile_f );
  push ( @tempbox_fictitia8, $constrfile_f );
  push ( @tempbox_fictitia9, $constrfile_f );
  push ( @tempbox_fictitia10, $constrfile_f );
  push ( @tempbox_fictitia11, $constrfile_f );
  print REPORT "cp -R -f $constrfile $constrfile_f\n"; #PROBABLY UNUSED
  `cp -R -f $constrfile $constrfile_f\n`; $flagconstr = "y"; #PROBABLY UNUSED

  my $shdfile = $zonedata[3];

  my $shdfile_f5 = $shdfile;
  my $shdfile_f5 =~ s/\.shd/_f5\.shd/;

  push ( @tempbox_originals, $shdfile);

  push ( @tempbox_fictitia1, $shdfile );
  push ( @tempbox_fictitia2, $shdfile );
  push ( @tempbox_fictitia3, $shdfile );
  push ( @tempbox_fictitia4, $shdfile );
  push ( @tempbox_fictitia5, $shdfile_f5 );
  push ( @tempbox_fictitia6, $shdfile );
  push ( @tempbox_fictitia7, $shdfile_f5 );
  push ( @tempbox_fictitia8, $shdfile_f );
  push ( @tempbox_fictitia9, $shdfile );
  push ( @tempbox_fictitia10, $shdfile_f5 );
  push ( @tempbox_fictitia11, $shdfile_f5 );

  my $zonenum_cfg = $zonedata[0];
  push ( @tempbox_originals, $zonenum_cfg);

  push ( @tempbox_fictitia1, $zonenum_cfg );
  push ( @tempbox_fictitia2, $zonenum_cfg );
  push ( @tempbox_fictitia3, $zonenum_cfg );
  push ( @tempbox_fictitia4, $zonenum_cfg );
  push ( @tempbox_fictitia5, $zonenum_cfg );
  push ( @tempbox_fictitia6, $zonenum_cfg );
  push ( @tempbox_fictitia7, $zonenum_cfg );
  push ( @tempbox_fictitia8, $zonenum_cfg );
  push ( @tempbox_fictitia9, $zonenum_cfg );
  push ( @tempbox_fictitia10, $zonenum_cfg );
  push ( @tempbox_fictitia11, $zonenum_cfg );

  push ( @originals, [ @tempbox_originals ] );
  push ( @fictitia1, [ @tempbox_fictitia1 ] );
  push ( @fictitia2, [ @tempbox_fictitia2 ] );
  push ( @fictitia3, [ @tempbox_fictitia3 ] );
  push ( @fictitia4, [ @tempbox_fictitia4 ] );
  push ( @fictitia5, [ @tempbox_fictitia5 ] );
  push ( @fictitia6, [ @tempbox_fictitia6 ] );
  push ( @fictitia7, [ @tempbox_fictitia7 ] );
  push ( @fictitia8, [ @tempbox_fictitia8 ] );
  push ( @fictitia9, [ @tempbox_fictitia9 ] );
  push ( @fictitia10, [ @tempbox_fictitia10 ] );
  push ( @fictitia11, [ @tempbox_fictitia11 ] );

  my ( @correctlines, $addline );

  unless ( -e $modishlock )
  {
    open ( CONFFILE_F1, ">$conffile_f1");
    foreach my $line ( @conflines )
    {
      my $counter = 0;
      foreach my $elt ( @fictitia1 )
      {
        if ( not ( ref($elt) ) )
        {
          my $original = $originals[$counter];
          $elt =~ s/$path//;
          $original =~ s/$path//;
          if ( $elt )
          {
            $line =~ s/$original/$elt/;
          }
        }
        else
        {
          my @elts = @$elt;
          my @originalelts = @{$originals[$counter]};
          my $count = 0;
          foreach my $el ( @elts )
          {
            my $original = $originalelts[$count];
            $el =~ s/$path//;
            $original =~ s/$path//;
            if ( $el )
            {
              $line =~ s/$original/$el/;
            }
            $count++;
          }
        }

        if ( ( $counter == 0 ) and ( not ( $line =~ /^\*/ ) ) )
        {
          open( CORRECTCONF, $conffile ) or die;
          @correctlines = <CORRECTCONF>;
          close CORRECTCONF;
        }

        if ( @correctlines )
        {
          $addline = /^(.)$correctlines[ $counter ]/;
          if ( $addline )
          {
            $line = $addline . $line;
          }
        }
        $counter++;
      }

      if ( $line =~ /\*mat/ )
      {
        unless ( $line =~ /_f1/ )
        {
          $line = $line . "_f1" ;
        }
      }

      print CONFFILE_F1 $line;
    }
    close CONFFILE_F1;

    open ( CONFFILE_F1 , "$conffile_f1" ) or die;
    my  @conflines1 = <CONFFILE_F1> ;
    close CONFFILE_F1;

    #unless ( "noreflections" ~~ @calcprocedures )
    {
      my @conflines = @{ dclone( \@conflines1 ) } ;
      open ( CONFFILE_F2, ">$conffile_f2");
      foreach my $line ( @conflines )
      {
        if ( $line =~ /\*mat/ )
        {
          $line =~ s/_f1/_f2/ ;
        }
        print CONFFILE_F2 $line;
      }
      close CONFFILE_F2;

      if ( scalar( @selectives ) > 0 )
      {
        my @conflines = @{ dclone( \@conflines1 ) } ;
        open ( CONFFILE_F3, ">$conffile_f3");
        foreach my $line ( @conflines )
        {
          if ( $line =~ /\*mat/ )
          {
            $line =~ s/_f1/_f3/ ;
          }
          print CONFFILE_F3 $line;
        }
        close CONFFILE_F3;

        my @conflines = @{ dclone( \@conflines1 ) } ;

        open ( CONFFILE_F4, ">$conffile_f4");
        foreach my $line ( @conflines )
        {
          if ( $line =~ /\*mat/ )
          {
            $line =~ s/_f1/_f4/ ;
          }
          print CONFFILE_F4 $line;
        }
        close CONFFILE_F4;
      }
    }

    #if ( "radical" ~~ @calcprocedures )
    {
      my @conflines = @{ dclone( \@conflines1 ) } ;
      open ( CONFFILE_F5, ">$conffile_f5" );
      foreach my $line ( @conflines )
      {
        if ( $line =~ /\*mat/ )
        {
          if ($line =~ /_f1/ )
          {
            $line =~ s/_f1/_f2/ ;
          }
        }
        elsif ( $line =~ /\*geo/ )
        {
          $line =~ s/_f\./_f5\./ ;
        }
        elsif ( ( $line =~ /\*isi/ ) or ( $line =~ /\*ise/ ) )
        {
          my @els = split( " ", $line );

          unless ( $line =~ /_f5\./ )
          {
            $line =~ s/\.shd/_f5\.shd/ ;
          }

          my $shdafile = $shdfile . "a" ;
          my $shd5 = $shdfile;
          $shd5 =~ s/\.shd/_f5\.shd/ ;
          my $shda5 = $shd5 . "a" ;
          `cp -f $shdfile $shd5` ;
          `cp -f $shdafile $shda5` ;
          say REPORT "cp -f $shdfile $shd5" ;
          say REPORT_ "cp -f $shdafile $shda5" ;
        }
        print CONFFILE_F5 $line;
      }
      close CONFFILE_F5;
    }


    if ( ( "composite" ~~ @calcprocedures ) or ( "radical" ~~ @calcprocedures ) or ( "noreflections" ~~ @calcprocedures ) )
    {
      my @conflines = @{ dclone( \@conflines1 ) } ;
      open ( CONFFILE_F6, ">$conffile_f6");
      foreach my $line ( @conflines )
      {
        if ( $line =~ /\*mat/ )
        {
          $line =~ s/_f1/_f6/ ;
        }
        elsif ( $line =~ /ite exposure & ground reflectivity/ )
        {
          chomp $line;
          $line =~ s/ +/ / ;
          $line =~ s/^ // ,
          my @elts = split( " ", $line );
          $line = "      $elts[0]   0.000 $elts[2] $elts[3] $elts[4] $elts[5] $elts[6] $elts[7]\n";
        }
        print CONFFILE_F6 $line;
      }
      close CONFFILE_F6;

      if ( ( ( "composite" ~~ @calcprocedures ) and not( "groundreflections" ~~ @calcprocedures ) )
        or ( "radical" ~~ @calcprocedures )
        or ( "noreflections" ~~ @calcprocedures ) )
      {
        my @conflines = @{ dclone( \@conflines1 ) } ;
        open ( CONFFILE_F7, ">$conffile_f7");
        foreach my $line ( @conflines )
        {
          if ( $line =~ /\*mat/ )
          {
            $line =~ s/_f1/_f6/ ;
          }
          elsif ( $line =~ /\*geo/ )
          {
            $line =~ s/_f\./_f5\./ ;
          }
          elsif ( ( $line =~ /\*isi/ ) or ( $line =~ /\*ise/ ) )
          {
            $line =~ s/\.shd/_f5\.shd/ ;
          }
          elsif ( $line =~ /ite exposure & ground reflectivity/ )
          {
            chomp $line;
            $line =~ s/ +/ / ;
            $line =~ s/^ // ,
            my @elts = split( " ", $line );
            $line = "      $elts[0]   0.000 $elts[2] $elts[3] $elts[4] $elts[5] $elts[6] $elts[7]\n";
          }
          print CONFFILE_F7 $line;
        }
        close CONFFILE_F7;
      }
    }

    if ( ( ( "composite" ~~ @calcprocedures ) and not( "groundreflections" ~~ @calcprocedures ) )
      or ( "noreflections" ~~ @calcprocedures ) )
    {
      my @conflines = @{ dclone( \@conflines1 ) } ;
      open ( CONFFILE_F8, ">$conffile_f8");
      foreach my $line ( @conflines )
      {
        if ( $line =~ /\*mat/ )
        {
          $line =~ s/_f1/_f6/ ;
        }
        print CONFFILE_F8 $line;
      }
      close CONFFILE_F8;
    }

    if ( "something_used" ~~ @calcprocedures ) # CURRENTLY UNUSED
    {
      my @conflines = @{ dclone( \@conflines1 ) } ;
      open ( CONFFILE_F9, ">$conffile_f9");
      foreach my $line ( @conflines )
      {
        if ( $line =~ /ite exposure & ground reflectivity/ )
        {
          chomp $line;
          $line =~ s/ +/ / ;
          $line =~ s/^ // ,
          my @elts = split( " ", $line );
          $line = "      $elts[0]   0.000 $elts[2] $elts[3] $elts[4] $elts[5] $elts[6] $elts[7]\n";
        }
        print CONFFILE_F9 $line;
      }
      close CONFFILE_F9;

      my @conflines = @{ dclone( \@conflines1 ) } ;
      open ( CONFFILE_F10, ">$conffile_f10"); # CURRENTLY UNUSED
      foreach my $line ( @conflines )
      {
        if ( $line =~ /\*geo/ )
        {
          $line =~ s/_f\./_f5\./ ;
        }
        elsif ( ( $line =~ /\*isi/ ) or ( $line =~ /\*ise/ ) )
        {
            $line =~ s/\.shd/_f5\.shd/ ;
        }
        elsif ( $line =~ /ite exposure & ground reflectivity/ )
        {
          chomp $line;
          $line =~ s/ +/ / ;
          $line =~ s/^ // ,
          my @elts = split( " ", $line );
          $line = "      $elts[0]   0.000 $elts[2] $elts[3] $elts[4] $elts[5] $elts[6] $elts[7]\n";
        }
        print CONFFILE_F10 $line;
      }
      close CONFFILE_F10;
    }

    if ( ( ( "composite" ~~ @calcprocedures ) and not( "groundreflections" ~~ @calcprocedures ) )
      or ( "noreflections" ~~ @calcprocedures ) )
    {
      my @conflines = @{ dclone( \@conflines1 ) } ;
      open ( CONFFILE_F11, ">$conffile_f11");
      foreach my $line ( @conflines )
      {
        if ( $line =~ /\*mat/ )
        {
          $line =~ s/_f1/_f6/ ;
        }
        elsif ( $line =~ /\*geo/ )
        {
          $line =~ s/_f\./_f5\./ ;
        }
        elsif ( ( $line =~ /\*isi/ ) or ( $line =~ /\*ise/ ) )
        {
            $line =~ s/\.shd/_f5\.shd/ ;
        }
        print CONFFILE_F11 $line;
      }
      close CONFFILE_F11;
    }
  }

  return ( $conffile, $conffile_f1, $conffile_f2, $conffile_f3, $conffile_f4, $conffile_f5, $conffile_f6,
  $conffile_f7, $conffile_f8, $conffile_f9, $conffile_f10, $conffile_f11, $constrdbfile, $constrdbfile_f,
    $matdbfile, $matdbfile_f1, $matdbfile_f2, $matdbfile_f6, $geofile_f, $geofile_f5, $flagconstrdb, $flagmatdb,
    $flaggeo, $flagconstr, [ @originals ], [ @fictitia1], [ @fictitia2 ], [ @fictitia3 ], [ @fictitia4 ], [ @fictitia5 ],
    [ @fictitia6 ], [ @fictitia7 ], [ @fictitia8 ], [ @fictitia9 ], [ @fictitia10 ], [ @fictitia11 ], \%paths );
}


sub readgeofile
{  # THIS READS A GEO FILE TO GET THE DATA OF THE REQUESTED SURFACES
  my ( $geofile, $transpsurfs_ref, $zonenum, $calcprocedures_ref, $modishlock ) = @_;
  my @transpsurfs = @{ $transpsurfs_ref };
  my @calcprocedures = @{ $calcprocedures_ref };
  open ( GEOFILE, "$geofile") or die;
  my @lines = <GEOFILE>;
  close GEOFILE;
  my ( @geofilestruct, @transpelts, @obs );
  my %datalist;
  my %surfs;
  my $countsurf = 0;

  my @transps;
  if ( scalar( @transpsurfs ) == 0 )
  {
    foreach my $line ( @lines )
    {
      if ( $line =~ /\*surf,/ )
      {
        my @es = split( /\s+|,/, $line );
        if ( ( not ( $es[7] =~ /OPAQUE/ ) ) and ( $es[8] =~ /EXTERIOR/ ) )
        {
          say MONITOR "NOW FOUND: $line" ;
          push ( @transps, $es[12] );
        }
      }
    }
    @transpsurfs = @transps;
  }

  say MONITOR "NOW TRANSPSURFS: " . dump( @transpsurfs );

  foreach my $surfnum ( @transpsurfs )
  {
    foreach my $line (@lines)
    {
      my @elts = split(/\s+|,/, $line);
      if ( $line =~ /\*surf/ )
      {
        my $surfname = $elts[1];
        my $surfnum = $elts[12];
        my $parent = $elts[3];
        my $constr = $elts[6];
        # THIS POPULATES THE VARIABLE %surfslist (HASH - DICTIONARY - ASSOCIATIVE ARRAY) LINKING ZONES, SURFACES NAMES AND SURFACE NUMBER:

        $surfslist{ $zonenum }{ $surfnum }{surfname} = $surfname;
        $surfslist{ $zonenum }{ $surfname }{surfnum} = $surfnum;
        $datalist{ $zonenum }{ $surfnum }{ surfname } = $surfname;
        $datalist{ $zonenum }{ $surfnum }{ parent } = $parent;
        $datalist{ $zonenum }{ $surfnum }{ constr } = $constr;
        $datalist{ $zonenum }{ $surfnum }{ surfnum } = $surfnum;
        $datalist{ $zonenum }{ $surfnum }{ geofile } = $geofile;
        unless ( $parent eq "-" )
        {
          my $parentnum = $surfslist{ $zonenum }{ $parent }{surfnum};
          push ( @{ $datalist{ $zonenum }{ children }{ $parentnum } }, $surfnum );
          @{ $datalist{ $zonenum }{ children }{ $parentnum } } = uniq( @{ $datalist{ $zonenum }{ children }{ $parentnum } } );
        }
      }

      if ( $line =~ /\*vertex/ )
      {
        my $x =  $elts[1];
        my $y =  $elts[2];
        my $z =  $elts[3];
        my $vertnum =  $elts[5];
        $datalist{ $zonenum }{ vertex }{ $vertnum } = [ $x, $y, $z ];
      }

      if ( $line =~ /\*edges/ )
      {
        my $surfnum = $elts[ $#surfnum ];
        my $border = scalar( @elts - 3 );
        my @vertnums = @elts[ 1..$border ];
        $datalist{ $zonenum }{ $surfnum }{ edges }{ $surfnum } = [ @vertnums ];
      }

    if ( ( $line =~ /\*surf/ ) and ( $surfnum == $elts[12] ) )
      {
        my $surfname = $elts[1];
        my $parent = $elts[3];
        my $constr = $elts[6];
        my $surfnum = $elts[12];
        push (@transpelts, [ $surfname, $parent, $constr, $surfnum, $geofile, $zonenum ] );
        $surfs{name}{$surfname} = $surfnum;
        $surfs{num}{$surfnum} = $surfname;
      }

      if ( $line =~ /\*obs/ )
      {
        my $obsconstr = $elts[10];
        my $obsname = $elts[9];
        my $obsnum = $elts[13];
        push (@obs, [ $obsname, $obsconstr, $obsnum ] );
      }

      if ( $countsurf == 0 )
      {
        push ( @geofilestruct, [ @elts ] );
      }
    }
    $countsurf++;
  }

  foreach ( @geofilestruct )
  {
    my $obsmaterial = $_->[9];
    my $obsnumber = $_->[12];
    if ( ( $_->[0] eq "#" ) and ( $_->[1] eq "*obs" ) )
    {
      $semaphore = 1;
    }
    if ( ( $semaphore == 1) and ( $_->[0] eq "*obs" ) )
    {

    }
    $obsinfo{ $obsnumber } = $obsnumber;
    $obsinfo{ $obsmaterial } = $obsmaterial;
  }

  return ( \@transpelts, \@geofilestruct, \%surfslist, \@obs, \%datalist, \@obsmaterials, \%surfs, \@transpsurfs );
}

sub readverts
{
  # THIS READS THE VERTEX NUMBERS OF THE REQUESTED SURFACES IN A GEO FILE
  my @transpelts = @{$_[0]};
  my $geofile = $_[1];
  my @geodata = @{$_[2]};
  my %datalist = %{$_[3]};
  my @winselts;
  foreach my $transpelt (@transpelts)
  {
    my $surfname = $transpelt->[0];
    my $parent = $transpelt->[1];
    my $constr = $transpelt->[2];
    my $surfnum = $transpelt->[3];
    my $geofile = $transpelt->[4];
    my $zonenum = $transpelt->[5];
    my @winelts;
    foreach my $datum (@geodata)
    {
      my @data = @$datum;
      if ( ($data[0] eq "*edges") and ( $data[$#data] == $surfnum ) )
      {
        push ( @winelts, [ [ @data[ 2..( $#data - 2 ) ] ], $surfnum ] );
        my @surfverts = @data[ 2..( $#data - 2 ) ];
        $datalist{ $zonenum }{ $surfnum }{vertnums} = [ @surfverts ];
      }
    }
    push ( @winselts, [ @winelts ] );
  }
  return ( \@winselts, \%datalist );
}

sub readcoords
{
  # THIS READS THE COORDINATES OF THE REQUESTED VERTEX NUMBERS
  my ( $winseltsref, $geofile, $geodataref, $datalistref, $transpeltsref ) = @_;
  my @winselts = @$winseltsref;
  my @geodata = @$geodataref;
  my %datalist = %$datalistref;
  my @transpelts = @$transpeltsref;
  my @allcoords;
  my $count = 1;
  foreach my $winseltref (@winselts)
  {
    my @transpelt = @{ $transpelts[ $count -1 ] };
    my $zonenum = $transpelt[5];

    my @winselt = @$winseltref;
    my @vertnums = @{ $winselt[0][0] };
    my $surfnum = $winselt[0][1];
    my @coords;
    foreach my $num (@vertnums)
    {
      foreach my $datum (@geodata)
      {
        my @data = @$datum;
        if ( ($data[0] eq "*vertex") and ( $data[5] == $num ) )
        {
          push ( @coords, [ [ @data[ 1..3 ] ], $num ] );
          $datalist{ $zonenum }{ $num }{vertcoords} = [ @data[ 1..3 ] ];
        }
      }
    }
    push ( @allcoords, [ @coords ] );
    $count++;
  }
  return (\@allcoords, \%datalist );
}

sub getcorners
{
  # THIS PACKS THE X, Y, AND Z COORDINATES OF THE VERTICES OF THE REQUESTED SURFACES INTO SUBARRAYS
  my ( $winscoordsref, $winseltsref ) = @_;
  my @winscoords = @$winscoordsref;
  my @winselts = @$winseltsref;
  my @packsurfsdata;
  my $countsurf = 0;
  foreach $surfcoordsref ( @winscoords )
  {
    my @surfcoords = @$surfcoordsref;
    my ( @xdata, @ydata, @zdata );
    my @packsurfdata;
    my $surfnum = $winselts[$countsurf][0][1];
    foreach my $coordsetref (@surfcoords)
    {
      my @coordset = @$coordsetref;
      push (@xdata, $coordset[0][0]);
      push (@ydata, $coordset[0][1]);
      push (@zdata, $coordset[0][2]);
    }
    push (@packsurfdata, [ @xdata ], [ @ydata ], [ @zdata ], $surfnum  );
    push ( @packsurfsdata, [ @packsurfdata ] );
    $countsurf++;
  }
  return ( @packsurfsdata );
}

sub findextremes
{
  # THIS FINDS THE MAXIMA AND THE MINIMA FOR EACH COORDINATE FOR THE REQUESTED SURFACE
  my @xyzcoords = @_;
  my @surfsdata;
  foreach my $coordsdataref ( @xyzcoords )
  {
    my @coordsdata = @$coordsdataref;
    my $count = 0;
    my @surfdata;
    foreach $coordstyperef (@coordsdata)
    {
      if ($count < 3)
      {
        my @coordstype = @$coordstyperef;
        my $extreme1 = max(@coordstype);
        my $extreme2 = min(@coordstype);
        my $countpos = 0;
        my (@extreme1positions, @extreme2positions);
        foreach my $elt ( @coordstype )
        {
          if ( $elt ~~ $extreme1 )
          {
            push ( @extreme1positions, $countpos );
          }
          if ( $elt ~~ $extreme2 )
          {
            push ( @extreme2positions, $countpos );
          }
          $countpos++;
        }
        push ( @surfdata, [ [ $extreme1, [ @extreme1positions ] ], [ $extreme2, [ @extreme2positions ] ] ] );
        $count++;
      }
      else
      {
        if ( $surfdata[0][0][1] ~~ $surfdata[1][1][1] )
        {
          my $swap = $surfdata[1][1];
          $surfdata[1][1] = $surfdata[1][0];
          $surfdata[1][0] = $swap;
        }

        my $surfnum = $coordstyperef;
        push ( @surfdata, $surfnum );
      }
    }
    push (@surfsdata, [ @surfdata ] );
  }
  return ( @surfsdata );
}

sub makecoordsgrid
{
  # THIS FORMS A GRID OVER EACH REQUESTED SURFACE
  my ($extremesref, $resolutionsref, $dirsvectorsrefsref) = @_;
  my @extremesdata = @$extremesref;
  my @resolutions = @$resolutionsref;
  my @dirsvectorsrefs = @$dirsvectorsrefsref;
  my @wholegrid;
  my $countsurf = 0;
  foreach my $surfcase ( @extremesdata )
  {
    my $dirsvectorsref = $dirsvectorsrefs[$countsurf];
    my @surfdata = @$surfcase;
    my $surf = pop @surfdata;
    my @coordspoints;
    my $count = 0;
    foreach ( @surfdata )
    {
      my $extreme1 = $_->[0][0];
      my $extreme2 = $_->[1][0];
      my @extreme1positions = @{$_->[0][1]};
      my @extreme2positions = @{$_->[1][1]};
      my $resolution = $resolutions[$counter];
      my $diffextremes = ( $extreme1 - $extreme2 );
      my $variation = ( $diffextremes / ( $resolution + 1) );
      my @coordpoints;
      my $othercount = 1;
      while ( $othercount < ( $resolution +1 ) )
      {
        my $compoundvariation = ( $variation * $othercount );
        my $coordvalue = ( $extreme2 + $compoundvariation );
        push ( @coordpoints, $coordvalue );
        $othercount++;
      }
      push ( @coordspoints, [ @coordpoints ] );
      $count++;
    }
    push ( @coordspoints, $surf, $dirsvectorsref );
    push ( @wholegrid, [ @coordspoints ] );
    $countsurf++;
  }
  return(@wholegrid);
}

sub makegrid
{ # THIS CONVERTS THE GRID DATA IN VERTEX FORM
  my @gridcoords = @_;
  my @gridsvertices;
  foreach my $surfdataref ( @gridcoords )
  {
    my @xyzcoords;
    my @surfdata = @$surfdataref;
    my @xdata = @{$surfdata[0]};
    my @ydata = @{$surfdata[1]};
    my @zdata = @{$surfdata[2]};
    my $surf = $surfdata[3];
    my $dirvectorsref = $surfdata[4];
    my $counter = 0;
    my @gridvertices;
    my ( @groups, @xyzdata );
    foreach my $xdatum (@xdata)
    {
      my $ydatum = $ydata[$counter];
      push ( @xyzdata, [ $xdatum, $ydatum ] );
      $counter++;
    }
    foreach my $elt (@xyzdata)
    {
      foreach my $zdatum ( @zdata )
      {
        my @group = @$elt;
        push ( @group, $zdatum );
        push ( @groups, [ @group ] );
      }
    }
    push ( @gridvertices, [ @groups ], $surf, $dirvectorsref );
    push ( @gridsvertices, [ @gridvertices ] );
  }
  return ( @gridsvertices );
}

sub adjust_dirvector
{  # THIS SCALES THE DIRECTION VECTORS TO EASE THE MANIPULATION OF THE GRIDS IN DETACHING THEM FROM THE SURFACES.
  my ( $vectorref, $distgrid ) = @_;
  my @vector = @$vectorref;
  my $denominator = ( 1 / $distgrid );
  my @adjusted_vector;
  foreach my $elt ( @vector )
  {
    my $adjusted_component = ( $elt / $denominator );
    $adjusted_component = sprintf ( "%.3f", $adjusted_component );
    push ( @adjusted_vector, $adjusted_component );
  }
  return ( @adjusted_vector );
}

sub adjustgrid
{  # THIS ADJUSTS THE GRIDS OF POINTS OVER THE REQUESTED SURFACES BY DETACHING THEM OF ABOUT 1 CM TOWARDS THE OUTSIDE.
  my ( $griddataref, $distgrid )  = @_;
  my @griddata = @$griddataref;
  my @adjustedsurfs;
  foreach my $elt ( @griddata )
  {
    my @surfdata = @$elt;
    my @vertexdatarefs = @{$surfdata[0]};
    my $surfnum = $surfdata[1];
    my @dirvector = @{$surfdata[2]};
    my @adjusted_dirvector = adjust_dirvector( \@dirvector, $distgrid );
    my @adjustedsurfs;
    foreach my $vertexref ( @vertexdatarefs )
    {
      my @vertexcoords = @$vertexref;
      my @adjustedvertex;
      $countcomp = 0;
      foreach my $el ( @vertexcoords )
      {
        my $component = $adjusted_dirvector[$countcomp];
        my $newel = ( $el + $component );
        push ( @adjustedvertex, $newel );
        $countcomp++;
      }
      push ( @adjustedsurfs, [ @adjustedvertex ] );
    }
    push ( @adjusteddata, [ [ @adjustedsurfs ], $surfnum, [ @dirvector ] ] );
  }
  return ( @adjusteddata );
}

sub treatshdfile
{ # THIS PREPARES THE SHDA FILES IN MEMORY FOR USE.
  my @lines = @_;
  my @newlines;
  my $count = 0;
  foreach my $line ( @lines )
  {
    my $lineafter = $lines[ $count + 1 ];
    my $linebefore = $lines[ $count - 1 ];
    my $linecontrol = $line;
    if ( ( $lineafter =~ /# direct - surface/ ) or ( $lineafter =~ /# diffuse - surface/ ) )
    {
      $linecontrol = "";
    }
    elsif ( ( $line =~ /# direct - surface/ ) or ( $line =~ /# diffuse - surface/ ) )
    {
      chomp $linebefore;
      $line = "$linebefore" . " " . "$line" ;
    }

    unless ( $linecontrol eq "" )
    {
      push ( @newlines, $line );
    }
    $count++;
  }
  return ( @newlines );
}

sub defendshd # CURRENTLY UNUSED
{
  my ( $shdfile ) = @_;
  my $shdfilebak = $shdfile . ".bak";

  my $shdfile_f5 = $shdfile;
  $shdfile_f5 = s/\.shd// ;
  $shdfile_f5 = $shdfile_f5 . "_f5.shd";
  $shdfile_f5bak = $shdfile_f5 . ".bak";

  if ( -e $shdfile )
  {
    `cp -f $shdfile $shdfilebak`;
    say REPORT "cp -f $shdfile $shdfilebak";
  }

  if ( -e $shdfile_f5 )
  {
    `cp -f $shdfile_f5 $shdfile_f5bak`;
    say REPORT "cp -f $shdfile_f5 $shdfile_f5bak";
  }
}

sub restoreshd # CURRENTLY UNUSED
{
  my ( $shdfile ) = @_;
  my $shdfilebak = $shdfile . ".bak";

  my $shdfile_f5 = $shdfile;
  $shdfile_f5 = s/\.shd// ;
  $shdfile_f5 = $shdfile_f5 . "_f5.shd";
  $shdfile_f5bak = $shdfile_f5 . ".bak";

  if ( -e $shdfilebak )
  {
    `mv -f $shdfilebak $shdfile`;
    say REPORT "mv -f $shdfilebak $shdfile";
  }

  if ( -e $shdfile_f5bak )
  {
    `mv -f $shdfile_f5bak $shdfile_f5`;
    say REPORT "mv -f $shdfile_f5bak $shdfile_f5";
  }
}

sub readshdfile
{ # THIS READS THE RELEVANT CONTENT OF THE SHDA FILE.
  my ( $shdfile, $calcprocedures_ref, $conffile, $paths_ref, $zonenum, $zonenumname_ref, $mymonth, $message ) = @_;
  my %paths = %{ $paths_ref };
  my @calcprocedures = @{ $calcprocedures_ref };
  my %zonenumname = %{ $zonenumname_ref };
  my $zoneletter = $zonenumname{$zonenum};
  say MONITOR "4,5 SHDFILE: $shdfile";

  my $mymonthname;
  if ( "embedded" ~~ @calcprocedures )
  {
    $mymonthname = getmonthname($mymonth);
  }

  my $shdafile = $shdfile . "a";

    if ( ( "keepdirshdf" ~~ @calcprocedures ) and ( $message eq "go" ) )
    {

      #if ( "embedded" ~~ @calcprocedures )
      #{
      #  defendshd( $shdfile );
      #}

`cd $paths{cfgpath} \n prj -file $paths{rootconffile} -mode script<<YYY
m
c
f
b
$zoneletter
-
-
-
-
-
-
YYY
`;

say REPORT "cd $paths{cfgpath} \n prj -file $paths{rootconffile} -mode script<<YYY
m
c
f
b
$zoneletter
-
-
-
-
-
-
YYY
";

    #if ( "embedded" ~~ @calcprocedures )
    #{
    #  restoreshd( $shdfile );
    #}
  }


  say MONITOR "6 SHDAFILE_: $shdafile";
  if ( ( not ( -e $shdafile ) ) and ( not ( "embedded" ~~ @calcprocedures ) ) )
  {
    say MONITOR "\nExiting. A file \".shda\" must be present in the model folders for the operation to be performed. Now it isn't. To obtain that, a shading and insolation calculation must have been performed.";
    say "\nExiting. A file \".shda\" must be present in the model folders for the operation to be performed. Now it isn't. To obtain that, a shading and insolation calculation must have been performed." and die;
  }

  if ( "bbembedded" ~~ @calcprocedures )
  {
    #defendshd( $shdfile );

`cd $paths{cfgpath} \n ish -file $paths{rootconffile} -mode text<<YYY
b
$zoneletter
-
YYY
`;

say REPORT "cd $paths{cfgpath} \n ish -file $paths{rootconffile} -mode text<<YYY
b
$zoneletter
-
YYY
";

    #restoreshd( $shdfile );
  } ###ZZZ NOT ACTIVE

  my $tempfile = $shdafile;
  $tempfile =~ s/\.shda/\.temp\.shda/ ;

  open ( SHDAFILE, "$shdafile");   # open ( SHDAFILE, "$shdafile") or die;
  my @shdalines = <SHDAFILE>;
  close SHDAFILE;

  my (@filearray, @rawlines, @months);

  if ( "embedded" ~~ @calcprocedures )
  {
    foreach my $line ( @shdalines )
    {
      if ( $line =~ /\* month:/ )
      {
        $line =~ s/(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)/$mymonthname/ ;
      }
      push ( @rawlines, $line );
    }
  }
  else
  {
    foreach my $line ( @shdalines )
    {
      push ( @rawlines, $line );
    }
  }

  my @treatedlines = treatshdfile ( @rawlines );

  foreach my $line ( @treatedlines )
  { # THIS READS THE ".shda" FILES.
    my @elts = split(/\s+|,/, $line);
    if ( $line =~ /\* month:/ )
    {
      if ( not ( "embedded" ~~ @calcprocedures ) )
      {
        my $month = $elts[2];
        push ( @months, $month );
      }
      elsif ( "embedded" ~~ @calcprocedures )
      {
        $elts[2] = $mymonthname;
        push ( @months, $mymonthname );
      }
    }
    push ( @filearray, [ @elts ] );
  }

  open ( TEMP , ">$tempfile" ) or die;
  foreach my $line ( @treatedlines )
  {
    print TEMP $line;
  }
  close TEMP;

  return ( \@treatedlines, \@filearray, \@months );
}

sub tellsurfnames
{ # THIS RETURNS THE NAMES OF THE SURFACES.
  my ( $transpsurfsref, $geodataref ) = @_;
  my @transpsurfs = @$transpsurfsref;
  my @geodata = @$geodataref;
  my ( @containernums, @containernames, @nums, @names );
  my $count = 0;
  foreach my $surf ( @transpsurfs )
  {
    foreach my $rowref ( @geodata )
    {
      my @row = @$rowref;
      if ( ( $surf eq $row[12] ) and ( $row[0] eq "*surf" ) )
      {
        push ( @nums, $surf );
        push ( @names, $row[1] );
      }
      $count++;
    }
  }
  return ( \@nums, \@names );
}

sub getsurfshd
{ # THIS RETURN ALL THE DATA NEEDED FROM THE ".shda" FILE.
  my ( $shdfilearrayref, $monthsref, $surfnumsref, $surfnamesref ) = @_;
  my @shdfilearray = @$shdfilearrayref;
  my @months = @$monthsref;
  my @surfnums = @$surfnumsref;
  my @surfnames = @$surfnamesref;

  my @yearbag;
  foreach my $month ( @months )
  {
    my $semaphore = 0;
    my @monthbag;
    foreach my $rowref ( @shdfilearray )
    {
      my @row = @$rowref;
      if ( ( $row[0] eq "*") and ( $row[1] eq "month:" ) and ( $row[2] eq "$month" ) )
      {
        $semaphore = 1;
      }
      elsif ( ( $row[0] eq "*") and ( $row[1] eq "month:" ) and ( not ( $row[2] eq "$month" ) ) )
      {
        $semaphore = 0;
      }
      foreach my $surfname ( @surfnames )
      {
        if ( ( $row[25] eq "diffuse") and ( $row[27] eq "surface") and ( $row[28] eq "$surfname" ) and ( $semaphore == 1 ) )
        {
          push ( @monthbag, [ [ @row[0..23] ], $surfname ] );
        }
      }
    }
    push ( @yearbag, [ [ @monthbag ], $month ] );
  }
  return ( @yearbag );
}

sub checklight
{ # THIS LOOKS INTO THE "shda" DATA AND SEES WHEN DAYLIGHTING IS PRESENT.
  my ( $shdfilearrayref, $monthsref ) = @_;
  my @shdfilearray = @$shdfilearrayref;
  my @months = @$monthsref;

  my @yearbag;
  foreach my $month ( @months )
  {
    my @monthbag;
    my $countrow = 0;
    my $semaphore = 0;
    foreach my $rowref ( @shdfilearray )
    {
      my @row = @$rowref;
      if ( ( $row[0] eq "*") and ( $row[1] eq "month:" ) and ( $row[2] eq "$month" ) )
      {
        $semaphore = 1;
      }
      elsif ( ( $row[0] eq "*") and ( $row[1] eq "month:" ) and ( not ( $row[2] eq "$month" ) ) )
      {
        $semaphore = 0;
      }

      if ( ( $row[0] eq "surfaces") and ( $row[1] eq "insolated") and ( $semaphore == 1 ) )
      {
        my @bag;

        foreach my $el ( @{ $shdfilearray[ $countrow + 1 ] } )
        {
          if ( $el < 0 )
          {
            $el = 1;
            push ( @bag, $el );
          }
          else
          {
            $el = 0;
            push ( @bag, $el );
          }
        }
        push ( @monthbag, [ @bag ] );
      }
      $countrow++;
    }
    push ( @yearbag, [ [ @monthbag ], $month ] );
  }
  return ( @yearbag );
}

sub tellradfilenames
{ # THIS RETURNS THE NAMES OF THE RADIANCE FILES.
  my ( $path, $conffile_f1, $conffile_f2, $conffile_f3, $conffile_f4,
    $conffile_f5, $conffile_f6, $conffile_f7, $conffile_f8, $conffile_f9, $conffile_f10, $conffile_f11, $paths_ref ) = @_;
  my %paths =%{ $paths_ref };
  my @confs = ( $conffile_f1, $conffile_f2, $conffile_f3, $conffile_f4, $conffile_f5, $conffile_f6, $conffile_f7,
    $conffile_f8, $conffile_f9, $conffile_f10, $conffile_f11 );
  my @result;
  foreach my $conf ( @confs )
  {
    my $confstripped = $conf;

    if ( $confstripped =~ /$path\/cfg\// )
    {
      $confstripped =~ s/$path\/cfg\///;
    }
    else
    {
      $confstripped =~ s/$path\///;
    }

    $confstripped =~ s/.cfg//;
    my $radoctfile = "$confstripped" . "_Extern.oct";
    my $rcffile = "$confstripped" . ".rcf" ;
    push ( @result, [ $conf, $radoctfile, $rcffile ] );
  }
  return( @result );
}

sub tellradnames
{
  my ( $conffile, $path, $radpath ) = @_;
  my $confroot = $conffile;

  if ( $confroot =~ /$path\/cfg\// )
  {
    $confroot =~ s/$path\/cfg\/// ;
  }
  else
  {
    $confroot =~ s/$path\/// ;
  }

  $confroot =~ s/\.cfg$// ;
  my $fileroot = "$path/$confroot";
  my $rcffile = "$radpath/$confroot.rcf" ;
  my $radoctfile = "$radpath/$confroot" . "_Extern.oct";
  my $riffile = $radoctfile;
  $riffile =~ s/\.oct$/\.rif/ ;
  my $skyfile = $radoctfile;
  $skyfile =~ s/\.oct$/\.sky/ ;
  my $radmatfile = $radoctfile;
  $radmatfile =~ s/\.oct$/\.mat/ ;
  my $radmatcopy = $radmatfile . ".copy";
  my $diffskyfile = $skyfile;
  $diffskyfile =~ s/\.sky$/_diff\.sky/ ;
  my $blackmatfict = $radoctfile;
  $blackmatfict =~ s/\.oct$/\.mat_blackbak/ ;

  my $extradfile = "$radpath/$confroot" . "_Extern-out.rad";
  my $inradfile = "$radpath/$confroot" . "_Extern-in.rad";
  my $glzradfile = "$radpath/$confroot" . "_Extern-glz.rad";

  return ( $fileroot, $rcffile, $radoctfile, $riffile, $skyfile, $radmatfile, $radmatcopy, $diffskyfile,
    $blackmatfict, $extradfile, $inradfile, $glzradfile );
}

sub adjustlaunch
{
  my ( $skyfile, $diffskyfile, $path, $radpath ) = @_;

  $skyfile_short = $skyfile;
  $skyfile_short =~ s/$radpath\///;
  $diffskyfile_short = $diffskyfile;
  $diffskyfile_short =~ s/$radpath\///;

  open( SKYFILE, "$skyfile" ) or die "Can't open $skyfile $!";
  my @lines = <SKYFILE>;
  close SKYFILE;
  open( DIFFSKYFILE, ">$diffskyfile" ) or die "$!";
  foreach my $line ( @lines )
  {
    $line =~ s/^3 (.+)$/3 0 0 0/ ;
    $line =~ s/^4 (.+)$/4 0 0 0 0/ ;
    print DIFFSKYFILE $line;
  }
  close DIFFSKYFILE;

  my $oldskyfile = $skyfile . ".old";
  `mv -f $skyfile $oldskyfile`;
  "mv -f $skyfile $oldskyfile\n";
  `mv -f $diffskyfile $skyfile`;
  print REPORT "mv -f $diffskyfile $skyfile\n";
}

sub setrad
{
  # THIS CREATES THE RADIANCE SCENES.
  my ( $conffile, $radoctfile, $rcffile, $path, $radpath, $monthnum, $day, $hour, $countfirst, $exportconstrref,
    $exportreflref, $skycondition_ref, $countrad, $specularratios_ref, $calcprocedures_ref, $debug, $paths_ref,
    $groundrefl, $count, $shdfile, $d_ref ) = @_;
    my $message;
    my %d = %{ $d_ref };

    if ( $conffile =~ /_f1\./ )
    {
      $message = "Model with black obstructions.";
    }
    elsif ( $conffile =~ /_f2/ )
    {
      $message = "Model with reflective obstructions.";
    }
    elsif ( $conffile =~ /_f3/ )
    {
      $message = "Model with obstructions reflective in the solar range of light.";
    }
    elsif ( $conffile =~ /_f4/ )
    {
      $message = "Model with obstructions reflective in the infrared solar range.";
    }
    elsif ( $conffile =~ /_f5/ )
    {
      $message = "Model without obstructions.";
    }
    elsif ( $conffile =~ /_f6/ )
    {
      $message = "Black model with black obstructions and black ground.";
    }
    elsif ( $conffile =~ /_f7/ )
    {
      $message = "Black model without obstructions and with black ground.";
    }
    elsif ( $conffile =~ /_f8/ )
    {
      $message = "Black model with black obstructions.";
    }
    elsif ( $conffile =~ /_f9/ )
    {
      $message = "Model with black obstructions and black ground.";
    }
    elsif ( $conffile =~ /_f10/ )
    {
      $message = "Model without obstructions and with black ground.";
    }
    elsif ( $conffile =~ /_f11/ )
    {
      $message = "Black model without obstructions.";
    }

    my $action ;
    if ( $conffile =~ /_f1\./ )
    {
      $action = 1;
    }
    elsif ( $conffile =~ /_f2/ )
    {
      $action = 2;
    }
    elsif ( $conffile =~ /_f3/ )
    {
      $action = 3;
    }
    elsif ( $conffile =~ /_f4/ )
    {
      $action = 4;
    }
    elsif ( $conffile =~ /_f5/ )
    {
      $action = 5;
    }
    elsif ( $conffile =~ /_f6/ )
    {
      $action = 6;
    }
    elsif ( $conffile =~ /_f7/ )
    {
      $action = 7;
    }
    elsif ( $conffile =~ /_f8/ )
    {
      $action = 8;
    }
    elsif ( $conffile =~ /_f9/ )
    {
      $action = 9;
    }
    elsif ( $conffile =~ /_f10/ )
    {
      $action = 10;
    }
    elsif ( $conffile =~ /_f11/ )
    {
      $action = 11;
    }

    unless ( "embedded" ~~ @calcprocedures )
    {
      say "\nConfiguration file: $conffile"; say REPORT "\$conffile: $conffile";
      say "$message";
    }

  my %paths = %{ $paths_ref };
  my %skycondition = %$skycondition_ref;
  my @calcprocedures = @$calcprocedures_ref;
  if ( $debug == 1 )
  {
    $debugstr = ">>out.txt";
  }
  else
  {
    $debugstr = "";
  }

  my $skycond = $skycondition{$monthnum};

  my $radoctroot = $radoctfile;
  $radoctroot =~ s/$radoctfile/\.oct/ ;

  my $shortrcffile = $rcffile;
  $shortrcffile =~ s/$radpath\/// ;

  my $skyfile = $rcffile;
  $skyfile =~ s/rif$/sky/ ;

  my $riffile = $rcffile;
  $riffile =~ s/\.rcf$/_Extern\.rif/ ;

  my $shortriffile = $riffile;
  $shortriffile =~ s/$radpath\/// ;

  my $add;
  if ( $skycond eq "cloudy" ) { $add = "\nf"; }
  if ( $skycond eq "overcast" ) { $add = "\nf\nf"; }

  my $moment;

  if ( ( ( $monthnum == 12 ) or ( $monthnum == 1 ) or ( $monthnum == 11 ) or ( $monthnum == 2 ) ) and ( $hour < 11 ) )
  { $moment = "a"; }
  elsif ( ( ( $monthnum == 12 ) or ( $monthnum == 1 ) or ( $monthnum == 11 ) or ( $monthnum == 2 ) ) and ( ( $hour == 11 ) or ( $hour == 12 ) or ( $hour == 13 ) ) )
  { $moment = "b"; }
  elsif  ( ( ( $monthnum == 12 ) or ( $monthnum == 1 ) or ( $monthnum == 11 ) or ( $monthnum == 2 ) ) and ( $hour > 13 ) )
  { $moment = "c"; }
  elsif ( ( ( $monthnum == 3 ) or ( $monthnum == 4 ) or ( $monthnum == 9 ) or ( $monthnum == 10 ) ) and ( $hour < 11 ) )
  { $moment = "d"; }
  elsif ( ( ( $monthnum == 3 ) or ( $monthnum == 4 ) or ( $monthnum == 9 ) or ( $monthnum == 10 ) ) and ( ( $hour == 11 ) or ( $hour == 12 ) or ( $hour == 13 ) ) )
  { $moment = "e"; }
  elsif  ( ( ( $monthnum == 3 ) or ( $monthnum == 4 ) or ( $monthnum == 9 ) or ( $monthnum == 10 ) ) and ( $hour > 13 ) )
  { $moment = "f"; }
  elsif  ( ( ( $monthnum == 5 ) or ( $monthnum == 6 ) or ( $monthnum == 7 ) or ( $monthnum == 8 ) ) and ( $hour < 11 ) )
  { $moment = "g"; }
  elsif  ( ( ( $monthnum == 5 ) or ( $monthnum == 6 ) or ( $monthnum == 7 ) or ( $monthnum == 8 ) ) and ( ( $hour == 11 ) or ( $hour == 12 ) or ( $hour == 13 ) ) )
  { $moment = "h"; }
  elsif  ( ( ( $monthnum == 5 ) or ( $monthnum == 6 ) or ( $monthnum == 7 ) or ( $monthnum == 8 ) ) and ( $hour > 13 ) )
  { $moment = "i"; }

  if ( not ( -e "$paths{cfgpath}" ) )
  {
    `mkdir $paths{cfgpath}`;
    print REPORT "mkdir $paths{cfgpath}";
  }

  #if ( "embedded" ~~ @calcprocedures )
  #{
  #  defendshd( $shdfile );
  #}


  my $cfgpath = $paths{cfgpath};
  my $radpath = $paths{radpath};
  my $shortconffile = $conffile;
  $shortconffile =~ s/$cfgpath\///;
  my $shortrcffile = $rcffile;
  $shortrcffile =~ s/$radpath\///;
  my $shortriffile = $riffile;
  $shortriffile =~ s/$radpath\///;
  my $parproc = $d{parproc};


`cd $paths{cfgpath}
rm fort.*
e2r -file $shortconffile -mode text <<YYY
c
a
a
a
$moment
1
n
d

d
$day $monthnum $hour
g
-
h
c
a
d
a
f
c
h
y
>
$shortriffile
u
$parproc
-
-
YYY
`;

say REPORT "cd $paths{cfgpath}
rm fort.*
e2r -file $shortconffile -mode text <<YYY
c
a
a
a
$moment
1
n
d

d
$day $monthnum $hour
g
-
h
c
a
d
a
f
c
h
y
>
$shortriffile
u
$parproc
-
-
YYY
.Done this.
";

  #if ( "embedded" ~~ @calcprocedures )
  #{
  #  restoreshd( $shdfile );
  #}


  my $groundroughn;
  if ( "grounddirdiff" ~~ @calcprocedures )
  {
    foreach my $el ( @calcprocedures )
    {
      if ( $el =~ /^groundspecroughness/ )
      {
        my @es = split( ":", $el );
        $groundspec = $es[1];
        $groundroughn = $es[2];
      }
    }
  }

  adjust_radmatfile( $exportconstrref, $exportreflref, $conffile, $path, \@specularratios,
    \%obslayers, \@selectives, \%paths, \@calcprocedures, $count, $groundrefl, $action, "no" );
}

sub setroot
{ # THIS SETS THE MODELS' ROOT NAME.
  my ( $conffile, $path, $debug, $paths_ref, $calcprocedures_ref, $shdfile ) = @_;
  my %paths = %{ $paths_ref };
  my @calcprocedures = @{ $calcprocedures_ref };
  my $rootname = $conffile;
  if ( $rootname =~ /$path\/cfg\// )
  {
    $rootname =~ s/$path\/cfg\///;
  }
  else
  {
    $rootname =~ s/$path\///;
  }

  $rootname =~ s/\.cfg//;
  if ( $debug == 1 )
  {
    $debugstr = ">>out.txt";
  }
  else
  {
    $debugstr = "";
  }

  $paths{rootconffile} = $rootname . ".cfg";

  #if ( "embedded" ~~ @calcprocedures )
  #{
  #  defendshd( $shdfile );
  #}

`cd $paths{cfgpath}
prj -file $paths{rootconffile} -mode text $debugstr <<YYY

s

$rootname

m
c
b
#
y
-
-
-
-

YYY
`;

print REPORT "cd $paths{cfgpath}
prj -file $paths{rootconffile} -mode text $debugstr <<YYY

s

$rootname

m
c
b
#
y
-
-
-
-

YYY
";

  #if ( "embedded" ~~ @calcprocedures )
  #{
  #  restoreshd( $shdfile );
  #}

}

sub populatelight
{ # THIS POPULATES THE DATA STRUCTURE DEDICATED TO SIGNAL THE DAYLIT HOURS.
  my @daylighthoursarr = @_;
  my %daylighthours;
  my $count = 0;
  foreach my $monthref ( @daylighthoursarr )
  {
    my @monthdata = @$monthref;
    my $month = $monthdata[1];
    $month =~ s/`//g;
    my @lithours = @{$monthdata[0][0]};
    $daylighthours{$month} = [ @lithours ] ;
    $count++;
  }
  return ( %daylighthours );
}


sub deg2rad
{
	my $degrees = shift;
	return ( ( $degrees / 180 ) * 3.14159265358979 );
}

sub rad2deg
{
	my $radians = shift;
	return ( ( $radians / 3.14159265358979 ) * 180 ) ;
}

sub rotate2d
{   # SELF-EXPLAINING.
    my ( $x, $y, $angle ) = @_;
    $angle = deg2rad( $angle );
    my $x_new = cos($angle)*$x - sin($angle)*$y;
    my $y_new = sin($angle)*$x + cos($angle)*$y;
  return ( $x_new, $y_new);
}

sub getdirvectors
{ # THIS GETS THE NEEDED DIRECTION VECTORS AT EACH GRID POINT DEPENDING FROM THE LAUNCH SETTINGS.
   ( $basevectorsref, $dirvectorref, $pointcoordsref ) = @_;
   my @basevectors = @$basevectorsref;;
   my @dirvector = @$dirvectorref;
   my @topcoords = @{$basevectors[0]};
   my @newdirvectors;
   my $xbase = $topcoords[0];
   my $ybase = $topcoords[1];
   my $zbase = $topcoords[2];
   my $xnew = $dirvector[0];
   my $ynew = $dirvector[1];
   my $znew = $dirvector[2];
   my $anglebasexz = acos($xbase);
   my $anglebaseyz = acos($zbase);
   my $anglenewxz = acos($xnew);
   my $anglenewyz = acos($znew);
   my $anglediffxz = ( $anglenewxz - $anglebasexz );
   my $anglediffyz = ( $anglenewyz - $anglebaseyz );
   foreach my $eltsref ( @basevectors )
   {
     my @elts = @$eltsref;
     my ( $x, $y, $z ) = @elts ;
     my ( $x_ok, $tempy ) = rotate2d( $x, $y, $anglediffxz );
     my ( $y_ok, $z_ok ) = rotate2d( $tempy, $z, $anglediffyz );
     $x_ok = sprintf ( "%.3f", $x_ok );
     $y_ok = sprintf ( "%.3f", $y_ok );
     $z_ok = sprintf ( "%.3f", $z_ok );
     push ( @newdirvectors, [ $x_ok, $y_ok, $z_ok ] );
   }
   return ( @newdirvectors );
}


sub pursue
{ # THIS CALCULATES THE IRRADIANCES BY THE MEANS OF RADIANCE.
  # RADIANCE EXAMPLE: echo 1 dat-0.01 2 0 -1 0 | rtrace  -I -ab 2 -lr 7 -h /home/luca/boxform/rad/boxform_f1_Extern.oct | rcalc -e '$1=179*(.265*$1+.670*$2+.065*$3)'
  $" = " ";
  my $dat = shift;
  my %d = %$dat;
  my $zonenum = $d{zonenum};
  my $geofile = $d{geofile};
  my %paths = %{ $d{paths} };
  my $constrfile = $d{constrfile};
  my $shdfile = $d{shdfile};
  my @gridpoints = @{ $d{gridpoints} };
  my @shdsurfdata = @{ $d{shdsurfdata} };
  my @daylighthoursarr = @{ $d{daylighthoursarr} };
  my %daylighthours =  %{ $d{daylighthours} };
  my @shdfilearray = @{ $d{shdfilearray} };
  my $exportconstrref = $d{exportconstrref};
  my $exportreflref = $d{exportreflref};
  my $conffile = $d{conffile};
  my $path = $d{path};
  my $radpath = $paths{radpath};
  my @basevectors = @{ $d{basevectors} };
  my @resolutions = @{ $d{resolutions} };
  my $dirvectorsnum = $d{dirvectorsnum};
  my @calcprocedures = @{ $d{calcprocedures} };
  my @specularratios = @{ $d{specularratios} };
  my $bounceambnum = $d{bounceambnum};
  my $bouncemaxnum = $d{bouncemaxnum};
  my ( $fict1ref, $fict2ref, $fict3ref, $fict4ref, $fict5ref, $fict6ref,
    $fict7ref, $fict8ref, $fict9ref, $fict10ref, $fict11ref ) = @{ $d{radfilesrefs} };
  my @surfsnums = @{ $d{transpsurfs} };
  my @selectives = @{ $d{selectives} };
  my $lat = $paths{lat};
  my $longdiff = $paths{longdiff};
  my $long = $paths{long};
  my $groundrefl = $paths{groundrefl};
  my $standardmeridian = $paths{standardmeridian};
  my $clmavgs = $paths{clmavgs};
  my @dirgridpoints = @{ $d{dirgridpoints} };
  my @boundbox = @{ $d{boundbox} };
  my $add = @{ $d{add} };
  my %dirdiffs = %{ $d{dirdiffs} };
  my $myday = $d{myday};
  my $mymonth = $d{mymonth};
  #say MONITOR "BEFORE PURSUE MYDAY $myday MYMONTH $mymonth";
  say REPORT "BEFORE PURSUE MYDAY $myday MYMONTH $mymonth \$zonenum $zonenum ";

  if ( $add eq "" ) { $add = " -aa 0.5 -dc .25 -dr 2 -ss 1 -st .05 -ds .004 -dt .002 -bv "; }

  my $parproc = $d{parproc};
  my $parpiece;
  if ( $parproc ne "" )
  {
    $parpiece = " -n $parproc ";
  }
  else
  {
    $parpiece = "";
  }

  $parpiece = $add . $parpiece ;

  my ( $conffile_f1, $radoctfile_f1, $rcffile_f1 ) = @$fict1ref;
  my ( $conffile_f2, $radoctfile_f2, $rcffile_f2 ) = @$fict2ref;
  my ( $conffile_f3, $radoctfile_f3, $rcffile_f3 ) = @$fict3ref;
  my ( $conffile_f4, $radoctfile_f4, $rcffile_f4 ) = @$fict4ref;
  my ( $conffile_f5, $radoctfile_f5, $rcffile_f5 ) = @$fict5ref;
  my ( $conffile_f6, $radoctfile_f6, $rcffile_f6 ) = @$fict6ref;
  my ( $conffile_f7, $radoctfile_f7, $rcffile_f7 ) = @$fict7ref;
  my ( $conffile_f8, $radoctfile_f8, $rcffile_f8 ) = @$fict8ref;
  my ( $conffile_f9, $radoctfile_f9, $rcffile_f9 ) = @$fict9ref;
  my ( $conffile_f10, $radoctfile_f10, $rcffile_f10 ) = @$fict10ref;
  my ( $conffile_f11, $radoctfile_f11, $rcffile_f11 ) = @$fict11ref;

  #my @conffiles = ( $conffile_f1, $conffile_f2, $conffile_f3, $conffile_f4, $conffile_f5, $conffile_f6, $conffile_f7,    $conffile_f8, $conffile_f9, $conffile_f10 );
  #
  #if ( scalar( @selectives) > 0)
  #{
  #  push ( @conffiles, $conffile_f3, $conffile_f4 );
  #}

  my ( @radoctfiles, @rcffiles, @cnums );

  if ( ( "composite" ~~ @calcprocedures ) and ( "groundreflections" ~~ @calcprocedures ) and not( "alldiff" ~~ @calcprocedures ))
  {
    @radoctfiles = ( $radoctfile_f1, $radoctfile_f2 );
    @conffiles = ( $conffile_f1, $conffile_f2 );
    @cnums = ( 0, 1 );
  }
  if ( ( "composite" ~~ @calcprocedures ) and ( "groundreflections" ~~ @calcprocedures ) )
  {
    @radoctfiles = ( $radoctfile_f1, $radoctfile_f2, $radoctfile_f6 );
    @conffiles = ( $conffile_f1, $conffile_f2, $conffile_f6 );
    @cnums = ( 0, 1, 5 );
  }
  elsif ( "composite" ~~ @calcprocedures )
  {
    @radoctfiles = ( $radoctfile_f1, $radoctfile_f2, $radoctfile_f6,  $radoctfile_f7, $radoctfile_f8, $radoctfile_f11 );
    @conffiles = ( $conffile_f1, $conffile_f2, $conffile_f6, $conffile_f7, $conffile_f8, $conffile_f11 );
    @cnums = ( 0, 1, 5, 6, 7, 10 );
  }
  elsif ( ( "radical" ~~ @calcprocedures ) and not( "alldiff" ~~ @calcprocedures ) )
  {
    @radoctfiles = ( $radoctfile_f2, $radoctfile_f5 );
    @conffiles = ( $conffile_f2, $conffile_f5 );
    @cnums = ( 1, 4 );
  }
  elsif ( "radical" ~~ @calcprocedures )
  {
    @radoctfiles = ( $radoctfile_f2, $radoctfile_f5, $radoctfile_f6, $radoctfile_f7 );
    @conffiles = ( $conffile_f2, $conffile_f5, $conffile_f6, $conffile_f7 );
    @cnums = ( 1, 4, 5, 6 );
  }
  elsif ( "noreflections" ~~ @calcprocedures )
  {
    @radoctfiles = ( $radoctfile_f6, $radoctfile_f7, $radoctfile_f8, $radoctfile_f11 );
    @conffiles = ( $conffile_f6, $conffile_f7, $conffile_f8, $conffile_f11 );
    @cnums = ( 5, 6, 7, 10 );
  }
  elsif ( scalar( @selectives ) > 0 )
  {
    @radoctfiles = ( $radoctfile_f1, $radoctfile_f3, $radoctfile_f4 );
    @conffiles = ( $conffile_f1, $conffile_f3, $conffile_f4 );
    @cnums = ( 0, 2, 3 );
  }
  elsif ( "plain" ~~ @calcprocedures )
  {
    @radoctfiles = ( $radoctfile_f1, $radoctfile_f2 );
    @conffiles = ( $conffile_f1, $conffile_f2 );
    @cnums = ( 0, 1 );
  }
  else
  {
    @radoctfiles = ( $radoctfile_f1, $radoctfile_f2 );
    @conffiles = ( $conffile_f1, $conffile_f2 );
    @cnums = ( 0, 1 );
  }

  my $radoctnum = ( scalar( @radoctfiles ) - 1 );

  my $resolnumber = ( $resolutions[0] * $resolutions[1] );

  my $fileroot = $radoctfile_f2;
  $fileroot =~ s/_f2_Extern.oct//;
  my ( %irrs, %irrs_dir, %irrs_amb);
  my $setoldfiles = "on";
  my ( %surftests, %surftestsdiff, %surftestsdir, %irrs );

  my ( $totaldirect, $totalrad, $directratio, $diffuseratio );


  my ( $t_ref, $clmlines_ref, %t, @clmlines, %avgs );
  if ( ( "getweather" ~~ @calcprocedures ) and ( not ( "embedded" ~~ @calcprocedures ) ) )
  {
    if ( not( -e $clmavgs ) )
    {
      ( $t_ref, $clmlines_ref ) = getsolar( \%paths );
      %t = %{ $t_ref };
      @clmlines = @{ $clmlines_ref };
    }

    if ( ( scalar( @clmlines ) == 0 ) )
    {
      open( CLMAVGS, "$clmavgs" ) or die;
      @clmlines = <CLMAVGS>;
      close CLMAVGS;
    }

    foreach my $clmline ( @clmlines )
     {
       chomp $clmline;
       my @es = split( ",", $clmline );
       $avgs{dir}{$es[0]}{$es[2]} = $es[3];
       $avgs{diff}{$es[0]}{$es[2]} = $es[4];
       $avgs{alt}{$es[0]}{$es[2]} = $es[5];
       $avgs{azi}{$es[0]}{$es[2]} = $es[6];
     }
  }

  my $countrad;
  my $count = 0;
  foreach my $radoctfile ( @radoctfiles )
  {
    my $countrad = $cnums[$count];
    my $conffile = $conffiles[$count];
    my ( $fileroot, $rcffile, $radoctfile, $riffile, $skyfile, $radmatfile, $radmatcopy, $diffskyfile, $blackmatfict,
      $extradfile, $inradfile, $glzradfile ) =
    tellradnames( $conffile, $path, $radpath );

    if ( ( "blackground" ~~ @calcprocedures ) and ( $countrad == 0 ) )
    {
      $groundrefl = 0.000;
    }
    else
    {
      $groundrefl = $paths{groundrefl};
    }

    setroot( $conffile, $path, $debug, \%paths, \@calcprocedures, $shdfile );
    setrad( $conffile, $radoctfile, $rcffile, $path, $radpath, 3,
      15, 12, $countfirst, $exportconstrref, $exportreflref, \%skycondition, $countrad,
      \@specularratios, \@calcprocedures, $debug, \%paths, $groundrefl,
      $count, $shdfile, \%d );
    $count++;
  }

  my $radoctnum = ( scalar( @radoctfiles ) - 1 );
  my $countfirst = 0;
  my $count = 0;
  my $countrad;
  foreach my $radoctfile ( @radoctfiles )
  {
    my $countrad = $cnums[$count];
    my $conffile = $conffiles[$count];

    my ( $fileroot, $rcffile, $radoctfile, $riffile, $skyfile, $radmatfile, $radmatcopy, $diffskyfile, $blackmatfict,
      $extradfile, $inradfile, $glzradfile ) =
    tellradnames( $conffile, $path, $radpath );

    my $riffileold = $riffile . ".old";

    `cp $riffile $riffileold`;
    say REPORT "cp $riffile $riffileold";
    open( RIFFILE, "$riffile" ) or die;
    my @riflines = <RIFFILE>;
    close RIFFILE;

    open( RIFFILE, ">$riffile" ) or die;
    foreach my $rifline ( @riflines )
    {
      if ( $rifline =~ /^ZONE=/ )
      {
        # $rifline = "ZONE= Exterior     $boundbox[0]    $boundbox[1]    $boundbox[2]    $boundbox[3]    $boundbox[4]    $boundbox[5]\n";
      }
      elsif ( $rifline =~ /INDIRECT=/ )
      {
        $rifline = "INDIRECT= 2 \n";
      }
      elsif ( $rifline =~ /PENUMBRAS=/ )
      {
        $rifline = "PENUMBRAS= True \n";
      }
      elsif ( $rifline =~ /DETAIL=/ )
      {
        $rifline = "DETAIL= Low \n";
      }
      elsif ( $rifline =~ /QUALITY=/ )
      {
        $rifline = "QUALITY= Low \n";
      }
      elsif ( $rifline =~ /render=/ )
      {
        $rifline = "render=  -av 0 0 0 \n";
      }
      print RIFFILE $rifline;
    }
    close RIFFILE;

    my $countmonth = 0;
    foreach my $monthsdataref ( @daylighthoursarr )
    {
      my @monthsdata = @$monthsdataref;
      my @hourslight = @{$monthsdata[0][0]};
      my $month = $monthsdata[1];
      $month =~ s/`//g;
      my $monthnum = getmonthnum( $month );
      my $day = $days_inmonths{ $month };

      my $countsurf = 0;
      foreach my $surfref ( @gridpoints )
      {
        my @surfdata = @$surfref;
        my @pointrefs = @{ $surfdata[0] };
        my $surfnum = $surfdata[1];
        my @dirvector = @{ $surfdata[2] };
        my ( $dirvectorx, $dirvectory, $dirvectorz ) = @dirvector;
        my $countlithour = 0;
        my $counthour = 0;
        foreach my $hourlight ( @hourslight )
        {

          if ( $hourlight != 1 )
          {
            $countlithour++;
            my $hour = ( $counthour + 1) ;

            my ( $dir, $diff );
            if ( "getweather" ~~ @calcprocedures )
            {
              if ( "embedded" ~~ @calcprocedures )
              {
                $dir = $dirdiffs{$mymonth}{$myday}{$hour}{dir};
                $diff = $dirdiffs{$mymonth}{$myday}{$hour}{diff};
                #say MONITOR "FOR MONTH $mymonth DAY $myday HOUR $hour GOTTEN DIR $dir DIFF $diff";
                say REPORT "FOR MONTH $mymonth DAY $myday HOUR $hour GOTTEN DIR $dir DIFF $diff";
              }
              else
              {
                $dir = $avgs{dir}{$monthnum}{$hour};
                $diff = $avgs{diff}{$monthnum}{$hour};
              }
            }

            my ( $alt, $azi );
            if ( "embedded" ~~ @calcprocedures )
            {
              ( $alt, $azi ) = getaltaz( $myday, $mymonth, \%paths, $hour );
              #say MONITOR " FOR $\myday $myday \$mymonth $mymonth \%paths " . dump(%paths) . " \$hour $hour GOT ALT $alt AZI $azi";
              say REPORT " FOR $\myday $myday \$mymonth $mymonth \%paths " . dump(%paths) . " \$hour $hour GOT ALT $alt AZI $azi";
            }
            else
            {
              $alt = $avgs{alt}{$monthnum}{$hour};
              $azi = $avgs{azi}{$monthnum}{$hour};
            }

            my $countpoint = 0;

            if ( ( $countmonth == 0 ) and ( $countsurf == 0 ) and ( $countlithour == 1 ) )
            {

              if ( $countrad >= 1 )
              {
                my ( $conffileother1, $filerootother1, $rcffileother1, $radoctfileother1,
                $riffileother1, $skyfileother1, $radmatfileother1, $radmatcopyother1, $diffskyfileother1 );

                say REPORT "cp -f $radmatfile $radmatcopy";
                `cp -f $radmatfile $radmatcopy`;

                open ( FIXLIST, ">$path/rad/fixl.pl" ) or die( $! );
                print FIXLIST "$radmatfile\n";
                print FIXLIST "$radmatcopy\n";
                close FIXLIST;
              }
              $setoldfiles = "off";
            }

            ## HERE THE CALLS FOR THE DIFFUSE IRRADIANCES FOLLOW.
            unless ( ( "gendaylit" ~~ @calcprocedures ) or ( "gensky" ~~ @calcprocedures ) )
            {
              setrad( $conffile, $radoctfile, $rcffile, $path, $radpath, $monthnum,
                $day, $hour, $countfirst, $exportconstrref, $exportreflref, \%skycondition, $countrad,
                \@specularratios, \@calcprocedures, $debug, \%paths, $groundrefl,
                $count, $shdfile, \%d );
            }

            my $skycond = $skycondition{$monthnum};

            #say MONITOR "\@calcprocedures = " . dump( @calcprocedures );
            if ( not( "alldiff" ~~ @calcprocedures )
                  and ( ( "getweather" ~~ @calcprocedures )
                    or ( ( "gensky" ~~ @calcprocedures ) and ( "altcalcdiff" ~~ @calcprocedures ) )
                    or ( ( "gendaylit" ~~ @calcprocedures ) and ( "altcalcdiff" ~~ @calcprocedures ) ) ) ) # IF CONDITION, PRODUCE LIGHT WITH NO SUN TO CHECK DIFFUSE RADIATION
            {
              my ( @returns, $altreturn );
              if ( ( ( "radical" ~~ @calcprocedures ) and ( ( $countrad == 1 ) or ( $countrad == 4 ) ) )
                  or ( ( ( ( "composite" ~~ @calcprocedures ) and ( "groundreflections" ~~ @calcprocedures ) )
                    and ( ( $countrad == 0 ) or ( $countrad == 1 ) ) ) )
                  or ( ( "composite" ~~ @calcprocedures )
                    and ( ( $countrad == 0 ) or ( $countrad == 1 ) ) ) )
              {

                if ( $alt <= 0 ) { $alt = 0.0001; say REPORT "IMPOSED \$alt = 0.0001;"; } # say "IMPOSED \$alt = 0.0001;"; #IMPORTANT: THIS SETS THE ALTITUDE > 0 OF A TINY AMOUNT IF IT IS < 0 DUE TO THE FACT
                # THAT THE MAJORITY OF THAT HOUR THE SUN WAS BELOW THE HORIZON, WHILE THE NET GAINED AMOUNT OF RADIATION WAS STILL > 0.

                if ( ( "getweather" ~~ @calcprocedures ) and ( "getsimple" ~~ @calcprocedures ) )
                {

                  if ( $dir == 0 ){ $dir = 0.0001; say REPORT "IMPOSED \$dir = 0.0001;"; }
                  if ( $diff == 0 ){ $diff = 0.0001; say REPORT "IMPOSED \$diff = 0.0001;"; } #say "IMPOSED \$dir = 0.0001;";
                }
                # IMPORTANT: THE TWO LINES ABOVE SET THE DIFFUSE AND DIRECT IRRADIANCE > 0 OF A TINY AMOUNT IF THERE ARE 0
                # TO AVOID ERRORS IN THE rtrace CALLS WHEN THE ALTITUDE IS > 0.

                if ( "gendaylit" ~~ @calcprocedures )
                {
                  if ( ( "getweather" ~~ @calcprocedures ) and ( "getsimple" ~~ @calcprocedures ) )
                  {
                    @returns = `gendaylit -ang $alt $azi +s -g $groundrefl -W 0 $diff -a $lat -o $long -m $standardmeridian`;
                    say REPORT "gendaylit -ang $alt $azi +s -g $groundrefl -W 0 $diff -a $lat -o $long -m $standardmeridian";
                  }
                  else
                  {
                    if ( not( "getweather" ~~ @calcprocedures ) and not( "getsimple" ~~ @calcprocedures ) )
                    {
                      @returns = `gendaylit -ang $alt $azi -s -g $groundrefl -a $lat -o $long -m $standardmeridian`;
                      say REPORT "gendaylit -ang $alt $azi -s -g $groundrefl -a $lat -o $long -m $standardmeridian";
                    }
                    elsif ( ( "getweather" ~~ @calcprocedures ) and not( "getsimple" ~~ @calcprocedures ) )
                    {
                      @returns = `gendaylit $monthnum $day $hour -s -g $groundrefl -W $dir $diff -a $lat -o $long -m $standardmeridian`;
                      say REPORT "gendaylit $monthnum $day $hour -s -g $groundrefl -W $dir $diff -a $lat -o $long -m $standardmeridian";
                    }
                  }
                }
                elsif ( "gensky" ~~ @calcprocedures )
                {
                  if ( $skycond eq "clear" )
                  {
                    @returns = `gensky $monthnum $day $hour -s -g $groundrefl -a $lat -o $long -m $standardmeridian`;
                    say REPORT "gensky $monthnum $day $hour -s -g $groundrefl -a $lat -o $long -m $standardmeridian";
                  }
                  elsif ( $skycond eq "cloudy" )
                  {
                    @returns = `gensky $monthnum $day $hour -i -g $groundrefl -a $lat -o $long -m $standardmeridian`;
                    say REPORT "gensky $monthnum $day $hour -i -g $groundrefl -a $lat -o $long -m $standardmeridian";
                  }
                  elsif ( $skycond eq "overcast" )
                  {
                    @returns = `gensky $monthnum $day $hour -c -g $groundrefl -a $lat -o $long -m $standardmeridian`;
                    say REPORT "gensky $monthnum $day $hour -c -g $groundrefl -a $lat -o $long -m $standardmeridian";
                  }
                }

                foreach my $li ( @returns )
                {
                  if ( $li =~ /Warning: sun altitude below zero/ )
                  {
                    if ( "gensky" ~~ @calcprocedures )
                    {
                      if ( $skycond eq "clear" )
                      {
                        @returns = `gensky -ang 0.0001 $azi -s -g 0 -a $lat -o $long -m $standardmeridian`;
                        say REPORT "gendaylit -ang 0.0001 $azi -s -g 0 -a $lat -o $long -m $standardmeridian";
                      }
                      elsif ( $skycond eq "cloudy" )
                      {
                        @returns = `gendaylit -ang 0.0001 $azi -i -g 0 -a $lat -o $long -m $standardmeridian`;
                        say REPORT "gendaylit -ang 0.0001 $azi -i -g 0 -a $lat -o $long -m $standardmeridian";
                      }
                      elsif ( $skycond eq "overcast" )
                      {
                        @returns = `gendaylit -ang 0.0001 $azi -c -g 0 -a $lat -o $long -m $standardmeridian`;
                        say REPORT "gendaylit -ang 0.0001 $azi -c -g 0 -a $lat -o $long -m $standardmeridian";
                      }
                      last;
                    }
                  }
                  elsif ( "gendaylit" ~~ @calcprocedures )
                  {
                    @returns = `gendaylit -ang 0.0001 $azi -s -g 0 -a $lat -o $long -m $standardmeridian`;
                    say REPORT "gendaylit -ang 0.0001 $azi -s -g 0 -a $lat -o $long -m $standardmeridian";
                    last;
                  }
                }


                if ( ( "gendaylit" ~~ @calcprocedures ) or ( "gensky" ~~ @calcprocedures ) )
                {
                  open( SKYFILE, ">$skyfile" ) or die;

                  foreach my $line ( @returns )
                  {
                    print SKYFILE $line;
                    print REPORT $line;
                  }

                  if ( not ( ( "getweather" ~~ @calcprocedures ) and ( "getsimple" ~~ @calcprocedures ) ) )
                  {

print SKYFILE "

void light solar
0
0
3 0.0 0.0 0.0

solar source sun
0
0
4 0.0 0.0 0.0 0.0
";
close SKYFILE;


print REPORT"
NOW ADDING TO SKY FILE, 1:

void light solar
0
0
3 0.0 0.0 0.0

solar source sun
0
0
4 0.0 0.0 0.0 0.0
";
                  }
                }

                say REPORT "IN CALCULATIONS FOR DIFFUSE RADIATION, cycle " . ( $countrad + 1 ) . ", \$hour: $hour, \$surfnum: $surfnum, \$month: $month";

                my @shuffled;
                if ( ( $countrad == 5 ) and ( "radical" ~~ @calcprocedures ) and ( "sparecycles" ~~ @calcprocedures ) )
                {
                  @shuffleds = shuffle( @pointrefs );
                  @shuffleds = @shuffleds[0..1];
                }

                my $countpoint = 0;
                unless ( ( $altreturn < 0 ) and ( ( "gensky" ~~ @calcprocedures ) or ( "gendaylit" ~~ @calcprocedures ) )
                    and ( not ( ( "getweather" ~~ @calcprocedures ) and ( "getsimple" ~~ @calcprocedures ) ) ) )
                {
                  foreach my $pointref ( @pointrefs )
                  {
                    my $shuffled = $shuffleds[$countpoint];
                    if ( $shuffled ne "" )
                    {
                      $pointref = $shuffled;
                    }
                    my @pointcoords = @$pointref;
                    my ( $xcoord, $ycoord, $zcoord ) = @pointcoords;

                    my $raddir = $paths{radpath};
                    my $cfgpath = $paths{cfgpath};

                    my @dirvgroup = getdirvectors ( \@basevectors, \@dirvector );


                    foreach my $dirvector ( @dirvgroup )
                    {
                      my ( $valstring, $valstring1, $valstring2, $irr, $irr1, $irr2 );
                      my ( $dirvx, $dirvy, $dirvz ) = @{ $dirvector };

                      $valstring = `cd $raddir \n echo $xcoord $ycoord $zcoord $dirvx $dirvy $dirvz | rtrace  -I -ab $bounceambnum -lr $bouncemaxnum $parpiece -h $radoctfile`;
                      say REPORT "cd $raddir \n echo $xcoord $ycoord $zcoord $dirvx $dirvy $dirvz | rtrace  -I -ab $bounceambnum -lr $bouncemaxnum $parpiece -h $radoctfile";
                      say REPORT "OBTAINED: $valstring";#DDD
                      my ( $x, $y, $z ) = ( $valstring =~ m/(.+)\t(.+)\t(.+)\t/ );
                      $irr = ( 179 * ( ( .265 * $x ) + ( .670 * $y ) + ( .065 * $z ) ) );
                      say REPORT "OBTAINED IRR: $irr";#DDD
                      push ( @{ $surftestsdiff{$countrad+1}{$monthnum}{$surfnum}{$hour} }, $irr );
                    }
                    $countpoint++;
                  }
                  unless ( "embedded" ~~ @calcprocedures )
                  {
                    say "\nSurface $surfnum, zone $zonenum, month $monthnum, day $day, hour $hour, octree $radoctfile";
                    say "Diffuse irradiances: " . mean( @{ $surftestsdiff{$countrad+1}{$monthnum}{$surfnum}{$hour} } );
                  }
                  say REPORT "\nSurface $surfnum, zone $zonenum, month $monthnum, day $day, hour $hour, octree $radoctfile";
                  say REPORT "Diffuse irradiances: " . mean( @{ $surftestsdiff{$countrad+1}{$monthnum}{$surfnum}{$hour} } );
                }
                else
                {
                  push ( @{ $surftestsdiff{$countrad+1}{$monthnum}{$surfnum}{$hour} }, 0 );
                }
              }
            }


            ## HERE FOLLOW THE OPERATIONS FOR THE TOTAL IRRADIANCES

            if ( not ( "keepdirshdf" ~~ @calcprocedures ) )
            {
              my ( @returns );
              if ( ( ( ( "composite" ~~ @calcprocedures ) and not( "groundreflections" ~~ @calcprocedures ) )
                    and ( ( $countrad == 0 ) or ( $countrad == 1 ) or ( $countrad == 5 ) or ( $countrad == 6 ) or ( $countrad == 7 ) or ( $countrad == 10 ) ) )
                or ( ( ( "composite" ~~ @calcprocedures ) and ( "groundreflections" ~~ @calcprocedures ) )
                    and ( ( $countrad == 0 ) or ( $countrad == 1 ) ) )
                or ( ( "radical" ~~ @calcprocedures ) and ( ( $countrad == 1 ) or ( $countrad == 4 ) ) )
                or ( ( "noreflections" ~~ @calcprocedures ) and ( ( $countrad == 5 )
                  or ( $countrad == 6 ) or ( $countrad == 7 ) or ( $countrad == 10 ) ) )
                or ( ( not( "noreflections" ~~ @calcprocedures ) and not( "composite" ~~ @calcprocedures ) and not( "radical" ~~ @calcprocedures )
                  and ( ( $countrad == 0 ) or ( $countrad == 1 ) ) ) or ( "plain" ~~ @calcprocedures ) ) )
              {

                if ( $alt <= 0 ) { $alt = 0.0001; say REPORT "IMPOSED \$alt = 0.0001;"; } #say "IMPOSED \$alt = 0.0001;";  # IMPORTANT: THIS SETS THE ALTITUDE > 0 OF A TINY AMOUNT IF IT IS < 0 DUE TO THE FACT
                # THAT THE MAJORITY OF THAT HOUR THE SUN WAS BELOW THE HORIZON, WHILE THE NET GAINED AMOUNT OF RADIATION WAS STILL > 0.

                if ( ( "getweather" ~~ @calcprocedures ) and ( "getsimple" ~~ @calcprocedures ) )
                {
                  if ( $dir == 0 ){ $dir = 0.0001; say REPORT "IMPOSED \$dir = 0.0001;"; }  # say "IMPOSED \$dir = 0.0001;";
                  if ( $diff == 0 ){ $diff = 0.0001; say REPORT "IMPOSED \$diff = 0.0001;"; } # say "IMPOSED \$diff = 0.0001;";
                }
                # IMPORTANT: THE TWO LINES ABOVE SET THE DIFFUSE AND DIRECT IRRADIANCE > 0 OF A TINY AMOUNT IF THERE ARE 0
                # TO AVOID ERRORS IN THE rtrace CALLS WHEN THE ALTITUDE IS > 0.

                my $thisgref;
                unless ( ( $countrad == 5 ) or ( $countrad == 6 )
                  or ( $countrad == 8 ) or ( $countrad == 9 ) ) # I.E. UNLESS GROUND REFLECTANCE HAS TO BE 0. # TAKE CARE!
                {
                  $thisgref= $groundrefl;
                }
                else
                {
                  $thisgref = 0;
                }

                my ( $altreturn, $lightsolar );

                if ( ( "getweather" ~~ @calcprocedures ) and ( "getsimple" ~~ @calcprocedures ) ) # IF CONDITION, CHECK DIRECT RADIATION
                {
                  @returns = `gendaylit -ang $alt $azi +s -g $thisgref -W $dir 0 -a $lat -o $long -m $standardmeridian`;
                  say REPORT "gendaylit -ang $alt $azi +s -g $thisgref -W $dir 0 -a $lat -o $long -m $standardmeridian";
                }
                elsif ( ( "gendaylit" ~~ @calcprocedures ) and ( "getweather" ~~ @cacprocedures ) and ( not ( ( "getsimple" ~~ @calcprocedures ) ) ) )
                {
                  @returns = `gendaylit -ang $alt $azi +s -g $thisgref -W $dir $diff -a $lat -o $long -m $standardmeridian`;
                  say REPORT "gendaylit -ang $alt $azi +s -g $thisgref -W $dir $diff -a $lat -o $long -m $standardmeridian";
                }
                elsif ( not( "getweather" ~~ @calcprocedures ) and not( "getsimple" ~~ @calcprocedures )
                  and ( "gensky" ~~ @calcprocedures ) )
                {
                  if ( $skycond eq "clear" )
                  {
                    @returns = `gensky $monthnum $day $hour +s -g $thisgref -a $lat -o $long -m $standardmeridian`;
                    say REPORT "gensky $monthnum $day $hour +s -g $thisgref -a $lat -o $long -m $standardmeridian";
                  }
                  elsif ( $skycond eq "cloudy" )
                  {
                    @returns = `gensky $monthnum $day $hour +i -g $thisgref -a $lat -o $long -m $standardmeridian`;
                    say REPORT "gensky $monthnum $day $hour +i -g $thisgref -a $lat -o $long -m $standardmeridian";
                  }
                  elsif ( $skycond eq "overcast" )
                  {
                    @returns = `gensky $monthnum $day $hour -c -g $thisgref black:ref -a $lat -o $long -m $standardmeridian`;
                    say REPORT "gensky $monthnum $day $hour -c -g $thisgref -a $lat -o $long -m $standardmeridian";
                  }

                  my $ct = 0;
                  foreach my $el ( @returns )
                  {
                    if ( $el =~ /void light solar/ )
                    {
                      $lightsolar = $returns[$ct+3];
                      if ( $lightsolar =~ /3 0.0 0.0 0.0/ )
                      {
                        say REPORT "NOW EDITING _void light solar_ IN SKY FILE, 2.";
                        $lightsolar = "3 0.1 0.1 0.1";
                      }
                    }
                    $ct++;
                  }

                }
                elsif ( not( "getweather" ~~ @calcprocedures ) and not( "getsimple" ~~ @calcprocedures )
                  and ( "gendaylit" ~~ @calcprocedures ) )
                {
                  if ( $skycond eq "clear" )
                  {
                    @returns = `gensky -ang $alt $azi +s -g $thisgref -a $lat -o $long -m $standardmeridian`;
                    say REPORT "gensky -ang $alt $azi +s -g $thisgref -a $lat -o $long -m $standardmeridian";
                  }
                  elsif ( $skycond eq "cloudy" )
                  {
                    @returns = `gensky -ang $alt $azi +i -g $thisgref -a $lat -o $long -m $standardmeridian`;
                    say REPORT "gensky -ang $alt $azi +i -g $thisgref -a $lat -o $long -m $standardmeridian";
                  }
                  elsif ( $skycond eq "overcast" )
                  {
                    @returns = `gensky -ang $alt $azi -c -g $thisgobs black:ref -a $lat -o $long -m $standardmeridian`;
                    say REPORT "gensky -ang $alt $azi -c -g $thisgref -a $lat -o $long -m $standardmeridian";
                  }

                  my $ct = 0;
                  foreach my $el ( @returns )
                  {
                    if ( $el =~ /void light solar/ )
                    {
                      $lightsolar = $returns[$ct+3];
                      if ( $lightsolar =~ /3 0.0 0.0 0.0/ )
                      {
                        say REPORT "NOW EDITING _void light solar_ IN SKY FILE, 3.";
                        $lightsolar = "3 0.1 0.1 0.1";
                      }
                    }
                    $ct++;
                  }

                  @returns = `gendaylit -ang $alt $azi +s -g $thisgref -a $lat -o $long -m $standardmeridian`;
                  say REPORT "gendaylit -ang $alt $azi +s -g $thisgref -a $lat -o $long -m $standardmeridian";

                }

                my $positive = "no";
                my $ct = 0;
                foreach my $li ( @returns )
                {

                  if ( $li =~ /void light solar/ )
                  {
                    $positive = "yes";
                    if ( $lightsolar ne "" )
                    {
                      $returns[$ct+3] = $lightsolar;
                      if ( $returns[$ct+3] =~ /3 0.0 0.0 0.0/ )
                      {
                        say REPORT "NOW EDITING _void light solar_ IN SKY FILE, 4.";
                        $returns[$ct+3] = "3 0.1 0.1 0.1";
                      }
                    }
                  }
                  elsif ( $li =~ /solar source sun/ )
                  {
                    if ( $returns[$ct+3] =~ /4 0.0 0.0 0.0 0/ )
                    {
                      $returns[$ct+3] = "4 0.1 0.1 0.1 0.1";
                    }
                  }

                  if ( $li =~ /^Warning: sun altitude below zero/ )
                  {
                    if ( $skycond eq "clear" )
                    {
                      @returns = `gensky -ang 0.0001 $azi +s -g $thisgref -a $lat -o $long -m $standardmeridian`;
                      say REPORT "gensky -ang 0.0001 $azi +s -g $thisgref -a $lat -o $long -m $standardmeridian";
                      #say REPORT "gensky " . dump( @returns );
                    }
                    elsif ( $skycond eq "cloudy" )
                    {
                      @returns = `gensky -ang 0.0001 $azi +i -g $thisgref -a $lat -o $long -m $standardmeridian`;
                      say REPORT "gensky -ang 0.0001 $azi +i -g $thisgref -a $lat -o $long -m $standardmeridian";
                    }
                    elsif ( $skycond eq "overcast" )
                    {
                      @returns = `gensky -ang 0.0001 $azi -c -g $thisgref -a $lat -o $long -m $standardmeridian`;
                      say REPORT "gensky -ang 0.0001 $azi -c -g $thisgref -a $lat -o $long -m $standardmeridian";
                    }
                    last;
                  }
                  $ct++;
                }

                say REPORT "IN CALCULATIONS FOR TOTAL RADIATION, cycle " . ( $countrad + 1 ) . ", \$hour: $hour, \$surfnum: $surfnum, \$month: $month";

                if ( $positive ne "yes" )
                {
                  say REPORT "NOW PUSHING _void light solar_ INTO SKY FILE DATA STRUCTURE, 5.";
                  push ( @returns,
                  "\n",
                  "void light solar\n",
                  "0\n",
                  "0\n",
                  "3 0.1 0.1 0.1\n",
                  "\n",
                  "solar source sun\n",
                  "0\n",
                  "0\n",
                  "4 0.1 0.1 0.1 0.1\n" );
                }

                open( SKYFILE, ">$skyfile" ) or die;
                print REPORT "OBTAINED\n";
                foreach my $line ( @returns )
                {
                  print SKYFILE $line;
                  print REPORT $line;
                }
                close SKYFILE;

                my $countpoint = 0;

                unless ( ( $altreturn < 0 ) and ( ( "gensky" ~~ @calcprocedures ) or ( "gendaylit" ~~ @calcprocedures ) ) )
                {
                  foreach my $pointref ( @pointrefs )
                  {
                    my @pointcoords = @$pointref;
                    my ( $xcoord, $ycoord, $zcoord ) = @pointcoords;
                    my $raddir = $radpath;
                    my $cfgpath = $paths{cfgpath};
                    my @dirvgroup = getdirvectors ( \@basevectors, \@dirvector );

                    foreach my $dirvector ( @dirvgroup )
                    {
                      my ( $valstring, $valstring1, $valstring2, $irr, $irr1, $irr2 );
                      my ( $dirvx, $dirvy, $dirvz ) = @{ $dirvector };
                      $valstring = `cd $raddir \n echo $xcoord $ycoord $zcoord $dirvx $dirvy $dirvz | rtrace  -I -ab $bounceambnum -lr $bouncemaxnum $parpiece -h $radoctfile`;
                      say REPORT "cd $raddir \n echo $xcoord $ycoord $zcoord $dirvx $dirvy $dirvz | rtrace  -I -ab $bounceambnum -lr $bouncemaxnum $parpiece -h $radoctfile";
                      say REPORT "OBTAINED: $valstring";#DDD
                      my ( $x, $y, $z ) = ( $valstring =~ m/(.+)\t(.+)\t(.+)\t/ );
                      $irr = ( 179 * ( ( .265 * $x ) + ( .670 * $y ) + ( .065 * $z ) ) );
                      say REPORT "OBTAINED IRR: $irr";#DDD
                      push ( @{ $surftests{$countrad+1}{$monthnum}{$surfnum}{$hour} }, $irr );
                    }
                    $countpoint++;
                  }
                  unless ( "embedded" ~~ @calcprocedures )
                  {
                    say "\nSurface $surfnum, zone $zonenum, month $monthnum, day $day, hour $hour, octree $radoctfile";
                    say "Total irradiances: " . mean( @{ $surftests{$countrad+1}{$monthnum}{$surfnum}{$hour} } );
                  }
                  say REPORT "\nSurface $surfnum, zone $zonenum, month $monthnum, day $day, hour $hour, octree $radoctfile";
                  say REPORT "Total irradiances: " . mean( @{ $surftests{$countrad+1}{$monthnum}{$surfnum}{$hour} } );
                }
                else
                {
                  push ( @{ $surftests{$countrad+1}{$monthnum}{$surfnum}{$hour} }, 0 );
                }
              }
            }


            ## HERE FOLLOW THE OPERATIONS FOR THE DIRECT IRRADIANCES.
            if ( ( "alldiff" ~~ @calcprocedures ) and not( "plain" ~~ @calcprocedures ) )
            {
              my @gridpoints = @gridpoints;
              if ( "espdirres" ~~ @calcprocedures )
              {
                @gridpoints = @dirgridpoints;
              }
              my ( @returns, $altreturn, $positive );
              if ( ( ( "composite" ~~ @calcprocedures ) and ( $countrad == 5 ) )
                or ( ( "radical" ~~ @calcprocedures ) and ( ( $countrad == 5 ) or ( $countrad == 6 ) ) ) )
              {

                if ( $alt <= 0 ) { $alt = 0.0001; say REPORT "IMPOSED \$alt = 0.0001;"; } # say "IMPOSED \$alt = 0.0001;"; # IMPORTANT: THIS SETS THE ALTITUDE > 0 OF A TINY AMOUNT IF IT IS < 0 DUE TO THE FACT
                # THAT THE MAJORITY OF THAT HOUR THE SUN WAS BELOW THE HORIZON, WHILE THE NET GAINED AMOUNT OF RADIATION WAS STILL > 0.

                if ( ( "getweather" ~~ @calcprocedures ) and ( "getsimple" ~~ @calcprocedures ) )
                {
                  if ( $dir == 0 ){ $dir = 0.0001; say REPORT "IMPOSED \$dir = 0.0001;"; } # say "IMPOSED \$dir = 0.0001;";
                  if ( $diff == 0 ){ $diff = 0.0001;  say REPORT "IMPOSED \$diff = 0.0001;"; } # say "IMPOSED \$diff = 0.0001;";
                }
                # IMPORTANT: THE TWO LINES ABOVE SET THE DIFFUSE AND DIRECT IRRADIANCE > 0 OF A TINY AMOUNT IF THERE ARE 0
                # TO AVOID ERRORS IN THE rtrace CALLS WHEN THE ALTITUDE IS > 0.

                my ( $altreturn, $lightsolar );
                if ( ( "getweather" ~~ @calcprocedures ) and ( "getsimple" ~~ @calcprocedures ) ) # IF CONDITION, CHECK DIRECT RADIATION
                {
                  @returns = `gendaylit -ang $alt $azi +s -g 0 -W $dir 0 -a $lat -o $long -m $standardmeridian`;
                  say REPORT "gendaylit -ang $alt $azi +s -g 0 -W $dir 0 -a $lat -o $long -m $standardmeridian";
                }
                elsif ( ( "gendaylit" ~~ @calcprocedures ) and ( "getweather" ~~ @calcprocedures ) and not( "getsimple" ~~ @calcprocedures ) )
                {
                  @returns = `gendaylit -ang $alt $azi +s -g 0 -W $dir $diff -a $lat -o $long -m $standardmeridian`;
                  say REPORT "gendaylit -ang $alt $azi +s -g 0 -W $dir $diff -a $lat -o $long -m $standardmeridian";
                }
                elsif ( ( "gensky" ~~ @calcprocedures ) and not( "getweather" ~~ @calcprocedures )
                  and not( "getsimple" ~~ @calcprocedures ) )
                {
                  if ( $skycond eq "clear" )
                  {
                    @returns = `gensky $monthnum $day $hour +s -g 0 -a $lat -o $long -m $standardmeridian`;
                    say REPORT "gensky $monthnum $day $hour +s -g 0 -a $lat -o $long -m $standardmeridian";
                  }
                  elsif ( $skycond eq "cloudy" )
                  {
                    @returns = `gensky $monthnum $day $hour +i -g 0 -a $lat -o $long -m $standardmeridian`;
                    say REPORT "gensky $monthnum $day $hour +i -g 0 -a $lat -o $long -m $standardmeridian";
                  }
                  elsif ( $skycond eq "overcast" )
                  {
                    @returns = `gensky $monthnum $day $hour -c -g 0 -a $lat -o $long -m $standardmeridian`;
                    say REPORT "gensky $monthnum $day $hour -c -g 0 -a $lat -o $long -m $standardmeridian";
                  }

                  my $ct = 0;
                  foreach my $el ( @returns )
                  {
                    if ( $el =~ /void light solar/ )
                    {
                      $lightsolar = $returns[$ct+3];
                      if ( $lightsolar =~ /3 0.0 0.0 0.0/ )
                      {
                        say REPORT "NOW EDITING _void light solar_ IN SKY FILE, 6.";
                        $lightsolar = "3 0.1 0.1 0.1";
                      }
                    }
                    $ct++;
                  }

                }
                elsif ( ( "gendaylit" ~~ @calcprocedures ) and not( "getweather" ~~ @calcprocedures )
                  and not( "getsimple" ~~ @calcprocedures ) )
                {
                  if ( $skycond eq "clear" )
                  {
                    @returns = `gensky -ang $alt $azi +s -g 0 -a $lat -o $long -m $standardmeridian`;
                    say REPORT "gensky -ang $alt $azi +s -g 0 -a $lat -o $long -m $standardmeridian";
                  }
                  elsif ( $skycond eq "cloudy" )
                  {
                    @returns = `gensky -ang $alt $azi +i -g 0 -a $lat -o $long -m $standardmeridian`;
                    say REPORT "gensky -ang $alt $azi +i -g 0 -a $lat -o $long -m $standardmeridian";
                  }
                  elsif ( $skycond eq "overcast" )
                  {
                    @returns = `gensky -ang $alt $azi -c -g 0 -a $lat -o $long -m $standardmeridian`;
                    say REPORT "gensky -ang $alt $azi -c -g 0 -a $lat -o $long -m $standardmeridian";
                  }

                  my $ct = 0;
                  foreach my $el ( @returns )
                  {
                    if ( $el =~ /void light solar/ )
                    {
                      $lightsolar = $returns[$ct+3];
                      if ( $lightsolar =~ /3 0.0 0.0 0.0/ )
                      {
                        say REPORT "NOW EDITING _void light solar_ IN SKY FILE, 7.";
                        $lightsolar = "3 0.1 0.1 0.1";
                      }
                    }
                    $ct++;
                  }

                  @returns = `gendaylit -ang $alt $azi +s -g 0 -a $lat -o $long -m $standardmeridian`;
                  say REPORT "gendaylit -ang $alt $azi +s -g 0 -a $lat -o $long -m $standardmeridian";
                }

                my $positive = "no";
                my $ct = 0;
                foreach my $li ( @returns )
                {

                  if ( $li =~ /void light solar/ )
                  {
                    $positive = "yes";
                    if ( $lightsolar ne "" )
                    {
                      $returns[$ct+3] = $lightsolar;
                      if ( $returns[$ct+3] =~ /3 0.0 0.0 0.0/ )
                      {
                        say REPORT "NOW EDITING _void light solar_ IN SKY FILE, 8.";
                        $returns[$ct+3] = "3 0.1 0.1 0.1";
                      }
                    }
                  }
                  elsif ( $li =~ /solar source sun/ )
                  {
                    if ( $returns[$ct+3] =~ /4 0.0 0.0 0.0 0/ )
                    {
                      say REPORT "NOW EDITING _void light solar_ IN SKY FILE, 9.";
                      $returns[$ct+3] = "4 0.1 0.1 0.1 0.1";
                    }
                  }
                  elsif ( $line =~ /void brightfunc skyfunc/ )
                  {
                    $returns[$counter+3] = "7 0 0 0 0 0 0 0\n",
                  }

                  if ( $liXXX =~ /Warning: sun altitude below zero/ )
                  {
                    if ( "gensky" ~~ @calcprocedures )
                    {
                      if ( $skycond eq "clear" )
                      {
                        @returns = `gensky -ang 0.0001 $azi +s -g 0 -a $lat -o $long -m $standardmeridian`;
                        say REPORT "gensky -ang 0.0001 $azi +s -g 0 -a $lat -o $long -m $standardmeridian";
                      }
                      elsif ( $skycond eq "cloudy" )
                      {
                        @returns = `gensky -ang 0.0001 $azi +i -g 0 -a $lat -o $long -m $standardmeridian`;
                        say REPORT "gensky -ang 0.0001 $azi +i -g 0 -a $lat -o $long -m $standardmeridian";
                      }
                      elsif ( $skycond eq "overcast" )
                      {
                        @returns = `gensky -ang 0.0001 $azi -c -g 0 -a $lat -o $long -m $standardmeridian`;
                        say REPORT "gensky -ang 0.0001 $azi -c -g 0 -a $lat -o $long -m $standardmeridian";
                      }
                      last;
                    }
                    elsif ( "gendaylit" ~~ @calcprocedures )
                    {
                      @returns = `gendaylit -ang 0.0001 $azi +s -g 0 -a $lat -o $long -m $standardmeridian`;
                      say REPORT "gendaylit -ang 0.0001 $azi +s -g 0 -a $lat -o $long -m $standardmeridian";
                    }
                  }
                  $ct++;
                }

                if ( $positive ne "yes" )
                {
                  say REPORT "NOW PUSHING _void light solar_ IN SKY FILE DATA STRUCTURE, 10.";
                  push ( @returns,
                  "\n",
                  "void light solar\n",
                  "0\n",
                  "0\n",
                  "3 0.1 0.1 0.1\n",
                  "\n",
                  "solar source sun\n",
                  "0\n",
                  "0\n",
                  "4 0.1 0.1 0.1 0.1\n" );
                }

                say REPORT "IN CALCULATIONS FOR DIRECT RADIATION, cycle " . ( $countrad + 1 ) . ", \$hour: $hour, \$surfnum: $surfnum, \$month: $month";

                open( SKYFILE, ">$skyfile" ) or die;

                print REPORT "OBTAINED";
                foreach my $line ( @returns )
                {
                  print SKYFILE $line;
                  print REPORT $line;
                }
                close SKYFILE;

                my $countpoint = 0;

                unless ( ( $altreturn < 0 ) and ( ( "gensky" ~~ @calcprocedures ) or ( "gendaylit" ~~ @calcprocedures ) ) )
                {
                  foreach my $pointref ( @pointrefs )
                  {
                    my @pointcoords = @$pointref;
                    my ( $xcoord, $ycoord, $zcoord ) = @pointcoords;
                    my $raddir = $radpath;
                    my $cfgpath = $paths{cfgpath};
                    my @dirvgroup = getdirvectors ( \@basevectors, \@dirvector );

                    foreach my $dirvector ( @dirvgroup )
                    {
                      my ( $valstring, $valstring1, $valstring2, $irr, $irr1, $irr2 );
                      my ( $dirvx, $dirvy, $dirvz ) = @{ $dirvector };

                      $valstring = `cd $raddir \n echo $xcoord $ycoord $zcoord $dirvx $dirvy $dirvz | rtrace  -I -ab 0 -lr 0 $parpiece -h $radoctfile`;
                      say REPORT "5TO SHELL: cd $raddir \n echo $xcoord $ycoord $zcoord $dirvx $dirvy $dirvz | rtrace  -I -ab 0 -lr 0 $parpiece -h $radoctfile";
                      say REPORT "OBTAINED: $valstring";#DDD
                      my ( $x, $y, $z ) = ( $valstring =~ m/(.+)\t(.+)\t(.+)\t/ );
                      $irr = ( 179 * ( ( .265 * $x ) + ( .670 * $y ) + ( .065 * $z ) ) );
                      say REPORT "OBTAINED IRR: $irr";#DDD
                      push ( @{ $surftestsdir{$countrad+1}{$monthnum}{$surfnum}{$hour} }, $irr );
                    }
                    $countpoint++;
                  }
                  unless ( "embedded" ~~ @calcprocedures )
                  {
                    say "\nSurface $surfnum, zone $zonenum, month $monthnum, day $day, hour $hour, octree $radoctfile";
                    say "Direct unreflected irradiances: " . mean ( @{ $surftestsdir{$countrad+1}{$monthnum}{$surfnum}{$hour} } );
                  }
                  say REPORT "\nSurface $surfnum, zone $zonenum, month $monthnum, day $day, hour $hour, octree $radoctfile";
                  say REPORT "Direct unreflected irradiances: " . mean ( @{ $surftestsdir{$countrad+1}{$monthnum}{$surfnum}{$hour} } );
                }
                else
                {
                  push ( @{ $surftestsdir{$countrad+1}{$monthnum}{$surfnum}{$hour} }, 0 );
                }
              }
            }


            my ( $meanvaluesurf1, $meanvaluesurf_diff1, $meanvaluesurf_dir1,
                  $meanvaluesurf2, $meanvaluesurf_diff2, $meanvaluesurf_dir2,
                  $meanvaluesurf3, $meanvaluesurf_diff3, $meanvaluesurf_dir3,
                  $meanvaluesurf5, $meanvaluesurf9, $meanvaluesurf10, $meanvaluesurf11,
                  $modrelation );

            if ( $count == $radoctnum )
            {
              if ( not( "alldiff" ~~ @calcprocedures ) )
              {
                if ( "radical" ~~ @calcprocedures )
                {
                  if ( @{ $surftests{5}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf1 = mean( @{ $surftests{5}{$monthnum}{$surfnum}{$hour} } );
                  }

                  if ( @{ $surftestsdiff{5}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf_diff1 = mean( @{ $surftestsdiff{5}{$monthnum}{$surfnum}{$hour} } );
                  }

                  $meanvaluesurf_dir1 = ( $meanvaluesurf1 - $meanvaluesurf_diff1 );

                  if ( @{ $surftests{2}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf2 = mean( @{ $surftests{2}{$monthnum}{$surfnum}{$hour} } );
                  }

                  if ( @{ $surftestsdiff{2}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf_diff2 = mean( @{ $surftestsdiff{2}{$monthnum}{$surfnum}{$hour} } );
                  }

                  $meanvaluesurf_dir2 = ( $meanvaluesurf2 - $meanvaluesurf_diff2 );


                  if ( @{ $surftests{3}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf3 = mean( @{ $surftests{3}{$monthnum}{$surfnum}{$hour} } );
                  }

                  if ( @{ $surftestsdiff{3}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf_diff3 = mean( @{ $surftestsdiff{3}{$monthnum}{$surfnum}{$hour} } );
                  }

                  $meanvaluesurf_dir3 = ( $meanvaluesurf3 - $meanvaluesurf_diff3 );
                }
                elsif ( scalar( @selectives ) > 0 )
                {
                  if ( @{ $surftests{1}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf1 = mean( @{ $surftests{1}{$monthnum}{$surfnum}{$hour} } );
                  }

                  if ( @{ $surftestsdir{1}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf_dir1 = mean( @{ $surftestsdir{1}{$monthnum}{$surfnum}{$hour} } );
                  }

                  $meanvaluesurf_diff1 = ( $meanvaluesurf1 - $meanvaluesurf_dir1 );


                  if ( @{ $surftests{3}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf3 = mean( @{ $surftests{3}{$monthnum}{$surfnum}{$hour} } );
                  }

                  if ( @{ $surftestsdir{3}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf_dir3 = mean( @{ $surftestsdir{3}{$monthnum}{$surfnum}{$hour} } );
                  }

                  $meanvaluesurf_diff3 = ( $meanvaluesurf3 - $meanvaluesurf_dir3 );


                  if ( @{ $surftests{4}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf1 = mean( @{ $surftests{4}{$monthnum}{$surfnum}{$hour} } );
                  }

                  if ( @{ $surftestsdir{4}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf_dir4 = mean( @{ $surftestsdir{4}{$monthnum}{$surfnum}{$hour} } );
                  }

                  $meanvaluesurf_diff4 = ( $meanvaluesurf4 - $meanvaluesurf_dir4 );
                }
                else
                {
                  if ( @{ $surftests{1}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf1 = mean( @{ $surftests{1}{$monthnum}{$surfnum}{$hour} } );
                  }

                  if ( @{ $surftestsdiff{1}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf_diff1 = mean( @{ $surftestsdiff{1}{$monthnum}{$surfnum}{$hour} } );
                  }

                  $meanvaluesurf_dir1 = ( $meanvaluesurf1 - $meanvaluesurf_diff1 );

                  if ( @{ $surftests{2}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf2 = mean( @{ $surftests{2}{$monthnum}{$surfnum}{$hour} } );
                  }

                  if ( @{ $surftestsdiff{2}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf_diff2 = mean( @{ $surftestsdiff{2}{$monthnum}{$surfnum}{$hour} } );
                  }

                  $meanvaluesurf_dir2 = ( $meanvaluesurf2 - $meanvaluesurf_diff2 );


                  if ( @{ $surftests{3}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf3 = mean( @{ $surftests{3}{$monthnum}{$surfnum}{$hour} } );
                  }

                  if ( @{ $surftestsdiff{3}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf_diff3 = mean( @{ $surftestsdiff{3}{$monthnum}{$surfnum}{$hour} } );
                  }

                  $meanvaluesurf_dir3 = ( $meanvaluesurf3 - $meanvaluesurf_diff3 );
                }
              }
              elsif ( "alldiff" ~~ @calcprocedures )
              {
                if ( ( "noreflections" ~~ @calcprocedures ) or ( ( "composite" ~~ @calcprocedures ) and not( "groundreflections" ~~ @calcprocedures ) ) )
                {
                  my ( $meanvaluesurf1, $meanvaluesurf2, $meanvaluesurf5, $meanvaluesurf6, $meanvaluesurf8, $meanvaluesurf9, $meanvaluesurf10, $meanvaluesurf11 );
                  if ( @{ $surftests{8}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf8 = mean( @{ $surftests{8}{$monthnum}{$surfnum}{$hour} } );
                    $meanvaluesurf2 = $meanvaluesurf8 ;
                  }

                  if ( @{ $surftests{6}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf6 = mean( @{ $surftests{6}{$monthnum}{$surfnum}{$hour} } );
                    $meanvaluesurf_dir2 = $meanvaluesurf6 ;
                  }

                  my $groundradshad = $meanvaluesurf8 - $meanvaluesurf6;
                  if ( $groundradshad < 0 )
                  {
                    $groundradshad = 0;
                  }
                  my $meanvaluesurf_diff2 = $groundradshad;

                  if ( @{ $surftests{11}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf11 = mean( @{ $surftests{11}{$monthnum}{$surfnum}{$hour} } );
                    $meanvaluesurf1 = $meanvaluesurf11;
                  }

                  if ( @{ $surftests{7}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf7 = mean( @{ $surftests{7}{$monthnum}{$surfnum}{$hour} } );
                    $meanvaluesurf_dir1 = $meanvaluesurf7;
                  }

                  my $groundradnoshad = $meanvaluesurf11 - $meanvaluesurf7;
                  if ( $groundradnoshad < 0 )
                  {
                    $groundradnoshad = 0;
                  }
                  $meanvaluesurf_diff1 = $groundradnoshad;

                  my $groundraddifference = $groundradnoshad - $groundradshad;

                  if ( $meanvaluesurf8 > 0 )
                  {;
                    $modrelation = ( $groundraddifference / ( $meanvaluesurf8 - mean( @{ $surftestsdir{6}{$monthnum}{$surfnum}{$hour} } ) ) );
                    if ( $modrelation > 1 )
                    {
                      $modrelation = 0;
                    }
                    elsif ( $modrelation < 0 )
                    {
                      $modrelation = 0;
                    }
                  }
                  else
                  {
                    $modrelation = "0";
                  }
                  $surftests{groundradnoshad}{$monthnum}{$surfnum}{$hour} = $groundradnoshad;
                  $surftests{groundradshad}{$monthnum}{$surfnum}{$hour} = $groundradshad;
                  $surftests{groundraddifference}{$monthnum}{$surfnum}{$hour} = $groundraddifference;
                  $surftests{modrelation}{$monthnum}{$surfnum}{$hour} = $modrelation;
                }

                if ( "composite" ~~ @calcprocedures )
                {
                  if ( @{ $surftests{2}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf2 = mean( @{ $surftests{2}{$monthnum}{$surfnum}{$hour} } );
                  }

                  if ( @{ $surftestsdir{6}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf_dir2 = mean( @{ $surftestsdir{6}{$monthnum}{$surfnum}{$hour} } );
                  }

                  $meanvaluesurf_diff2 = ( $meanvaluesurf2 - $meanvaluesurf_dir2 );


                  if ( @{ $surftests{1}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf1 = mean( @{ $surftests{1}{$monthnum}{$surfnum}{$hour} } );
                  }

                  $meanvaluesurf_dir1 = $meanvaluesurf_dir2;

                  $meanvaluesurf_diff1 = ( $meanvaluesurf1 - $meanvaluesurf_dir1 );
                }
                elsif ( "radical" ~~ @calcprocedures )
                {
                  if ( @{ $surftests{2}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf2 = mean( @{ $surftests{2}{$monthnum}{$surfnum}{$hour} } );
                  }

                  if ( @{ $surftestsdir{6}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf_dir2 = mean( @{ $surftestsdir{6}{$monthnum}{$surfnum}{$hour} } );
                  }

                  $meanvaluesurf_diff2 = ( $meanvaluesurf2 - $meanvaluesurf_dir2 );

                  if ( @{ $surftests{5}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf1 = mean( @{ $surftests{5}{$monthnum}{$surfnum}{$hour} } );
                  }

                  if ( @{ $surftestsdir{7}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf_dir1 = mean( @{ $surftestsdir{7}{$monthnum}{$surfnum}{$hour} } );
                  }

                  $meanvaluesurf_diff1 = ( $meanvaluesurf1 - $meanvaluesurf_dir1 );
                }
                elsif ( "plain" ~~ @calcprocedures )
                {
                  if ( @{ $surftests{1}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf_diff1 = mean( @{ $surftests{1}{$monthnum}{$surfnum}{$hour} } );
                  }

                  if ( @{ $surftests{2}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf_diff2 = mean( @{ $surftests{2}{$monthnum}{$surfnum}{$hour} } );
                  }
                }
                elsif ( scalar( @selectives ) > 0 )
                {
                  if ( @{ $surftests{1}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf1 = mean( @{ $surftests{1}{$monthnum}{$surfnum}{$hour} } );
                  }

                  if ( @{ $surftestsdir{1}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf_dir1 = mean( @{ $surftestsdir{1}{$monthnum}{$surfnum}{$hour} } );
                  }

                  $meanvaluesurf_diff1 = ( $meanvaluesurf1 - $meanvaluesurf_dir1 );


                  if ( @{ $surftests{3}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf3 = mean( @{ $surftests{3}{$monthnum}{$surfnum}{$hour} } );
                  }

                  if ( @{ $surftestsdir{3}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf_dir3 = mean( @{ $surftestsdir{3}{$monthnum}{$surfnum}{$hour} } );
                  }

                  $meanvaluesurf_diff3 = ( $meanvaluesurf3 - $meanvaluesurf_dir3 );


                  if ( @{ $surftests{4}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf1 = mean( @{ $surftests{4}{$monthnum}{$surfnum}{$hour} } );
                  }

                  if ( @{ $surftestsdir{4}{$monthnum}{$surfnum}{$hour} } )
                  {
                    $meanvaluesurf_dir4 = mean( @{ $surftestsdir{4}{$monthnum}{$surfnum}{$hour} } );
                  }

                  $meanvaluesurf_diff4 = ( $meanvaluesurf4 - $meanvaluesurf_dir4 );
                }
              }

              if ( $surftests{modrelation}{$monthnum}{$surfnum}{$hour} and $surfnum and $hour )
              {
                $irrs{ $zonenum }{ 0 }{ $monthnum }{ $surfnum }{ $hour }{ modrelation } = $surftests{modrelation}{$monthnum}{$surfnum}{$hour};
              }

              if ( $meanvaluesurf_diff1 and $surfnum and $hour )
              {
                $irrs{ $zonenum }{ 1 }{ $monthnum }{ $surfnum }{ $hour }{ meandiffirr } = $meanvaluesurf_diff1;
              }

              if ( $meanvaluesurf_dir1 and $surfnum and $hour )
              {
                $irrs{ $zonenum }{ 1 }{ $monthnum }{ $surfnum }{ $hour }{ meandirirr } = $meanvaluesurf_dir1;
              }

              if ( $meanvaluesurf_diff2 and $surfnum and $hour )
              {
                $irrs{ $zonenum }{ 2 }{ $monthnum }{ $surfnum }{ $hour }{ meandiffirr } = $meanvaluesurf_diff2;
              }

              if ( $meanvaluesurf_dir2 and $surfnum and $hour )
              {
                $irrs{ $zonenum }{ 2 }{ $monthnum }{ $surfnum }{ $hour }{ meandirirr } = $meanvaluesurf_dir2;
              }

              if ( $meanvaluesurf_diff3 and $surfnum and $hour )
              {
                $irrs{ $zonenum }{ 3 }{ $monthnum }{ $surfnum }{ $hour }{ meandiffirr } = $meanvaluesurf_diff3;
              }

              if ( $meanvaluesurf_dir3 and $surfnum and $hour )
              {
                $irrs{ $zonenum }{ 3 }{ $monthnum }{ $surfnum }{ $hour }{ meandirirr } = $meanvaluesurf_dir3;
              }

              if ( $meanvaluesurf_diff4 and $surfnum and $hour )
              {
                $irrs{ $zonenum }{ 4 }{ $monthnum }{ $surfnum }{ $hour }{ meandiffirr } = $meanvaluesurf_diff4;
              }

              if ( $meanvaluesurf_dir4 and $surfnum and $hour )
              {
                $irrs{ $zonenum }{ 4 }{ $monthnum }{ $surfnum }{ $hour }{ meandirirr } = $meanvaluesurf_dir4;
              }

              if ( ( ( ( "noreflections" ~~ @calcprocedures ) or ( ( "composite" ~~ @calcprocedures ) and not( "groundreflections" ~~ @calcprocedures ) ) and ( "alldiff" ~~ @calcprocedures ) ) ) and not ( "embedded" ~~ @calcprocedures ) )
              {
                print "obs black: " . mean( @{ $surftests{8}{$monthnum}{$surfnum}{$hour} } ) .
                "; obs & ground black: " . mean( @{ $surftests{6}{$monthnum}{$surfnum}{$hour} } ) .
                "; rel difference=: $surftests{groundradshad}{$monthnum}{$surfnum}{$hour} .\nno obs: " .
                mean( @{ $surftests{11}{$monthnum}{$surfnum}{$hour} } ) . "; no obs black ground: " .
                mean( @{ $surftests{7}{$monthnum}{$surfnum}{$hour} } ) .
                "; rel difference: $surftests{groundradnoshad}{$monthnum}{$surfnum}{$hour} .\nabs difference: $surftests{groundraddifference}{$monthnum}{$surfnum}{$hour}; shdf for ground $surftests{modrelation}{$monthnum}{$surfnum}{$hour}\n" ;
              }

              if ( ( ( ( "composite" ~~ @calcprocedures ) and ( "groundreflections" ~~ @calcprocedures ) and ( "alldiff" ~~ @calcprocedures ) ) and not( "plain" ~~ @calcprocedures ) ) and not ( "embedded" ~~ @calcprocedures ) )
              {
                print "obs black: " . mean( @{ $surftests{8}{$monthnum}{$surfnum}{$hour} } ) .
                "; obs & ground black: " . mean( @{ $surftests{6}{$monthnum}{$surfnum}{$hour} } ) .
                "\n" ;
              }


              if ( ( ( "composite" ~~ @calcprocedures ) and not( "plain" ~~ @calcprocedures ) ) and not ( "embedded" ~~ @calcprocedures ) )
              {
                print "obs refl: $meanvaluesurf2; dir unrefl: $meanvaluesurf_dir2; diff: $meanvaluesurf_diff2 .
obs black: $meanvaluesurf1; dir unrefl: $meanvaluesurf_dir1; diff: $meanvaluesurf_diff1\n" ;
              }

              if ( ( "plain" ~~ @calcprocedures ) and not ( "embedded" ~~ @calcprocedures ) )
              {
                print "obs refl: $meanvaluesurf2; obs black: $meanvaluesurf1\n" ;
              }

              if ( ( "radical" ~~ @calcprocedures ) and not ( "embedded" ~~ @calcprocedures ) )
              {
                print "model obs refl: $meanvaluesurf2; dir unrefl: $meanvaluesurf_dir2; diff: $meanvaluesurf_diff2 .
no obs: $meanvaluesurf1; dir unrefl: $meanvaluesurf_dir1; diff: $meanvaluesurf_diff1\n" ;
              }

              unless ( ( "noreflections" ~~ @calcprocedures ) or ( "plain" ~~ @calcprocedures ) or ( "embedded" ~~ @calcprocedures ) )
              {
                say "model 1 diff: $meanvaluesurf_diff1, model 1 tot: $meanvaluesurf1, model 1 dir: $meanvaluesurf_dir1. \nmodel 2 diff $meanvaluesurf_diff2, model 2 tot: $meanvaluesurf2, model 2 dir: $meanvaluesurf_dir2\n";
              }
            }
          }
          $counthour++;
        }
        $countsurf++;
      }
      $countmonth++;
    }
    $count++;
  }
  $" = ",";
  say MONITOR "OBTAINED IRRS: " . dump( \%irrs );
  return ( \%irrs );
}


sub cleanblanks
{ # SELF-EXPLAINING.
  my @arr = @_;
  my @box;
  my $count = 0;
  foreach my $el ( @arr )
  {
    unless ( ( $el eq "" ) or ( $el eq "\s+" ) )
    {
      push ( @box, $el );
    }
    $count++;
  }
  return( @box );
}


sub createconstrdbfile
{ # THIS CREATES THE CONSTRUCTION DB FILE OF THE FICTITIOUS ESP-r MODELS.
  my ( $constrdbfile, $constrdbfile_f, $obscon_ref, $matdbfile, $calcprocedures_ref, $paths_ref, $modishlock ) = @_;
  my @obscon = @ { $obscon_ref };
  my @calcprocedures = @{ $calcprocedures_ref };

  say MONITOR "OBSCON: " . dump ( @obscon );

  my ( @bigcopy, @updatedlines, %exportconstr );

  open ( DBFILE, "$constrdbfile" ) or die;
  my @lines = <DBFILE>;
  close DBFILE;

  my $countcat = 0;
  open ( MATFILE, "$matdbfile" ) or die;
  my @matlines = <MATFILE>;
  close MATFILE;

  my @box;
  foreach my $matline ( @matlines )
  {
    if ( $matline =~ /^\*item/ )
    {
      my @elts = split( /,/, $matline );
      push( @box, $elts[2] );
    }
    elsif ( $matline =~ /^\*class/ )
    {
      $countcat++;
    }
  }
  my $countm = max( @box );

  say MONITOR "COUNTMAT $countm";
  say MONITOR "COUNTCAT $countcat";

  if ( $lines[0] =~ /\*CONSTRUCTIONS/ )
  {
    push ( @calcprocedures, "newconstrdb" );
  }
  else
  {
    push ( @calcprocedures, "oldconstrdb" );
  }

  say MONITOR "\@calcprocedures: " . dump ( @calcprocedures );

  foreach my $line ( @lines )
  { # THIS PUSHES IN @UPDATEDLINES THE CONSTR DATABASE EXCEPTED THE FIRST LINES (HEADER LINES)
    # This actually pushes all lines, including header lines, which explains the "if ( $countline > 2 )" below.
    chomp $line;
    my @row = split( /\s+|,/ , $line);
    @row = cleanblanks( @row );
    my ( $oldnumber, $newnumber );
    my $atline;
    if ( $line =~ /\# no of composites/ )
    {
      $atline == $countline;
      $oldnumber = $row[0];
      say MONITOR "\@obscon: " . dump( @obscon );
      $newnumber = ( $oldnumber + scalar( @obscon ) );
      $line =~ s/$oldnumber/$newnumber/;
      push ( @updatedlines, $line );
    }
    else
    {
      push ( @updatedlines, $line );
    }
    $countline++;
  }

  if ( "newconstrdb" ~~ @calcprocedures )
  {
    if ( $line =~ /^\*date,/ )
    {
      push ( @updatedlines, "*Category,Modish_fict, Modish fictitious constructions, fictitious versions of existing constructions used for shading factor modifier script Modish\n" );
    }
  }

  #say MONITOR "\@updatedlines: " . dump ( @updatedlines );

  if ( "oldconstrdb" ~~ @calcprocedures )
  {
    # --- OLD CONSTR DATABASE ---
    foreach my $el ( @obscon )
    { #PUSHES EACH CONSTRUCTION USED IN THE OBSTRUCTIONS IN @COPY, AND PUSHES EACH [ @COPY ] IN @BIGCOPY
      my @copy;
      my $semaphore = 0;
      $countel = 0;
      $countline = 0;
      foreach my $line ( @updatedlines )
      {
        my @row = split( /\s+|,/ , $line);
        @row = cleanblanks( @row );

        if ( $el eq $row[1] )
        {
          $semaphore = 1;
        }

        if ( ( $semaphore == 1 ) and ( $countel == 0) )
        {
          push ( @copy, "# layers  description   optics name   symmetry tag" );
          push ( @copy, $line );
          $countel++;
        }
        elsif ( ( $semaphore == 1 ) and ( $countel > 0) )
        {
          push ( @copy, $line );
          if (  ( $row[0] eq "#" ) and ( $row[1] eq "layers" ) and ( $row[2] eq "description" )  )
          {
            pop(@copy);
            $semaphore = 0;
          }
          $countel++;
        }
        $countline++;
      }
      say MONITOR "COPY: " . dump ( @copy );
      push ( @bigcopy, [ @copy ] );
    } # --- END OLD CONSTR DATABASE ---
    say MONITOR "BIGCOPY: " . dump ( @bigcopy );
  }
  elsif ( not ( "oldconstrdb" ~~ @calcprocedures ) )
  {
    # --- NEW CONSTR DATABASE ---
    # If there are obstruction constructions, loop through each @updatedlines.
    # If an obstruction construction is found, push this into @copy, and push each [ @copy ] into @bigcopy.
    my @copy;
    my $semaphore = 0;
    my $nummatches = 0;
    my $numobsconstr = scalar @obscon;
    say MONITOR "\$numobsconstr : " . dump ( $numobsconstr );
    say MONITOR "\%obsinf " . dump ( %obsinf );

    if
    ( $numobsconstr > 0 )
    {
      foreach my $line ( @updatedlines )
      {
        my @row = split( /,/ , $line);
        @row = cleanblanks( @row );
        if ( ( $semaphore == 0 ) and ( $row[0] == "*item" ) and ( any { $_ eq $row[1] } @obscon ) )
        {
          $semaphore = 1;
        }
        if ( $semaphore == 1 )
        {
          push ( @copy, $line );

          if ( $row[0] =~ /\*end_item/ )
          {
            $semaphore = 0;
            push ( @bigcopy, [ @copy ] );
            undef ( @copy );
            $nummatches++;
            if ( $nummatches == $numobsconstr ) { last; }
          }
        }
      }
    }
  }

  #say MONITOR "BIGCOPY  " .dump ( @bigcopy );

  my ( @newbigcopy, @materials, @newmaterials, @materialnums );
  if ( "oldconstrdb" ~~ @calcprocedures )
  {  # --- OLD CONSTR DATABASE ---
     my ( $newmatinssurf, $newmatextsurf );
     foreach my $copyref ( @bigcopy )
     {
       my @newcopy;
       my @constrlines = @$copyref;

       my $onlyonelayer = 0;
       if ( scalar( @constrlines ) == 4 )
       {
         $onlyonelayer = 1;
       }

       my $firstrow = $constrlines[1];
       my @row = split ( /\s+|,/ , $firstrow );
       @row = cleanblanks( @row );
       my $constrname = $row[1];
       my $newconstrname = $constrname;


        chop  $newconstrname;
	chop $newconstrname;
	$newconstrname = "f_" . "$newconstrname";


       my $intlayer = $constrlines[3];
       my @row = split ( /\s+|,/ , $intlayer );
       @row = cleanblanks( @row );


	my $matintlayer = $row[2];
	my $newmatintlayer = $matintlayer;
	my $matintlayernum = $row[0];


	$newmatintlayer = "f_" . "$newmatintlayer";

       my $extlayer = $constrlines[$#constrlines];
       my @row = split ( /\s+|,/ , $extlayer );
       @row = cleanblanks( @row );

        my $matextlayer = $row[2];
        my $newmatextlayer = $matextlayer;
        my $matextlayernum = $row[0];


        $newmatextlayer = "f_" . "$newmatextlayer";

	if ( ( not( $matintlayer ~~ @materials ) ) and ( $matintlayer ne "" ) and ( not ( $matintlayer =~ /^f_/ ) ) )
        {
	   push ( @materials, $matintlayer );
	   push ( @materialnums, $matintlayernum );
        }

	if ( ( not( $matextlayer ~~ @materials ) ) and ( $matextlayer ne "" ) and ( not ( $matextlayer =~ /^f_/ ) ) )
        {
	   push ( @materials, $matextlayer );
	   push ( @materialnums, $matextlayernum );
        }

	 if ( ( not( $newmatintlayer ~~ @newmaterials ) ) and ( $newmatintlayer ne "" ) and ( $newmatintlayer =~ /^f_/ ) )
         {
	    push ( @newmaterials, $newmatintlayer );
	 }

	 if ( ( not( $newmatextlayer ~~ @newmaterials ) ) and ( $newmatextlayer ne "" ) and ( $newmatextlayer =~ /^f_/ ) )
         {
	    push ( @newmaterials, $newmatextlayer );
         }


       $constrlines[1] =~ s/$constrname/$newconstrname/g;

       unless ( $onlyonelayer == 1 )
       {
         $constrlines[3] =~ s/$matintlayer/$newmatintlayer/g;
       }

       my @intels = split( /\s+/, $constrlines[3] ); say MONITOR "INTELS " . dump( @intels );
       @intels = cleanblanks( @intels ); say MONITOR "CLEANED-INTELS " . dump( @intels );
       my $intoldmat = $intels[0]; say MONITOR "\$intoldmat-INTELS " . dump( $intoldmat );

       $constrlines[$#constrlines] =~ s/$matextlayer/$newmatextlayer/g;

	my @extels = split( /\s+/, $constrlines[$#constrlines] ); say MONITOR "\@extels " . dump( @extels );
	@extels = cleanblanks( @extels ); say MONITOR "CLEANED-extels " . dump( @extels );
	my $extoldmat = $extels[0]; say MONITOR "\$extoldmat-INTELS " . dump( $extoldmat );

       foreach my $line ( @constrlines )
       {
         push ( @newcopy, $line );
       }
       push ( @newbigcopy, [ @newcopy ] );

       if ( $newmatextlayer ne "" )
       {
         $exportconstr{ $newconstrname }{ extnewlayername } = $newmatextlayer;
       }

       if ( $matextlayer ne "" )
       {
         $exportconstr{ $newconstrname }{ extoldlayername } = $matextlayer;
       }

       if ( $extoldmat ne "" )
       {
         $exportconstr{ $newconstrname }{ extoldlayernum } = $extoldmat;
       }

       if ( $newmatintlayer ne "" )
       {
         $exportconstr{ $newconstrname }{ intnewlayername } = $newmatintlayer;
       }

       if ( $matintlayer ne "" )
       {
         $exportconstr{ $newconstrname }{ intoldlayername } = $matintlayer;
       }

       if ( $intoldmat ne "" )
       {
         $exportconstr{ $newconstrname }{ intoldlayernum } = $intoldmat;
       }
     } # --- END OLD CONSTR DATABASE ---
  }
  elsif ( "newconstrdb" ~~ @calcprocedures )
  {
    # --- NEW CONSTR DATABASE ---
    # In each [ @copy ], modify the:
    # construction name and documentation,
    # category,
    # internal material name, and
    # external material name.
    # Store the old and new material names, and form a hash relating new materials to the new constructions.

    my ( $newmatinssurf, $newmatextsurf );
    foreach my $copyref ( @bigcopy )
    {
      my @newcopy;
      my @constrlines = @{ $copyref };

      my $onlyonelayer = 0;
      if ( scalar( @constrlines ) == 6 )
      {
        $onlyonelayer = 1;
      }

      my $firstrow = $constrlines[0];
      my @row = split ( /,/ , $firstrow );
      @row = cleanblanks( @row );
      my $constrname = $row[1];
      my $newconstrname = $constrname;

      chop $newconstrname;
      chop $newconstrname;

      $newconstrname = "f_" . "$newconstrname";

      my $intlayer = $constrlines[4];
      my @row = split ( / : |,/ , $intlayer );
      @row = cleanblanks( @row );
      my $matintlayer = $row[3];
      my $newmatintlayer = $matintlayer;
      my $matintlayernum = $row[1];

      chop $newmatintlayer ;
      chop $newmatintlayer ;

      $newmatintlayer = "f_" . "$newmatintlayer";


      my $extlayer = $constrlines[$#constrlines-1];
      my @row = split ( / : |,/ , $extlayer );
      @row = cleanblanks( @row );


      my $matextlayer = $row[3];
      my $newmatextlayer = $matextlayer;
      my $matextlayernum = $row[1];

      chop $newmatextlayer ;
      chop $newmatextlayer ;

      $newmatextlayer = "f_" . "$newmatextlayer";

       if ( ( not( $matintlayer ~~ @materials ) ) and ( $matintlayer ne "" ) and ( not ( $matintlayer =~ /^f_/ ) ) )
        {
	   push ( @materials, $matintlayer );
	   push ( @materialnums, $matintlayernum );
        }

	if ( ( not( $matextlayer ~~ @materials ) ) and ( $matextlayer ne "" ) and ( not ( $matextlayer =~ /^f_/ ) ) )
        {
	   push ( @materials, $matextlayer );
	   push ( @materialnums, $matextlayernum );
        }

	 if ( ( not( $newmatintlayer ~~ @newmaterials ) ) and ( $newmatintlayer ne "" ) and ( $newmatintlayer =~ /^f_/ ) )
         {
	    push ( @newmaterials, $newmatintlayer );
	 }

	 if ( ( not( $newmatextlayer ~~ @newmaterials ) ) and ( $newmatextlayer ne "" ) and ( $newmatextlayer =~ /^f_/ ) )
         {
	    push ( @newmaterials, $newmatextlayer );
         }

      $constrlines[0] =~ s/$constrname/$newconstrname/;
      $constrlines[1] = "*itemdoc,fictitious version of construction " . $constrname . " created by the Modish procedure";
      $constrlines[2] = "*incat,Modish_fict";

      unless ( $onlyonelayer == 1 )
      {
         $constrlines[4] =~ s/$matintlayer/$newmatintlayer/;
      }

      my @intels = split( /,/, $constrlines[4] ); say MONITOR "INTELS " . dump( @intels );
      @intels = cleanblanks( @intels ); say MONITOR "CLEANED-INTELS " . dump( @intels );
      my $intoldmat = $intels[1]; say MONITOR "\$intoldmat-INTELS " . dump( $intoldmat );

       $constrlines[$#constrlines-1] =~ s/$matextlayer/$newmatextlayer/;

       my @extels = split( /,/, $constrlines[$#constrlines-1] ); say MONITOR "\@extels " . dump( @extels );
       @extels = cleanblanks( @extels ); say MONITOR "CLEANED-extels " . dump( @extels );
       my $extoldmat = $extels[1]; say MONITOR "\$extoldmat-INTELS " . dump( $extoldmat );

      foreach my $line ( @constrlines )
      {
        push ( @newcopy, $line );
      }
      push ( @newbigcopy, [ @newcopy ] );

      if ( $newmatextlayer ne "" )
      {
        $exportconstr{ $newconstrname }{ extnewlayername } = $newmatextlayer;
      }

      if ( $matextlayer ne "" )
      {
        $exportconstr{ $newconstrname }{ extoldlayername } = $matextlayer;
      }

      if ( $extoldmat ne "" )
      {
        $exportconstr{ $newconstrname }{ extoldlayernum } = $extoldmat;
      }

      if ( $newmatintlayer ne "" )
      {
        $exportconstr{ $newconstrname }{ intnewlayername } = $newmatintlayer;
      }

      if ( $matintlayer ne "" )
      {
        $exportconstr{ $newconstrname }{ intoldlayername } = $matintlayer;
      }

      if ( $intoldmat ne "" )
      {
        $exportconstr{ $newconstrname }{ intoldlayernum } = $intoldmat;
      }
    }
  }

  my @oldmaterials =  @materials;
  say MONITOR "\@newmaterials " . dump( @newmaterials );
  say MONITOR "\@oldmaterials " . dump( @oldmaterials );
  @newmaterials = uniq( @newmaterials ); say MONITOR "CLEANED \@newmaterials " . dump( @newmaterials );
  @oldmaterials = uniq( @oldmaterials ); say MONITOR "CLEANED \@oldmaterials " . dump( @oldmaterials );

  my $newclass = $countcat + 1;

  my %newms;
  my $oldcountm = $countm;
  foreach my $newmat ( @newmaterials )
  {
    $newms{$newmat}{num} = $countm++;
  }

  my %conv;
  foreach $newconstrname ( sort { $a <=> $b } ( keys %exportconstr ) )
  {
    if ( $newconstrname ne "" )
    {
      if ( $newms{$exportconstr{ $newconstrname }{intnewlayername}}{num} ne "" )
      {
	if ( $exportconstr{ $newconstrname }{intnewlayername} =~ /^f_/ )
	{
          $exportconstr{ $newconstrname }{ intnewlayernum } = $newms{$exportconstr{ $newconstrname }{intnewlayername}}{num};
	}
      }

      if ( $newms{$exportconstr{ $newconstrname }{extnewlayername}}{num} ne "" )
      {
	if ( $exportconstr{ $newconstrname }{extnewlayername} =~ /^f_/ )
	{
	 $exportconstr{ $newconstrname }{ extnewlayernum } = $newms{$exportconstr{ $newconstrname }{extnewlayername}}{num};
	}
      }


      if ( $exportconstr{ $newconstrname }{ intoldlayernum } ne "" )
      {
        $conv{$exportconstr{ $newconstrname }{ intnewlayernum }}{num} = $exportconstr{ $newconstrname }{ intoldlayernum };
      }

      if ( $exportconstr{ $newconstrname }{ extoldlayernum } ne "" )
      {
        $conv{$exportconstr{ $newconstrname }{ extnewlayernum }}{num} = $exportconstr{ $newconstrname }{ extoldlayernum };
      }

      if ( $exportconstr{ $newconstrname }{ intnewlayernum } ne "" )
      {
        $conv{$exportconstr{ $newconstrname }{ intoldlayernum }}{num} = $exportconstr{ $newconstrname }{ intnewlayernum };
      }

      if ( $exportconstr{ $newconstrname }{ extnewlayernum } ne "" )
      {
        $conv{$exportconstr{ $newconstrname }{ extoldlayernum }}{num} = $exportconstr{ $newconstrname }{ extnewlayernum };
      }


      if ( $exportconstr{ $newconstrname }{ intoldlayername } ne "" )
      {
        $conv{$exportconstr{ $newconstrname }{ intnewlayernum }}{name} = $exportconstr{ $newconstrname }{ intoldlayername };
      }

      if ( $exportconstr{ $newconstrname }{ extoldlayername } ne "" )
      {
        $conv{$exportconstr{ $newconstrname }{ extnewlayernum }}{name} = $exportconstr{ $newconstrname }{ extoldlayername };
      }

      if ( $exportconstr{ $newconstrname }{ intnewlayername } ne "" )
      {
        $conv{$exportconstr{ $newconstrname }{ intoldlayernum }}{name} = $exportconstr{ $newconstrname }{ intnewlayername };
      }

      if ( $exportconstr{ $newconstrname }{ extnewlayername } ne "" )
      {
        $conv{$exportconstr{ $newconstrname }{ extoldlayernum }}{name} = $exportconstr{ $newconstrname }{ extnewlayername };
      }
    }
  }

  my %throwaway;
  foreach my $key ( keys %conv )
  {
      unless ( $key eq "" )
      {
          $throwaway{$key} = $conv{$key};
      }
   }
   %conv = %throwaway;
   say MONITOR "CONV " . dump( \%conv );

  my @oldmatnums = @materialnums;
  say MONITOR "\@oldmatnums " . dump( @oldmatnums );
  @oldmatnums = uniq( @oldmatnums ); say MONITOR "CLEANED \@oldmatnums " . dump( @oldmatnums );

  say MONITOR "NEWBIGCOPY  " .dump ( @newbigcopy );
  say MONITOR "\%exportconstr " .dump ( \%exportconstr );

  my  @newmatnums;
  foreach my $oldmatnum ( @oldmatnums )
  {
     push( @newmatnums, $conv{$oldmatnum}{num} );
  }
  say MONITOR "\@newmatnums " . dump( @newmatnums );
  @newmatnums = uniq( @newmatnums ); say MONITOR "CLEANED \@newmatnums " . dump( @newmatnums );

  my ( @lastcopy );
  if ( "oldconstrdb" ~~ @calcprocedures )
  {
     foreach my $copyref ( @newbigcopy )
     {
       my @constrlines = @{ $copyref };

       my $onlyonelayer = 0;
       if ( scalar( @constrlines ) == 4 ) { $onlyonelayer = 1 }

       unless ( $onlyonelayer == 1)
       {
         my $intlayer = $constrlines[3];
         my @row = split ( /\s+/ , $intlayer ); say MONITOR "\@row INT " .dump( @row );
         @row = cleanblanks( @row ); say MONITOR "\@row CLEANED " .dump( @row ); say MONITOR "\@row0 " .dump( $row[0] );
         $row[0] = $conv{ $row[0] }{num}; say MONITOR "\@row0 CONVERTED " .dump( $row[0] );

         if ( length( $row[0] ) == 1 )
         {
           $row[0] = "    " . $row[0] . "   ";
           $row[1] = $row[1] . " ";
           $constrlines[3] = join( " ", @row)
         }
         elsif ( length( $row[0] ) == 2 )
         {
           $row[0] = "   " . $row[0] . "   ";
           $row[1] = $row[1] . " ";
           $constrlines[3] = join( " ", @row )
         }
         elsif ( length( $row[0] ) == 3 )
         {
           $row[0] = "  " . $row[0] . "   ";
           $row[1] = $row[1] . " ";
           $constrlines[3] = join( " ", @row)
         }
         elsif ( length( $row[0] ) == 4 )
         {
           $row[0] = " " . $row[0] . "   ";
           $row[1] = $row[1] . " ";
           $constrlines[3] = join( " ", @row)
         }
       }

       my $extlayer = $constrlines[$#constrlines];
       my @row = split ( /\s+/ , $extlayer ); say MONITOR "\@row EXT " .dump( @row );
       @row = cleanblanks( @row ); say MONITOR "\@row CLEANED " .dump( @row ); say MONITOR "\@row0 " .dump( $row[0] );
       $row[0] = $conv{ $row[0] }{num}; say MONITOR "\@row0 CONVERTED " .dump( $row[0] );

       if ( length( $row[0] ) == 1 )
       {
         $row[0] = "    " . $row[0] . "   ";
         $row[1] = $row[1] . " ";
         $constrlines[$#constrlines] = join( " ", @row)
       }
       elsif ( length( $row[0] ) == 2 )
       {
         $row[0] = "   " . $row[0] . "   ";
         $row[1] = $row[1] . " ";
         $constrlines[$#constrlines] = join( " ", @row)
       }
       elsif ( length( $row[0] ) == 3 )
       {
         $row[0] = "  " . $row[0] . "   ";
         $row[1] = $row[1] . " ";
         $constrlines[$#constrlines] = join( " ", @row)
       }
       elsif ( length( $row[0] ) == 4 )
       {
         $row[0] = " " . $row[0] . "   ";
         $row[1] = $row[1] . " ";
         $constrlines[$#constrlines] = join( " ", @row)
       }
       push ( @lastcopy, @constrlines );
     }
  }
  elsif ( "newconstrdb" ~~ @calcprocedures )
  { # --- NEW CONSTR DATABASE ---
    foreach my $copyref ( @newbigcopy )
    {
      my @constrlines = @{ $copyref };

      $onlyonelayer = 0;
      if ( scalar( @constrlines ) == 6 )
      {
        $onlyonelayer = 1;
      }

      unless ( $onlyonelayer == 1)
      {

        my $intlayer = $constrlines[4];
        my @row = split ( /,/ , $intlayer );
        @row = cleanblanks( @row );
        $row[1] = $conv{ $row[1] }{num}; say MONITOR "\@row0 CONVERTED " .dump( $row[1] );

        $constrlines[4] = join( ",", @row);
      }

      my $extlayer = $constrlines[$#constrlines-1];
      my @row = split ( /,/ , $extlayer ); say MONITOR "\@row EXT " .dump( @row );
      @row = cleanblanks( @row ); say MONITOR "\@row CLEANED " .dump( @row ); say MONITOR "\@row0 " .dump( $row[0] );
      $row[1] = $conv{ $row[1] }{num}; say MONITOR "\@row1 CONVERTED " . dump( $row[1] );

      $constrlines[$#constrlines-1] = join( ",", @row);

      push ( @lastcopy, @constrlines );
    }
  }

  say MONITOR "LASTCOPY " .dump ( @lastcopy );
  push ( @updatedlines, @lastcopy );

  open ( CONSTRDBFILE_F, ">$constrdbfile_f" ) or die;

  if ( "oldconstrdb" ~~ @calcprocedures )
  {
    foreach ( @updatedlines )
    {
      say CONSTRDBFILE_F $_;
    }
  }
  elsif ( "newconstrdb" ~~ @calcprocedures )
  {
    foreach ( @updatedlines )
    {
      unless( $_ =~ /\*db_end/ )
      {
        say CONSTRDBFILE_F $_;
      }
    }
    say CONSTRDBFILE_F "*db_end";
  }

  close CONSTRDBFILE_F;
  return ( \@oldmaterials, \@newmaterials, \%exportconstr, \%conv,
    $countm, $oldcountm, $newclass, \@oldmatnums, \@newmatnums );
}


sub compareirrs
{ # THIS COMPARES THE IRRADIANCES TO OBTAIN THE IRRADIANCE RATIOS.
  my ( $zonefilelistsref, $irrsref, $computype, $calcprocedures_ref, $selectives_ref ) = @_;
  my %zonefilelists = %$zonefilelistsref;
  my %irrs = %$irrsref;
  my @calcprocedures = @$calcprocedures_ref;
  my @selectives = @{ $selectives_ref };

  my %irrvars;
  foreach my $zonenum ( sort {$a <=> $b} ( keys %irrs ) )
  {
    my $shdfile = $zonefilelists{ $zonenum }{ shdfile };
    foreach my $monthnum ( sort {$a <=> $b} ( keys %{ $irrs{ $zonenum }{ 1 } } ) )
    {
      foreach my $surfnum ( sort {$a <=> $b} ( keys %{ $irrs{ $zonenum }{ 1 }{ $monthnum } } ) )
      {
        foreach my $hour ( sort {$a <=> $b} ( keys %{ $irrs{ $zonenum }{ 1 }{ $monthnum }{ $surfnum } } ) )
        {
          my $diffsurfirr = $irrs{ $zonenum }{ 1 }{ $monthnum }{ $surfnum }{ $hour }{ meandiffirr }; say REPORT "\$diffsurfirr: $diffsurfirr";
          my $whitediffsurfirr;

          if ( scalar( @selectives == 0 ) )
          {
            $whitediffsurfirr = $irrs{ $zonenum }{ 2 }{ $monthnum }{ $surfnum }{ $hour }{ meandiffirr };
            say REPORT "$whitediffsurfirr: $whitediffsurfirr";
          }
          elsif ( scalar( @selectives > 0 ) )
          {
            $whitediffsurfirr = ( ( $irrs{ $zonenum }{ 3 }{ $monthnum }{ $surfnum }{ $hour }{ meandiffirr }
              + $irrs{ $zonenum }{ 4 }{ $monthnum }{ $surfnum }{ $hour }{ meandiffirr } ) / 2 ); # CHECK THESE 3 AND 4.
          }

          my ( $diffirrratio, $midterm );

          if ( $diffsurfirr < 0 ) { $diffsurfirr = 0 }
          if ( $whitediffsurfirr < 0 ) { $whitediffsurfirr = 0 }


          unless ( $diffsurfirr == 0 )
          {
            $diffirrratio = ( $whitediffsurfirr / $diffsurfirr ); say REPORT "\$diffirrratio: $diffirrratio";
          }
          else
          {
            $diffirrratio = "1.0000";
          }

          $irrvars{ $zonenum }{ $monthnum }{ $surfnum }{ $hour }{ diffirrratio } = $diffirrratio;

          $irrvars{ $zonenum }{ $monthnum }{ $surfnum }{ $hour }{ modrelation } = $irrs{ $zonenum }{ 0 }{ $monthnum }{ $surfnum }{ $hour }{ modrelation };

          unless ( ( "alldiff" ~~ @calcprocedures ) or ( "keepdirshdf" ~~ @calcprocedures ) )
          {

            $dirsurfirr = $irrs{ $zonenum }{ 1 }{ $monthnum }{ $surfnum }{ $hour }{ meandirirr }; say REPORT "\$dirsurfirr: $dirsurfirr";

            my $whitedirsurfirr;
            if ( scalar( @selectives == 0 ) )
            {
              $whitedirsurfirr = $irrs{ $zonenum }{ 2 }{ $monthnum }{ $surfnum }{ $hour }{ meandirirr }; say REPORT "\$whitedirsurfirr: $whitedirsurfirr";
            }
            elsif ( scalar( @selectives > 0 ) )
            {
              $whitedirsurfirr = ( ( $irrs{ $zonenum }{ 3 }{ $monthnum }{ $surfnum }{ $hour }{ meandirirr }
                + $irrs{ $zonenum }{ 4 }{ $monthnum }{ $surfnum }{ $hour }{ meandirirr } ) / 2 ); # CHECK THESE 3 AND 4.
            }

            my $dirirrratio;

            if ( $dirsurfirr < 0 ) { $dirsurfirr = 0 }
            if ( $whitedirsurfirr < 0 ) { $whitedirsurfirr = 0 }


            unless ( $dirsurfirr == 0 )
            {
              $dirirrratio = ( $whitedirsurfirr / $dirsurfirr );
            }
            else
            {
              $dirirrratio = "1.0000";
            }

            $irrvars{ $zonenum }{ $monthnum }{ $surfnum }{ $hour }{ dirirrratio } = $dirirrratio;

            $irrvars{ $zonenum }{ $monthnum }{ $surfnum }{ $hour }{ modrelation } = $irrs{ $zonenum }{ 0 }{ $monthnum }{ $surfnum }{ $hour }{ modrelation };
          }

          $countsurf++;
        }
        $counthour++;
      }
      $countmonth++;
    }
    $countzone++;
  }
  say MONITOR "OBTAINED IRRVARS: " . dump ( \%irrvars );
  return ( \%irrvars );
}


sub fillhours
{ # THIS COMPLETES THE FILLING OF THE DATA STRUCTURES GIVING INFORMATION ABOUT THE DAYLIT HOURS.
  my ( $newhourvalsref, $monthname, $daylighthoursref ) = @_;
  my @hadhours = @$newhourvalsref;
  my %lithours = %$daylighthoursref;
  my @monthhours = @{ $lithours{ $monthname } };
  my @values;

  my $sunhoursnum = 0;
  foreach my $lightcond ( @monthhours )
  {
    unless ( $lightcond == 1 )
    {
      $sunhoursnum++;
    }
  }

  if ( $sunhoursnum == scalar( @hadhours ) )
  {
    my $counthr = 1;
    my $countlit = 0;
    foreach my $lightcond ( @monthhours )
    {
      if ( $lightcond == 1 )
      {
        push ( @values, "1.0000" );
      }
      else
      {
        push ( @values, $hadhours[ $countlit ] );
        $countlit++;
      }
      $counthr++;
    }
    return ( @values );
  }
}


sub modifyshda
{ # THIS MODIFIES THE ".shda" FILE ON THE BASIS OF THE IRRADIANCE RATIOS.
  my ( $comparedirrsref, $surfslistref, $zonefilelistsref, $shdfileslistref, $daylighthoursref, $irrvarsref, $tempmod, $tempreport, $tempmoddir, $tempreportdir, $elm, $radtype, $calcprocedures_ref, $irrs_ref, $conffile_f2, $shdfile, $surfs_ref, $mymonth ) = @_; ##### CONDITION! "diffuse" AND "direct".
  my %surfslist = %$surfslistref;
  my %zonefilelists = %$zonefilelistsref;
  my %shdfileslist = %$shdfileslistref;
  my %daylighthours = %$daylighthoursref;
  my %irrvars = %$irrvarsref;
  my @calcprocedures = @$calcprocedures_ref;
  my %irrs = %{ $irrs_ref };

  if ( "embedded" ~~ @calcprocedures )
  {
    $monthnum = $mymonth;
  }

  my ( @printcontainer, @monthnames, @pushmodline, @pushreportline, @mainbag, @mainoriginal );

  foreach my $zonenum ( sort {$a <=> $b} ( keys %irrvars ) )
  {
    my $inlinesref = $shdfileslist{ $zonenum };
    my @inlines = @$inlinesref;

    my ( @surfsdo, @vehicle );
    my ( @insertlines, @inserts, $bringline ) ;
    if ( ( "keepdirshdf" ~~ @calcprocedures ) and ( $radtype eq "direct" ) )
    {
      foreach my $monthnum ( sort {$a <=> $b} ( keys %{ $irrvars{ $zonenum } } ) )
      {
        foreach my $surfnum ( sort {$a <=> $b} ( keys %{ $irrvars{ $zonenum }{ $monthnum } } ) )
        {
          my $surfname = $surfslist{$zonenum}{$surfnum}{surfname};
          push( @surfsdo, $surfname );
        }
        last;
      }

      my ( $insertlines_ref ) = readshdfile( $shdfile, \@calcprocedures, $conffile_f2, \%paths, $zonenum, \%zonenumname, $mymonth, "go" );
      @insertlines = @{ $insertlines_ref };

      foreach my $lin ( @insertlines )
      {
        my $signal = "no";
        foreach my $surf ( @surfsdo )
        {
          if ( $lin =~ /$surf/ )
          {
            $signal = "yes";
          }
        }

        if ( ( $lin =~ /direct - surface / ) and ( $signal eq "yes" ) )
        {
          my @elts = split( "#", $lin );
          $bringline = $elts[0];
          push ( @vehicle, $bringline );
        }
      }
    }

    my $semaphore = 0;
    my ( $readmonthname, $readmonthnum );
    my $c = 0;
    foreach my $line ( @inlines )
    {
      my $line2;
      my @row = split( /\s+|,/ , $line);
      my ( $readsurfname, $readsurfnum );

      if ( ( $row[0] eq "*" ) and ( $row[1] eq "month:" ) )
      {
        $semaphore = 1;
        $readmonthname = $row[2];
        $readmonthname =~ s/`//g;
        $readmonthnum = getmonthnum( $readmonthname );
      }

      if ( ( ( $row[0] eq "24" ) and ( $row[1] eq "hour" ) and ( $row[1] eq "surface" ) ) or ( $row[0] eq "*end" ) )
      {
        $semaphore = 0;
      }

      my ( @newhourvals, @newhourvals2, @were );
      foreach my $monthnum ( sort {$a <=> $b} ( keys %{ $irrvars{ $zonenum } } ) )
      {
        my $monthname = getmonthname( $monthnum );
        push ( @monthnames, $monthname );
        push ( @monthnames2, $monthname );

        foreach my $surfnum ( sort {$a <=> $b} ( keys %{ $irrvars{ $zonenum }{ $monthnum } } ) )
        {
          my $surfname = $surfslist{$zonenum}{$surfnum}{surfname};
          push( @surfsdo, $surfname );
          foreach my $hour ( sort {$a <=> $b} ( keys %{ $irrvars{ $zonenum }{ $monthnum }{ $surfnum } } ) )
          {
            my ( $irrratio, $newshadingvalue );
            if ( $radtype eq "diffuse" )
            {
              $irrratio = $irrvars{ $zonenum }{ $monthnum }{ $surfnum }{ $hour }{ diffirrratio };
            }
            elsif ( $radtype eq "direct" )
            {
              $irrratio = $irrvars{ $zonenum }{ $monthnum }{ $surfnum }{ $hour }{ dirirrratio };
            }
            $modrelation = $irrs{ $zonenum }{ 0 }{ $monthnum }{ $surfnum }{ $hour }{ modrelation };

            my $pass;
            unless ( ( ( "noreflections" ~~ @calcprocedures ) or ( ( "composite" ~~ @calcprocedures ) and not ( "groundreflections" ~~ @calcprocedures ) ) )
              and not( $modrelation =~ /$RE{num}{real}/ ) )
            {
              $pass ="yes";
            }

            if ( ( $zonenum ) and ( $monthnum ) and ( $hour ) and ( $surfnum ) and ( $surfname ) and ( $monthname ) and ( $surfnum eq $elm ) ) # I.E. IF ALL THE NEEDED DATA EXIST
            {
              if ( $semaphore == 1 )
              {
                if ( $row[27] eq "surface" )
                {
                  $readsurfname = $row[28];
                  $readsurfnum = $surfslist{$zonenum}{$readsurfname}{surfnum} ;
                  my @filledhourvals;
                  if ( ( $row[25] eq $radtype ) and ( $readsurfname eq $surfname ) )
                  {
                    my @hourvals = ( @row[ 0..23 ] );
                    my $counthour = 1;
                    foreach my $el ( @hourvals )
                    { # $el IS THE SHADING FACTOR IN THE ORIGINAL SHDA FILE.
                      # %irrvariation IS THE IRRADIANCE DIFFERENCE BETWEEN THE "WHITE" MODEL AND THE "BLACK" MODEL.
                      # $ambase IS THE AMBIENT RADIATION WITHOUT SHADINGS

                      my ( $calcamount, $improvedguess, $newshadingvalue );

                      if ( ( $el ne "" ) and ( $el ne " " ) and ( defined($el) ) )
                      {
                        if ( $radtype eq "diffuse" )
                        {
                          unless ( "radical" ~~ @calcprocedures )
                          {
                            if ( "noreflections" ~~ @calcprocedures )
                            {
                              my ( $skycomp, $newskycomp );
                              if ( ( $modrelation ne "" ) and ( $pass eq "yes" ) )
                              {
                                my $skycomp = 1 - $el;
                                my $newskycomp = $skycomp - ( $skycomp * ( $modrelation ) );
                                $newshadingvalue = 1 - $newskycomp;
                                if ( $newshadingvalue > 1 )
                                {
                                  $newshadingvalue = 1.0000;
                                }
                              }
                              else
                              {
                                $newshadingvalue = $el;
                              }
                              $irrratio = $modrelation;
                            }
                            elsif ( ( ( "composite" ~~ @calcprocedures ) and ( "groundreflections" ~~ @calcprocedures ) ) or ( "plain" ~~ @calcprocedures ) )
                            {
                              $calcamount = ( 1 - $el ); # THIS IS THE RATIO OF NON-SHADED IRRADIATION AS CALCULATED BY THE ESP-r's ISH MODULE
                              $improvedguess = ( $calcamount * $irrratio ); # THIS IS THE RATIO ABOVE CORRECTED BY MULTIPLYING IT BY THE IRRADIANCE RATIO TO TAKE REFLECTIONS INTO ACCOUNT.
                              $newshadingvalue = ( 1 - $improvedguess ); # AS THE NAME SAYS, THIS IS THE NEW SHADING FACTOR.
                              if ( $newshadingvalue > $el ) { $newshadingvalue = $el }; # IF THE SHADING FACTOR IS INCREASING, KEEP THE OLD ONE.
                            }
                            elsif ( "composite" ~~ @calcprocedures )
                            {
                              if ( ( $modrelation ne "" ) and ( $pass eq "yes" ) )
                              {
                                my $skycomp = 1 - $el;
                                my $newskycomp = $skycomp - ( $skycomp * ( $modrelation ) );
                                $el = 1 - $newskycomp;
                                if ( $el > 1 )
                                {
                                $el = 1.0000;
                                }
                              }
                              $calcamount = ( 1 - $el ); # THIS IS THE RATIO OF NON-SHADED IRRADIATION AS CALCULATED BY THE ESP-r's ISH MODULE
                              $improvedguess = ( $calcamount * $irrratio ); # THIS IS THE RATIO ABOVE CORRECTED BY MULTIPLYING IT BY THE IRRADIANCE RATIO TO TAKE REFLECTIONS INTO ACCOUNT.
                              $newshadingvalue = ( 1 - $improvedguess ); # AS THE NAME SAYS, THIS IS THE NEW SHADING FACTOR.
                              # THIS WOULD BE A PITY HERE # if ( $newshadingvalue > $el ) { $newshadingvalue = $el }; # IF THE SHADING FACTOR IS INCREASING, KEEP THE OLD ONE.
                            }
                            else
                            {
                              $calcamount = ( 1 - $el ); # THIS IS THE RATIO OF NON-SHADED IRRADIATION AS CALCULATED BY THE ESP-r's ISH MODULE
                              $improvedguess = ( $calcamount * $irrratio ); # THIS IS THE RATIO ABOVE CORRECTED BY MULTIPLYING IT BY THE IRRADIANCE RATIO TO TAKE REFLECTIONS INTO ACCOUNT.
                              $newshadingvalue = ( 1 - $improvedguess ); # AS THE NAME SAYS, THIS IS THE NEW SHADING FACTOR.
                              if ( $newshadingvalue > $el ) { $newshadingvalue = $el }; # IF THE SHADING FACTOR IS INCREASING, KEEP THE OLD ONE.
                            }
                          }
                        }
                        elsif ( $radtype eq "direct" )
                        {
                          unless ( ( "alldiff" ~~ @calcprocedures ) or ( "radical" ~~ @calcprocedures ) )
                          {
                            if ( "noreflections" ~~ @calcprocedures )
                            {
                              if ( ( $modrelation ne "" ) and ( $pass eq "yes" ) )
                              {
                                my $skycomp = 1 - $el;
                                my $newskycomp = $skycomp - ( $skycomp * ( $modrelation ) );
                                $newshadingvalue = 1 - $newskycomp;
                                if ( $el > 1 )
                                {
                                $newshadingvalue = 1.0000;
                                }
                              }
                              else
                              {
                                $newshadingvalue = $el;
                              }
                              $irrratio = $modrelation;
                            }
                            elsif ( ( ( "composite" ~~ @calcprocedures ) and ( "groundreflections" ~~ @calcprocedures ) ) or ( "plain" ~~ @calcprocedures ) )
                            {
                              $calcamount = ( 1 - $el ); # THIS IS THE RATIO OF NON-SHADED IRRADIATION AS CALCULATED BY THE ESP-r's ISH MODULE
                              $improvedguess = ( $calcamount * $irrratio ); # THIS IS THE RATIO ABOVE CORRECTED BY MULTIPLYING IT BY THE IRRADIANCE RATIO TO TAKE REFLECTIONS INTO ACCOUNT.
                              $newshadingvalue = ( 1 - $improvedguess ); # AS THE NAME SAYS, THIS IS THE NEW SHADING FACTOR.
                              if ( $newshadingvalue > $el ) { $newshadingvalue = $el }; # IF THE SHADING FACTOR IS INCREASING, KEEP THE OLD ONE.
                            }
                            elsif ( "composite" ~~ @calcprocedures )
                            {
                              if ( ( $modrelation ne "" ) and ( $pass eq "yes" ) )
                              {
                                my $skycomp = 1 - $el;
                                my $newskycomp = $skycomp - ( $skycomp * ( $modrelation ) );
                                $el = 1 - $newskycomp;
                                if ( $el > 1 )
                                {
                                $el = 1.0000;
                                }
                              }
                              $calcamount = ( 1 - $el ); # THIS IS THE RATIO OF NON-SHADED IRRADIATION AS CALCULATED BY THE ESP-r's ISH MODULE
                              $improvedguess = ( $calcamount * $irrratio ); # THIS IS THE RATIO ABOVE CORRECTED BY MULTIPLYING IT BY THE IRRADIANCE RATIO TO TAKE REFLECTIONS INTO ACCOUNT.
                              $newshadingvalue = ( 1 - $improvedguess ); # AS THE NAME SAYS, THIS IS THE NEW SHADING FACTOR.
                              # THIS WOULD BE A PITY HERE # if ( $newshadingvalue > $el ) { $newshadingvalue = $el }; # IF THE SHADING FACTOR IS INCREASING, KEEP THE OLD ONE.
                            }
                            else
                            {
                              $calcamount = ( 1 - $el ); # THIS IS THE RATIO OF NON-SHADED IRRADIATION AS CALCULATED BY THE ESP-r's ISH MODULE
                              $improvedguess = ( $calcamount * $irrratio ); # THIS IS THE RATIO ABOVE CORRECTED BY MULTIPLYING IT BY THE IRRADIANCE RATIO TO TAKE REFLECTIONS INTO ACCOUNT.
                              $newshadingvalue = ( 1 - $improvedguess ); # AS THE NAME SAYS, THIS IS THE NEW SHADING FACTOR.
                              if ( $newshadingvalue > $el ) { $newshadingvalue = $el }; # IF THE SHADING FACTOR IS INCREASING, KEEP THE OLD ONE.
                            }
                          }
                          else
                          {
                            $newshadingvalue = $el;
                          }
                        }

                        if ( "radical" ~~ @calcprocedures )
                        {
                          if ( $radtype eq "diffuse" )
                          {
                            my $noshads = $irrs{ $zonenum }{ 1 }{ $monthnum }{ $surfnum }{ $hour }{ meandiffirr };
                            my $withshads = $irrs{ $zonenum }{ 2 }{ $monthnum }{ $surfnum }{ $hour }{ meandiffirr };

                            unless ( ( $noshads == 0 ) or ( $noshads eq "" ) )
                            {

                              if ( ( $withshads == 0.0001 ) and ( $noshads == 0.0001 ) )
                              {
                                $newshadingvalue = $el;
                              }
                              elsif ( $withshads == 0.0001 )
                              {
                                $newshadingvalue = $el;
                              }
                              elsif ( $noshads == 0.0001 )
                              {
                                $newshadingvalue = $el;
                              }
                              else
                              {
                                $irrratio = ( $withshads / $noshads );
                                $newshadingvalue = ( 1 - ( $withshads / $noshads ) );
                              }
                            }
                            else
                            {                               $newshadingvalue = $el;
                            }
                          }
                          elsif ( $radtype eq "direct" )
                          {
                            if ( ( "keepdirshdf" ~~ @calcprocedures ) or ( "alldiff" ~~ @calcprocedures ) )
                            {
                              $newshadingvalue = $el;
                            }
                            else
                            {
                              my $nodirshads = $irrs{ $zonenum }{ 1 }{ $monthnum }{ $surfnum }{ $hour }{ meandirirr };
                              my $withdirshads = $irrs{ $zonenum }{ 2 }{ $monthnum }{ $surfnum }{ $hour }{ meandirirr };
                              unless ( ( $nodirshads == 0 ) and ( $nodirshads eq "" ) )
                              {
                                if ( ( $withdirshads == 0.0001 ) and ( $nodirshads == 0.0001 ) )
                                {
                                  $newshadingvalue = $el;
                                }
                                elsif ( $withdirshads == 0.0001 )
                                {
                                  $newshadingvalue = $el;
                                }
                                elsif ( $nodirshads == 0.0001 )
                                {
                                  $newshadingvalue = $el;
                                }
                                else
                                {
                                  $irrratio = ( $withdirshads / $nodirshads );
                                  $newshadingvalue = ( 1 - ( $withdirshads / $nodirshads ) );
                                  if ( abs( $irrratio ) > 3 )
                                  {
                                    $newshadingvalue = $el;
                                  }
                                }
                              }
                              else
                              {
                                $newshadingvalue = $el;
                              }
                            }
                          }
                        }

                      }

                      if ( ( $counthour == $hour ) and ( $readmonthname eq $monthname ) and ( $readsurfnum == $surfnum ) ) # I.E.: IF THIS LINE IS THE RIGHT ONE...
                      {
                        if ( $newshadingvalue eq "" )
                        {
                          $newshadingvalue = $el;
                        }
                        elsif ( $newshadingvalue eq " " )
                        {
                          $newshadingvalue = $el;
                        }
                        elsif ( $newshadingvalue eq "  " )
                        {
                          $newshadingvalue = $el;
                        }
                        elsif ( $newshadingvalue eq " " )
                        {
                          $newshadingvalue = $el;
                        }
                        elsif ( $newshadingvalue >= 1 )
                        {
                          $newshadingvalue = "1.0000";
                        }
                        elsif ( $newshadingvalue == 0 )
                        {
                          $newshadingvalue = "0.0000";
                        }
                        elsif ( ( $newshadingvalue > 0 ) and ( $newshadingvalue < 1 ) ) # IF THE VARIATION OF IRRADIANCE FROM MODEL A AND MODEL B IS NEGATIVE...
                        { # ...INCREASE THE SHADING FACTOR ACCORDINGLY.
                          $newshadingvalue = sprintf ( "%.4f", $newshadingvalue ); # FORMAT THE NUMBER SO THAT IT HAS FOUR DECIMALS
                        }
                        elsif ( ( $newshadingvalue > -10 ) and ( $newshadingvalue < 0 ) )
                        {
                          $newshadingvalue = sprintf ( "%.3f", $newshadingvalue ); # IF THE NUMBER IS COMPRISED BETWEEN -10 AND = 0 FORMAT IT SO THAT IT HAS 3 DECIMALS
                        }
                        elsif ( $newshadingvalue <= -10 )
                        {
                          $newshadingvalue = sprintf ( "%.2f", $newshadingvalue ); # IF THE NUMBER IS SMALLER THAN -10 FORMAT IT SO THAT IT HAS 2 DECIMALS
                        }
                        elsif ( ( $newshadingvalue eq "" ) or undef( $newshadingvalue ) )
                        {
                          $newshadingvalue = $el;
                        }
                        else
                        {
                          $newshadingvalue = $el;
                        }

                        if ( ( $newshadingvalue eq "1.0000" ) and ( $el ne "1.0000" ) )
                        {
                          $newshadingvalue = $el;
                        }

                        my $irrratio = sprintf ( "%.4f", $irrratio );

                        unless ( "embedded" ~~ @calcprocedures )
                        {
                          print REPORT "For $radtype radiation, obtained: zone number: $zonenum; month number $monthnum; surface number: $surfnum; hour: $hour; old shading factor: $el, new shading factor: $newshadingvalue; irradiance ratio: $irrratio\n";
                          say "Radiation type: $radtype; surface: $surfnum, month: $monthname, hour: $hour";
                          say "new shading factor: $newshadingvalue; irradiance ratio: $irrratio; old shading factor: $el\n";
                        }
                        push ( @newhourvals, $newshadingvalue );
                        push ( @newhourvals2, $irrratio );
                        push ( @were, $el );
                      }
                      $counthour++;
                    }

                    my @filledhourvals = fillhours( \@newhourvals, $monthname, \%daylighthours );

                    my @filledhourvals2 = fillhours( \@newhourvals2, $monthname, \%daylighthours );

                    #if ( ( scalar ( @filledhourvals ) > 1 ) and ( $monthname eq $monthnames[0] ) )
                    {
                      shift @monthnames;
                      my @firstarr = @filledhourvals[ 0..11 ];
                      my @secondarr = @filledhourvals[ 12..$#filledhourvals ];
                      my $joinedfirst = join ( ' ' , @firstarr );
                      my $joinedsecond = join ( ' ' , @secondarr );

                      if ( $radtype eq "diffuse" )
                      {
                        my $newline = "$joinedfirst " . "$joinedsecond" . " # diffuse - surface " . "$readsurfname $monthname\n";
                        print TEMPMOD $newline;
                      }
                      elsif ( $radtype eq "direct" )#
                      {
                        my $newline;
                        if ( "keepdirshdf" ~~ @calcprocedures )
                        {
                          my $bringline = shift( @vehicle );
                          $newline = "$bringline" . "# direct - surface " . "$readsurfname $monthname\n";
                        }
                        else
                        {
                          $newline = "$joinedfirst " . "$joinedsecond" . " # direct - surface " . "$readsurfname $monthname\n";
                        }
                        print TEMPMODDIR $newline;
                      }
                    }


                    #if ( ( scalar ( @filledhourvals2 ) > 1 ) and ( $monthname eq $monthnames2[0] ) )
                    unless ( "embedded" ~~ @calcprocedures )
                    {
                      shift @monthnames2;
                      my @firstarr2 = @filledhourvals2[ 0..11 ];
                      my @secondarr2 = @filledhourvals2[ 12..$#filledhourvals2 ];
                      my $joinedfirst2 = join ( ' ' , @firstarr2 );
                      my $joinedsecond2 = join ( ' ' , @secondarr2 );
                      if ( $radtype eq "diffuse" )
                      {
                        my $newline2 = "$joinedfirst2 " . "$joinedsecond2" . " # diffuse for surface " . "$readsurfname in $monthname\n";
                        print TEMPREPORT $newline2;
                      }
                      elsif ( $radtype eq "direct" )#
                      {
                        my $newline2 = "$joinedfirst2 " . "$joinedsecond2" . " # direct for surface " . "$readsurfname in $monthname\n";
                        print TEMPREPORTDIR $newline2;#
                      }
                    }
                  }
                }
              }
            }
            $countref++;
          }
        }
      }
      $c++;
    }
  }
}


sub getbasevectors
{ # THIS GETS THE PRE-COMPUTED EVENLY DISTRIBUTED N POINTS ON THE SURFACE OF A HEMISPHERE.
  my ( $dirvectorsnum ) = @_;
  my @basevectors;
  if ( $dirvectorsnum == 1 )
  {
    @basevectors = ( #[ 0, 0, 0 ] , # origin, base point of direction vector
        [ 0, 0, 1 ], # direction vector of high, central, vertical point
         ); # lowest vertices
  }
  if ( $dirvectorsnum == 5 )
  {
    @basevectors = ( #[ 0, 0, 0 ] , # origin, base point of direction vector
        [ 0, 0, 1 ], # direction vector of high, central, vertical point
        [ 0.8660, -0.8660, 0.5000 ] , [ 0.8660, 0.8660, 0.5000 ], [ -0.8660, 0.8660, 0.5000 ], [ -0.8660, -0.8660, 0.5000 ] ); # lowest vertices
  }
  elsif ( $dirvectorsnum == 17 )
  {
    @basevectors = ( #[ 0, 0, 0 ] , # origin, base point of direction vector
    [ 0, 0, 1 ], # direction vector of high, central, vertical point
    [ 0.1624, 0.4999, 0.8506 ], [ 0.1624, -0.4999, 0.8506 ], [ -0.2628, 0.8090, 0.5257 ], [ -0.2628, -0.8090, 0.5257 ],
    [ 0.2763, 0.8506, 0.4472 ], [ 0.2763, -0.8506, 0.4472 ], [ -0.4253, 0.3090, 0.8506 ], [ -0.4253, -0.3090, 0.8506,  ],
    [ 0.5257, 0.0, 0.8506 ], [ 0.5877, 0.8090, 0.0 ], [ 0.6881, 0.4999, 0.5257 ], [ 0.6881, -0.4999, 0.5257 ],
    [ -0.7236, 0.5257, 0.4472 ], [ -0.7236, -0.5257, 0.4472 ], [ -0.8506, 0.0, 0.5257 ], [ 0.8944, 0.0, 0.4472 ]
    ); # lowest vertices
  }
  return ( @basevectors );
}

sub createfictgeofile
{  # THIS MANAGES THE MODIFICATION OF THE FICTITIOUS GEO FILES FOR THE ZONE BY ADJUSTING THE OBSTRUCTION CONSTRUCTIONS TO FICTITIOUS EQUIVALENTS
  my ( $geofile, $geofile_f, $geofile_f5, $paths_ref, $calcprocedures_ref, $groups_ref, $conffile, $conffile_f5, $modishlock ) = @_;
  my %paths = %{ $paths_ref };
  my @calcprocedures = @{ $calcprocedures_ref };
  my @groups = @{ $groups_ref };

  open ( GEOFILE, "$geofile" ) or die;
  my @geolines = <GEOFILE>;
  close GEOFILE;

  my %obsinf;

  unless ( -e $modishloock )
  {
    open( GEOFILE_F, ">$geofile_f" ) or die;
  }

  foreach my $geoline ( @geolines )
  {
    if ( $geoline =~ /^\*obs/ )
    {
      chomp $line;
      my @elts = split ( /,|\s+/, $geoline );

      if ( $count == 0 )
      {
        $obsinf{$elts[13]}{name} = $elts[9];
        $obsinf{$elts[13]}{mlc} = $elts[10];
      }

      unless ( $elts[10] =~ /^f_/ )
      {
        $elts[10] = "f_" . $elts[10] ;
        chop $elts[10] ;
	chop $elts[10] ;
        $obsinf{$elts[13]}{newmlc} = $elts[10];
      }
      else
      {
        $obsinf{$elts[13]}{newmlc} = $elts[10];
      }

      unless ( -e $modishloock )
      {
        if ( length( $elts[13] ) == 1 )
        {
          $geoline = $elts[0] . "," . $elts[1] . "," . $elts[2] . "," . $elts[3] . "," . $elts[4] . "," .
            $elts[5] . "," . $elts[6] . "," . $elts[7] . "," . $elts[8] . "," . $elts[9] . " " .
            $elts[10] . "  " . $elts[11] . " " . $elts[12] . "   " . $elts[13] . "\n";
        }
        elsif ( length( $elts[13] ) == 2 )
        {
          $geoline = $elts[0] . "," . $elts[1] . "," . $elts[2] . "," . $elts[3] . "," . $elts[4] . "," .
            $elts[5] . "," . $elts[6] . "," . $elts[7] . "," . $elts[8] . "," . $elts[9] . " " .
            $elts[10] . "  " . $elts[11] . " " . $elts[12] . "  " . $elts[13] . "\n";chop
        }
        elsif ( length( $elts[13] ) == 3 )
        {
          $geoline = $elts[0] . "," . $elts[1] . "," . $elts[2] . "," . $elts[3] . "," . $elts[4] . "," .
            $elts[5] . "," . $elts[6] . "," . $elts[7] . "," . $elts[8] . "," . $elts[9] . " " .
            $elts[10] . "  " . $elts[11] . " " . $elts[12] . " " . $elts[13] . "\n";
        }
      }
    }
    unless ( -e $modishloock )
    {
      print GEOFILE_F $geoline;
    }
  }
  unless ( -e $modishloock )
  {
    close GEOFILE_F;
  }

  unless ( -e $modishloock )
  {
    my ( $shortgeofile_f, $shortgeofile_f1, $geofile_f1 );
    if ( ( "radical" ~~ @calcprocedures ) or ( "composite" ~~ @calcprocedures ) or ( "noreflections" ~~ @calcprocedures ) )
    {
      $geofile_f5 = $geofile_f;
      $geofile_f5 =~ s/\.geo$// ;
      $geofile_f5 = $geofile_f5 . "5.geo";

      open( GEOFILE_F, "$geofile_f" ) or die;
      my @lines_f = <GEOFILE_F>;
      close GEOFILE_F;

      open( GEOFILE_F5, ">$geofile_f5" );
      foreach my $line_f ( @lines_f )
      {
        if ( $line_f =~ /^\*obs/ )
        {
          $line =~ s/(\s+)$// ;
          my @elts = split( ",", $line_f );
          $elts[4] = 0.01;
          $elts[5] = 0.01;
          $elts[6] = 0.01;
          $line_f = "$elts[0],$elts[1],$elts[2],$elts[3],$elts[4],$elts[5],$elts[6],$elts[7],$elts[8],$elts[9],$elts[10]";
          $line_f =~ s/^,//;
          $line_f =~ s/,$//;
        }
        print GEOFILE_F5 $line_f;
      }
      close GEOFILE_F5;
      my $zonepath = $paths{zonepath};
      $shortgeofile_f5 = $geofile_f5;
      $shortgeofile_f5 =~ s/^$zonepath// ;
      $shortgeofile_f5 =~ s/^\/// ;
      $shortgeofile_f = $geofile_f;
      $shortgeofile_f =~ s/^$zonepath// ;
      $shortgeofile_f =~ s/^\/// ;

      open ( CONFFILE_F5, "$conffile_f5" ) or die;
      my @lines_old = <CONFFILE_F5>;
      close CONFFILE_F5;

      my $conffile_f5_old = $conffile_f5 . ".old";
      `mv -f $conffile_f5 $conffile_f5_old`;
      say REPORT "mv -f $conffile_f5 $conffile_f5_old";

      open( CONFFILE_F5, ">$conffile_f5" ) or die;
      foreach my $line ( @lines_old )
      {
        if ( $line =~ /^\*geo/ )
        {
          $line =~ s/$shortgeofile_f/$shortgeofile_f5/ ;
        }
        print CONFFILE_F5 $line;
      }
    }
  }

  say MONITOR "OBSINF\%obsinf : " .dump ( \%obsinf );
  $paths{obsinf} = \%obsinf;
  return( %paths )
}


sub creatematdbfiles
{ # THIS MANAGES THE CREATION OF THE TWO FICTITIOUS MATERIALS DATABASES:
  # ONE FOR THE THE "UNREFLECTIVE" MODEL AND THE OTHER FOR THE "REFLECTIVE" ONE.
  my ( $matdbfile,  $matdbfile_f1, $matdbfile_f2, $matdbfile_f6, $calcprocedures_ref, $constrdbfile_f, $obsdata_ref,
        $obscon_ref, $newobscon_ref, $exportconstrref, $conv_ref, $countm, $oldcountm, $newclass, $oldmatnums_ref, $newmatnums_ref, $oldmaterials_ref, $newmaterials_ref ) = @_;

  my @calcprocedures = @{ $calcprocedures_ref };
  my @obsdata = @{ $obsdata_ref }; # NOW UNUSED
  my @obscon = @{ $obscon_ref }; say MONITOR "\@obscon " . dump( @obscon ); #ADDED
  my @newobscon = @{ $newobscon_ref }; say MONITOR "\@newobscon " . dump( @newobscon ); #NEW BUT NOW UNUSED
  my %exportconstr = %{ $exportconstrref }; #ADDED
  my %conv = %{ $conv_ref }; say MONITOR "\%conv " . dump( \%conv ); #ADDED
  my @oldmatnums = @{ $oldmatnums_ref }; say MONITOR "\@oldmatnums IN SUB CREATEMATDBFILES" . dump( @oldmatnums ); #ADDED
  my @newmatnums = @{ $newmatnums_ref }; say MONITOR "\@newmatnums IN SUB CREATEMATDBFILES" . dump( @newmatnums ); #ADDED
  my @oldmaterials = @{ $oldmaterials_ref }; say MONITOR "\@oldmaterials IN SUB CREATEMATDBFILES" . dump( @oldmaterials ); #ADDED
  my @newmaterials = @{ $newmaterials_ref }; say MONITOR "\@newmaterials IN SUB CREATEMATDBFILES" . dump( @newmaterials ); #ADDED
  say MONITOR "NEWCLASS $newclass";

  my ( @box, %exportrefl, %obslayers, @bowl );

  open ( MATDBFILE, "$matdbfile" ) or die;
  my @matlines = <MATDBFILE>;
  close MATDBFILE;

  open( CONSTRDBFILE_F, "$constrdbfile_f" ) or die;
  my @constrlines = <CONSTRDBFILE_F>;
  close CONSTRDBFILE_F;

  #my @obs = uniq( map{ $_->[1] } @obsdata );

  my $count = 0;
  foreach my $ob ( @newobscon )
  {
    my $semaphore = "off";
    foreach my $constrline ( @constrlines )
    {
      chomp $constrline;
      my @row = split( ",", $constrline );
      if ( $row[0] eq "*item" )
      {
        if ( $row[1] eq $ob )
        {
          $semaphore = "on";
        }
      }

      if ( $row[0] eq "*end_item" )
      {
        $semaphore = "off";
        $count++;
      }

      if ( $semaphore eq "on" )
      {
        if ( $row[0] eq "*layer" )
        {
          my @els = split( "\s+|,|:", $row[3] );
          $els[0] =~ s/(\s+)// ;
          push( @{ $obslayers{$ob} }, $els[0] );
        }
      }
    }
  }

  my @obsconstr;
  foreach my $obkey ( keys %obslayers )
  {
    my @ob = @{ $obslayers{$obkey} };
    push( @obsconstr, $ob[0], $ob[-1] );
  }
  @obsconstr = uniq( @obsconstr );

  my ( @newmatlines );
  if ( scalar( @newobscon ) > 0 )
  {
    if ( $matlines[-1] =~ /\*end/ )
    {
      pop( @matlines );
    }
    elsif ( $matlines[-2] =~ /\*end/ )
    {
      pop( @matlines );
      pop( @matlines );
    }

    my $addnum0;
    if( $newclass < 10 )
    {
      $addnum0 = " " . $newclass;
    }
    else
    {
      $addnum0 = $newclass;
    }

    my $addnum;
    if( scalar( @newmaterials ) < 10 )
    {
      $addnum = " " . scalar( @newmaterials );
    }
    else
    {
      $addnum = scalar( @newmaterials );
    }

    my $addclass = "*class," . $addnum0 . "," . $addnum . ",Modish fictitious\n";
    push( @matlines, $addclass );

    my $addcomment = "Category (" . $newclass . ") holds the fictitious materials required by the Modish procedure.\n";
    push( @matlines, $addcomment );

    my $countline = 0;
    foreach my $matline( @matlines )
    {
      my $addmat;
      if ( $countline == 4 )
      {
	 my @elms = split( "\s+|#+", $matline );
	 {
	    $elms[0] = $newclass;
	    my $newline = join( " #", @elms );
	    $matline = $newline;
         }
       }
       push ( @bowl, $matline );

      my @els = split( /,/, $matline );
      if ( $matline =~ /^\*item/ )
      {
        my $countmat = 0;
        foreach my $oldmatnum ( @oldmatnums )
        {
          my $prov = $els[2];
          $prov =~ s/^\s+// ;
          if ( $oldmatnum eq "$prov" )
          {
	    say MONITOR "FOUND";
            if ( $conv{$oldmatnum}{num} < 10 )
            {
              $addmat = "  " . $conv{$oldmatnum}{num};
            }
            elsif ( ( $conv{$oldmatnum}{num} >= 10 ) and ( $conv{$oldmatnum}{num} < 100 ) )
            {
              $addmat = " " . $conv{$oldmatnum}{num};
            }
            elsif ( $conv{$oldmatnum}{num} >= 100 )
            {
              $addmat = $conv{$oldmatnum}{num};
            }
            $els[2] = $addmat;
	    $els[1] = $conv{$oldmatnum}{name};
	    my $newmatline = join( ",", @els );
            push( @newmatlines, $newmatline );
            push( @newmatlines, $matlines[$countline+1] );
	    last;
          }
          $countmat++;
        }
      }
      $countline++;
    }
  }

  my @matlines = @bowl;

  push( @newmatlines, "*end");
  push( @matlines, @newmatlines );

  my $countline = 0;
  my ( @bag, @row, @firstloop, @secondloop );
  my $semaphore = "off";

  foreach my $matline ( @matlines )
  {
    chomp $matline;
    my @row = split( ",", $matline );
    if ( $row[0] eq "*item" )
    {
      if ( $row[1] ~~ @newmaterials )
      {
        $semaphore = "on";
      }
      else
      {
        $semaphore = "off";
      }
    }

    my @e = split( ",", $matline );
    if ( ( $e[0] =~ /^\d/ ) and ( $e[-1] =~ /\D$/ ) )
    {
      if ( $semaphore eq "off" )
      {
        if ( not( "diluted" ~~ @calcprocedures ) )
        {
          if ( ( not( "radical" ~~ @calcprocedures ) ) or ( not( "noreflections" ~~ @calcprocedures ) ) )
          {
            my $lin = "$e[0],$e[1],$e[2],$e[3],$e[4],0.999,0.999,$e[7],$e[8],$e[9]";
            push( @firstloop, $lin );
          }
          my $linn = "$e[0],$e[1],$e[2],$e[3],$e[4],0.999,0.999,$e[7],$e[8],$e[9]";
          push( @secondloop, $linn );
        }
        elsif ( "diluted" ~~ @calcprocedures )
        {
          if ( ( not( "radical" ~~ @calcprocedures ) ) or ( not( "noreflections" ~~ @calcprocedures ) ) )
          {
            my $lin = "$e[0],$e[1],$e[2],$e[3],$e[4],$e[5],$e[6],$e[7],$e[8],$e[9]";
            push( @firstloop, $lin );
          }
          my $linn = "$e[0],$e[1],$e[2],$e[3],$e[4],$e[5],$e[6],$e[7],$e[8],$e[9]";
          push( @secondloop, $linn );
        }
      }
      elsif ( $semaphore eq "on" )
      {
        $exportrefl{ $row[1] }{ absout } =  $e[5];
        $exportrefl{ $row[1] }{ absin } = $e[6];
        if ( not( "diluted" ~~ @calcprocedures ) )
        {
          if ( ( not( "radical" ~~ @calcprocedures ) ) or ( not( "noreflections" ~~ @calcprocedures ) ) )
          {
            my $lin = "$e[0],$e[1],$e[2],$e[3],$e[4],0.999,0.999,$e[7],$e[8],$e[9]";
            push( @firstloop, $lin );
          }
          my $linn = "$e[0],$e[1],$e[2],$e[3],$e[4],$e[5],$e[6],$e[7],$e[8],$e[9]";
          push( @secondloop, $linn );
        }
        elsif ( "diluted" ~~ @calcprocedures )
        {
          if ( ( not( "radical" ~~ @calcprocedures ) ) or ( not( "noreflections" ~~ @calcprocedures ) ) )
          {
            my $lin = "$e[0],$e[1],$e[2],$e[3],$e[4],0.999,0.999,$e[7],$e[8],$e[9]";
            push( @firstloop, $lin );
          }
          my $linn = "$e[0],$e[1],$e[2],$e[3],$e[4],$e[5],$e[6],$e[7],$e[8],$e[9]";
          push( @secondloop, $linn );
        }
      }
    }
    else
    {
      push( @firstloop, $matline );
      push( @secondloop, $matline );
    }
  }

  #if ( ( not ( "radical" ~~ @calcprocedures ) ) and  ( not ( -e $matdbfile_f1 ) ) )
  if ( not ( "radical" ~~ @calcprocedures ) )
  {
    open( my $MATDBFILE_F1, ">$matdbfile_f1" ) or die;
    foreach my $line ( @firstloop )
    {
      say $MATDBFILE_F1 $line ;
    }
    close $MATDBFILE_F1;
  }

  #if ( not ( -e $matdbfile_f2 ) )
  {
    open( my $MATDBFILE_F2, ">$matdbfile_f2" ) or die;
    foreach my $line ( @secondloop )
    {
      say $MATDBFILE_F2 $line ;
    }
    close $MATDBFILE_F2;
  }


  #if ( not ( -e $matdbfile_f6 ) )
  {
    my ( @bag, @row, @firstloop, @secondloop );
    my $semaphore = "off";
    foreach my $matline ( @matlines )
    {
      chomp $matline;
      $matline =~ s/\s+// ;
      my @row = split( ",", $matline );
      if ( $row[0] eq "*item" )
      {
        if ( $row[1] ~~ @obsconstr )
        {
          $semaphore = "on";
        }
        else
        {
          $semaphore = "off";
        }
      }

      my @e = split( ",", $matline );
      if ( ( $e[0] =~ /^\d/ ) and ( $e[-1] =~ /\D$/ ) )
      {
        if ( $semaphore eq "off" )
        {
          if ( not( "diluted" ~~ @calcprocedures ) )
          {
            my $lin = "$e[0],$e[1],$e[2],$e[3],$e[4],0.999,0.999,$e[7],$e[8],$e[9]";
            push( @firstloop, $lin );
            my $linn = "$e[0],$e[1],$e[2],$e[3],$e[4],0.999,0.999,$e[7],$e[8],$e[9]";
            push( @secondloop, $linn );
          }
          elsif ( "diluted" ~~ @calcprocedures )
          {
            my $lin = "$e[0],$e[1],$e[2],$e[3],$e[4],0.999,0.999,$e[7],$e[8],$e[9]";
            push( @firstloop, $lin );
            my $linn = "$e[0],$e[1],$e[2],$e[3],$e[4],0.999,0.999,$e[7],$e[8],$e[9]";
            push( @secondloop, $linn );
          }
        }
        elsif ( $semaphore eq "on" )
        {
          $exportrefl{ $row[1] }{ absout } =  $e[5];
          $exportrefl{ $row[1] }{ absin } = $e[6];
          if ( not( "diluted" ~~ @calcprocedures ) )
          {
            my $lin = "$e[0],$e[1],$e[2],$e[3],$e[4],0.999,0.999,$e[7],$e[8],$e[9]";
            push( @firstloop, $lin );
            my $linn = "$e[0],$e[1],$e[2],$e[3],$e[4],0.999,0.999,$e[7],$e[8],$e[9]";
            push( @secondloop, $linn );
          }
          elsif ( "diluted" ~~ @calcprocedures )
          {
            my $lin = "$e[0],$e[1],$e[2],$e[3],$e[4],0.999,0.999,$e[7],$e[8],$e[9]";
            push( @firstloop, $lin );
            my $linn = "$e[0],$e[1],$e[2],$e[3],$e[4],0.999,0.999,$e[7],$e[8],$e[9]";
            push( @secondloop, $linn );
          }
        }
      }
      else
      {
        push( @firstloop, $matline );
        push( @secondloop, $matline );
      }
    }

    open( my $MATDBFILE_F6, ">$matdbfile_f6" ) or die;
    foreach my $line ( @firstloop )
    {
      say $MATDBFILE_F6 $line ;
    }
    close $MATDBFILE_F6;
  }

  return ( \%exportrefl, \%obslayers );
}


sub adjust_radmatfile
{ # THIS CHECKS IF THE RADIANCE MATERIALS FILE HAS BEEN PROPERLY MODIFIED.
# IF NOT, THIS DOES THE MODIFICATION. THIS IS USED WHEN THE SPECULAR FRACTIONS
# AND ROUGHNESSES IN THE MATERIALS DATABASE ARE ALL SET TO 0.
  my ( $exportconstrref, $exportreflref, $conffile, $path, $specularratios_ref,
  $obslayers_ref, $selectives_ref, $paths_ref, $calcprocedures_ref, $count, $groundrefl, $action, $speedy ) = @_;

  my %exportconstr = %$exportconstrref;
  my %exportrefl = %$exportreflref; # UNUSED
  my %paths = %{ $paths_ref };
  my @calcprocedures = @{ $calcprocedures_ref };

  my @specularratios = @$specularratios_ref;
  my %obslayers = %{ $obslayers_ref };
  my @selectives = @{ $selectives_ref };

  my $specroughn;
  if ( "dirdiff" ~~ @calcprocedures )
  {
    foreach my $proc ( @calcprocedures )
    {
      if ( $proc =~ /^specroughness/ )
      {
        my @temps = split( ":", $proc );
      }
      $allspec = $temps[1];
      $allroughn = $temps[2];
    }
  }

  my %hs;
  foreach $el ( @specularratios )
  {
    my @row = split( ":", $el );
    {
      $row[0] = "_" . $row[0] ;
      $hs{$row[0]}{spec} = $row[1];
      $hs{$row[0]}{roughn} = $row[2];
    }
  }

  my $conffileor = $conffile;

  my $action ;
  if ( $conffile =~ /_f1\./ )
  {
    $action = 1;
  }
  elsif ( $conffile =~ /_f2/ )
  {
    $action = 2;
  }
  elsif ( $conffile =~ /_f3/ )
  {
    $action = 3;
  }
  elsif ( $conffile =~ /_f4/ )
  {
    $action = 4;
  }
  elsif ( $conffile =~ /_f5/ )
  {
    $action = 5;
  }
  elsif ( $conffile =~ /_f6/ )
  {
    $action = 6;
  }
  elsif ( $conffile =~ /_f7/ )
  {
    $action = 7;
  }
  elsif ( $conffile =~ /_f8/ )
  {
    $action = 8;
  }
  elsif ( $conffile =~ /_f9/ )
  {
    $action = 9;
  }
  elsif ( $conffile =~ /_f10/ )
  {
    $action = 10;
  }
  elsif ( $conffile =~ /_f11/ )
  {
    $action = 11;
  }


  if ( ( $action == 6 ) or ( $action == 7 )
    or ( $action == 9 ) or ( $action == 10 ) ) # TAKE CARE!
  {
    $groundrefl = 0;
  }

  $conffile =~ s/_f1// ;
  $conffile =~ s/_f2// ;
  $conffile =~ s/_f3// ;
  $conffile =~ s/_f4// ;
  $conffile =~ s/_f5// ;
  $conffile =~ s/_f6// ;
  $conffile =~ s/_f7// ;
  $conffile =~ s/_f8// ;
  $conffile =~ s/_f9// ;
  $conffile =~ s/_f10// ;
  $conffile =~ s/_f11// ;

  my $radpath = $paths{radpath};

  my $radmat_f1 = $conffile;
  if ( $radmat_f1 =~ /$path\/cfg\// )
  {
    $radmat_f1 =~ s/$path\/cfg\///;
  }
  else
  {
    $radmat_f1 =~ s/$path\///;
  }

  $radmat_f1 =~ s/\.cfg//;
  $radmat_f1 = $radmat_f1 . "_f1_Extern.mat";
  $radmat_f1 = "$radpath/$radmat_f1";

  my $radmat_f2 = $conffile;
  if ( $radmat_f2 =~ /$path\/cfg\// )
  {
    $radmat_f2 =~ s/$path\/cfg\///;
  }
  else
  {
    $radmat_f2 =~ s/$path\///;
  }
  $radmat_f2 =~ s/\.cfg//;
  $radmat_f2 = $radmat_f2 . "_f2_Extern.mat";
  $radmat_f2 = "$radpath/$radmat_f2";


  my $radmat_f6 = $conffile;
  if ( $radmat_f6 =~ /$path\/cfg\// )
  {
    $radmat_f6 =~ s/$path\/cfg\///;
  }
  else
  {
    $radmat_f6 =~ s/$path\///;
  }
  $radmat_f6 =~ s/\.cfg//;
  $radmat_f6 = $radmat_f6 . "_f6_Extern.mat";
  $radmat_f6 = "$radpath/$radmat_f6";

  my $extrad = $conffileor;
  if ( $extrad =~ /$path\/cfg\// )
  {
    $extrad =~ s/$path\/cfg\///;
  }
  else
  {
    $extrad =~ s/$path\///;
  }

  $extrad =~ s/\.cfg//;
  $extrad = $extrad . "_Extern-out.rad";
  $extrad = "$radpath/$extrad";


  if ( ( $action == 2 ) and not( $speedy eq "yes" ) )
  {
    my $radmattemp = $radmat_f2 . ".temp";
    `cp -f $radmat_f2 $radmattemp`;
    say REPORT "cp -f $radmat_f2 $radmattemp";

    open( RADMATTEMP, "$radmattemp" ) or die;
    my @lines = <RADMATTEMP>;
    close RADMATTEMP;

    open( RADMAT_F2, ">$radmat_f2" ) or die;
    my $count = 0;
    my @constrs = keys %exportconstr;
    foreach ( @lines )
    {
      my ( $spec, $roughn );
      my $lin = $lines[ $count + 4 ];
      my @arr = split( /\s+/, $lin );
      if ( ( $_ =~ /^#/ ) and ( $_ =~ /ternal MLC Colours.../ ) )
      {
        my $description = $lines[ $count + 1 ] ;

        foreach my $const ( keys %hs )
        {
          if ( $description =~ /$const/ )
          {
            $spec = $hs{$const}{spec};
            $roughn = $hs{$const}{roughn};
            $lines[ $count + 4 ] = "5  $arr[1] $arr[2] $arr[3] $spec $roughn \n";
            last;
          }
          else
          {
            $lines[ $count + 4 ] = "5  $arr[1] $arr[2] $arr[3] $allspec $allroughn \n";
            last;
          }
        }
      }
      print RADMAT_F2 $lines[ $count ];
      $count++;
    }
    close RADMAT_F2;
  }

  if ( ( scalar( @selectives ) > 0 ) and ( $action == 3 ) and not( $speedy eq "yes" ) )
  {
    my $radmat_f3 = $conffile;
    if (  $radmat_f3 =~ /$path\/cfg\// )
    {
      $radmat_f3 =~ s/$path\/cfg\///;
    }
    else
    {
      $radmat_f3 =~ s/$path\///;
    }

    $radmat_f3 =~ s/.cfg//;
    $radmat_f3 = $radmat_f3 . "_Extern.mat";
    $radmat_f3 = "$path/rad/$radmat_f3";
    my $radmattemp3 = $radmat_f3 . ".temp";
    `mv -f $radmat_f3 $radmattemp3`;
    say REPORT "mv -f $radmat_f3 $radmattemp3";

    open( RADMATTEMP3, "$radmattemp3" ) or die;
    my @lines = <RADMATTEMP3>;
    close RADMATTEMP3;
    open( RADMAT_F3, ">$radmat_f3" ) or die;
    my $count = 0;
    my @constrs = keys %exportconstr;
    foreach ( @lines )
    {
      my ( $spec, $roughn );
      my $lin = $lines[ $count + 4 ];
      my @arr = split( /\s+/, $lin );
      if ( ( $_ =~ /^#/ ) and ( $_ =~ /ternal MLC Colours.../ ) )
      {
        my $description = $lines[ $count + 1 ] ;

        foreach my $const ( keys %hs )
        {
          if ( $description =~ /$const/ )
          {
            $spec = $hs{$const}{spec};
            $roughn = $hs{$const}{roughn};
            $lines[ $count + 4 ] = "5  $arr[1] $arr[2] $arr[3] $spec $roughn \n";
            last;
          }
          else
          {
            $lines[ $count + 4 ] = "5  $arr[1] $arr[2] $arr[3] $allspec $allroughn \n";
            last;
          }
        }
      }
      print RADMAT_F3 $lines[ $count ];
      $count++;
    }
    close RADMAT_F3;
  }

  if ( $action == 6 )
  {
    my @lines;
    if ( ( "composite" ~~ @calcprocedures ) or( "radical" ~~ @calcprocedures ) )
    {
      open( RADMATTEMP, "$radmat_f2" ) or die;
      @lines = <RADMATTEMP>;
      close RADMATTEMP;
    }
    elsif ( "noreflections" ~~ @calcprocedures )
    {
      open( RADMATTEMP, "$radmat_f6" ) or die;
      @lines = <RADMATTEMP>;
      close RADMATTEMP;
    }

    open( RADMAT_F6, ">$radmat_f6" ) or die;
    my $count = 0;
    foreach ( @lines )
    {
      my $lin = $lines[ $count + 4 ];
      if ( ( $_ =~ /#/ ) and ( $_ =~ /ternal MLC Colours.../ ) )
      {
        $lines[ $count + 4 ] = "5  0 0 0 0 0 \n";
      }
      print RADMAT_F6 $lines[ $count ];
      $count++;
    }
    close RADMAT_F6;
  }

  if ( $action == 6 )
  {

    my $oldextrad = $extrad . ".old";
    `mv -f $extrad $oldextrad`;
    say REPORT "mv -f $extrad $oldextrad";


    open ( OLDEXTRAD, "$oldextrad" ) or die;
    my @extlines = <OLDEXTRAD>;
    close OLDEXTRAD;

    open( EXTRAD, ">$extrad" ) or die;
    my $count = 0;
    foreach my $extline ( @extlines )
    {
      if ( $extline =~ /mud plastic ground_mat/ )
      {
        my $ext3 = $extlines[$count+3] ;
        my @els3 = split( " +", $ext3 );
        $extlines[$count+3] = "$els3[0]  $groundrefl  $groundrefl $groundrefl $els3[4] $els3[5]\n";
      }

      if ( $extline =~ /ground_mat ring groundplane/ )
      {
        my $ext4 = $extlines[$count+3] ;
        my @els4 = split( " +", $ext4 );
        $extlines[$count+3] = "$els4[0]  $els4[1] $els4[2] $els4[3] $els4[4] $els4[5] $els4[6] $els4[7] 60.0 \n";
      }

      if ( $extline =~ /skyfunc glow ground_glow/ )
      {
        my $ext5 = $extlines[$count+3];
        my @els5 = split( " +", $ext5 );
        $extlines[$count+3] = "$els5[0] $groundrefl $groundrefl $groundrefl $els5[4]\n";
      }
      print EXTRAD $extline;
      $count++;
    }
    close EXTRAD;
  }
}


sub calcdirvectors
{ # THIS CALCULATES THE NEEDED DIRECTION VECTORS AT EACH GRID POINT.
  my @winscoords = @_;
  my ( @groupbag );
  foreach my $surf ( @winscoords )
  {
    my ( @surfbag );
    foreach my $v ( @$surf )
    {
      my @fields = @$v;
      my $coordsref = $fields[0];
      my @coords = @$coordsref;
      my $vertex = Vector::Object3D::Point->new( x => $coords[0], y => $coords[1], z => $coords[2], );
      push ( @surfbag, $vertex);
    }

    my $polygon = Vector::Object3D::Polygon->new(vertices => [ @surfbag ]);
    my $normal_vector = $polygon->get_normal_vector;
    my ($x_, $y_, $z_) = $normal_vector->array;
    my ( $x, $y, $z ) = ( -$x_, -$y_, -$z_ );
    my $max = max( abs($x), abs($y), abs($z) );
    my @dirvector;
    unless ( $max == 0 )
    {
      $x = ( $x / $max );
      $y = ( $y / $max );
      $z = ( $z / $max );
      @dirvector =  ( $x, $y, $z );
    }
    else
    {
      @dirvector =  ( $x, $y, $z );
    }
    push ( @groupbag, [ @dirvector ] )
  }
  return ( @groupbag);
}

sub prunepoints
{ # IT PRUNES AWAY THE GRID POINTS FALLING OUTSIDE THE SURFACE.
  my ( $gridpoints_transitionalref, $xyzcoordsref ) = @_;
  my @gridpoints = @$gridpoints_transitionalref;
  my @vertstaken = @$xyzcoordsref;

  my @verts;
  foreach ( @vertstaken )
  {
    my @fields = @$_;
    my @xs = @{ $fields[0] };
    my @ys = @{ $fields[1] };
    my @zs = @{ $fields[2] };
    my $i = 0;
    my @bag;
    foreach ( @xs )
    {
      push ( @bag, [ $xs[ $i ], $ys[ $i ], $zs[ $i ] ] );
      $i++;
    }
    push ( @verts, [ @bag ] );
  }

  my ( @coords, @prunegridpoints );

  foreach my $v ( @gridpoints )
  {
    my @fields = @$v;
    my $coordsref = $fields[0];
    push( @coords, [ @$coordsref ] );
  }

  my ( @boxpointxy, @boxpointxz, @boxpointyz );
  foreach my $gridpoint ( @coords )
  {
    my ( @point_xys, @point_xzs, @point_yzs );
    foreach ( @$gridpoint )
    {
      push ( @point_xys, [ $_->[0], $_->[1] ] );
      push ( @point_xzs, [ $_->[0], $_->[2] ] );
      push ( @point_yzs, [ $_->[1], $_->[2] ] );
    }
    push ( @boxpointxy, [ @point_xys ] );
    push ( @boxpointxz, [ @point_xzs ] );
    push ( @boxpointyz, [ @point_yzs ] );
  }

  my ( @boxvertxy, @boxvertxz, @boxvertyz );
  foreach my $surf ( @verts )
  {
    my ( @vert_xys, @vert_xzs, @vert_yzs );
    foreach my $vert ( @$surf )
    {
      push ( @vert_xys, [ $vert->[0], $vert->[1] ] );
      push ( @vert_xzs, [ $vert->[0], $vert->[2] ] );
      push ( @vert_yzs, [ $vert->[1], $vert->[2] ] );
    }
    push ( @boxvertxy, [ @vert_xys ] );
    push ( @boxvertxz, [ @vert_xzs ] );
    push ( @boxvertyz, [ @vert_yzs ] );
  }

  my $count = 0;
  my ( $vert_xys, $vert_xzs, $vert_yzs, $polyxy, $polyxz, $polyyz );
  my ( @verts_xys, @verts_xzs, @verts_yzs);
  foreach my $case ( @boxvertxy )
  {
    $vert_xys = $boxvertxy[$count];
    $vert_xzs = $boxvertxz[$count];
    $vert_yzs = $boxvertyz[$count];
    $polyxy = Math::Polygon::Tree->new( $vert_xys );
    $polyxz = Math::Polygon::Tree->new( $vert_xzs );
    $polyyz = Math::Polygon::Tree->new( $vert_yzs );
    $count++;
  }

  my $count = 0;
  my @newbox;
  foreach my $caseref ( @gridpoints )
  {
    my @case = @$caseref;
    my $surfnum = $case[ 1 ];
    my $dirvector = $case[ 2 ];
    my @bag;
    foreach my $vert ( @{ $case[ 0 ] } )

    {
      my $xyref = $boxpointxy[ $count ][ 0 ];
      my $xzref = $boxpointxz[ $count ][ 0 ];
      my $yzref = $boxpointyz[ $count ][ 0 ];
      unless ( ( ( $polyxy->contains( $xyref ) ) == 0 ) and ( ( $polyxz->contains( $xzref ) ) == 0 ) and ( ( $polyyz->contains( $yzref ) ) == 0 ) )
      {
        push( @bag, $vert );
      }
    }
    push ( @newbox, [ [ @bag ], $surfnum, $dirvector ] );
    $count++;
  }
  return ( @newbox );
}


sub solveselective
{
  my ( $matdbfile_f2, $selectives_ref, $conffile, $conffile_f2, $path ) = @_;
  my @selectives = @{ $selectives_ref };

  my $matdbfile_f3 = $matdbfile_f2;
  $matdbfile_f3 =~ s/_f2/_f3/ ;
  my $matdbfile_f4 = $matdbfile_f2;
  $matdbfile_f4 =~ s/_f2/_f4/ ;

  my $shortmatdbfile_f2 = $matdbfile_f2;
  my $shortmatdbfile_f3 = $matdbfile_f3;
  my $shortmatdbfile_f4 = $matdbfile_f4;
  $shortmatdbfile_f2 =~ s/$path\/dbs\/// ;
  $shortmatdbfile_f3 =~ s/$path\/dbs\/// ;
  $shortmatdbfile_f4 =~ s/$path\/dbs\/// ;

  open( my $MATDBFILE_F2, "$matdbfile_f2" ) or die;
  my @matlines = <$MATDBFILE_F2>;
  close $MATDBFILES_F2;

  my @mats = map{ $_->[0] } @selectives;

  my ( @row, @thirdloop, @fourthloop );
  my $semaphore = "off";
  foreach my $matline ( @matlines )
  {
    chomp $matline;
    $matline =~ s/\s+// ;
    my @e = split( ",", $matline );
    if ( $e[0] eq "*item" )
    {
      if ( $e[1] ~~ @mats )
      {
        $semaphore = "on";
      }
      else
      {
        $semaphore = "off";
      }
    }

    if ( ( $e[0] =~ /^\d/ ) and ( $e[-1] =~ /\D$/ ) )
    {
      if ( $semaphore eq "on" )
      {
        my $ratio = $selective->[1];
        my $changeratio3 = ( 1 - ( ( $ratio - 1 ) / 2 ) );
        my $changeratio4 = ( 1 + ( ( $ratio - 1 ) / 2 ) );
        my $e5_3 = $e[5] * $changeratio3;
        my $e5_4 = $e[5] * $changeratio4;
        my $e6_3 = $e[6]  * $changeratio3;
        my $e6_4 = $e[6] * $changeratio4;

        my $lin = "$e[0],$e[1],$e[2],$e[3],$e[4],$e5_3,$e6_3,$e[7],$e[8],$e[9]";
        push( @thirdloop, $lin );
        my $linn = "$e[0],$e[1],$e[2],$e[3],$e[4],$e5_4,$e6_4,$e[7],$e[8],$e[9]";
        push( @fourthloop, $linn );
      }
      elsif ( $semaphore eq "off" )
      {
        push( @thirdloop, $matline );
        push( @fourthloop, $matline );
      }
    }
    else
    {
      push( @thirdloop, $matline );
      push( @fourthloop, $matline );
    }

    open( my $MATDBFILE_F3, ">$matdbfile_f3" ) or die;
    foreach my $line ( @thirdloop )
    {
      say $MATDBFILE_F3 $line ;
    }
    close $MATDBFILE_F3;

    open( my $MATDBFILE_F4, ">$matdbfile_f4" ) or die;
    foreach my $line ( @fourthloop )
    {
      say $MATDBFILE_F4 $line ;
    }
    close $MATDBFILE_F4;
  }

  my $conffile_f3 = $conffile;
  $conffile_f3 =~ s/\.cfg/\_f3\.cfg/;
  print REPORT "cp -R -f $conffile_f2 $conffile_f3\n";
  `cp -R -f $conffile_f2 $conffile_f3\n`;
  say REPORT "cp -R -f $conffile_f2 $conffile_f3\n";

  open( my $CONFFILE_F2, "$conffile_f2" ) or die;
  my @lines2 =<$CONFFILE_F2>;
  close $CONFFILE_F2;

  open( my $CONFFILE_F2, ">$conffile_f2" ) or die;
  foreach my $line2 ( @lines2 )
  {
    $line2 =~ s/$shortmatdbfile_f2/$shortmatdbfile_f3/ ;
    print $CONFFILE_F2 $line2;
  }
  close $CONFFILE_F2;

  open( my $CONFFILE_F3, "$conffile_f3" ) or die;
  my @lines3 =<$CONFFILE_F3>;
  close $CONFFILE_F3;

  open( my $CONFFILE_F3, ">$conffile_f3" ) or die;
  foreach my $line3 ( @lines3 )
  {
    $line3 =~ s/$shortmatdbfile_f2/$shortmatdbfile_f4/ ;
    print $CONFFILE_F3 $line4;
  }
  close $CONFFILE_F3;

  return( $conffile_f3 );
}


sub getsolar
{
  my ( $paths_ref ) = @_;
  my %paths = %{ $paths_ref };
  my $clma = $paths{clmfilea};
  my $lat = $paths{lat};
  my $longdiff = $paths{longdiff};
  my $clmavgs = $paths{clmavgs};
  my $lstm = $paths{standardmeridian};
  my $long = $lstm + $longdiff;

  my %daymonths = ( 1 => 16, 2 => 15, 3 => 16, 4 => 16, 5 => 16, 6 => 16, 7 => 16, 8 => 16, 9 => 16, 10 => 16, 11 => 16, 12 => 16 );
  my $daynumber;

  say MONITOR "CLMA; $clma";
  open ( WFILE, "$clma" ) or die; #open ( WFILE, "$clma" ) or die;
  my @wlines = <WFILE>;
  close WFILE;

  my $countline = 0;
  my $count = -1;
  my $c = -1;
  my ( %ts, $month, $day, $hour, $dir, $diff );
  my $sem = 1;
  foreach my $line ( @wlines )
  {
    chomp $line;
    if ( $countline >= 12 )
    {
      my @elts;
      my $sem = 0;
      $line =~ s/  / / ;
      $line =~ s/  / / ;
      $line =~ s/ /,/ ;
      $line =~ s/^\,// ;
      my @elts = split( "  | |,", $line );

      if ( $elts[0] eq "*" )
      {
        $sem = 0;
        $month = $elts[4];
        $day = $elts[2];
        $count = 0;
        if ( $countline == 12 )
        {
          $c = 0;
        }
      }
      else
      {
        $sem = 1;
      }

      if ( ( $sem == 1 ) and ( $month ne "" ) )
      {
        $diff = $elts[0];
        $dir = $elts[2];
        $hour = $count;
        $ts{or}{dir}{$month}{$day}{$hour} = $dir;
        $ts{or}{diff}{$month}{$day}{$hour} = $diff;
        $count++;
      }
      $c++;
    }
    $countline++;
  } #say "TS: " . dump ( \%ts );

  if ( $c <= 8776 )
  {
    %daynums = ( 1 => 16, 2 => 46, 3 => 75, 4 => 106, 5 => 136, 6 => 167, 7 => 197, 8 => 228, 9 => 259, 10 => 289, 11 => 320, 12 => 351 );
  }
  else
  {
    %daynums = ( 1 => 16, 2 => 46, 3 => 76, 4 => 107, 5 => 137, 6 => 168, 7 => 198, 8 => 229, 9 => 260, 10 => 290, 11 => 321, 12 => 352 );
  }

  my @dds = ( "dir", "diff" );
  my ( %t, %ti );
  foreach my $dd ( @dds )
  {
    foreach my $m ( sort { $a <=> $b } ( keys %{ $ts{or}{$dd} } ) )
    {
      my @bag;
      foreach my $d ( sort { $a <=> $b } ( keys %{ $ts{or}{$dd}{$m} } ) )
      {
        foreach my $h ( sort { $a <=> $b } ( keys %{ $ts{or}{$dd}{$m}{$d} } ) )
        {
          push ( @{ $t{vals}{$dd}{$m}{$h} }, $ts{or}{$dd}{$m}{$d}{$h} );
        }
      }
    }
  }


  foreach my $dd ( @dds )
  {
    foreach my $m ( sort { $a <=> $b} ( keys %{ $t{vals}{$dd} } ) )
    {
      foreach my $h ( sort { $a <=> $b} ( keys %{ $t{vals}{$dd}{$m} } ) )
      {
        #say "h: " . dump ( $h );
        my @ddvals = @{ $t{vals}{$dd}{$m}{$h} };
        my $ddval = mean( @ddvals );
        $t{avg}{$dd}{$m}{$h} = $ddval;
      }
    }
  }



  open( NEWCLM, ">$clmavgs" ) or die;
  foreach my $m ( sort { $a <=> $b} ( keys %{ $t{avg}{dir} } ) )
  {
    foreach my $h ( sort { $a <=> $b} ( keys %{ $t{avg}{dir}{$m} } ) )
    {

      my $decl = 23.45 * sin( deg2rad( 280.1 + 0.9863 * $daynums{$m} ) );
      my $declrad = deg2rad($decl);
                                                       #A                                                                          #B
      #my $timeq = ( 0.1645 *          sin( deg2rad( ( 1.978 * $daynums{$m}  )- 160.22 ) )        ) - ( 0.1255 *            cos( #deg2rad( ( 0.989 * $daynums{$m} )- 80.11 ) )          )               #B
      #  - ( 0.025 *                       sin( deg2rad( ( 0.989 * $daynums{$m}  ) - 80.11 ) ) );  #say "\$timeq: $timeq";

      my $timeq = ( 9.87 * sin( deg2rad( ( 1.978 * $daynums{$m}  )- 160.22 ) ) ) - ( 7.53 * cos( deg2rad( ( 0.989 * $daynums{$m} )- 80.11 ) ) )
        - ( 1.5 * sin( deg2rad( ( 0.989 * $daynums{$m}  ) - 80.11 ) ) );  #say "\$timeq: $timeq";

      my $tcf = ( 4 * ( $lstm - $long ) ) + $timeq; #say "\$tcf: $tcf";

      #my $solartime = ( $h + ( $longdiff / 15 ) + $timeq ); #say "\$solartime1: $solartime";

      my $solartime = ( $h + ( $tcf / 60 ) ); #say "\$solartime2: $solartime";

      my $hourangle = ( 15 * ( 12 - $solartime ) );
      my $houranglerad = deg2rad($hourangle);

      my $latrad = deg2rad($lat);

      my $altrad = asin( ( cos( $latrad ) * cos( $declrad ) * cos( $houranglerad ) ) + ( sin( $latrad ) * sin( $declrad ) ) );
      my $alt = rad2deg($altrad);
      $alt = sprintf ( "%.3f", $alt );
      $t{avg}{alt}{$m}{$h} = $alt;

      my $azirad = ( asin( cos( $declrad ) * ( sin( $houranglerad ) / cos( $altrad ) ) ) );
      my $azi = rad2deg($azirad);
      $azi = sprintf ( "%.3f", $azi );
      $t{avg}{azi}{$m}{$h} = $azi;

      say NEWCLM "$m,$daymonths{$m},$h,$t{avg}{dir}{$m}{$h},$t{avg}{diff}{$m}{$h},$alt,$azi";
    }
  }
  close NEWCLM;
  return( \%t );
}


sub getaltaz
{
  ( $day, $month, $paths_ref, $hour ) = @_;
  my %paths = %{ $paths_ref };
  my $lat = $paths{lat};
  my $longdiff = $paths{longdiff};
  my $lstm = $paths{standardmeridian};
  my $long = $lstm + $longdiff;
  my $yeartype = $paths{yeartype};
  #say MONITOR "IN GETALTA YEARTYPE: $yeartype \$day $day \$month $month \$hour $hour";

  if ( $yeartype eq "normalyear" )
  {
    %year = ( 1 => {1=>1, 2=>2, 3=>3, 4 =>4, 5 =>5, 6 =>6, 7 =>7, 8 =>8, 9 =>9, 10 =>10, 11 =>11, 12 =>12, 13 =>13, 14 =>14, 15 =>15,
                16=>16, 17=>17, 18 =>18, 19 =>19, 20 =>20, 21 =>21, 22 =>22, 23 =>23, 24 =>24, 25 =>25, 26 =>26, 27 =>27, 28 =>28, 29 =>29,
                30 =>30, 31 =>31},
                2 => {1=>32, 2=>33, 3=>34, 4 =>35, 5 =>36, 6 =>37, 7 =>38, 8 =>39, 9 =>40, 10 =>41, 11 =>42, 12 =>43, 13 =>44, 14 =>45, 15 =>46,
                 16=>47, 17=>48, 18 =>49, 19 =>50, 20 =>51, 21 =>52, 22 =>53, 23 =>54, 24 =>55, 25 =>56, 26 =>57, 27 =>58, 28 =>59},
                 3 => {1=>60, 2=>61, 3=>62, 4 =>63, 5 =>64, 6 =>65, 7 =>66, 8 =>67, 9 =>68, 10 =>69, 11 =>70, 12 =>71, 13 =>72, 14 =>73, 15 =>74,
                  16=>75, 17=>76, 18 =>77, 19 =>78, 20 =>79, 21 =>80, 22 =>81, 23 =>82, 24 =>83, 25 =>84, 26 =>85, 27 =>86, 28 =>87, 29 =>88,
                  30 =>89, 31 =>90},
                  4 => {1=>91, 2=>92, 3=>93, 4 =>94, 5 =>95, 6 =>96, 7 =>97, 8 =>98, 9 =>99, 10 =>100, 11 =>101, 12 =>102, 13 =>103, 14 =>104, 15 =>105,
                  16=>106, 17=>107, 18 =>108, 19 =>109, 20 =>110, 21 =>111, 22 =>112, 23 =>113, 24 =>114, 25 =>115, 26 =>116, 27 =>117, 28 =>118, 29 =>119,
                  30 =>120},
                  5 => {1=>121, 2=>122, 3=>123, 4 =>124, 5 =>125, 6 =>126, 7 =>127, 8 =>128, 9 =>129, 10 =>130, 11 =>131, 12 =>132,
                   13 =>133, 14 =>134, 15 =>135, 16=>136, 17=>137, 18 =>138, 19 =>139, 20 =>140, 21 =>141, 22 =>142, 23 =>143, 24 =>144,
                   25 =>145, 26 =>146, 27 =>147, 28 =>148, 29 =>149, 30 =>150, 31 =>151},
                  6 => {1=>152, 2=>153, 3=>154, 4 =>155, 5 =>156, 6 =>157, 7 =>158, 8 =>159, 9 =>160, 10 =>161, 11 =>162, 12 =>163, 13 =>164,
                   14 =>165, 15 =>166, 16=>167, 17=>168, 18 =>169, 19 =>170, 20 =>171, 21 =>172, 22 =>173, 23 =>174, 24 =>175, 25 =>176,
                   26 =>177, 27 =>178, 28 =>179, 29 =>180, 30 =>181},
                   7 => {1=>182, 2=>183, 3=>184, 4 =>185, 5 =>186, 6 =>187, 7 =>188, 8 =>189, 9 =>190, 10 =>191, 11 =>192, 12 =>193,
                    13 =>194, 14 =>195, 15 =>196, 16=>197, 17=>198, 18 =>199, 19 =>200, 20 =>201, 21 =>202, 22 =>203, 23 =>204, 24 =>205,
                    25 =>206, 26 =>207, 27 =>208, 28 =>209, 29 =>210, 30 =>211, 31 =>212},
                    8 => {1=>213, 2=>214, 3=>215, 4 =>216, 5 =>217, 6 =>218, 7 =>219, 8 =>220, 9 =>221, 10 =>222, 11 =>223, 12 =>224,
                     13 =>225, 14 =>226, 15 =>227, 16=>228, 17=>229, 18 =>230, 19 =>231, 20 =>232, 21 =>233, 22 =>234, 23 =>235, 24 =>236,
                     25 =>237, 26 =>238, 27 =>239, 28 =>240, 29 =>241, 30 =>242, 31 =>243},
                    9 => {1=>244, 2=>245, 3=>246, 4 =>247, 5 =>248, 6 =>249, 7 =>250, 8 =>251, 9 =>252, 10 =>253, 11 =>254, 12 =>255,
                     13 =>256, 14 =>257, 15 =>258, 16=>259, 17=>260, 18 =>261, 19 =>262, 20 =>263, 21 =>264, 22 =>265, 23 =>266, 24 =>267,
                     25 =>268, 26 =>269, 27 =>270, 28 =>271, 29 =>272, 30 =>273},
                    10 => {1=>274, 2=>275, 3=>276, 4 =>277, 5 =>278, 6 =>279, 7 =>280, 8 =>281, 9 =>282, 10 =>283, 11 =>284, 12 =>285,
                      13 =>286, 14 =>287, 15 =>288, 16=>289, 17=>290, 18 =>291, 19 =>292, 20 =>293, 21 =>294, 22 =>295, 23 =>296, 24 =>297,
                      25 =>298, 26 =>299, 27 =>300, 28 =>301, 29 =>302, 30 =>303, 31 =>304},
                    11 => {1=>305, 2=>306, 3=>307, 4 =>308, 5 =>309, 6 =>310, 7 =>311, 8 =>312, 9 =>313, 10 =>314, 11 =>315, 12 =>316,
                      13 =>317, 14 =>318, 15 =>319, 16=>320, 17=>321, 18 =>322, 19 =>323, 20 =>324, 21 =>325, 22 =>326, 23 =>327, 24 =>328,
                      25 =>329, 26 =>330, 27 =>331, 28 =>332, 29 =>333, 30 =>334},
                    12 => {1=>335, 2=>336, 3=>337, 4 =>338, 5 =>339, 6 =>340, 7 =>341, 8 =>342, 9 =>343, 10 =>344, 11 =>345, 12 =>346,
                       13 =>347, 14 =>348, 15 =>349, 16=>350, 17=>351, 18 =>352, 19 =>353, 20 =>354, 21 =>355, 22 =>356, 23 =>357, 24 =>358,
                       25 =>359, 26 =>360, 27 =>361, 28 =>362, 29 =>363, 30 =>364, 31 =>365} );
  }
  elsif ( $yeartype eq "leapyear" )
  {
    %year = ( 1 => {1=>1, 2=>2, 3=>3, 4 =>4, 5 =>5, 6 =>6, 7 =>7, 8 =>8, 9 =>9, 10 =>10, 11 =>11, 12 =>12, 13 =>13, 14 =>14, 15 =>15,
                16=>16, 17=>17, 18 =>18, 19 =>19, 20 =>20, 21 =>21, 22 =>22, 23 =>23, 24 =>24, 25 =>25, 26 =>26, 27 =>27, 28 =>28, 29 =>29,
                30 =>30, 31 =>31},
                2 => {1=>32, 2=>33, 3=>34, 4 =>35, 5 =>36, 6 =>37, 7 =>38, 8 =>39, 9 =>40, 10 =>41, 11 =>42, 12 =>43, 13 =>44, 14 =>45, 15 =>46,
                 16=>47, 17=>48, 18 =>49, 19 =>50, 20 =>51, 21 =>52, 22 =>53, 23 =>54, 24 =>55, 25 =>56, 26 =>57, 27 =>58, 28 =>59, 29=>60},
                 3 => {1=>61, 2=>62, 3=>63, 4 =>64, 5 =>65, 6 =>66, 7 =>67, 8 =>68, 9 =>69, 10 =>70, 11 =>71, 12 =>72, 13 =>73, 14 =>74, 15 =>75,
                  16=>76, 17=>77, 18 =>78, 19 =>79, 20 =>80, 21 =>81, 22 =>82, 23 =>83, 24 =>84, 25 =>85, 26 =>86, 27 =>87, 28 =>88, 29 =>89,
                  30 =>90, 31 =>91},
                  4 => {1=>92, 2=>93, 3=>94, 4 =>95, 5 =>96, 6 =>97, 7 =>98, 8 =>99, 9 =>100, 10 =>101, 11 =>102, 12 =>103, 13 =>104, 14 =>105, 15 =>106,
                  16=>107, 17=>108, 18 =>109, 19 =>110, 20 =>111, 21 =>112, 22 =>113, 23 =>114, 24 =>115, 25 =>116, 26 =>117, 27 =>118, 28 =>119, 29 =>120,
                  30 =>121},
                  5 => {1=>122, 2=>123, 3=>124, 4 =>125, 5 =>126, 6 =>127, 7 =>128, 8 =>129, 9 =>130, 10 =>131, 11 =>132, 12 =>133,
                   13 =>134, 14 =>135, 15 =>136, 16=>137, 17=>138, 18 =>139, 19 =>140, 20 =>141, 21 =>142, 22 =>143, 23 =>144, 24 =>145,
                   25 =>146, 26 =>147, 27 =>148, 28 =>149, 29 =>150, 30 =>151, 31 =>152},
                  6 => {1=>153, 2=>154, 3=>155, 4 =>156, 5 =>157, 6 =>158, 7 =>159, 8 =>160, 9 =>161, 10 =>162, 11 =>163, 12 =>164, 13 =>165,
                   14 =>166, 15 =>167, 16=>168, 17=>169, 18 =>170, 19 =>171, 20 =>172, 21 =>173, 22 =>174, 23 =>175, 24 =>176, 25 =>177,
                   26 =>178, 27 =>179, 28 =>180, 29 =>181, 30 =>182},
                   7 => {1=>183, 2=>184, 3=>185, 4 =>186, 5 =>187, 6 =>188, 7 =>189, 8 =>190, 9 =>191, 10 =>192, 11 =>193, 12 =>194,
                    13 =>195, 14 =>196, 15 =>197, 16=>198, 17=>199, 18 =>200, 19 =>201, 20 =>202, 21 =>203, 22 =>204, 23 =>205, 24 =>206,
                    25 =>207, 26 =>208, 27 =>209, 28 =>210, 29 =>211, 30 =>212, 31 =>213},
                    8 => {1=>214, 2=>215, 3=>216, 4 =>217, 5 =>218, 6 =>219, 7 =>220, 8 =>221, 9 =>222, 10 =>223, 11 =>224, 12 =>225,
                     13 =>226, 14 =>227, 15 =>228, 16=>229, 17=>230, 18 =>231, 19 =>232, 20 =>233, 21 =>234, 22 =>235, 23 =>236, 24 =>237,
                     25 =>238, 26 =>239, 27 =>240, 28 =>241, 29 =>242, 30 =>243, 31 =>244},
                    9 => {1=>245, 2=>246, 3=>247, 4 =>248, 5 =>249, 6 =>250, 7 =>251, 8 =>252, 9 =>253, 10 =>254, 11 =>255, 12 =>256,
                     13 =>257, 14 =>258, 15 =>259, 16=>260, 17=>261, 18 =>262, 19 =>263, 20 =>264, 21 =>265, 22 =>266, 23 =>267, 24 =>268,
                     25 =>269, 26 =>270, 27 =>271, 28 =>272, 29 =>273, 30 =>274},
                    10 => {1=>275, 2=>276, 3=>277, 4 =>278, 5 =>279, 6 =>280, 7 =>281, 8 =>282, 9 =>283, 10 =>284, 11 =>285, 12 =>286,
                      13 =>287, 14 =>288, 15 =>289, 16=>290, 17=>291, 18 =>292, 19 =>293, 20 =>294, 21 =>295, 22 =>296, 23 =>297, 24 =>298,
                      25 =>299, 26 =>300, 27 =>301, 28 =>302, 29 =>303, 30 =>304, 31 =>305},
                    11 => {1=>306, 2=>307, 3=>308, 4 =>309, 5 =>310, 6 =>311, 7 =>312, 8 =>313, 9 =>314, 10 =>315, 11 =>316, 12 =>317,
                      13 =>318, 14 =>319, 15 =>320, 16=>321, 17=>322, 18 =>323, 19 =>324, 20 =>325, 21 =>326, 22 =>327, 23 =>328, 24 =>329,
                      25 =>330, 26 =>331, 27 =>332, 28 =>333, 29 =>334, 30 =>335},
                    12 => {1=>336, 2=>337, 3=>338, 4 =>339, 5 =>340, 6 =>341, 7 =>342, 8 =>343, 9 =>344, 10 =>345, 11 =>346, 12 =>347,
                       13 =>348, 14 =>349, 15 =>350, 16=>351, 17=>352, 18 =>353, 19 =>354, 20 =>355, 21 =>356, 22 =>357, 23 =>358, 24 =>359,
                       25 =>360, 26 =>361, 27 =>362, 28 =>363, 29 =>364, 30 =>365, 31 =>366} );
  }

  #say MONITOR "HERE! \$year\{$month}\{$day}: $year{$month}{$day}";
  my $h = $hour;
  my $decl = 23.45 * sin( deg2rad( 280.1 + 0.9863 * $year{$month}{$day} ) );
  my $declrad = deg2rad($decl);

  my $timeq = ( 9.87 * sin( deg2rad( ( 1.978 * $year{$month}{$day}  )- 160.22 ) ) ) - ( 7.53 * cos( deg2rad( ( 0.989 * $year{$month}{$day} )- 80.11 ) ) )
    - ( 1.5 * sin( deg2rad( ( 0.989 * $year{$month}{$day}  ) - 80.11 ) ) );  #say "\$timeq: $timeq";

  my $tcf = ( 4 * ( $lstm - $long ) ) + $timeq; #say "\$tcf: $tcf";

  my $solartime = ( $h + ( $tcf / 60 ) ); #say "\$solartime2: $solartime";

  my $hourangle = ( 15 * ( 12 - $solartime ) );
  my $houranglerad = deg2rad($hourangle);

  my $latrad = deg2rad($lat);

  my $altrad = asin( ( cos( $latrad ) * cos( $declrad ) * cos( $houranglerad ) ) + ( sin( $latrad ) * sin( $declrad ) ) );
  my $alt = rad2deg($altrad);
  $alt = sprintf ( "%.3f", $alt );

  my $azirad = ( asin( cos( $declrad ) * ( sin( $houranglerad ) / cos( $altrad ) ) ) );
  my $azi = rad2deg($azirad);
  $azi = sprintf ( "%.3f", $azi );
  #say MONITOR "CALCULATED ALT $alt AZI $azi";
  return ( $alt, $azi );
}


sub getdirdiff
{
  ( $paths_ref, $calcprocedures_ref ) = @_;
  my %paths = %{ $paths_ref };
  my @calcprocedures = @{ $calcprocedures_ref };
  my $clma = $paths{clmfilea};
  my ( %year, $begun, %dirdiffs, $day, $monthnum );
  my $begun = "no";

  open ( CLM, "$clma" ) or die;
  my @lines = <CLM>;
  close CLM;
  #say MONITOR "\$clma $clma";

  my $countday = 0;
  foreach my $line ( @lines )
  {
    chomp $line;
    $line =~ s/^\s+//;
    my @elts = split( /\s+|,/, $line );
    if ( $line =~ /^\* day/ )
    {
      $begun = "yes";
      $day = $elts[2];
      $monthnum = $elts[4];
      $countday = $countday + 1;
      $counthour = 0;
    }
    else
    {
      if ( "getweather" ~~ @calcprocedures )
      {
        if ( $begun eq "yes" )
        {
          $counthour = $counthour + 1;
          $diff = $elts[1];
          $dir = $elts[2];
          $dirdiffs{$monthnum}{$day}{$counthour}{diff} = $diff;
          $dirdiffs{$monthnum}{$day}{$counthour}{dir} = $dir;
        }
      }
    }
    #say MONITOR "LINE: $line";
    #say MONITOR "ELTS " . dump( @elts );
  }

  #say MONITOR "IN GETDIRDIF F \$countday $countday";
  if ( $countday >= 366 )
  { $paths{yeartype} = "leapyear"; }
  elsif ( $countday <= 365 )
  { $paths{yeartype} = "normalyear"; }

  #say MONITOR "MODIRDIFFS " . dump( %dirdiffs );
  return ( \%dirdiffs, \%paths );
}


sub refilter
{
  my ( $infile  )= @_;

  my $oldinfile = $infile . ".old";
  `cp -f $infile $oldinfile`;
  say REPORT "cp -f $infile $oldinfile";

  open( INFILE, "$infile" ) or die;
  my @lins = <INFILE>;
  close INFILE;

  open( INFILE, ">$infile" ) or die;
  my @nums = ( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 );
  my @names = qw( Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec );
  my ( @sems, @bag );
  foreach my $num ( @nums )
  {
    foreach $lin ( @lins )
    {
      chomp $lin;
      my @els = split( " +", $lin );
      if ( scalar( @els ) > 10 )
      {
        my $name = $names[$num-1];
        if ( $lin =~ /$name?/ )
        {
          unless ( $sems[$num] eq "done" )
          {
            $sems[$num] = "done";
            push( @bag, $lin );
          }
        }
      }
    }
  }

  foreach my $li ( @bag )
  {
    print INFILE "$li\n";
  }
  close INFILE;
}


sub modish
{ # MAIN PROGRAM
  open( MONITOR, ">>./monitor.txt" ) or die;

  say MONITOR "ARGV: " . dump( @ARGV );

  if ( "-setdefaults" ~~ @ARGV )
  {
    shift @ARGV;


    say MONITOR "GOT BEFORE1 " . dump( @ARGV );

    my @files = <*.cfg>;
    my $conf;
    foreach my $file ( @files )
    {
      if ( not ( $file =~ /_f/ ) )
      {
        $conf = $file;
        last;
      }
    }

    my $pwd = `pwd`;
    chomp $pwd;
    $conf = $pwd . "/" . $conf;
    my $confpath_ = $pwd . "/" ;

    my @news;
    foreach ( @ARGV )
    {
      if ( $_ eq "\1" ){ $_ = "1"; }; # % compute reflections from obs.
      if ( $_ eq "\2" ){ $_ = "2"; }; # % do not compute refl.from obs.
      if ( $_ eq "\3" ){ $_ = "3"; }; # #
      if ( $_ eq "\4" ){ $_ = "4"; }; # # shd.f. corr. + diffuse piping
      if ( $_ eq "\5" ){ $_ = "5"; }; # # shd.f.c.+dif.pip.+ground refl
      if ( $_ eq "\6" ){ $_ = "6"; }; # # complete recalculation
      if ( $_ eq "\a" ){ $_ = "7"; }; # # compl.recalc.+ diffuse piping
      if ( $_ eq "\b" ){ $_ = "8"; }; # pour direct reflections into the diffuse calculation channel
      if ( $_ eq "\13" ){ $_ = "11"; }; # * with Perez sky from weather
      if ( $_ eq "\f" ){ $_ = "12"; }; # * CIE sky not from weather
      if ( $_ eq "\r" ){ $_ = "13"; }; # * with Perez sky no weather
      if ( $_ eq "\16" ){ $_ = "14"; }; # $ 1 diffuse bounce
      if ( $_ eq "\17" ){ $_ = "15"; }; # $ 2 diffuse bounces
      if ( $_ eq "\20" ){ $_ = "16"; }; # $ 3 diffuse bounces
      if ( $_ eq "\21" ){ $_ = "17"; }; # 1 direction vector
      if ( $_ eq "\22" ){ $_ = "18"; }; # 5 direction vectors
      if ( $_ eq "\23" ){ $_ = "19"; }; # 17 direction vectors
      if ( $_ eq "\24" ){ $_ = "20"; }; # | resolution: 2x2 diffuse & dir
      if ( $_ eq "\25" ){ $_ = "21"; }; # | resolution:2x2 diff 20x20 dir
      if ( $_ eq "\26" ){ $_ = "22"; }; # | resolution:1x1 diff 10x10 dir
      if ( $_ eq "\27" ){ $_ = "23"; }; # |
      if ( $_ eq "\28" ){ $_ = "24"; }; # | compute all zones &all surfs
      if ( $_ eq "\29" ){ $_ = "25"; }; # | include all zones &all surfs
      if ( $_ eq "\30" ){ $_ = "26"; }; # | if non-embedded: compute now.
      push( @news, $_ );
    }

    say MONITOR "GOT AFTER \$ARGV[0] $news[0]";

    if ( $news[0] eq "1" )
    {
      if ( not ( -e "./modish_defaults.pl" ) )
      {
        if ( not ( -e "./_modish_defaults.pl") )
        {
          `cp -f /opt/esp-r/bin/modish/modish_defaults.pl ./`;
        }
        else
        {
          `mv -f ./_modish_defaults.pl ./modish_defaults.pl`;
        }
      }
    }
    elsif ( $news[0] eq "2" )
    {
      if ( -e "./modish_defaults.pl" )
      {
        `mv -f ./modish_defaults.pl ./_modish_defaults.pl`;
      }
    }
    elsif ( $news[0] eq "3" )
    #{ # shading factor correction
    #  open(FIL, "./modish_defaults.pl" ) or die;
    #  my @lins = <FIL>;
    #  close FIL;
    #
    #  open(FIL, ">./modish_defaults.pl" ) or die;
    #
    #  foreach my $lin ( @lins )
    #  {
    #    if ( $lin =~ /^\@calcprocedures/ )
    #    {
    #      $lin = "\@calcprocedures = ( \"diluted\", \"gensky\", \"composite\", \"groundreflections\" ) ;\n";
    #    }
    #    print FIL $lin;
    #  }
    #  close FIL;
    #}
    elsif ( $news[0] eq "4" )
    {
      open(FIL, "./modish_defaults.pl" ) or die;
      my @lins = <FIL>;
      close FIL;

      open(FIL, ">./modish_defaults.pl" ) or die;

      foreach my $lin ( @lins )
      {
        if ( $lin =~ /^\@calcprocedures/ )
        {
          $lin = "\@calcprocedures = ( \"diluted\", \"gensky\", \"composite\", \"alldiff\", \"groundreflections\" ) ;\n";
        }
        print FIL $lin;
      }
      close FIL;
    }
    elsif ( $news[0] eq "5" )
    {
      open(FIL, "./modish_defaults.pl" ) or die;
      my @lins = <FIL>;
      close FIL;

      open(FIL, ">./modish_defaults.pl" ) or die;

      foreach my $lin ( @lins )
      {
        if ( $lin =~ /^\@calcprocedures/ )
        {
          $lin = "\@calcprocedures = ( \"diluted\", \"gensky\", \"composite\", \"alldiff\" ) ;\n";
        }
        print FIL $lin;
      }
      close FIL;
    }
    elsif ( $news[0] eq "6" )
    {
      open(FIL, "./modish_defaults.pl" ) or die;
      my @lins = <FIL>;
      close FIL;

      open(FIL, ">./modish_defaults.pl" ) or die;

      foreach my $lin ( @lins )
      {
        if ( $lin =~ /^\@calcprocedures/ )
        {
          $lin = "\@calcprocedures = ( \"diluted\", \"gensky\", \"radical\" ) ;\n";
        }
        print FIL $lin;
      }
      close FIL;
    }
    elsif ( $news[0] eq "7" )
    {
      open(FIL, "./modish_defaults.pl" ) or die;
      my @lins = <FIL>;
      close FIL;

      open(FIL, ">./modish_defaults.pl" ) or die;

      foreach my $lin ( @lins )
      {
        if ( $lin =~ /^\@calcprocedures/ )
        {
          $lin = "\@calcprocedures = ( \"diluted\", \"gensky\", \"radical\", \"alldiff\" ) ;\n";
        }
        print FIL $lin;
      }
      close FIL;
    }
    elsif ( $news[0] eq "8" )
    {
      open(FIL, "./modish_defaults.pl" ) or die;
      my @lins = <FIL>;
      close FIL;

      open(FIL, ">./modish_defaults.pl" ) or die;

      foreach my $lin ( @lins )
      {
        if ( $lin =~ /^\@calcprocedures/ )
        {
            $lin = "\@calcprocedures = ( \"diluted\", \"gensky\", \"plain\", \"alldiff\" ) ;\n";
        }
        print FIL $lin;
      }
      close FIL;
    }
    elsif ( $news[0] eq "11" )
    {
      open(FIL, "./modish_defaults.pl" ) or die;
      my @lins = <FIL>;
      close FIL;

      open(FIL, ">./modish_defaults.pl" ) or die;

      foreach my $lin ( @lins )
      {
        if ( $lin =~ /^\@calcprocedures/ )
        {
          $lin =~ s/\"gensky\"//g ;
          $lin =~ s/\"getweather\"//g ;
          $lin =~ s/\"gendaylit\"//g ;
          $lin =~ s/\"getsimple\"//g ;
          $lin =~ s/\"diluted\"/\"diluted\", \"getweather\", , \"getsimple\", \"gendaylit\"/ ;
        }
        print FIL $lin;
      }
      close FIL;
    }
    elsif ( $news[0] eq "12" )
    {
      open(FIL, "./modish_defaults.pl" ) or die;
      my @lins = <FIL>;
      close FIL;

      open(FIL, ">./modish_defaults.pl" ) or die;

      foreach my $lin ( @lins )
      {
        if ( $lin =~ /^\@calcprocedures/ )
        {
            $lin =~ s/\"getweather\"//g ;
            $lin =~ s/\"gendaylit\"//g ;
            $lin =~ s/\"gensky\"//g ;
            $lin =~ s/\"diluted\"/\"diluted\", \"gensky\"/ ;
        }
        print FIL $lin;
      }
      close FIL;
    }
    elsif ( $news[0] eq "13" )
    {
      open(FIL, "./modish_defaults.pl" ) or die;
      my @lins = <FIL>;
      close FIL;

      open(FIL, ">./modish_defaults.pl" ) or die;

      foreach my $lin ( @lins )
      {
        if ( $lin =~ /^\@calcprocedures/ )
        {
          $lin =~ s/gensky/gendaylit/ ;
          $lin =~ s/\"getweather\"// ;
        }
        print FIL $lin;
      }
      close FIL;
    }
    elsif ( $news[0] eq "14" )
    {
      open(FIL, "./modish_defaults.pl" ) or die;
      my @lins = <FIL>;
      close FIL;

      open(FIL, ">./modish_defaults.pl" ) or die;

      foreach my $lin ( @lins )
      {
        if ( $lin =~ /^\@defaults/ )
        {
          chomp $lin;chomp $lin;
          $lin =~ s/^\@defaults// ;
          $lin =~ s/=//g ;
          $lin =~ s/\(//g ;
          $lin =~ s/\)//g ;
          $lin =~ s/\[//g ;
          $lin =~ s/\]//g ;
          $lin =~ s/\;//g ;
          $lin =~ s/\s+//g ;
          $lin =~ s/\n//g ;
          my @elts = split( /,/, $lin );
          if ( scalar( @elts ) == 6 )
          {
            $elts[3] = 1;
            $elts[5] =~ s/\n// ;
            chomp $elts[5];
            $lin = "\@defaults = ( [ " . $elts[0] . ", " . $elts[1] . " ], " . $elts[2] . ", " . $elts[3] . ", " . $elts[4] . ", " . $elts[5] . " );\n";
          }
          elsif ( scalar( @elts ) == 8 )
          {
            $elts[5] = 1;
            chomp $elts[7];
            $lin = "\@defaults = ( [[ " . $elts[0] . ", " . $elts[1] . " ], [ " . $elts[2] . ", " . $elts[3] . " ]], " . $elts[4] . ", " . $elts[5] . ", " . $elts[6] . ", " . $elts[7] . " );\n";
          }
        }
        print FIL $lin;
      }
      close FIL;
    }
    elsif ( $news[0] eq "15" )
    {
      open(FIL, "./modish_defaults.pl" ) or die;
      my @lins = <FIL>;
      close FIL;

      open(FIL, ">./modish_defaults.pl" ) or die;

      foreach my $lin ( @lins )
      {
        if ( $lin =~ /^\@defaults/ )
        {
          chomp $lin;
          $lin =~ s/^\@defaults// ;
          $lin =~ s/=//g ;
          $lin =~ s/\(//g ;
          $lin =~ s/\)//g ;
          $lin =~ s/\[//g ;
          $lin =~ s/\]//g ;
          $lin =~ s/\;//g ;
          $lin =~ s/\s+//g ;
          $lin =~ s/\n//g ;
          my @elts = split( /,/, $lin );
          if ( scalar( @elts ) == 6 )
          {
            $elts[3] = 2;
            chomp $elts[5];
            $lin = "\@defaults = ( [ " . $elts[0] . ", " . $elts[1] . " ], " . $elts[2] . ", " . $elts[3] . ", " . $elts[4] . ", " . $elts[5] . " );\n";
          }
          elsif ( scalar( @elts ) == 8 )
          {
            $elts[5] = 2;
            chomp $elts[7];
            $lin = "\@defaults = ( [[ " . $elts[0] . ", " . $elts[1] . " ], [ " . $elts[2] . ", " . $elts[3] . " ]], " . $elts[4] . ", " . $elts[5] . ", " . $elts[6] . ", " . $elts[7] . " );\n";
          }
        }
        print FIL $lin;
      }
      close FIL;
    }
    elsif ( $news[0] eq "16" )
    {
      open(FIL, "./modish_defaults.pl" ) or die;
      my @lins = <FIL>;
      close FIL;

      open(FIL, ">./modish_defaults.pl" ) or die;

      foreach my $lin ( @lins )
      {
        if ( $lin =~ /^\@defaults/ )
        {
          chomp $lin;
          $lin =~ s/^\@defaults// ;
          $lin =~ s/=//g ;
          $lin =~ s/\(//g ;
          $lin =~ s/\)//g ;
          $lin =~ s/\[//g ;
          $lin =~ s/\]//g ;
          $lin =~ s/\;//g ;
          $lin =~ s/\s+//g ;
          $lin =~ s/\n//g ;
          my @elts = split( /,/, $lin );
          if ( scalar( @elts ) == 6 )
          {
            $elts[3] = 3;
            chomp $elts[5];
            $lin = "\@defaults = ( [ " . $elts[0] . ", " . $elts[1] . " ], " . $elts[2] . ", " . $elts[3] . ", " . $elts[4] . ", " . $elts[5] . " );\n";
          }
          elsif ( scalar( @elts ) == 8 )
          {
            $elts[5] = 3;
            chomp $elts[7];
            $lin = "\@defaults = ( [[ " . $elts[0] . ", " . $elts[1] . " ], [ " . $elts[2] . ", " . $elts[3] . " ]], " . $elts[4] . ", " . $elts[5] . ", " . $elts[6] . ", " . $elts[7] . " );\n";
          }
        }
        print FIL $lin;
      }
      close FIL;
    }
    elsif ( $news[0] eq "17" )
    {
      open(FIL, "./modish_defaults.pl" ) or die;
      my @lins = <FIL>;
      close FIL;

      open(FIL, ">./modish_defaults.pl" ) or die;

      foreach my $lin ( @lins )
      {
        if ( $lin =~ /^\@defaults/ )
        {
          chomp $lin;
          $lin =~ s/^\@defaults// ;
          $lin =~ s/=//g ;
          $lin =~ s/\(//g ;
          $lin =~ s/\)//g ;
          $lin =~ s/\[//g ;
          $lin =~ s/\]//g ;
          $lin =~ s/\;//g ;
          $lin =~ s/\s+//g ;
          $lin =~ s/\n//g ;
          my @elts = split( /,/, $lin );
          if ( scalar( @elts ) == 6 )
          {
            $elts[2] = 1;
            chomp $elts[5];
            $lin = "\@defaults = ( [ " . $elts[0] . ", " . $elts[1] . " ], " . $elts[2] . ", " . $elts[3] . ", " . $elts[4] . ", " . $elts[5] . " );\n";
          }
          elsif ( scalar( @elts ) == 8 )
          {
            $elts[4] = 1;
            chomp $elts[7];
            $lin = "\@defaults = ( [[ " . $elts[0] . ", " . $elts[1] . " ], [ " . $elts[2] . ", " . $elts[3] . " ]], " . $elts[4] . ", " . $elts[5] . ", " . $elts[6] . ", " . $elts[7] . " );\n";
          }
        }
        print FIL $lin;
      }
      close FIL;
    }
    elsif ( $news[0] eq "18" )
    {
      open(FIL, "./modish_defaults.pl" ) or die;
      my @lins = <FIL>;
      close FIL;

      open(FIL, ">./modish_defaults.pl" ) or die;

      foreach my $lin ( @lins )
      {
        if ( $lin =~ /^\@defaults/ )
        {
          chomp $lin;
          $lin =~ s/^\@defaults// ;
          $lin =~ s/=//g ;
          $lin =~ s/\(//g ;
          $lin =~ s/\)//g ;
          $lin =~ s/\[//g ;
          $lin =~ s/\]//g ;
          $lin =~ s/\;//g ;
          $lin =~ s/\s+//g ;
          $lin =~ s/\n//g ;
          my @elts = split( /,/, $lin );
          if ( scalar( @elts ) == 6 )
          {
            $elts[2] = 5;
            chomp $elts[5];
            $lin = "\@defaults = ( [ " . $elts[0] . ", " . $elts[1] . " ], " . $elts[2] . ", " . $elts[3] . ", " . $elts[4] . ", " . $elts[5] . " );\n";
          }
          elsif ( scalar( @elts ) == 8 )
          {
            $elts[4] = 5;
            chomp $elts[7];
            $lin = "\@defaults = ( [[ " . $elts[0] . ", " . $elts[1] . " ], [ " . $elts[2] . ", " . $elts[3] . " ]], " . $elts[4] . ", " . $elts[5] . ", " . $elts[6] . ", " . $elts[7] . " );\n";
          }
        }
        print FIL $lin;
      }
      close FIL;
    }
    elsif ( $news[0] eq "19" )
    {
      open(FIL, "./modish_defaults.pl" ) or die;
      my @lins = <FIL>;
      close FIL;

      open(FIL, ">./modish_defaults.pl" ) or die;

      foreach my $lin ( @lins )
      {
        if ( $lin =~ /^\@defaults/ )
        {
          chomp $lin;
          $lin =~ s/^\@defaults// ;
          $lin =~ s/=//g ;
          $lin =~ s/\(//g ;
          $lin =~ s/\)//g ;
          $lin =~ s/\[//g ;
          $lin =~ s/\]//g ;
          $lin =~ s/\;//g ;
          $lin =~ s/\s+//g ;
          $lin =~ s/\n//g ;
          my @elts = split( /,/, $lin );
          if ( scalar( @elts ) == 6 )
          {
            $elts[2] = 17;
            chomp $elts[5];
            $lin = "\@defaults = ( [ " . $elts[0] . ", " . $elts[1] . " ], " . $elts[2] . ", " . $elts[3] . ", " . $elts[4] . ", " . $elts[5] . " );\n";
          }
          elsif ( scalar( @elts ) == 8 )
          {
            $elts[4] = 17;
            chomp $elts[7];
            $lin = "\@defaults = ( [[ " . $elts[0] . ", " . $elts[1] . " ], [ " . $elts[2] . ", " . $elts[3] . " ]], " . $elts[4] . ", " . $elts[5] . ", " . $elts[6] . ", " . $elts[7] . " );\n";
          }
        }
        print FIL $lin;
      }
      close FIL;
    }
    elsif ( $news[0] eq "20" )
    {
      open(FIL, "./modish_defaults.pl" ) or die;
      my @lins = <FIL>;
      close FIL;

      open(FIL, ">./modish_defaults.pl" ) or die;

      foreach my $lin ( @lins )
      {
        if ( $lin =~ /^\@defaults/ )
        {
          chomp $lin;
          $lin =~ s/^\@defaults// ;
          $lin =~ s/=//g ;
          $lin =~ s/\(//g ;
          $lin =~ s/\)//g ;
          $lin =~ s/\[//g ;
          $lin =~ s/\]//g ;
          $lin =~ s/\;//g ;
          $lin =~ s/\s+//g ;
          $lin =~ s/\n//g ;
          my @elts = split( /,/, $lin );
          if ( scalar( @elts ) == 6 )
          {
            $elts[0] = 2;
            $elts[1] = 2;
            chomp $elts[5];
            $lin = "\@defaults = ( [ " . $elts[0] . ", " . $elts[1] . " ], " . $elts[2] . ", " . $elts[3] . ", " . $elts[4] . ", " . $elts[5] . " );\n";
          }
          elsif ( scalar( @elts ) == 8 )
          {
            $elts[0] = 2;
            $elts[1] = 2;
            chomp $elts[7];
            $lin = "\@defaults = ( [ " . $elts[0] . ", " . $elts[1] . " ], " . $elts[4] . ", " . $elts[5] . ", " . $elts[6] . ", " . $elts[7] . " );\n";
          }
        }
        print FIL $lin;
      }
      close FIL;
    }
    elsif ( $news[0] eq "21" )
    {
      open(FIL, "./modish_defaults.pl" ) or die;
      my @lins = <FIL>;
      close FIL;

      open(FIL, ">./modish_defaults.pl" ) or die;

      foreach my $lin ( @lins )
      {
        if ( $lin =~ /^\@defaults/ )
        {
          chomp $lin;
          $lin =~ s/^\@defaults// ;
          $lin =~ s/=//g ;
          $lin =~ s/\(//g ;
          $lin =~ s/\)//g ;
          $lin =~ s/\[//g ;
          $lin =~ s/\]//g ;
          $lin =~ s/\;//g ;
          $lin =~ s/\s+//g ;
          $lin =~ s/\n//g ;
          my @elts = split( /,/, $lin );
          if ( scalar( @elts ) == 6 )
          {
            $elts[7] = $elts[5];
            $elts[6] = $elts[4];
            $elts[5] = $elts[3];
            $elts[4] = $elts[2];
            $elts[0] = 2;
            $elts[1] = 2;
            $elts[2] = 20;
            $elts[3] = 20;
            chomp $elts[7];
            $lin = "\@defaults = ( [[ " . $elts[0] . ", " . $elts[1] . " ], [ " . $elts[2] . ", " . $elts[3] . " ]], " . $elts[4] . ", " . $elts[5] . ", " . $elts[6] . ", " . $elts[7] . " );\n";
          }
          elsif ( scalar( @elts ) == 8 )
          {
            $elts[0] = 2;
            $elts[1] = 2;
            $elts[2] = 20;
            $elts[3] = 20;
            chomp $elts[7];
            $lin = "\@defaults = ( [[ " . $elts[0] . ", " . $elts[1] . " ], [ " . $elts[2] . ", " . $elts[3] . " ]], " . $elts[4] . ", " . $elts[5] . ", " . $elts[6] . ", " . $elts[7] . " );\n";
          }
        }
        print FIL $lin;
      }
      close FIL;
    }
    elsif ( $news[0] eq "22" )
    {
      open(FIL, "./modish_defaults.pl" ) or die;
      my @lins = <FIL>;
      close FIL;

      open(FIL, ">./modish_defaults.pl" ) or die;

      foreach my $lin ( @lins )
      {
        if ( $lin =~ /^\@defaults/ )
        {
          chomp $lin;
          $lin =~ s/^\@defaults// ;
          $lin =~ s/=//g ;
          $lin =~ s/\(//g ;
          $lin =~ s/\)//g ;
          $lin =~ s/\[//g ;
          $lin =~ s/\]//g ;
          $lin =~ s/\;//g ;
          $lin =~ s/\s+//g ;
          $lin =~ s/\n//g ;
          my @elts = split( /,/, $lin );
          if ( scalar( @elts ) == 6 )
          {
            $elts[7] = $elts[5];
            $elts[6] = $elts[4];
            $elts[5] = $elts[3];
            $elts[4] = $elts[2];
            $elts[0] = 1;
            $elts[1] = 1;
            $elts[2] = 10;
            $elts[3] = 10;
            chomp $elts[7];
            $lin = "\@defaults = ( [[ " . $elts[0] . ", " . $elts[1] . " ], [ " . $elts[2] . ", " . $elts[3] . " ]], " . $elts[4] . ", " . $elts[5] . ", " . $elts[6] . ", " . $elts[7] . " );\n";
          }
          elsif ( scalar( @elts ) == 8 )
          {
            $elts[0] = 1;
            $elts[1] = 1;
            $elts[2] = 10;
            $elts[3] = 10;
            chomp $elts[7];
            $lin = "\@defaults = ( [[ " . $elts[0] . ", " . $elts[1] . " ], [ " . $elts[2] . ", " . $elts[3] . " ]], " . $elts[4] . ", " . $elts[5] . ", " . $elts[6] . ", " . $elts[7] . " );\n";
          }
        }
        print FIL $lin;
      }
      close FIL;
    }
    elsif ( $news[0] eq "24" )
    { # specify zones & surfs - optional
      if (-e "./_modish_request.pl" )
      {
        `mv -f ./_modish_request.pl ./modish_request.pl`
      }

      if ( ( not (-e "./modish_request.pl" ) ) and ( not (-e "./_modish_request.pl" ) ) )
      {
        open(FIL, ">./modish_request.pl" );
        say FIL "\n\n# #Fill in the values in the first row of this \"modish_request.pl\"`file,
# then write here, in the second row, the name of the ESP-r config file,
# then launch the shading reflection calculation from the ESP-r menu
# about the shading calculations, in \"model context\".
# The values to be filled into the first row above have to have this format:
#
# zone_number  opening_n_number  opening_y_number and ... and zone_number  opening_m_number  opening_z_number,
#
# This series of numbers have to be written all in the first row,
# and the row has always to be terminated by a comma.
# For example:
# 1 1 7 and 3 5,
# would means: take into account the reflection from obstruction for zone 1, surfaces 1 and 7,
# and zone 3, surface 5.
# If calculations of reflection from obstruction are not requested, leave the line blank.
# If the file \"modish_request.pl\" is absent,
# all the existing transparent surfaces in all the existing zones of model, however,
# will be taken into account in the calculation of reflections from obstructions,
# and this may be very slow."
      }
      `nedit ./modish_request.pl`;
    }
    elsif ( $news[0] eq "25" )
    { # do not specify zones & surfaces
      `mv -f ./modish_request.pl ./_modish_request.pl`;
    }
    elsif ( $news[0] eq "26" )
    { # launch monthly recalculation
      my ($add, $file);
      if ( scalar( @ARGV ) == 3 )
      {
        $add = pop ( @ARGV );
      }
      else
      {
        if ( -e "./modish_request.pl" )
        {
          $file = "./modish_request.pl";
        }
        elsif ( -e "./_modish_request.pl" )
        {
          $file = "./_modish_request.pl";
        }

        open(FIL, $file );
        my @lins = <FIL>;
        close FIL;

        my $lin = $lins[0];
        chomp $lin;

        my $prepare = $lins[1];
        chomp $prepare;
        $prepare =~ s/  / /g;
        $prepare =~ s/  / /g;
        $prepare =~ s/  / /g;
        $prepare =~ s/  / /g;
        my @elts = split( " ", $prepare );
        $add = $elts[0];
      }
      my $fullcfg = $confpath_ . $add;

      say MONITOR "perl /opt/esp-r/bin/modish/Modish.pm $fullcfg $lin";
      `perl /opt/esp-r/bin/modish/Modish.pm $fullcfg $lin`;
    }
    exit;
  }

  if ( -e "./modish_defaults.pl" )
  {
    require "./modish_defaults.pl";
  }
  else
  {
    say "A \"modish_defaults.pl\" file must be present in the cfg model folder. Now it is not. Halting.";
    exit;
    #require "/opt/esp-r/bin/modish/modish_defaults.pl";
  }

  unless ( "report" ~~ @calcprocedures )
  {
    close MONITOR;
  }

  my ( @things, @things2, $launchfile, $modishdefpath, %paths, $path, $myfile, $myday, $mymonth, $myzone, $zonenum, $launchtype, $cfgfile );

  if ( ( @ARGV ) and ( ( not ( "-mode" ~~ @ARGV ) ) and ( not ( "-file" ~~ @ARGV ) ) and ( not ( "-zone" ~~ @ARGV ) ) ) )
  {
    $launchtype = "commandline";
    @things = @_;

    $launchfile = shift( @things );

    $path = $launchfile;

    if ( not ( "-finalizing" ~~ @ARGV ) )
    {
      if ( -e "./modish_request.pl" )
      {
        say MONITOR "I AM 1";
        open( THIS, "./modish_request.pl" );
        my @lines = <THIS>;
        close THIS;

        my $line = $lines[0];
        chomp $line;
        say MONITOR "HERE LINE: " . dump( $line );
        my @splits = split( "and", $line );
        say MONITOR "HERE SPLITS: " . dump( @splits );
        foreach my $elt ( @splits )
        {
          `perl /opt/esp-r/bin/modish/Modish.pm $launchfile $elt -finalizing`
        }
        exit;
      }
      elsif ( not ( scalar( @things ) == 0 ) )
      {
        say MONITOR "I AM 2";
        $" = " ";
        my @bucket;
        my $counter = 0;
        foreach my $elt ( @things )
        {
          if ( $elt ne "and" )
          {
            push ( @{ $bucket[$counter] }, $elt );
          }
          if ( $elt eq "and" )
          {
            $counter++;
          }
        }

        foreach my $reqs_ref ( @bucket )
        {
          my @requests = @{ $reqs_ref };
          `perl /opt/esp-r/bin/modish/Modish.pm $launchfile @requests -finalizing`;
        }
        exit;
      }
      elsif ( scalar( @things ) == 0 )
      { say MONITOR "I AM 3";
        open( THAT, $launchfile ) or die;
        my @lines = <THAT>;
        close THAT;

        my @zones;
        foreach my $line ( @lines )
        {
          chomp $line;
          if ( $line =~ /^\*zon / )
          {
            $line =~ s/  / /g;
            $line =~ s/  / /g;
            $line =~ s/  / /g;
            $line =~ s/  / /g;
            $line =~ s/  / /g;
            $line =~ s/  / /g;
            my @splits = split( " ", $line );
            my $num = $splits[1];
            push ( @zones, $num );
          }
        } say MONITOR "HERE ZONES: " . dump ( @zones );

        foreach my $zone ( @zones )
        {
          say MONITOR "HERE perl /opt/esp-r/bin/modish/Modish.pm $launchfile $zone -finalizing";
          `perl /opt/esp-r/bin/modish/Modish.pm $launchfile $zone -finalizing`;
        }
        exit;
      }
    }

    if ( "-finalizing" ~~ @ARGV )
    {
      pop @ARGV;
      pop @things;
    }

    if ( "embedded" ~~ @things )
    {
      push( @calcprocedures, "embedded" );
      @things = grep(!/embedded/, @things);
    }

    my @temps;
    foreach my $elt ( @things )
    {

      unless ( ( $elt =~ /myday-/ ) or ( $elt =~ /mymonth-/ ) )
      {
        $elt =~ s/myday-//;
        push ( @temps, $elt );
      }

      if ( $elt =~ /myday-/ )
      {
        $elt =~ s/myday-//;
        $myday = $elt;
      }
      elsif ( $elt =~ /mymonth-/ )
      {
        $elt =~ s/mymonth-//;
        $mymonth = $elt;
      }
    }
    @things = @temps;
  }
  elsif ( ( @ARGV ) and ( ( "-mode" ~~ @ARGV ) and ( "-file" ~~ @ARGV ) and ( "-zone" ~~ @ARGV) ) )
  {
    $launchtype = "ESP-r";
    @ARGV = grep( !/ish/, @ARGV );
    @ARGV = grep( !/-mode/, @ARGV );
    @ARGV = grep( !/text/, @ARGV );
    @ARGV = grep( !/-file/, @ARGV );
    @ARGV = grep( !/-day/, @ARGV );
    @ARGV = grep( !/-zone/, @ARGV );
    $myfile = $ARGV[0];
    $myday = $ARGV[1];
    $mymonth = $ARGV[2];
    $myzone = $ARGV[3];
    $path = `pwd`;
    chomp $path;
    $launchfile = $path . "/" . $myfile;
    push( @calcprocedures, "embedded" );
  }

  if ( "getweather" ~~ @calcprocedures )
  {
    push( @calcprocedures, "getsimple" );
  }
  say MONITOR "CALCPROCEDURES NOW: " . dump( @calcprocedures );
  say MONITOR "THINGS NOW: " . dump( @things );

  $path =~ s/\.cfg$// ;

  while ( not ( $path =~ /\/$/ ) )
  {
    $path =~ s/(\w+)$// ;
  }
  $path =~ s/\/$// ;

  if ( $path =~ /\/cfg$/ )
  {
    $path =~ s/\/cfg$// ;
  }

  if ( $launchtype eq "commandline" )
  {
    if ( $path =~ /cfg$/ )
    {
      $cfgfile = $launchfile;
      $cfgfile =~ s/$path\/// ;
      $path =~ s/\/cfg$// ;
    }
    else
    {
      $cfgfile = $launchfile;
      $cfgfile =~ s/$path\/// ;
    }
    $cfgfile =~ s/cfg\/// ;
  }
  elsif ( $launchtype eq "ESP-r" )
  {
    $cfgfile = $myfile
  }

  my ( @restpars, @settings, @received );

  my ( $zonenum, $dirvectorsnum, $bounceambnum, $bouncemaxnum, $distgrid );
  my ( @transpdata, @surfaces, @dirvectorsrefs, @transpsurfs, @resolutions, @treatedlines );
  my ( @specularratios, @boundbox, @specularratios, %skycondition, $add );

  my %zonenumname = ( 1 => "a", 2 => "b", 3 => "c", 4 => "d", 5 => "e", 6 => "f", 7 => "g", 8 => "h", 9 => "i", 10 => "j",
                    11 => "k", 12 => "l", 13 => "m", 14 => "n", 15 => "o", 16 => "p", 17 => "q", 18 => "r", 19 => "s", 20 => "t",
                    21 => "u", 22 => "v", 23 => "x", 24 => "y", 25 => "w", 26 => "z");

  if ( $launchtype eq "commandline" )
  {
    @received = @things;
    foreach ( @received )
    {
      if ( not ( ref( $_ ) ) )
      {
        push( @restpars, $_ );
      }
      else
      {
        push( @settings );
      }
    }

    if ( scalar( @restpars ) == 0 ) { say "NO ZONE HAS BEEN SPECIFIED. EXITING." and die; }

    $zonenum = $restpars[0];
    @transpsurfs = @restpars[ 1..$#restpars ];
  }
  elsif ( $launchtype eq "ESP-r" )
  {
    open( CONFIG, "$myfile" ) or die;
    my @lines = <CONFIG>;

    foreach my $line ( @lines )
    {
      chomp $line;
      my @elts = split ( /\s+|,/, $line );
      if ( ( $elts[0] eq "*zon" ) and ( $elts[-1] eq $myzone ) )
      {
        $zonenum = $elts[1];
      }
    }

    if ( -e "./modish_request.pl" )
    {
      open(THIS, ">./modish_request.pl" );
      my @lines = <THIS>;
      close THIS;

      my $line = $lines[0];
      chomp $line;
      my @splits = split( "and", $line );

      my @bag;
      foreach my $elt ( @splits )
      {
        my @resplits = split( " ", $elt );
        push( @bag, [ @resplits ] );
      }

      foreach my $elt ( @bag )
      {
        if ( $zonenum == $elt-->[0] )
        {
          shift @bag;
          @transpsurfs = @bag;
        }
      }
    }
  }

  my $zoneletter = $zonenumname{$zonenum};

  say "Processing reflections...\n";

  say MONITOR "\$launchfile $launchfile";
  say MONITOR "\$cfgfile $cfgfile";
  say MONITOR "\$zonenum $zonenum";
  say MONITOR "SURFNUMS " . dump( @transpsurfs );
  say MONITOR "CALCPROCEDURES " . dump( @calcprocedures );
  say MONITOR "\$zoneletter " . dump( $zoneletter );

  ##################################################



  my @dirresolutions;

  if ( scalar( @{ $settings } ) == 0 )
  {
    if ( -e "$modishdefpath" )
    {
      require "$modishdefpath";
      @resolutions = @{ $defaults[0] }; # ALL RESOLUTIONS
      if ( ref( $resolutions[0] ) )
      {
        @resolutions = @{ $resolutions[0] }; # DIFFUSE RESOLUTION
        @dirresolutions = @{ $resolutions[1] }; # DIRECT RESOLUTION
        push( @calcprocedures, "espdirres" );
      }
      $dirvectorsnum = $defaults[1];
      $bounceambnum = $defaults[2];
      $bouncemaxnum = $defaults[3];
      $distgrid = $defaults[4];
    }
    elsif ( -e "./modish_defaults.pl" )
    {
      @resolutions = @{ $defaults[0] }; # ALL RESOLUTIONS
      if ( ref( $resolutions[0] ) )
      {
        @resolutions = @{ $resolutions[0] }; # DIFFUSE RESOLUTION
        @dirresolutions = @{ $resolutions[1] }; # DIRECT RESOLUTION
        push( @calcprocedures, "espdirres" );
      }
      $dirvectorsnum = $defaults[1];
      $bounceambnum = $defaults[2];
      $bouncemaxnum = $defaults[3];
      $distgrid = $defaults[4];
    }
  }
  else
  {
    @resolutions = @{ $settings[0] }; # ALL RESOLUTIONS
    if ( ref( $resolutions[0] ) )
    {
      @resolutions = @{ $resolutions[0] }; # DIFFUSE RESOLUTION
      @dirresolutions = @{ $resolutions[1] }; # DIRECT RESOLUTION
      push( @calcprocedures, "espdirres" );
    }
    $dirvectorsnum = $settings[1];
    $bounceambnum = $settings[2];
    $bouncemaxnum = $settings[3];
    $distgrid = $settings[4];
  }

  if ( "report" ~~ @calcprocedures )
  {
    my $writefile = "$path/writefile.txt";
    open ( REPORT, ">>$writefile" ) or die "Can't open $writefile !";
  }

  if ( scalar( @resolutions ) == 0 ) { @resolutions = ( 2, 2 ); };
  if ( not defined( $dirvectorsnum ) ) { $dirvectorsnum = 1; };
  if ( not defined( $bounceambnum ) ) { $bounceambnum = 1; };
  if ( not defined( $bouncemaxnum ) ) { $bouncemaxnum = 7; };
  if ( not defined( $distgrid ) ) { $distgrid = 0.01; };

  if ( scalar( @calcprocedures ) == 0 ) { @calcprocedures = ( "diluted", "gensky", "alldiff", "composite" ); }
  if ( scalar( @boundbox ) == 0 ) { @boundbox = ( -10, 20, -10, 20, -2, 10 ); }
  if ( not ( @specularratios ) ) { @specularratios = ( ) }
  if ( !keys %skycondition )
  {
    %skycondition = ( 1=> "clear", 2=> "clear", 3=> "clear", 4=> "clear", 5=> "clear", 6=> "clear", 7=> "clear", 8=> "clear", 9=> "clear", 10=> "clear", 11=> "clear", 12=> "clear" );
  }

  push ( @calcprocedures, "besides", "extra", "altcalcdiff" ); # THESE SETTINGS WERE ONCE SPECIFIABLE IN THE CONFIGURATION FILE.

  my $modishlock = "$path/tmp/Z" . $zonenum . ".lock.tmp";

  if ( not ( "embedded" ~~ @calcprocedures ) )
  {
    `rm -f $modishlock`;
    say REPORT "rm -f $modishlock";
  }

# Debug output from ESP-r (out.txt in /cfg and /rad), $debug = 1 to enable.
  my $debug = 0;
  if ( $debug == 1 )
  {
    say REPORT "ESP-r debug output activated.";
    if ( -e "$path/cfg/out.txt" )
    {
      `rm $path/cfg/out.txt`;
      say REPORT "rm $path/cfg/out.txt";
    }

    `touch $path/cfg/out.txt`;
    say REPORT "touch $path/cfg/out.txt";

    if ( -e "$path/rad/out.txt" )
    {
      `rm $path/rad/out.txt`;
      say REPORT "rm $path/rad/out.txt";
    }
    `touch $path/rad/out.txt`;
    say REPORT "touch $path/rad/out.txt";
  }

  my ( $conffile, $conffile_f1, $conffile_f2, $conffile_f3, $conffile_f4, $conffile_f5, $conffile_f6, $conffile_f7,
  $conffile_f8, $conffile_f9, $conffile_f10, $conffile_f11, $constrdbfile, $constrdbfile_f,
  $matdbfile, $matdbfile_f1, $matdbfile_f2, $matdbfile_f6, $geofile_f, $geofile_f5, $flagconstrdb, $flagmatdb, $flaggeo, $flagconstr, $originalsref,
  $fictitia1ref, $fictitia2ref, $fictitia3ref, $fictitia4ref, $fictitia5ref, $fictitia6ref, $fictitia7ref,
   $fictitia8ref,  $fictitia9ref,  $fictitia10ref, $fictitia11ref, $paths_ref );

  my $radpath;

  #unless ( -e $modishlock )
  {
    ( $conffile, $conffile_f1, $conffile_f2, $conffile_f3, $conffile_f4, $conffile_f5, $conffile_f6, $conffile_f7,
    $conffile_f8, $conffile_f9, $conffile_f10, $conffile_f11, $constrdbfile, $constrdbfile_f,
    $matdbfile, $matdbfile_f1, $matdbfile_f2, $matdbfile_f6, $geofile_f, $geofile_f5, $flagconstrdb, $flagmatdb, $flaggeo, $flagconstr, $originalsref,
    $fictitia1ref, $fictitia2ref, $fictitia3ref, $fictitia4ref, $fictitia5ref, $fictitia6ref, $fictitia7ref,
     $fictitia8ref,  $fictitia9ref,  $fictitia10ref, $fictitia11ref, $paths_ref ) = createfictitiousfiles( $launchfile, $path, $zonenum,
     \@calcprocedures, $cfgfile, $modishlock );
   %paths = %{ $paths_ref };
   $radpath = $paths{radpath};
  }

  my ( $alt, $az );

  my @groups;
  if  ( ( "composite" ~~ @calcprocedures ) and not( "groundreflections" ~~ @calcprocedures ) and ( not( "alldiff" ~~ @calcprocedures ) ) )
  {
    @groups = ( 1, 2, 6, 7, 8, 11 );
  }
  elsif  ( ( "composite" ~~ @calcprocedures ) and ( not( "groundreflections" ~~ @calcprocedures ) ) )
  {
    @groups = ( 1, 2, 6, 7, 8, 11 );
  }
  elsif ( ( "composite" ~~ @calcprocedures ) and ( "groundreflections" ~~ @calcprocedures ) and not( "alldiff" ~~ @calcprocedures ) )
  {
    @groups = ( 1, 2 );
  }
  elsif ( ( "composite" ~~ @calcprocedures ) and ( "groundreflections" ~~ @calcprocedures ) )
  {
    @groups = ( 1, 2, 6 );
  }
  elsif ( ( "radical" ~~ @calcprocedures ) and not( "alldiff" ~~ @calcprocedures ) )
  {
    @groups = ( 2, 5 );
  }
  elsif ( "radical" ~~ @calcprocedures )
  {
    @groups = ( 2, 5, 6, 7 );
  }
  elsif ( "noreflections" ~~ @calcprocedures )
  {
    @groups = ( 6, 7, 8, 11 );
  }
  else
  {
    @groups = ( 1, 2 );
  }

  if ( scalar( @selectives > 0 ) )
  {
    push ( @groups , ( 3, 4) );
  }

  my @basevectors;

  unless ( -e $modishlock )
  {
    @basevectors = getbasevectors( $dirvectorsnum );
    if ( "embedded" ~~ @calcprocedures )
    {
      store \@basevectors, "$path/tmp/basevectors.store.tmp";
    }
  }
  else
  {
    if ( "embedded" ~~ @calcprocedures )
    {
      @basevectors = @{ retrieve( "$path/tmp/basevectors.store.tmp" ) };
    }
  }


  my ( @originals, @fictitia1, @fictitia2, @fictitia3, @fictitia4, @fictitia5, @fictitia6, @fictitia7,
     @fictitia8, @fictitia9, @fictitia10, @fictitia11 );

   #unless ( -e $modishlock )
   {
     @originals = @$originalsref;
     @fictitia1 = @$fictitia1ref;
     @fictitia2 = @$fictitia2ref;
     @fictitia3 = @$fictitia3ref;
     @fictitia4 = @$fictitia4ref;
     @fictitia5 = @$fictitia5ref;
     @fictitia6 = @$fictitia6ref;
     @fictitia7 = @$fictitia7ref;
     @fictitia8 = @$fictitia8ref;
     @fictitia9 = @$fictitia9ref;
     @fictitia10 = @$fictitia10ref;
     @fictitia11 = @$fictitia11ref;
   }

  my ( @daylighthours);
  my %actiondata;

  my ( @zoneoriginals, @zonefictitia1, @zonefictitia2, @zonefictitia3, @zonefictitia4, @zonefictitia5, @zonefictitia6,
       @zonefictitia7, @zonefictitia8, @zonefictitia9, @zonefictitia10, @zonefictitia11 );

  #unless ( -e $modishlock )
  {
    @zoneoriginals = @originals;
    shift(@zoneoriginals); shift(@zoneoriginals);
    @zonefictitia1 = @fictitia1;
    shift(@zonefictitia1); shift(@zonefictitia1);
    @zonefictitia2 = @fictitia2;
    shift(@zonefictitia2); shift(@zonefictitia2);
    @zonefictitia3 = @fictitia3;
    shift(@zonefictitia3); shift(@zonefictitia3);
    @zonefictitia4 = @fictitia4;
    shift(@zonefictitia4); shift(@zonefictitia4);
    @zonefictitia5 = @fictitia5;
    shift(@zonefictitia5); shift(@zonefictitia5);
    @zonefictitia6 = @fictitia6;
    shift(@zonefictitia6); shift(@zonefictitia6);
    @zonefictitia7 = @fictitia7;
    shift(@zonefictitia7); shift(@zonefictitia7);
    @zonefictitia8 = @fictitia8;
    shift(@zonefictitia8); shift(@zonefictitia8);
    @zonefictitia9 = @fictitia9;
    shift(@zonefictitia9); shift(@zonefictitia9);
    @zonefictitia10 = @fictitia10;
    shift(@zonefictitia10); shift(@zonefictitia10);
    @zonefictitia11 = @fictitia11;
    shift(@zonefictitia11); shift(@zonefictitia11);
  }

  my ( %zonefilelists, %fict1filelists, %fict2filelists, %fict3filelists, %fict4filelists, %fict5filelists, %fict6filelists, %fict7filelists,
  %fict8filelists, %fict9filelists, %fict10filelists , %fict11filelists );
  my @daylighthoursarr;
  my %daylighthours;
  my ( $exportreflref__, $exportconstrref__ );

  my $tempmod = "$launchfile.mod.temp";
  my ( $tempmoddir, $tempreportdir );

  if ( $tempmod =~ /$path\/cfg\// )
  {
    $tempmod =~ s/$path\/cfg\///;
  }
  else
  {
    $tempmod =~ s/$path\///;
  }

  $tempmod = "$path/tmp/$tempmod";
  unless ( -e "$path/tmp" )
  {
    `mkdir $path/tmp`;
    say REPORT "mkdir $path/tmp";
  }

  say REPORT "\$tempmod $tempmod";
  open ( TEMPMOD, ">>$tempmod" ) or die "$!";

  my $tempreport = "$launchfile.report.temp";

  if ( $tempreport =~ /$path\/cfg\// )
  {
    $tempreport =~ s/$path\/cfg\///;
  }
  else
  {
    $tempreport =~ s/$path\///;
  }

  $tempreport = "$path/tmp/$tempreport";

  open ( TEMPREPORT, ">>$tempreport" ) or die "$!";

  $tempmoddir = $tempmod . ".dir";
  open ( TEMPMODDIR, ">>$tempmoddir" ) or die "$!";

  $tempreportdir = $tempreport . ".dir";
  open ( TEMPREPORTDIR, ">>$tempreportdir" ) or die "$!";

  my @treatedlines;

  my $countzone = 1;
  ####foreach my $elt (@zoneoriginals)
  my $elt = $zoneoriginals[0];
  {
    my ( @zonefiles, @fict1files, @fict2files, @fict3files, @fict4files, @fict5files, @fict6files,
        @fict7files, @fict8files, @fict9files, @fict10files, @fict11files );
    my ( $geofile, $constrfile, $shdfile, $zonenum_cfg, $geofile_f, $constrfile_fict );

    #unless ( -e $modishlock )
    {
      @zonefiles = @$elt;
      @fict1files = @{ $zonefictitia1[ $countzone - 1 ] };
      @fict2files = @{ $zonefictitia2[ $countzone - 1 ] };
      @fict3files = @{ $zonefictitia3[ $countzone - 1 ] };
      @fict4files = @{ $zonefictitia4[ $countzone - 1 ] };
      @fict5files = @{ $zonefictitia5[ $countzone - 1 ] };
      @fict6files = @{ $zonefictitia6[ $countzone - 1 ] };
      @fict7files = @{ $zonefictitia7[ $countzone - 1 ] };
      @fict8files = @{ $zonefictitia8[ $countzone - 1 ] };
      @fict9files = @{ $zonefictitia9[ $countzone - 1 ] };
      @fict10files = @{ $zonefictitia10[ $countzone - 1 ] };
      @fict11files = @{ $zonefictitia11[ $countzone - 1 ] };

      $geofile = $zonefiles[0];
      $constrfile = $zonefiles[1];

      $shdfile = $zonefiles[2];

      $zonenum_cfg = $zonefiles[3];
      $geofile_f = $fict1files[0];
      $constrfile_fict = $fict1files[1];
      $zonefilelists{ $zonenum }{ geofile } = $geofile;
      $zonefilelists{ $zonenum }{ geofile_f } = $geofile_f;
      $zonefilelists{ $zonenum }{ constrfile } = $constrfile;
      $zonefilelists{ $zonenum }{ constrfile_f } = $constrfile_f;
      $zonefilelists{ $zonenum }{ shdfile } = $shdfile;
    }

    my ( $transpeltsref, $geofilestructref, $surfslistref, $obsref, $datalistref,
      $obsmaterialsref, $surfs_ref, %storethis );
    my ( %surfs, @transpelts, @geodata, %surfslist, @obsdata, %datalist, @obsmaterials );

    unless ( ( "embedded" ~~ @calcprocedures ) and ( -e $modishlock ) )
    {
      ( $transpeltsref, $geofilestructref, $surfslistref, $obsref, $datalistref,
            $obsmaterialsref, $surfs_ref, $transpsurfs_ref ) = readgeofile( $geofile, \@transpsurfs, $zonenum, \@calcprocedures, $modishlock );
    }

    if ( $transpsurfs_ref )
    {
      @transpsurfs = @{ $transpsurfs_ref };
    } #say MONITOR "NEWLY ARRIVED \@transpsurfs: @transpsurfs ";

    if ( ( "embedded" ~~ @calcprocedures ) and ( not ( -e $modishlock ) ) )
	  {
      unless ( @{ $transpeltsref } == 0 )
      {
        $storethis{transpeltsref} = $transpeltsref;
      }

      unless ( @{ $geofilestructref } == 0 )
      {
        $storethis{geofilestructref} = $geofilestructref;
      }

      unless ( ( !keys %{ $surfslistref } ) or ( !values %{ $surfslistref } ) )###
      {
        $storethis{surfslistref} = $surfslistref;
      }

      unless ( @{ $obsref } == 0 ) ###
      {
        $storethis{obsref} = $obsref;
      }

      unless ( ( !keys %{ $datalistref } ) or ( !values %{ $datalistref } ) ) ###
      {
        $storethis{datalistref} = $datalistref;
      }

      unless ( @{ $obsmaterialsref } == 0 )
      {
        $storethis{obsmaterialsref} = $obsmaterialsref;
      }

      unless ( ( !keys %{ $surfs_ref } ) or ( !values %{ $surfs_ref } ) )
      {
        $storethis{surfs_ref} = $surfs_ref;
      }

      store \%storethis, "$path/tmp/storethis.store.tmp";
    }
    elsif ( ( "embedded" ~~ @calcprocedures ) and ( -e $modishlock ) )
    {
      %storethis = %{ retrieve( "$path/tmp/storethis.store.tmp" ) };
      if ( $storethis{transpeltsref} )
      {
        $transpeltsref = $storethis{transpeltsref};
      }

      if ( $storethis{geofilestructref} )
      {
        $geofilestructref = $storethis{geofilestructref};
      }

      if ( $storethis{surfslistref} )
      {
        $surfslistref = $storethis{surfslistref};
      }

      if ( $storethis{obsref} )
      {
        $obsref = $storethis{obsref};
      }

      if ( $surfs_ref )
      {
        %surfs = %{ $surfs_ref };
      }

      if ( $datalistref = $storethis{datalistref} )
      {
        $datalistref = $storethis{datalistref};
      }

      if ( $storethis{obsmaterialsref} )
      {
        $obsmaterialsref = $storethis{obsmaterialsref};
      }

      if ( $storethis{surfs_ref} )
      {
        $surfs_ref = $storethis{surfs_ref};
      }
    }


    @transpelts = @$transpeltsref;
    @geodata = @$geofilestructref;
    %surfslist = %$surfslistref;
    @obsdata = @$obsref;
    %datalist = %$datalistref; say MONITOR "\%datalist " . dump( %datalist );
    @obsmaterials = @{ $obsmaterialsref };

    #unless ( ( "embedded" ~~ @calcprocedures ) and ( ( -e $geofile_f ) or ( -e $geofile_f5 ) ) and ( -e $modishlock ) )
	unless ( ( "embedded" ~~ @calcprocedures ) and ( -e $modishlock ) )
    {
      %paths = createfictgeofile( $geofile, $geofile_f, $geofile_f5, \%paths, \@calcprocedures, \@groups, $conffile, $conffile_f5, $modishlock );
    }

    my %obsinf = %{ $paths{obsinf} }; # $obsinf{$obsnum}{name} # $obsinf{$obsnum}{mlc} # $obsinf{$obsnum}{mlc}
    say MONITOR "OBSINF: " . dump( \%obsinf );
    my ( @obscon, @newobscon );
    foreach my $key ( sort { $a <=> $b } ( keys %obsinf ) )
    {
      push ( @obscon, $obsinf{$key}{mlc} );
      push ( @newobscon, $obsinf{$key}{newmlc} );
    }
    @obscon = uniq( @obscon );
    @newobscon = uniq( @newobscon );

    if ( not( "radical" ~~ @calcprocedures ) )
    {
      setroot( $conffile_f1, $path, $debug, \%paths, \@calcprocedures, $shdfile );
    }

    if ( not( "noreflections" ~~ @calcprocedures ) )
    {
      setroot( $conffile_f2, $path, $debug, \%paths, \@calcprocedures, $shdfile );
    }

    if ( scalar( @selectives) > 0 )
    {
      setroot( $conffile_f3, $path, $debug, \%paths, \@calcprocedures, $shdfile );
      setroot( $conffile_f4, $path, $debug, \%paths, \@calcprocedures, $shdfile );
    }

    if ( ( "radical" ~~ @calcprocedures ) and ( "alldiff" ~~ @calcprocedures ) )
    {
      setroot( $conffile_f5, $path, $debug, \%paths, \@calcprocedures, $shdfile );
    }

    if ( ( "composite" ~~ @calcprocedures ) or ( "radical" ~~ @calcprocedures ) or ( "noreflections" ~~ @calcprocedures ) )
    {
      setroot( $conffile_f6, $path, $debug, \%paths, \@calcprocedures, $shdfile );
    }

    if ( ( ( "composite" ~~ @calcprocedures ) and not( "groundreflections" ~~ @calcprocedures ) )
        or ( "radical" ~~ @calcprocedures )
        or ( "noreflections" ~~ @calcprocedures ) )
    {
      setroot( $conffile_f7, $path, $debug, \%paths, \@calcprocedures, $shdfile );
    }

    if ( ( ( "composite" ~~ @calcprocedures ) and not( "groundreflections" ~~ @calcprocedures ) )
        or ( "noreflections" ~~ @calcprocedures ) )
    {
      setroot( $conffile_f8, $path, $debug, \%paths, \@calcprocedures, $shdfile );
    }

    if ( "something_used" ~~ @calcprocedures ) # CURRENTLY UNUSED
    {
      setroot( $conffile_f9, $path, $debug, \%paths, \@calcprocedures, $shdfile );
      setroot( $conffile_f10, $path, $debug, \%paths, \@calcprocedures, $shdfile );
    }

    if ( ( ( "composite" ~~ @calcprocedures ) and not( "groundreflections" ~~ @calcprocedures ) )
        or ( "noreflections" ~~ @calcprocedures ) )
    {
      setroot( $conffile_f11, $path, $debug, \%paths, \@calcprocedures, $shdfile );
    }

    my ( $materialsref, $newmaterialsref, $matnumsref, $newmatnumsref, $exportconstrref, %storethis2 );

    #if ( not ( ( "embedded" ~~ @calcprocedures ) and ( -e $constrdbfile_f ) and ( -e $modishlock ) ) )
	if ( not ( ( "embedded" ~~ @calcprocedures ) and ( -e $modishlock ) ) )
    {
      ( $oldmaterialsref, $newmaterialsref, $exportconstrref, $conv_ref,
        $countm, $oldcountm, $newclass, $oldmatnums_ref, $newmatnums_ref ) =
        createconstrdbfile( $constrdbfile, $constrdbfile_f, \@obscon, $matdbfile, \@calcprocedures, \%paths, $modishlock );
      if ( "embedded" ~~ @calcprocedures )
      {
	    $storethis2{oldmaterialsref} = $oldmaterialsref;
        $storethis2{newmaterialsref} = $newmaterialsref;
        $storethis2{exportconstrref} = $exportconstrref;
        $storethis2{conv_ref} = $conv_ref;
        $storethis2{countm} = $countm;
        $storethis2{oldcountm} = $oldcountm;
        $storethis2{newclass} = $newclass;
	      $storethis2{oldmatnums_ref} = $oldmatnums_ref;
        $storethis2{newmatnums_ref} = $newmatnums_ref;
        store \%storethis2, "$path/tmp/storethis2.store.tmp";
      }
    }
    elsif ( ( "embedded" ~~ @calcprocedures ) and ( -e $modishlock ) )
    {
       %storethis2 = %{ retrieve( "$path/tmp/storethis2.store.tmp" ) };
       $oldmaterialsref = $storethis2{oldmaterialsref};
       $newmaterialsref = $storethis2{newmaterialsref};
       $exportconstrref = $storethis2{exportconstrref};
       $conv_ref = $storethis2{conv_ref};
       $countm = $storethis2{countm};
       $oldcountm = $storethis2{oldcountm};
       $newclass = $storethis2{newclass};
	     $oldmatnums_ref = $storethis2{oldmatnums_ref};
       $newmatnums_ref = $storethis2{newmatnums_ref};
    }

    my ( $exportreflref, $obslayers_ref, $selectives_ref, %storethis3 );
    my %obslayers;
    if ( not ( ( "embedded" ~~ @calcprocedures ) and ( -e $modishlock ) ) )
    {
      ( $exportreflref, $obslayers_ref ) = creatematdbfiles( $matdbfile,
         $matdbfile_f1, $matdbfile_f2, $matdbfile_f6, \@calcprocedures, $constrdbfile_f, \@obsdata, \@obscon,
         \@newobscon, $exportconstrref, $conv_ref, $countm, $oldcountm, $newclass, $oldmatnums_ref, $newmatnums_ref, $oldmaterialsref, $newmaterialsref );

      if ( "embedded" ~~ @calcprocedures )
      {
        if ( $exportreflref )
        {
          $storethis3{exportreflref} = $exportreflref;
        }

        if ( $obslayers_ref )
        {
          $storethis3{obslayers_ref} = $obslayers_ref;
        }

        if ( $selectives_ref )
        {
          $storethis3{selectives_ref} = $selectives_ref;
        }
        store \%storethis3, "$path/tmp/storethis3.store.tmp";
      }
    }
    elsif ( ( "embedded" ~~ @calcprocedures ) and ( -e $modishlock ) )
    {
      %storethis3 = %{ retrieve( "$path/tmp/storethis3.store.tmp" ) };
      $exportreflref = $storethis3{exportreflref};
      $obslayers_ref = $storethis3{obslayers_ref};
      $selectives_ref = $storethis3{selectives_ref};
    }

	%exportrefl = %{ $exportreflref };
    %obslayers = %{ $obslayers_ref };

    my @selectives;

    if ( not ( ( "embedded" ~~ @calcprocedures) and ( -e $modishlock ) ) )
    {
      foreach my $item ( @calcprocedures )
      {
        my @els = split( ":", $item );
        if ( $els[0] eq "light/infrared-ratio" )
        {
          my $mat = $els[1];
          my $ratio = $els[2];
          push( @selectives, [ $mat, $ratio ] );
        }
      }
      @selectives = uniq( @selectives );
      if ( "embedded" ~~ @calcprocedures )
      {
        if ( scalar( @selectives ) > 0 )
        {
          store \@selectives, "$path/tmp/selectives.store.tmp";
        }
      }
    }
    elsif ( ( "embedded" ~~ @calcprocedures) and ( -e $modishlock ) )
    {
      if ( "embedded" ~~ @calcprocedures )
      {
        if ( -e "selectives.store.tmp" )
        {
          @selectives = @{ retrieve( "$path/tmp/selectives.store.tmp" ) }
        }
      }
    }

    if ( ( scalar( @selectives ) > 0 ) and ( not ( "embedded" ~~ @calcprocedures ) ) )
    {
      solveselective( $matdbfile_f2, \@selectives, $conffile, $conffile_f2, $path );
    }
    elsif
    ( ( scalar( @selectives ) > 0 ) and ( ( "embedded" ~~ @calcprocedures ) and ( not ( -e $modishlock ) ) ) )
    {
      solveselective( $matdbfile_f2, \@selectives, $conffile, $conffile_f2, $path );
    }

    my ( $surfnumsref, $surfnamesref, @surfnums, @surfnames, $winseltsref, $datalistref, @winselts, %datalist,
       $winscoordsref, @winscoords, @dirvectorsrefs, @xyzcoords, @extremes, @gridcoords, @gridpoints_transitional,
       @gridpoints_newtransitional, @gridpoints );

    unless ( -e $modishlock )
    {
      ( $surfnumsref, $surfnamesref ) = tellsurfnames( \@transpsurfs, \@geodata );
      @surfnums = @$surfnumsref;
      @surfnames = @$surfnamesref;
      ( $winseltsref, $datalistref ) = readverts( \@transpelts, $geofile, \@geodata, \%datalist );
      @winselts = @$winseltsref;
      %datalist = %$datalistref;
      ( $winscoordsref, $datalistref ) = readcoords( \@winselts, $geofile, \@geodata, \%datalist, \@transpelts );
      @winscoords = @$winscoordsref;
      %datalist = %$datalistref;
      @dirvectorsrefs = calcdirvectors( @winscoords );
      @xyzcoords = getcorners( \@winscoords, \@winselts );
      @extremes = findextremes( @xyzcoords );
      @gridcoords = makecoordsgrid( \@extremes, \@resolutions, \@dirvectorsrefs );
      @gridpoints_transitional = makegrid( @gridcoords );
      @gridpoints_newtransitional = prunepoints( \@gridpoints_transitional, \@xyzcoords );
      @gridpoints = adjustgrid( \@gridpoints_newtransitional, $distgrid );
    }

    my ( @dirgridcoords, @dirgridpoints_transitional, @dirgridpoints_newtransitional, @dirgridpoints );

    unless ( -e $modishlock )
    {
      if ( "espdirres" ~~ @calcprocedures )
      {
        @dirgridcoords = makecoordsgrid( \@extremes, \@dirresolutions, \@dirvectorsrefs );
        @dirgridpoints_transitional = makegrid( @dirgridcoords );
        @dirgridpoints_newtransitional = prunepoints( \@dirgridpoints_transitional, \@xyzcoords );
        @dirgridpoints = adjustgrid( \@dirgridpoints_newtransitional, $distgrid );
      }
    }

    my ( $treatedlinesref, $filearrayref, $monthsref ) = readshdfile( $shdfile, \@calcprocedures, $conffile_f2, \%paths, $zonenum, \%zonenumname, $mymonth );
    @treatedlines = @$treatedlinesref;
    my @shdfilearray = @$filearrayref;
    my @months = @$monthsref; say REPORT "FOUNDMONTHS : " . dump( @months );
    say REPORT "\%zonenumname : " . dump( \%zonenumname );
    say REPORT "\$zonenum : " . dump( $zonenum );

    my @shdsurfdata = getsurfshd( \@shdfilearray, \@months, \@surfnums, \@surfnames );
    @daylighthoursarr = checklight( \@shdfilearray, \@months );
    %daylighthours = populatelight( @daylighthoursarr );
    $shdfileslist{ $zonenum } = \@treatedlines;
    my @radfilesrefs = tellradfilenames( $path, $conffile_f1, $conffile_f2, $conffile_f3, $conffile_f4, $conffile_f5,
      $conffile_f6, $conffile_f7, $conffile_f8, $conffile_f9, $conffile_f10, $conffile_f11, \%paths );


    my ( $dirdiffs_ref, $paths_ref, %storethis4 );
    if ( ( "embedded" ~~ @calcprocedures ) and ( not ( -e $modishlock ) ) )
    {
      ( $dirdiffs_ref, $paths_ref ) = getdirdiff( \%paths, \@calcprocedures );#DDD
      $storethis4{dirdiffs_ref} = $dirdiffs_ref;
      $storethis4{paths_ref} = $paths_ref;
      store \%storethis4, "$path/tmp/storethis4.store.tmp";
	  %paths = %{ $paths_ref };
	  %dirdiffs = %{ $dirdiffs_ref };
    }
    elsif ( ( "embedded" ~~ @calcprocedures ) and ( -e $modishlock ) )
    {
      %storethis4 = %{ retrieve( "$path/tmp/storethis4.store.tmp" ) };
      $dirdiffs_ref = $storethis4{dirdiffs_ref};
      $paths_ref = $storethis4{paths_ref};
	  %paths = %{ $paths_ref };
	  %dirdiffs = %{ $dirdiffs_ref };
    }

    my %storethis5;
    if ( ( "embedded" ~~ @calcprocedures ) and ( not ( -e $modishlock ) ) )
    {
      $storethis5{gridpoints_ref} = \@gridpoints;
      $storethis5{exportreflref} = $exportreflref;
      $storethis5{radfilesrefs_ref} = \@radfilesrefs;
      $storethis5{dirgridpoints_ref} = \@dirgridpoints;
      $storethis5{zonefilelists_ref} = \%zonefilelists;

      store \%storethis5, "$path/tmp/storethis5.store.tmp";

      `touch $modishlock`;
      say REPORT "touch $modishlock";
    }
    elsif ( ( "embedded" ~~ @calcprocedures ) and ( -e $modishlock  ) )
    {
      %storethis5 = %{ retrieve("$path/tmp/storethis5.store.tmp") };
      @gridpoints = @{ $storethis5{gridpoints_ref} };
      $storethis5{exportreflref} = $exportreflref;
      @radfilesrefs = @{ $storethis5{radfilesrefs_ref} };
      @dirgridpoints = @{ $storethis5{dirgridpoints_ref} };
      %zonefilelists = %{ $storethis5{zonefilelists_ref} };
    }

    my $hashirrsref = pursue( { zonenum => $zonenum, geofile => $geofile, constrfile => $constrfile,
      shdfile => $shdfile, gridpoints => \@gridpoints, shdsurfdata => \@shdsurfdata,
      daylighthoursarr => \@daylighthoursarr, daylighthours=> \%daylighthours,
      shdfilearray => \@shdfilearray, exportconstrref => $exportconstrref,
      exportreflref => $exportreflref, conffile => $conffile,  path => $path,
      radpath => $radpath, basevectors => \@basevectors, resolutions => \@resolutions,
      dirvectorsnum => $dirvectorsnum, calcprocedures => \@calcprocedures,
      specularratios => \@specularratios, bounceambnum => $bounceambnum,
      bouncemaxnum => $bouncemaxnum, radfilesrefs => \@radfilesrefs,
      conffile_f1 => $conffile_f1, conffile_f2 => $conffile_f2, conffile_f3 => $conffile_f3, conffile_f4 => $conffile_f4, conffile_f5 => $conffile_f5, conffile_f6 => $conffile_f6, conffile_f7 => $conffile_f7, conffile_f8 => $conffile_f8, conffile_f9 => $conffile_f9, conffile_f10 => $conffile_f10, conffile_f11 => $conffile_f11,
      transpsurfs=> \@transpsurfs, selectives => \@selectives, paths => \%paths, dirgridpoints => \@dirgridpoints, parproc => $parproc,
      boundbox => \@boundbox, add => $add, dirdiffs => $dirdiffs_ref, myday => $myday, mymonth => $mymonth } );

    my $irrvarsref = compareirrs( \%zonefilelists, $hashirrsref, $computype, \@calcprocedures, \@selectives );

    foreach my $elm ( @transpsurfs )
    {
      my @transpsurfs;
      push ( @transpsurfs, $elm );
      modifyshda( \@comparedirrs, \%surfslist, \%zonefilelists, \%shdfileslist, \%daylighthours, $irrvarsref, $tempmod,
        $tempreport,  $tempmoddir, $tempreportdir, $elm, "diffuse", \@calcprocedures, $hashirrsref, $conffile_f2, $shdfile, \%surfs, $mymonth );

      unless ( ( "noreflections" ~~ @calcprocedures ) or ( ( "composite" ~~ @calcprocedures ) and not ( "groundreflections" ~~ @calcprocedures ) ) )
      {
        modifyshda( \@comparedirrs, \%surfslist, \%zonefilelists, \%shdfileslist, \%daylighthours, $irrvarsref, $tempmod,
          $tempreport,  $tempmoddir, $tempreportdir, $elm, "direct", \@calcprocedures, $hashirrsref, $conffile_f2, $shdfile, \%surfs, $mymonth );
      }
    }
  }

  close TEMPMOD;

  refilter( $tempmod );

  open ( TEMPMOD, "$tempmod" ) or die;
  my @tempmodlines = <TEMPMOD>;

  print MONITOR "TEMPMODLINES:\n" . dump( @tempmodlines ); #DDD
  close TEMPMOD;

  close TEMPREPORT;

  refilter( $tempreport );

  my @tempreportlines;
  unless ( "embedded" ~~ @calcprocedures )
  {
    open ( TEMPREPORT, "$tempreport" ) or die;
    @tempreportlines = <TEMPREPORT>;
    close TEMPREPORT;
  }

  close TEMPMODDIR;

  refilter( $tempmoddir );

  open ( TEMPMODDIR, "$tempmoddir" ) or die;
  my @tempmoddirlines = <TEMPMODDIR>;
  close TEMPMODDIR;
  @tempmoddirlines = uniq( @tempmoddirlines );

  close TEMPREPORTDIR;

  refilter( $tempreportdir );

  my @tempreportdirlines;
  unless ( "embedded" ~~ @calcprocedures )
  {
    open ( TEMPREPORTDIR, "$tempreportdir" ) or die;
    @tempreportdirlines = <TEMPREPORTDIR>;
    close TEMPREPORTDIR;
    @tempreportdirlines = uniq( @tempreportdirlines );
  }

  setroot( $launchfile, $path, $debug, \%paths, \@calcprocedures, $shdfile );

  my $shdfile = $zonefilelists{ $zonenum }{ shdfile };
  my $shdafile = "$shdfile" . "a";
  my $shdafilemod = $shdafile;
  $shdafilemod =~ s/\.shda/\.mod.shda/;

  open ( SHDAMOD, ">$shdafilemod" ) or die;

  my $shdafilereport = $shdafile;
  $shdafilereport =~ s/\.shda/\.report\.shda/;

  #unless ( "embedded" ~~ @calcprocedures )
  {
    `cp -R -f $shdafile $shdafilereport`;
    `chmod 755 $shdafilereport`;
    say REPORT "cp -R -f $shdafile $shdafilereport";
    say REPORT "chmod 755 $shdafilereport";

    open ( SHDAREPORT, ">>$shdafilereport" ) or die;
    print SHDAREPORT "# FOLLOWING, THE VERIFIED VARIATIONS (AS RATIOS) OF IRRADIANCES DUE TO REFLECTIONS BY OBSTRUCTIONS.\n";

    my $counter = 0;;
    foreach my $lin ( @tempreportlines )
    {
      my $lindir = $tempreportdirlines[ $counter ];
      print SHDAREPORT $lindir;
      print SHDAREPORT $lin;
      $counter++;
    }
    close SHDAREPORT;
  }


  my @container;
  my $currentmonth ;
  my $signal ;


  foreach my $lin ( @treatedlines )
  {
    my @arr = split(/\s+|,/, $lin);  my $signal2 ;
    my $signal2 = "off";
    if ( $lin =~ /^\* month:/ )
    {
      $currentmonth = $arr[2];
      $currentmonth =~ s/`//g;
    }


    if ( $lin =~ /24 hour internal surface insolation/ )
    {
      $signal = "off";
    }

    if ( $lin =~ /24 hour external surface shading/ )
    {
      $signal = "on"; $signal2 = "on";
    }

    my $count = 0;
    my $i = 0;
    foreach my $el ( @tempmodlines )
    {
      my $eldir = $tempmoddirlines[ $i ];
      my @modarrdir = split( /\s+|,/, $eldir );
      my @modarr = split( /\s+|,/, $el );
      if ( $signal eq "on" )
      {
        push( @arr, $currentmonth );
        if ( ( $arr[25] eq $modarrdir[25] ) and ( $arr[28] eq $modarrdir[28] ) and ( $arr[29] eq $modarrdir[29] ) )
        {
          $eldir =~ s/ $currentmonth?//;
          push( @container, $eldir );
          $count++;
          last;
        }
        elsif ( ( $arr[25] eq $modarr[25] ) and ( $arr[28] eq $modarr[28] ) and ( $arr[29] eq $modarr[29] ) )
        {
          $el =~ s/ $currentmonth?//;
          push( @container, $el );
          $count++;
          last;
        }
      }
      $i++;
    }

    if ( ( ( $count == 0 ) and ( $lin ne "" ) and ( $lin ne undef ) ) or ( $signal2 eq "on" ) )
    {
      push( @container, $lin );
    }
  }

  my $signalins;

  foreach my $lin ( @container )
  {
    if ( ( $lin =~ / # diffuse - / ) or ( $lin =~ / # direct - / ) )
    {
      my  @arr = split(/\s+|,/, $lin);
      my @firstarr = @arr[ 0..11 ];
      my @secondarr = @arr[ 12..$#arr ];
      my $joinedfirst = join ( ' ' , @firstarr );
      my $joinedsecond = join ( ' ' , @secondarr );
      $lin = "$joinedfirst\n" . "$joinedsecond\n";
    }

    if ( "noins" ~~ @calcprocedures )
    {
      if ( ( $lin =~ /24 hour external surface shading/ ) or
        ( $lin =~ /\* month:/ ) or ( $lin =~ /\* end/ ) or
        ( $lin =~ /Shading and insolation data in db/ ) )
      {
        $signalins = "off";
      }
      elsif ( $lin =~ /24 hour internal surface insolation/ )
      {
        $signalins = "on";
      }
    }

    unless ( $signalins eq "on" )
    {
      print SHDAMOD $lin;
      #say REPORT "I AM GOING TO PRINT THIS IN $shdafilemod: " . "$lin, because \$signalins is $signalins." ;
    }
    print MONITOR $lin; #DDD
  }
  close SHDAMOD;

  if ( "embedded" ~~ @calcprocedures )
  {
    my $shdafileold = $shdafile . ".old";
    `cp -f $shdafile $shdafileold`;
    `cp -f $shdafilemod $shdafile`;
    `rm -f $shdfile`;

    say REPORT "cp -f $shdafile $shdafileold";
    say REPORT "cp -f $shdafilemod $shdafile";
    say REPORT "rm -f $shdfile";

    my $wait = `ish -file $cfgfile -mode script<<YYY
b
$zoneletter
m
b

-
y
YYY
`;

say REPORT "\`ish -file $cfgfile -mode script<<YYY
b
$zoneletter
m
b

-
y
YYY
\`;
";

    say REPORT $wait;
  }
  elsif ( not ( "embedded" ~~ @calcprocedures ) )
  {
    my $shdafileold = $shdafile . ".old";
    `cp -f $shdafile $shdafileold`;
    `cp -f $shdafilemod $shdafile`;

    say REPORT "cp -f $shdafile $shdafileold";
    say REPORT "cp -f $shdafilemod $shdafile";
  }

  exit;
}

open( MONITOR, ">>./monitor.txt" ) or die;

#say MONITOR "LAUNCHING: " . dump( @ARGV );

if ( @ARGV )
{
  modish( @ARGV );
}
else
{
  modish;
}

1;

__END__


=head1 NAME

Sim::OPT::Modish

=head1 SYNOPSIS

re.pl     (optional)
use Sim::OPT::Modish;
modish( "PATH_TO_THE_ESP-r_CONFIGURATION_FILE.cfg", zone_number, surface_1_number, surface_2_number, ..., surface_n_number );

Example:
modish( "/home/x/model/cfg/model.cfg", 1, 7, 9 );  (Which means: calculate for zone 1, surfaces 7 and 9.)

=head1 DESCRIPTION

modish is a program for altering the shading values calculated by the ESP-r building simulation platform to take into account reflections from obstructions.
More precisely, modish brings into account the reflective effect of solar obstructions on solar gains on building models on the basis of irradiance ratios. Those ratios are obtained combining the direct radiation on a surface and the total radiation calculated by the means of a raytracer (Radiance) on the same surface.

The effect of solar reflections is taken into account at each hour on the basis of the ratios between the irradiances measured at the models' surfaces in a model anologue of the original one, and a twin fictiotious model derived from that. The irradiances are calculated through Radiance and derive from a model in which the solar obstructions have their true reflectivity and a model in which the solar obstructions are black.

The value given by 1 minus those irradiance ratios gives the diffuse shading factors that are put in the ISH file in place of the original values.

The original ISH's ".shda" files are not substituted. Two new files are added in the "zone" folder of the ESP-r model: a ".mod.shda" file which is usable by ESP-r and features the new shading factors; a ".report.shda" file which lists the original shading factors and, at the bottom, the irradiance ratios from which the new shading factors in the ".mod.shda" file have been derived. When the radiation on a surface is increased, instead of decreased, as an effect of reflections on obstructions, the shading factor can be negative, and the irradiance ratio is greater than 1.

To this procedure another procedure can be chained to take additionally into account in the shading effect of the obstrucontions with respect to the radiation reflected from the ground.

Finally, modish is capable of calculating the shading factors from scratch.

At the moment, the documentation describing how these operations are obtained and other information is inserted at the beginning of the source code.

The settings for managing modish are specified in the "modish_defaults.pl" configuration file in the example folder and also written as a comment at the beginning of the source code. This file must be placed in the same directory from which modish is called.

To launch modish, if it is installed as a Perl module:

use Sim::OPT::Modish;

modish( "PATH_TO_THE_ESP-r_CONFIGURATION_FILE.cfg", zone_number, surface_1_number, surface_2_number, ..., surface_n_number );

Example:
modish( "/home/x/model/cfg/model.cfg", 1, 7, 9 );

If instead the file Modish.pm is used as a script, it has to be launched from the command like with:

perl ./Modish.pm PATH_TO_THE_ESP-r_CONFIGURATION_FILE.cfg zone_number surface_1_number surface_2_number surface_n_number

For example:

perl ./Modish.pm/home/x/model/cfg/model.cfg 1 7 9 (which means: calculate for zone 1, surfaces 7 and 9.)

(It should be noted that in embedded mode Modish.pm instead is called in a different manner by ESP-r: perl ./Modish.pm PATH_TO_THE_ESP-r_CONFIGURATION_FILE.cfg -day day_number month_number -zone zone_number,
and tests all the transparent surfaces of that zone.

The path of the ESP-r model configuration file has to be specified in full, like in the example above.

To be sure that the code works as a script, the header "package" etc. should be transformed into a comment.

In this case, if the surface numbers is omitted...:

perl ./Modish.pm PATH_TO_THE_ESP-r_CONFIGURATION_FILE.cfg zone_number surface_1_number surface_2_number surface_n_number

... all the transparent surfaces of that zone will be processed.

By specifying the zone explicitly, it is possible to process also opaque surfaces, if needed.

In the case that Modish is used as a command-line program, it is also possible to call specific zones and surfaces explicitly, by linking the sequences "zone_number surface_1_number ... surface_n_number with "and":

perl ./Modish.pm  PATH_TO_THE_ESP-r_CONFIGURATION_FILE.cfg 1 7 9 and 2 16 and 5 7 9

It is also possible to omit the surfaces from the lists. In that cases, all surfaces of those zones get processed:

perl ./Modish.pm PATH_TO_THE_ESP-r_CONFIGURATION_FILE.cfg 1 and 2 and 5

If no zones and no surfaces as specified, all transparent surfaces in all zones are processed. This might be slow, however:

perl ./Modish.pm PATH_TO_THE_ESP-r_CONFIGURATION_FILE.cfg

Another manner for specifying the zones and surfaces to be processed are writing them at the top of the file "modish_request.pl" in the cfg folder of the ESP-r model. That line will override the specifications from the command line.

The functionality of Modish can also be accessed from inside ESP-r. About how to do that, see the ESP-r menus ("context").

When Modish is launched from within ESP-r, it can also be used in embedded mode - that is, the calculations regarding the reflections from obstructions can be less than hourly.

In calculating the irradiance ratios, the program defaults to: 5 direction vectors; diffuse reflections: 2 ; direct reflections: 7; surface grid: 2 x 2; distance from the surface for calculating the irradiances: 0.01 (metres).

For the program to work correctly, the ESP-r model materials, construction and optics databases must be local to the model.

Included in the example folder there is there is an example of configuration file "modish_defaults.pl". Explanations are written in the comments at the beginning of the source code.



=head2 EXPORT

"modish".

=head1 AUTHOR

Gian Luca Brunetti, E<lt>gianluca.brunetti@polimi.itE<gt>. The subroutine "createconstrdbfile" has been modified by ESRU (2018), University of Strathclyde, Glasgow to adapt it to the new ESP-r construction database format.

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2008-2022 by Gian Luca Brunetti and Politecnico di Milano. This is free software. You can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, version 3.

=cut
