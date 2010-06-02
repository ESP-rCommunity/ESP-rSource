#!/bin/bash
# remove NCM specific files from models folders

rem=$1"res_98*" ; rm -f $rem
rem=$1"*_ref.cfg" ; rm -f $rem
rem=$1"*_typ.cfg" ; rm -f $rem
rem=$1"*_not.cfg" ; rm -f $rem
rem=$1"*_str.cfg" ; rm -f $rem
rem=$1"*ncm1" ; rm -f $rem
rem=$1"*txt" ; rm -f $rem
rem=$1"libb" ; rm -f $rem
rem=$1"*res" ; rm -f $rem
rem=$1"*not*cnn" ; rm -f $rem
rem=$1"fort*" ; rm -f $rem
#rem=$1"*inp" ; rm -f $rem
rem=$1"../ctl/*ctl" ; rm -f $rem
rem=$1"../nets/*tdfa" ; rm -f $rem
rem=$1"../zones/*_str.geo" ; rm -f $rem
rem=$1"../zones/*_str.con" ; rm -f $rem
rem=$1"../zones/*_str.tmc" ; rm -f $rem
rem=$1"../zones/*_not.geo" ; rm -f $rem
rem=$1"../zones/*_not.con" ; rm -f $rem
rem=$1"../zones/*_not.tmc" ; rm -f $rem
rem=$1"../zones/*_typ.geo" ; rm -f $rem
rem=$1"../zones/*_typ.con" ; rm -f $rem
rem=$1"../zones/*_typ.tmc" ; rm -f $rem
rem=$1"../zones/*_ref.geo" ; rm -f $rem
rem=$1"../zones/*_ref.con" ; rm -f $rem
rem=$1"../zones/*_ref.tmc" ; rm -f $rem
