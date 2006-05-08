#!/bin/csh -fb

echo "********************************"
if ($1 == "-h")then
  echo "cd to results directory and run this script giving "
  echo "path to archive results as argument "
  echo "The shell script TECdiff.csh and the awk file checkout.awk"
  echo "should be in the same folder as this script "
  echo "i.e. the results directory"
else
set STDdir = $1
set WORdir = `pwd`

echo "Which tests did you run?"
echo " (1) Fast (23 simulations; run time: ~15 minutes & <2hrs on a slow computer)"
echo " (2) Slow (3 simulations; run time: 12x time for fast tests)"
echo " (3) All (26 simulations)"
@ A=0
while ( $A<1 || $A>3 )
set A = $<
end

echo "checking $WORdir against $1 ..."
echo "results will be placed in CFD_res.text"

if ( $A == 1 || $A == 3 ) then
  echo "working on basiclma.TEC"
  ./TECdiff.csh basiclma.TEC        $1/basiclma.TEC        >  CFD_res.text
  echo "working on blockh.TEC"
  ./TECdiff.csh blockh.TEC          $1/blockh.TEC          >> CFD_res.text
  echo "working on displ.TEC"
  ./TECdiff.csh displ.TEC           $1/displ.TEC           >> CFD_res.text
  echo "working on sso.TEC"
  ./TECdiff.csh sso.TEC             $1/sso.TEC             >> CFD_res.text
  echo "working on tm1.TEC"
  ./TECdiff.csh tm1.TEC             $1/tm1.TEC             >> CFD_res.text
  echo "working on basicsol_bi-cg.TEC"
  ./TECdiff.csh basicsol_bi-cg.TEC  $1/basicsol_bi-cg.TEC  >> CFD_res.text
  echo "working on block2.TEC"
  ./TECdiff.csh block2.TEC          $1/block2.TEC          >> CFD_res.text
  echo "working on blocklma.TEC"
  ./TECdiff.csh blocklma.TEC        $1/blocklma.TEC        >> CFD_res.text
  echo "working on co2_so.TEC"
  ./TECdiff.csh co2_so.TEC          $1/co2_so.TEC          >> CFD_res.text
  echo "working on hum_so.TEC"
  ./TECdiff.csh hum_so.TEC          $1/hum_so.TEC          >> CFD_res.text
  echo "working on tm1a.TEC"
  ./TECdiff.csh tm1a.TEC            $1/tm1a.TEC            >> CFD_res.text
  echo "working on tm2_so.TEC"
  ./TECdiff.csh tm2_so.TEC          $1/tm2_so.TEC          >> CFD_res.text
  echo "working on basic.TEC"
  ./TECdiff.csh basic.TEC           $1/basic.TEC           >> CFD_res.text
  echo "working on block3.TEC"
  ./TECdiff.csh block3.TEC          $1/block3.TEC          >> CFD_res.text
  echo "working on block.TEC"
  ./TECdiff.csh block.TEC           $1/block.TEC           >> CFD_res.text
  echo "working on displ_ggdh.TEC"
  ./TECdiff.csh displ_ggdh.TEC      $1/displ_ggdh.TEC      >> CFD_res.text
  echo "working on lma.TEC"
  ./TECdiff.csh lma.TEC             $1/lma.TEC             >> CFD_res.text
  echo "working on tm1b.TEC"
  ./TECdiff.csh tm1b.TEC            $1/tm1b.TEC            >> CFD_res.text
  echo "working on tst33vol.TEC"
  ./TECdiff.csh tst33vol.TEC        $1/tst33vol.TEC        >> CFD_res.text
  echo "working on blockc.TEC"
  ./TECdiff.csh blockc.TEC          $1/blockc.TEC          >> CFD_res.text
  echo "working on displ_si.TEC"
  ./TECdiff.csh displ_si.TEC        $1/displ_si.TEC        >> CFD_res.text
  echo "working on porous.TEC"
  ./TECdiff.csh porous.TEC          $1/porous.TEC          >> CFD_res.text
  echo "working on tm1_so.TEC"
  ./TECdiff.csh tm1_so.TEC          $1/tm1_so.TEC          >> CFD_res.text
endif
if ( $A == 2 || $A == 3 ) then
  echo "working on blockt.TEC"
  ./TECdiff.csh blockt.TEC          $1/blockt.TEC          >> CFD_res.text
  echo "working on bi-cg.TEC"
  ./TECdiff.csh bi-cg.TEC           $1/bi-cg.TEC           >> CFD_res.text
  echo "working on bi-cg_bi-cg.TEC"
  ./TECdiff.csh bi-cg_bi-cg.TEC     $1/bi-cg_bi-cg.TEC     >> CFD_res.text
endif
echo "Finished checking TEC files working on differences ..."
echo "Writing summary of differences to file CFD_summary.text"
awk -f ./check_out.awk CFD_res.text > CFD_summary.text
endif
echo " "
