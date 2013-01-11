#!/bin/bash
#rep01+res_sum.1+rep02
cat textfiles/1.text res_sum.1 textfiles/2.text res_sum.2 > Report.A
cat textfiles/3.text res_sum.3 textfiles/4.text res_sum.4 > Report.B
cat textfiles/5.text res_sum.5 textfiles/6.text res_sum.6 > Report.C
cat textfiles/7.text res_sum.7 textfiles/8.text res_sum.8 > Report.D
cat textfiles/9.text res_sum.9 textfiles/10.text res_sum.10 > Report.E
cat Report.A Report.B Report.C Report.D Report.E > Report.AA
rm -rf Report.?

cat textfiles/A1.text res_det.1 textfiles/A2.text res_det.2 > Report.A
cat textfiles/A3.text res_det.3 textfiles/A4.text res_det.4 > Report.B
cat textfiles/A5.text res_det.5 textfiles/A6.text res_det.6 > Report.C
cat textfiles/A7.text res_det.7 textfiles/A8.text res_det.8 > Report.D
cat textfiles/A9.text res_det.9 textfiles/A10.text res_det.10 > Report.E
cat Report.A Report.B Report.C Report.D Report.E > Report.BB

cat Report.AA Report.BB textfiles/end.text > Report.tex

rm -rf Report.?? Report.?

dtime=`date +%d%b%y`

mv Report.tex "Report"$dtime".tex"

# Generate pdf from tex input file assuming latex and dvipdf are
# available on system
latex "Report"$dtime".tex"
dvipdf "Report"$dtime".dvi"
