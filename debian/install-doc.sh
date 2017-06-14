#!/bin/bash
DEBdir=$SRCdir/debian/espr/usr/share/doc/espr
cd $SRCdir/doc/manual/Manual
#create header of main page index.html
echo "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
\"http://www.w3.org/TR/html4/loose.dtd\">
<html>
<head>
<meta http-equiv=\"Content-Type\" content=\"text/html; charset=US-ASCII\">
<meta name=\"Content-Style\" content=\"text/css\">
<style type=\"text/css\">
       p       { margin-top: 0; margin-bottom: 0; vertical-align: top }
       pre     { margin-top: 0; margin-bottom: 0; vertical-align: top }
       table   { margin-top: 0; margin-bottom: 0; vertical-align: top }
       h1      { text-align: center }
</style>
<title>Esp-r User Guide</title>
</head>
<body>
<p align=\"center\" style=\"margin-top: 1em\"><b><big><big><big><big><big>Esp-r User Guide</big></big></big></big></big></b></p><hr>
<p>" >../index.html
#check if FIGS/ exists, Figs/ must be renamed FIGS/ to run with the actual groff documents
if [ -d Figs ]; then
    mv Figs/ FIGS/ 
fi
#clear FIGS if this script 's run more than one time
rm FIGS/*.png
#add informations from cover page
unroff cover_um >>../index.html
echo "</p><br>" >>../index.html



#Create the sections pages, add link in index
for f in preamble s1 s2 s3 s3- s4 s5;
do
  echo "Convert $f to html" 
  #add title of section in main page
  case "$f" in
    preamble) echo "<h2>Preamble</h2>">>../index.html
    ;;
    s1) echo "<h2>Section One: General description of ESP-r </h2>">>../index.html
    ;;
    s2) echo "<h2>Section Two: A guide to effective system use </h2>">>../index.html
    ;;
    s3) echo "<h2>Section Three: Exercises </h2>">>../index.html
    ;;
    s3-) echo "<h2>Section Three +: Exercises </h2>">>../index.html
    ;; 
    s4) echo "<h2>Section Four: Example applications </h2>">>../index.html
    ;;  
    s5) echo "<h2>Section Five: Theoretical basis and validity </h2>">>../index.html
    ;;                                    
    *) echo "<h2>$f</h2>">>../index.html
    ;;
  esac

  #remove contents chapter before send to grofhtml command
  sed '/^Contents/,/.SH/d' $f | groff -Thtml -P -s12 -P -DFIGS -te -ms > $f.html
  #upload links and add them in main page
  grep '<a href=\"#' $f.html | sed  "s|#|Manual/$f.html#|g">>../index.html
  #add link to top level page in each section page    
  sed -i '/<hr>/i<a href=\"../index.html\">Top</a>' $f.html
done
#add link navigation in each section page    
sed -i '/<hr>/i<a href=\"../index.html\"> Prec</a> <a href=\"s1.html\"> Next</a>' preamble.html
sed -i '/<hr>/i<a href=\"preamble.html\"> Prec</a> <a href=\"s2.html\"> Next</a>' s1.html
sed -i '/<hr>/i<a href=\"s1.html\"> Prec</a> <a href=\"s3.html\"> Next</a>' s2.html
sed -i '/<hr>/i<a href=\"s2.html\"> Prec</a> <a href=\"s3-.html\"> Next</a>' s3.html
sed -i '/<hr>/i<a href=\"s3.html\"> Prec</a> <a href=\"s4.html\"> Next</a>' s3-.html
sed -i '/<hr>/i<a href=\"s3-.html\"> Prec</a> <a href=\"s5.html\"> Next</a>' s4.html
sed -i '/<hr>/i<a href=\"s4.html\"> Prec</a>' s5.html
#Appendices
echo "<h2>Appendices</h2>">>../index.html
echo "<a href=\"Data_model/ESP-r_data_doc.pdf\">Data Model Summary</a><br>">>../index.html
echo "<a href=\"Data_model/ESP-r_data_decomp.pdf\">ESP-r Data Model Decomposition</a><br>">>../index.html
echo "<a href=\"Data_model/ESP-r_geo_description.pdf\">Documentation of ESP-r Geometry File</a><br>">>../index.html
echo "<a href=\"Data_model/ESP-r_meta_file_description.pdf\">Documentation of ESP-r META File</a><br>">>../index.html
#Add publications site
echo "<a href=\"http://www.esru.strath.ac.uk/publications.htm#manuals\">Publications</a><br>">>../index.html

#create footer of main page 
echo "<hr> Section page are created with $(groff -v | grep 'groff version')
</body>
</html>">>../index.html
#copy files in install path and clean
cd $SRCdir/doc/manual
mv index.html $DEBdir/
mv Manual/*.html $DEBdir/Manual/
mkdir $DEBdir/Manual/FIGS
mv Manual/FIGS/*.png $DEBdir/Manual/FIGS/
cp Data_model/*.pdf $DEBdir/Data_model/
#NB:need help about the good guess options for groff command: see grog command
