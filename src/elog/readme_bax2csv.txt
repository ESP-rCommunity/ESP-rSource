This application (bax2csv) takes a BIN file from a BuildAX LRS
and converts it into a csv file with each sensor on a separate
line:
 . . .
2015/03/03,14:25:03,42E3ED59,-67,1,200,20,3063,45.05,162,0,0,24032,1
2015/03/03,14:27:41,42144AC2,-104,1,82,20,3079,39.66,173,111,0,12806,1
2015/03/03,14:29:19,42555078,-59,1,50,20,3072,35.26,176,5,0,40698,1
2015/03/03,14:30:22,42E3ED59,-68,1,201,20,3064,45.09,162,0,0,28182,1
2015/03/03,14:34:44,42555078,-59,1,51,20,3072,35.26,176,3,0,48183,1
2015/03/03,14:35:40,42E3ED59,-66,1,202,20,3064,45.03,162,0,0,32349,1
 . . .
where:
2015/03/03 is the date
14:25:03 is the time (hh:mm:ss)
42E3ED59 is the 8 character ID of the BuildAX ENV sensor
-67   is the radio signal strength
1     is the packet type: 1=normal, 2=PIR event, 3=Switch event
200   packet counter
20    can be ignored
3063  is the battery level mV
45.05 is the humidity
162   it temperature in tenths of a degreee i.e. 16.2C
0     lux level
0     PIR event counter
24032 PIR energy level (needs to be post-processed)
1     Switch event conter (increments as events are recorded)

Syntax:
 ./bax2csv -h
BAX bax2csv converter (2014 Newcastle University)
Usage: bax2csv [-hsel] [-n name] [-f <topic>] [IN FILE] [OUT FILE]

Options:
	-h	Display this message
	-s	Stream data from stdin (for use with pipe)
	-e	Print encrypted and un-decodable packets
	-l	List EVN sensors and their frequency and then exit
	-n name	 extract a specific ENV sensor name and for the
	you can also FILTER for a single measurement via
	-f dbt -f rh -f lux -f pirc -f sw or -f bat

	-N name	 extract a specific ENV sensor single measurement
	as a tuple with SQL timings

Example convert all data in fetch.bax into data.csv: 
	bax2csv fetch.bax data.csv

Example convert and extract RH data in fetch.bax into datarh.csv: 
	bax2csv -f rh fetch.bax datarh.csv

Example list the sensors and their frequency found in fetch.bax: 
	bax2csv fetch.bax -l

Example filter for the sensor named 42D89A75: 
	bax2csv fetch.bax 42D89A75.csv -n 42D89A75

Example filter for relative humidity from the sensor named 42D89A75: 
	bax2csv fetch.bax 42D89A75.csv -n 42D89A75 -f rh

Example as above but with output as a tuple with SQL timings: 
	bax2csv fetch.bax 42D89A75.csv -N 42D89A75 -f rh

When no output file is specified (i.e. by drag and dropping a
file onto the executable), decoded data is written to stdout.

To compile:

Assumes the GNU tool chain in available (gcc).
All files needed are in the folder.

  make -file Makefile_bax2csv


