/* 
 * Copyright (c) 2013-2014, Newcastle University, UK.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * BAX File format converter
 * 2014 Samuel Mitchell Finnigan
 * 2015 (August) command line updates by Jon Hand, ESRU to -l list sensors and frequency
 *      as well as -n XXXXXXXX to focus on a single sensor as well as for single
 *      sensors -f dbt or -f rh or -f lux or -f pirc or -f sw or -f bat
 * 2015 (September  command line updates by Jon Hand, ESRU to allow -f filter for
 *      all sensors and add a -N option for output as a tuple (sql time,data).
 * 2016 (May command line updates by Jon Hand.
 *
 * Read a .bax data file containing 32-byte packets of data gathered from
 * the sensors, and write it out as formatted CSV to file or terminal.
 *
 * Bax Data packet format (32 bytes):
 *
 *  |--- 0 ---|--- 1 ---|--- 2 ---|--- 3 ---|--- 4 ---|--- 5 ---|--- 6 ---|--- 7 ---|
 *  |-------------------------------------------------------------------------------|
 *  |               dataNumber              |               DateTime                |
 *  |           (sequence number)           |           (Packet RX time)            |
 *  |-------------------------------------------------------------------------------|
 *  | Contin- |                Address                |  RSSI   |  pkt    |  pkt    |
 *  |  uation |            (of Sensor node)           |         |  Type   |  ID     |
 *  |-------------------------------------------------------------------------------|
 *  |xMit pwr |   Battery Level   |HumiditySaturation | Temperature deg C |  Light  >
 *  |  (dBm)  |   (millivolts)    |   (MSB and LSB)   |       (x10)       |  (lux)  >
 *  |-------------------------------------------------------------------------------|
 *  >  Light  |       PIR         |        PIR        |    Switch Count   |  null   |
 *  >  (lux)  |      Counts       |       Energy      |        Stat       |         |
 *  |-------------------------------------------------------------------------------|
 *
 * CSV Columns:
 *  Date, Time, Sensor Address, RSSI, Packet Type, Packet ID, Transmit Power (dBm), Battery Level (mv),
 *	Humidity, Temperature (deg C x 10), Light (lux), PIR activation count, PIR energy, Switch activation count
 *
 */

#ifdef _WIN32
	#define _CRT_SECURE_NO_WARNINGS
    #include <windows.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "Config.h"
#include "BaxUtils.h"
#include "BaxRx.h"

/*
// Global settings struct (may be safely extended)
typedef struct {
	FILE *in;
	FILE *out;
} Settings_t;

static Settings_t settings;
*/
#define MAX_ACTIVE 70
static int ilist = 0;   /* 0= ignore, 1= invoke only to list available sensors, number found returned as negative */
static int focus = 0;   /* 0= assume no single sensor 1= focus on  a sensor 2= focus and output SQL tuple */
static int topic = 0;   /* assume all measurements of interest */
static int howmany;  /* number of sensors found */
static int ignore2 = 0;  /* 0 = print type 1 & 2, 1 = type 1 only, 2 = type 2 only */
static int sensorknown[MAX_ACTIVE]; /* zero until unique sensor found */
char envname[MAX_ACTIVE][8]; /* sensor id list */
char name[8];  /* current sensor */
char* focusname; // if focus on single sensor buffer for the command line argument.
char* topicname; // if focus on single measurement.
static int icurrentsenarray;  // which part of the array to work with
static int mapsentoarray[MAX_ACTIVE]; /* hash list */
static int mapsenhits[MAX_ACTIVE]; /* how many hits for each sensor */
// future static int incdbt;  // 1 is include dry bulb T 0 is ignore
// future static int incrh;   // 1 is include RH 0 is ignore
// future static int inclux;  // 1 is include LUX 0 is ignore
// future static int incpir;  // 1 is include PIR 0 is ignore
// future static int incsw;   // 1 is include switch 0 is ignore

// Function Prototypes
void BaxNewEntry(binUnit_t*);
void BaxCountEntry(binUnit_t*);
void PrintList();
binUnit_t* ParseFileEntryToUnit(unsigned char*, binUnit_t*);

// Declare settings and status (extern in Config.h)
Settings_t gSettings;
Status_t gStatus;


/**
 * Function:
 *		BaxDataRead(int ilist)
 *
 * Summary:
 *		Perform read file, 32b at a time.
 **/
void BaxDataRead()
{
	unsigned long read = 0;
	unsigned char data[BINARY_DATA_UNIT_SIZE];	// Read 32b
	binUnit_t unit;

	do // Read file loop
	{
		read = fread(data, 1, BINARY_DATA_UNIT_SIZE, gSettings.inputFile);

		if(read == BINARY_DATA_UNIT_SIZE)			// If we have successfully read 32 bytes
		{
			ParseFileEntryToUnit(data, &unit);		// Parse it into a binUnit_t struct

// if only requested a listing then call an alternative
			if (ilist == 0) {
			  BaxNewEntry(&unit);			//  and send it to the destination stream
			} else if (ilist == 1) {
			  BaxCountEntry(&unit);			// process for listing
			}
		}
	}
	while(read);	// while not EOF or ^C

	if (ilist == 1) {
	  PrintList();	// and list what was found
	}
}



/**
 * Function:
 *		ParseFileEntryToUnit(unsigned char*, binUnit_t*)
 * Summary:
 *		Parse out an entry from the binary data file, populating
 *		the passed-in binUnit_t and returning its pointer.
 *
 * Description:
 *	 	This function will copy members of the binUnit_t struct individually:
 *	 	The reason for this is that neither the struct packing (despite using
 *		the __packed__ attribute) nor the width of the datatypes is guaranteed
 *		to be exactly the same on any given platform.
 **/
binUnit_t* ParseFileEntryToUnit(unsigned char* received, binUnit_t* unit)
{
	// For debugging, when viewing memory directly:
	//memset(unit, 0x00, sizeof(binUnit_t));

	unit->dataNumber = (unsigned short) received[0];
	memcpy(&unit->dataTime, &received[4], sizeof(DateTime));

	unit->continuation = (unsigned char) received[8];
	memcpy(&unit->data, &received[9], BINARY_DATA_SIZE);

	return unit;
}



/**
 * Function:
 *		BaxNewEntry(binUnit_t* unit)
 *
 * Summary:
 *		Write decoded line to output. Decoded packets are written
 *		to the destination configured in the gSettings struct.
 **/
void BaxNewEntry(binUnit_t* unit)
{
	// The new decoded packets are output by this function.
	//BaxPacket_t decoded;	// Split out so we don't need to stack as much memory
	unsigned char address[4];
	unsigned char rssi;
	signed char pktType;
	char cname[8];  /* current sensor being decoded */

	unsigned short *ptr;
	BaxSensorPacket_t pkt;	// Unpack bytes into a data packet

	// Copy everything by hand because we can't rely on struct packing.
	memcpy(address, unit->data, 4);

	// If focused on a single sensor setup current cname to compare.
	if(focus == 1 || focus == 2) {
      	  strcpy(cname,"        ");  // clear current name and write into cname buffer
	  sprintf(cname,"%02X%02X%02X%02X",address[3], address[2], address[1], address[0]);
	  if(strncmp(cname, focusname, 8) == 0) {
//	    fprintf(stderr, "found sensor  %s\n", cname);
	  } else {
	    return;	// not a match jump out and try reading another
	  }
	}
	rssi    = (unsigned char) unit->data[4];
	pktType = (signed char)   unit->data[5];

	// Offset of BaxSensorPacket_t data into binUnit_t == 6.
	pkt.pktId       = (unsigned char)  unit->data[6 + 0];
	pkt.xmitPwrdBm  = (signed char)    unit->data[6 + 1];

	// We want the following values as 16 bit shorts (cast the rest of the array)
	ptr = (unsigned short *) &unit->data[8];

	// Copy the rest of the values into the BaxSensorPacket_t struct:
	pkt.battmv      = ptr[0];
	pkt.humidSat    = ptr[1];
	pkt.tempCx10    = (signed short) ptr[2];
	pkt.lightLux    = ptr[3];
	pkt.pirCounts   = ptr[4];
	pkt.pirEnergy   = ptr[5];
	pkt.swCountStat = ptr[6];

	// If user asked to ignore type2 packets return.
	if( pktType == 2 && ignore2 == 1 )
	{
//		fprintf(stderr, "\tignore type 2\n");
		return;
	}

	// Print data to file or STDOUT
	// Note: because we opened the file in non-binary write mode,
	//  LF (\n) is automatically converted to CRLF (\n) on Windows
	// Now check the packet type is decoded (i.e. we want to print it out):
	if(( pktType <= 3 && pktType > 0) || gSettings.printEncryptedPackets ) // decoded ENV packets
	{
                if( focus == 2 ) {   // user requested output as tuple
		  fprintf(gSettings.outputFile,"%s,", RtcToStringSql(unit->dataTime));
	        } else {
		  fprintf(gSettings.outputFile,"%s,", RtcToString(unit->dataTime));
                }
	        if(focus == 0 || focus == 1) {  // exclude sensor name if command line included -N
		  fprintf(gSettings.outputFile,"%02X%02X%02X%02X",
				address[3], address[2], address[1], address[0]);
                }
                if( topic == 0 ) {
		  fprintf(gSettings.outputFile,",%d,%d,", (signed short)RssiTodBm(rssi), (signed short)pktType);
		}

		// If implementing decoding packets using a BAX_INFO.BIN file, they
		// would be decoded here.

		// Print out encrypted packets (if this case was hit with gSettings.printEncryptedPackets)
		if( pktType > RAW_PKT_LIMIT || pktType <= 0 || pktType == BAX_NAME_PKT )
		{
			fprintf(gSettings.outputFile, "\tencrypted packet\n");
			return;
		}

		// else print decoded packet
                if( topic == 0 )
                {
		  fprintf(gSettings.outputFile,"%u,%u,%u,%u.%02u,",
						pkt.pktId, 			// pktId
						pkt.xmitPwrdBm, 	// txPwr dbm
						pkt.battmv, 		// battmv
						pkt.humidSat >> 8, 	// humidSat MSB
						(((signed short)39*(pkt.humidSat & 0xff))/100));// humidSat LSB
		  fprintf(gSettings.outputFile,"%d,%u,%u,%u,%u\n",
						pkt.tempCx10,		// tempCx10
						pkt.lightLux, 		// lightLux
						pkt.pirCounts,		// pirCounts
						pkt.pirEnergy,		// pirEnergy
						pkt.swCountStat); 	// swCountStat
		} else if ( topic == 1) {
	      if(focus == 0 || focus == 1) {  // exclude comman if command line included -N
		    fprintf(gSettings.outputFile,",%d\n",pkt.tempCx10);// tempCx10
	      } else {
		    fprintf(gSettings.outputFile,"%d\n",pkt.tempCx10);// tempCx10
		  }
		} else if ( topic == 2) {
	      if(focus == 0 || focus == 1) {  // exclude comman if command line included -N
		    fprintf(gSettings.outputFile,",%u.%02u\n",
						pkt.humidSat >> 8, 	// humidSat MSB
						(((signed short)39*(pkt.humidSat & 0xff))/100));// humidSat LSB
	      } else {
		    fprintf(gSettings.outputFile,"%u.%02u\n",
						pkt.humidSat >> 8, 	// humidSat MSB
						(((signed short)39*(pkt.humidSat & 0xff))/100));// humidSat LSB
		  }
		} else if ( topic == 3) {
	      if(focus == 0 || focus == 1) {  // exclude comman if command line included -N
		    fprintf(gSettings.outputFile,",%u\n",pkt.lightLux);// lightLux
	      } else {
		    fprintf(gSettings.outputFile,"%u\n",pkt.lightLux);// lightLux
		  }
		} else if ( topic == 4) {
	      if(focus == 0 || focus == 1) {  // exclude comman if command line included -N
		    fprintf(gSettings.outputFile,",%u\n",pkt.pirCounts);// pirCounts
	      } else {
		    fprintf(gSettings.outputFile,"%u\n",pkt.pirCounts);// pirCounts
		  }
		} else if ( topic == 5) {
	      if(focus == 0 || focus == 1) {  // exclude comman if command line included -N
		    fprintf(gSettings.outputFile,",%u\n",pkt.swCountStat);// swCountStat
	      } else {
		    fprintf(gSettings.outputFile,"%u\n",pkt.swCountStat);// swCountStat
		  }
		} else if ( topic == 6) {
		  if(focus == 0 || focus == 1) {  // exclude comman if command line included -N
		  	fprintf(gSettings.outputFile,",%u\n",pkt.battmv);// battmv
		  } else {
				fprintf(gSettings.outputFile,"%u\n",pkt.battmv);// battmv
		  }
		}
	}
}


/**
 * Function:
 *		BaxCountEntry(binUnit_t* unit)
 *
 * Summary:
 *		Gather statistics for list function.
 **/
void BaxCountEntry(binUnit_t* unit)
{
	// The new decoded packets are output by this function.
	//BaxPacket_t decoded;	// Split out so we don't need to stack as much memory
	unsigned char address[4];
//	unsigned char rssi;
	signed char pktType;

	unsigned short *ptr;
        int loop = 0;
        int foundit = 0;  // signal not yet found
	int theloop = 0;  // which one it matches

	BaxSensorPacket_t pkt;	// Unpack bytes into a data packet

	// Copy everything by hand because we can't rely on struct packing.
	memcpy(address, unit->data, 4);

//	rssi    = (unsigned char) unit->data[4];
	pktType = (signed char)   unit->data[5];

	// Offset of BaxSensorPacket_t data into binUnit_t == 6.
	pkt.pktId       = (unsigned char)  unit->data[6 + 0];
	pkt.xmitPwrdBm  = (signed char)    unit->data[6 + 1];

	// We want the following values as 16 bit shorts (cast the rest of the array)
	ptr = (unsigned short *) &unit->data[8];

	// Copy the rest of the values into the BaxSensorPacket_t struct:
	pkt.battmv      = ptr[0];
	pkt.humidSat    = ptr[1];
	pkt.tempCx10    = (signed short) ptr[2];
	pkt.lightLux    = ptr[3];
	pkt.pirCounts   = ptr[4];
	pkt.pirEnergy   = ptr[5];
	pkt.swCountStat = ptr[6];

	// Now check the packet type is decoded (i.e. we want to gather info):
	if( pktType <= 3 && pktType > 0 ) // decoded ENV packets
	{
      		strcpy(name,"        ");  // clear current name and write into name buffer
		sprintf(name,"%02X%02X%02X%02X",address[3], address[2], address[1], address[0]);
// debug		fprintf(stderr, "checking sensor  %s\n", name);
                if (howmany == -1) {
		  howmany++;     // nothing known yet so increment and remember
		  strncpy(envname[howmany], name, 8);
		  envname[howmany][8] = '\0';	/* write terminator  */
		  mapsentoarray[howmany]=howmany;
		  mapsenhits[howmany]=1;
// debug		  fprintf(stderr, "added  %s %d %s\n", name,howmany,envname[howmany]);
		} else if (howmany == 0){
                  foundit = 0;	// zero slot filled increment and remember
		  loop = 0;
		  if(strncmp(envname[loop], name, 8) == 0) {
		    mapsenhits[loop]=mapsenhits[loop]+1;
		  } else {
		    howmany++;     // increment
		    strncpy(envname[howmany], name, 8);
		    envname[howmany][8] = '\0';	/* write terminator  */
		    mapsentoarray[howmany]=howmany;
		    mapsenhits[howmany]=1;
// debug		    fprintf(stderr, "added  %s %d %s\n", name,howmany,envname[howmany]);
		  }
		} else if (howmany >= 1 && howmany < MAX_ACTIVE ){
                    foundit = 0;	// for the remaining possible sensors up to MAX_ACTIVE
		    theloop = 0;
		    for (loop=0; loop<howmany; loop++) {
		      if(strncmp(envname[loop], name, 8) == 0) {
			foundit = 1;
		        theloop = loop;  // matches a known sensor
		      }
		    }
		    if ( foundit == 0) {
		      howmany++;     // not found so increment
		      strncpy(envname[howmany], name, 8);
		      envname[howmany][8] = '\0';	/* write terminator  */
		      mapsentoarray[howmany]=loop;
		      mapsenhits[howmany]=1;
// debug		      fprintf(stderr, "added  %s %d %s\n", name,howmany,envname[howmany]);
		    } else {
		      mapsenhits[theloop]=mapsenhits[theloop]+1;
		    }
		}
	}
}



/**
 * Function:
 *		PrintHelp(void)
 *
 * Summary:
 *		Print the help message (-h or bad option)
 **/
void PrintHelp(void)
{
	fprintf(stderr, "BAX bin2csv converter (2014 Newcastle University)\n"
		"Usage: bin2csv [-hseil] [-n name] [-f <topic>] [IN FILE] [OUT FILE]\n"
		"\n"
		"Options:\n"
		"\t-h\tDisplay this message\n"
		"\t-s\tStream data from stdin (for use with pipe)\n"
		"\t-e\tPrint encrypted and un-decodable packets\n"
		"\t-i\tIgnore PIR (type 2) entries\n"
		"\t-l\tList EVN sensors and their frequency and then exit\n"
		"\t-n name\t extract a specific ENV sensor name and for the\n"
		"\tyou can also FILTER for a single measurement via\n"
		"\t-f dbt -f rh -f lux -f pirc -f sw or -f bat\n"
		"\n"
		"\t-N name\t extract a specific ENV sensor single measurement\n"
		"\tas a tuple with SQL timings\n"
		"\n"
		"Example convert all data in fetch.bax into data.csv: \n"
		"\tbax2csv fetch.bax data.csv\n"
		"\n"
		"Example convert and extract RH data in fetch.bax into datarh.csv: \n"
		"\tbax2csv -f rh fetch.bax datarh.csv\n"
		"\n"
		"Example list the sensors and their frequency found in fetch.bax: \n"
		"\tbax2csv fetch.bax -l\n"
		"\n"
		"Example filter for the sensor named 42D89A75: \n"
		"\tbax2csv fetch.bax 42D89A75.csv -n 42D89A75\n"
		"\n"
		"Example filter for relative humidity from the sensor named 42D89A75: \n"
		"\tbax2csv fetch.bax 42D89A75.csv -n 42D89A75 -f rh\n"
		"\n"
		"Example as above but with output as a tuple with SQL timings: \n"
		"\tbax2csv fetch.bax 42D89A75.csv -N 42D89A75 -f rh\n"
		"\n"
		"When no output file is specified (i.e. by drag and dropping a\n"
		"file onto the executable), decoded data is written to stdout.\n" );
}

/**
 * Function:
 *		PrintList(void)
 *
 * Summary:
 *		Print the list of found sensors
 *		Make use of substring name as buffer for the loopth portion of the
 *		envname array of strings.
 **/
void PrintList(void)
{
  int loop;
  strcpy(name,"        ");  // clear current name
// debug  fprintf(stderr, "  %s \n",envname);
  fprintf(stderr, " Sensors found %d\n", howmany);
  fprintf(stderr, " Name   Number of hits\n" );
  for (loop=0; loop<howmany; loop++) {
    strncpy(name, envname+loop, 8);	// copy relevant 8 char into name
    fprintf(stderr, " %s %d\n",name,mapsenhits[loop]);
  }
}



/**
 * Function:
 *		main(int argc, char *argv[])
 *
 * Summary:
 *		Parse arguments and open files for reading/writing
 *
 * Command line args:
 *		[-hs] [somepath.bax] [outfile.csv]
 **/
int main(int argc, char *argv[])
{
	unsigned char i;
        int j;  // for looping
	gSettings.inputFile = NULL;
	gSettings.outputFile = NULL;

	for (i=1; argc > i; i++)	// Args to process? (Arg 0 is the executable, skip it)
	{
		if(argv[i][0] == '-')	// Parse dash args (eg: -so for streaming from stdin to stdout, etc). Interesting bit is after the dash
		{
			unsigned char offset = 1;
			unsigned char len = strlen(argv[i]);

			while(offset < len)	// Iterate over this arg
			{
				if(argv[i][offset] == 'h')		// Help flag -h
				{
					PrintHelp();
					return 0;
				}

				// To parse more -abc arguments in future, add them here.
				//   When a valid arg is detected, increment offset++ and put a
				//   continue statement to jump back up to the while() condition.
				//   This ensures all args will be parsed regardless of order.
				if(argv[i][offset] == 'e')		// Print encrypted packets
				{
					gSettings.printEncryptedPackets = TRUE;
					offset++;
					continue;
				}

				if(argv[i][offset] == 's')		// Stream from stdin
				{
					gSettings.inputFile = stdin;
					offset++;
					continue;
				}
				if(argv[i][offset] == 'l')		// List sensors and exit
				{
					ilist = 1; 			// Set ilist for use later
					offset++;
					fprintf(stderr, "Listing of ENV sensors follows:\n");

					// initial clear of sensor list
					howmany=-1;
					icurrentsenarray=0;
      					strcpy(name,"       ");  // clear current name
					for ( j = 0; j < MAX_ACTIVE-1; j++ ) {
      					  strcpy(envname[j],"       ");  // clear name
					  mapsentoarray[j]=-1;
					  mapsenhits[j]=0;
					  sensorknown[j]=0;
    					}
					continue;
				}
				if(argv[i][offset] == 'n')		// named sensor
				{
					offset++;
					focusname = argv[++i];
					focus = 1;	// signal output to focus on name
					fprintf(stderr, "Focus on the sensor: %s\n",focusname);
					continue;
				}
				if(argv[i][offset] == 'N')		// named sensor with output as tuple
				{
					offset++;
					focusname = argv[++i];
					focus = 2;	// signal output to focus on name with SQL timing format
					fprintf(stderr, "Focus on the sensor with tuple output: %s\n",focusname);
					continue;
				}
				if(argv[i][offset] == 'i')		// ignore type 2 PIR entries
				{
					offset++;
					ignore2 = 1;	// signal print type 1 only
					fprintf(stderr, "Ignore type 2 output\n");
					continue;
				}
				if(argv[i][offset] == 'f')		// focus on
				{
					offset++;
					topicname = argv[++i];
                                        if(strncmp(topicname, "dbt", 3) == 0) {
                                          topic = 1;
                                        } else if(strncmp(topicname, "rh", 2) == 0) {
                                          topic = 2;
                                        } else if(strncmp(topicname, "lux", 3) == 0) {
                                          topic = 3;
                                        } else if(strncmp(topicname, "pirc", 4) == 0) {
                                          topic = 4;
                                        } else if(strncmp(topicname, "sw", 2) == 0) {
                                          topic = 5;
                                        } else if(strncmp(topicname, "bat", 3) == 0) {
                                          topic = 6;
                                        }
					fprintf(stderr, "Filter on topic: %s %d\n",topicname,topic);
					continue;
				}

				// else
				fprintf(stderr, "Unknown option in %s\n", argv[i]);
				PrintHelp();
				return 1;
			}
		}
		else	// File name provided directly?
		{
			if(gSettings.inputFile == NULL)					// First hit, interpret as input file (unless seen -s already)
			{
				gSettings.inputFile = fopen(argv[i], "rb");	// Read binary mode data (will fail out early with just r)
				if(!gSettings.inputFile)
				{
					fprintf(stderr, "Could not open file %s for reading!\n", argv[i]);
					return 1;
				}
			}
			else if(gSettings.outputFile == NULL)
			{
				gSettings.outputFile = fopen(argv[i], "w");	// Next arg, output file
				if(!gSettings.outputFile)
				{
					fprintf(stderr, "Could not open file %s for writing!\n", argv[i]);
					return 1;
				}
			}
		}
	}

	// Check args parsed OK:
	if(argc == 1 || gSettings.inputFile == NULL)
	{
		fprintf(stderr, "Did not receive a valid argument set. Exiting...\n\n");
		PrintHelp();
		return 1;
	}

	// Default is to write to stdout unless otherwise specified above
	if(gSettings.outputFile == NULL)
	{
		gSettings.outputFile = stdout;
	}
	
	// OK, we have valid args now so go ahead and process data or just list contents:
	BaxDataRead();

	// Close files (if they aren't std streams)
	if(gSettings.inputFile != stdin)		fclose(gSettings.inputFile);
	if(gSettings.outputFile != stdout)		fclose(gSettings.outputFile);

	// Done. Exit.
	return 0;
}

//EOF
