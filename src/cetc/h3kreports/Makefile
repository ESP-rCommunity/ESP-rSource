CC=g++
IFC=g77
CFLAGS=-c -Wall -g -O3 -pipe  -I/usr/include/libxml2
FFLAGS=-c -Wall -g -O3 -pipe -Qoption,ld,-Bdynamic
FLFLAGS= -L/usr/lib/gcc-lib/i586-mandrake-linux-gnu/3.2.2 -lstdc++ -lgcc -lxml2 -lz -lm
LDFLAGS= -lxml2 -lz -lm
COBJECTS=TReportsManager.o TBinnedData.o TVariableData.o TXMLAdapter.o TWildCards.o TReportData.o
FOBJECTS=h3kmodule.o fort.o 

EXECUTABLE=h3kReports

all : fort cpp

cpp : $(COBJECTS)
	$(CC) $(CFLAGS) main.cpp
	$(CC) -o H3KReports $(COBJECTS) main.o $(LDFLAGS)

fort : $(COBJECTS) $(FOBJECTS)
	$(IFC) -o FH3KReports $(COBJECTS) $(FOBJECTS) $(FLFLAGS)

%.o : %.cpp
	$(CC) $(CFLAGS) $<

%.o : %.f
	$(IFC) $(FFLAGS) $<

clean :
	rm -f *.o FH3KReports H3KReports
