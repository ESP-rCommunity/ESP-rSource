#include <iostream>
#include <string>
#include <cstdlib>
#include "TReportsManager.h"

using namespace std;

void test1()
{

	TReportsManager *reportsManager = TReportsManager::Instance();
	
	for(int iHour= 0; iHour< 1  ; iHour++)
	{
	  reportsManager->SetMeta("/building/hour", "units", "time");
	  reportsManager->Report("/building/hour", iHour);
          int day = iHour/24;
          bool start = true;
	  reportsManager->Update( iHour, iHour, day, start );
	}
	std::cout << "output done";
	reportsManager->OutputXMLSummary("here.xml");
	
}


void test2()


{
 TXMLAdapter doc;
 doc.AddNode(NULL, "system", "");
 doc.AddAttribute(doc.RootNode(), "version", "1.0");
TXMLNode currentNode = doc.RootNode();
 currentNode = doc.AddNode(currentNode,"child1","");
 currentNode = doc.AddNode(currentNode,"child2","2");
 doc.WriteToFile("try.xml");
}
int main (int argc, char * const argv[]) {


	
	test2();
	return 0;
}
