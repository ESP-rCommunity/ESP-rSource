#ifndef TREPORTSMANAGER_H
#define TREPORTSMANAGER_H

#ifdef _WIN32
#pragma warning (disable: 4786) //to disable annoying "identifier was truncated to '255' characters in the debug information" in VC++
#endif

#include <string>
#include <vector>
#include <map>
#include <set>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <algorithm>
#include <cstdio>
#include <cstring>

#include "sys/stat.h"

#include "TWildCards.h"
#include "TXMLAdapter.h"
#include "TReportData.h"
#include "log.h"

//Optional SQLite support
#ifdef SQLITE
   #include "DBManager.h"
#else
   #include "DBManager_stub.cpp"
#endif

//ReportVariable.OutputType bitwise format
#define OUT_ALL     0x01  // 0000 0001
#define OUT_SUMMARY 0x02  // 0000 0010
#define OUT_LOG     0x04  // 0000 0100
#define OUT_STEP    0x08  // 0000 1000

//Used for the save_to_disk option to manage memory footprint
#define SAVE_TO_DISK_MIN 100
#define SAVE_TO_DISK_MAX 100000
#define SAVE_TO_DISK_DEFAULT 1000
#define SAVE_TO_DISK_TRIGGER 100000 //This should cap around 750MB max



using namespace std;

//Use as the key for the ReportDataMap object
struct stMapKey{
   int identifier;
   string delimiters;

   stMapKey(const int n, const string vn)
         : identifier(n), delimiters(vn){}

   //Used by the map object for sorting
   //if the int identifiers are the same check the delimiters
   bool operator<(const stMapKey & v) const{
      if (identifier < v.identifier)
         return true;
      else if (identifier == v.identifier)
         if (delimiters < v.delimiters)
            return true;
         else
            return false;
      else
         return false;
   }
};

//Used by the m_VariableInfoMap to store details about
//each variable sent from Fortran
struct stVariableInfo{
   const char* VarName;
   const char* MetaType;
   const char* MetaValue;
   const char* Description;
   
};

//Used by the TimeStepVecto to store details about each timestep
struct stTimeStep{
   int Step;
   int Hour;
   int Day;
   bool Startup;
};

//Structure only used to output data in sorted order
struct stSortedMapKeyRef{
   const char* cString;
   const stMapKey* mapKey;
};

//Used by the qsort prior to generating the output
int cmp_by_string(const void *a, const void *b)
{
    struct stSortedMapKeyRef *ia = (struct stSortedMapKeyRef *)a;
    struct stSortedMapKeyRef *ib = (struct stSortedMapKeyRef *)b;
    return strcmp(ia->cString, ib->cString);
}

typedef map<int,stVariableInfo> VariableInfoMap;
typedef map<stMapKey,TReportData> ReportDataMap;
typedef vector<stTimeStep> TimeStepVector;


//const int kMonthlyTimesteps[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
const int kMonthlyTimesteps[] = {31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365};



/**
 * class TReportsManager
 * This is the main reports class for h3k. It is to implemented as a singleton. This
 * is for ease of use in c and fortran environments.
 */

class TReportsManager
{
   public:
      // Return a pointer to the singleton.
      static TReportsManager* Instance();

      //Method that Returns the requested output from the input.xml
      unsigned char GetOutputType(const char* search_text);

      //Method that add a variable information to the list
      void AddToVariableInfoList(int id, const char* sVarName, const char* sMetaType,
                                 const char* sMetaValue, const char* sDescription);

      //Method to add the time step information to the list.
      void AddToTimeStepList(bool bStartup, int iStep, int iDay, int iHour);

      //Method to add variable data to the list
      void AddToReportDataList(int id, const char* sDelimiter, float fValue);

      //Method to add dynamic variable description ** avoid its use when possible **
      void AddToReportDetails(int id,const char*, const string& sUnit,const string& sType,const string& sDescription);

      //True/false if the report variable is enabled
      bool IsVariableEnable(int id);
      bool IsVariableEnable(const char* cPattern);//match by pattern (contains)

      //True/false if the report variable is set with a wild description (normally false)
      bool IsReportDetailWildSet(int id, const char* sDelimiter);


      //Setter method for the simulation information
      void SetSimulationInfo(int iStartDay,int iEndDay,int iTimeStep);

      //Public method, trigger to generate all the reports
      void GenerateOutput();

      //Public method to set report parameters
      void SetReportParameter(const std::string& param, const std::string& value);

     //void AddUserDefinedTimeRange(TTimeDataRange range);

      /**
      *  Parse an XML input file
      */
      void ParseConfigFile( const std::string& filePath );


      /**
      * Return the current status of h3k reports output (enabled/disabled)
      */
      bool ReportsEnabled();

      /**
      * Change the status of h3kreports output (enabled/disabled)
      */
      void EnableReports(bool& ReportsStatus);


      /**
       * Return the value of a requested configuration parameter
       *
       */
      string ReportConfig(std::string cParam);


      /**
       * Report if a parameter value is in a vector
       */

      bool ReportList (std::string cType, std::string cValue);

      /**
       * Update the value of a parameter
       */
       void UpdateConfig (std::string cParam,
                          std::string cValue );

      /**
      * Toggle an on-off configuration parameter
      */
      bool ToggleConfig(std::string cParam);

      /**
      * Update configuration file with new options
      */
      void UpdateConfigFile();


      // Clean-up files
      void Cleanup();

   private:
      //contains all variables found in h3kmodule.f90
      VariableInfoMap m_VariableInfoList;

      //contains all the TimeStep data send from  h3kmodule.f90::ReportNextTimeStep()
      TimeStepVector m_TimeStepList;

      //Contains all the data collected during a simulation
      ReportDataMap m_ReportDataList;



      //The constructor / destructure is private because we're a Singleton
      TReportsManager();
      ~TReportsManager();


      void GetVariableName(const char* sVarName, const char* sDelimiter, char *Destination);

      int GetMonthIndex(int iDay);

      //Methods used for generating outputs
      void OutputCSVData(const char* sFileName, stSortedMapKeyRef sortedRef[]);
      void InjectVariableToCSV(const char *sFileName, const char *sVarName, int iPosition);
      void OutputXMLData(const char* sFileName,stSortedMapKeyRef sortedRef[]);
      void OutputTXTsummary( const char* sFileName, stSortedMapKeyRef sortedRef[]);
      void OutputDictionary( const char* sFileName, stSortedMapKeyRef sortedRef[] );
      void OutputSQLiteData(DBManager *objDBManager);
      void OutputSQLiteStepData(DBManager *objDBManager);
      void GenerateStepOutput(unsigned long lStepCount);


      unsigned long m_lCurrentStep; //current step counter
      unsigned long m_lActiveSteps;//active step counter
      bool m_bStartUp;//If in current step in startup mode or not
      unsigned int m_iExpectedTimeSteps; //Number of expected steps for a simulation
      int m_iStartDay; //Simulation start day
      int m_iActualStartDay; //Simulation start day (when not reporting startup)
      int m_iEndDay; //Simulation end day
      int m_iStartMonthIndex; //Store the simulation start month index
      int m_iCurrentMonthIndex; //Store the current month index;
      int m_iTimeStepPerHour; //Store the number of timesteps in an hour (from cfg)
      int m_iYearCount; //Counts the current years number
      float m_fMinutePerTimeStep; //# of ts per min
      vector<long> m_BinStepCount; //Contains to total steps for each bin
      long m_AnnualBinStepCount; //Contains the total step count for annual bin... for now total step count

      //Since step output can be incremental during a simluation we need to variable
      //to store how many steps were outputed so far.
      unsigned long m_lOutputStepCount;


      // Update the module's simulation flags
      void SetFlags(  );

      //This is a pointer to it's sole instance.
      static TReportsManager* ptr_Instance;
   protected:
      std::string m_inputFilePath;
      std::map<std::string, std::string> m_params;

      // list of nodes to appear in out.xml <log_variable>
      std::set<std::string> m_nodes;
      // list of columns to appear in out.csv <step_variable>
      std::set<std::string> m_step_nodes;
      // list of token-value output to appear in out.summary <summary_variable>
      std::set<std::string> m_summary_nodes;
      // list of xsl styles sheet to be applied..in order..
      std::vector<std::string> m_stylesheet_list;

      // Stylesheets and transform files:
      map<std::string,std::string> m_StyleSheets;

      // list of transform targets.
      std::vector<std::string> m_xsl_targets;

      /**
      * Flags for results post-processing options
      */
      bool bSaveToDisk;

      bool bOutLogDB;
      bool bOutLogXML;
      bool bOutStepDB;
      bool bOutStepCSV;

      bool bReports_Enabled;

      bool bReportStartup;
      bool bDumpEverything;
      bool bWildCardsEnabled;
      bool bTS_averaging;
      bool bDumpDictionary;
      bool bSortOutput;
      bool bIndexDatabase;

      bool bStyleSheetGood;
      bool bLinkStyleSheet;
      bool bTransformXMLRequested;

      long m_lSaveToDisk;

};


#endif // TREPORTSMANAGER_H


