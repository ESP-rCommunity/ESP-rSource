#ifndef TREPORTSMANAGER_H
#define TREPORTSMANAGER_H

#ifdef _WIN32
#pragma warning (disable: 4786) //to disable annoying "identifier was truncated to '255' characters in the debug information" in VC++
#endif

#include <string>
#include <vector>
#include <map>
//#include <ext/hash_map>
#include "TWildCards.h"
#include "TVariableData.h"
#define iParam_MAX  50
#define iChar_Len   29

using namespace std;

typedef map<std::string,TVariableData> VariableDataMap;



struct TTimeDataRange
{
  TTimeData begin;
  TTimeData end;
};

bool TimeDataInRange(const TTimeData& data, const TTimeDataRange& range);



/**
 * class TReportsManager
 * This is the main reports class for h3k. It is to implemented as a singleton. This
 * is for ease of use in c and fortran environments.
 */

class TReportsManager
{
	
  /** Public methods: */
 public:
  /**
   * Return a pointer to the singleton.
   */

  static TReportsManager* Instance(  );
  void AddUserDefinedTimeRange(TTimeDataRange range);
  void SetReportParameter(const std::string& param, const std::string& value);
  /**
   * Update will iterate through each item in the VariableDataList container and..  1.
   * Record the current timestep data in the SingleDayTimestepData vector.   2. If a
   * new day has started bin the SingleDayTimestepData inforamtion into the Daily
   * Vector. Stream timestep information data to a on-going file if requested.
   * (UpdateDaily())  3. If a new month has started bin the days of that month into
   * the appropriate monthly array (Update Monthly()).  4. If the year is over,  bin
   * the months of the year values   into the appropriate Annual object.
   * (UpdateAnnually())  
   */
  bool Update( long step, float hour, long day, int iStartup );
	
  /**
   * 
   * @param sPassedName
   *        This is the name of the variable set assigned by the coder. It is important that
   *        it is unique. 
   * @param sPassedUnits
   *        This string contains the units of the variable set.
   */
  void AddReports( const std::string& sPassedName, const std::string& sPassedUnits );
	
  /**
   * 
   * @param sPassedName
   *        
   * @param sPassedValue
   *        This is the current value for the timestep, this will be stored until it is
   *        either overwritten by another call to report, or it will be stored and binned if
   *        the Update() method is called.
   */
  void Report( const std::string& sPassedName, const double& sPassedValue );
  void SetMeta(const std::string& sName, const std::string& sMetaName, const std::string& sMetaValue);
  /**
   *   Summary in .txt file? 
   */
  bool OutputSummary( const std::string& outFilePath );
  bool OutputSummary();
  void Log();
	
  /**
   *   Summary in XML format 
   */
  void OutputXMLSummary( const std::string& outFilePath );
  void OutputXMLSummary();
	

  /**
   *   Output CSV data directly
   */
  bool OutputCSVData( const std::string& outFilePath );
  bool OutputCSVData();
  

  /**
   *   Output token-value summary directly
   */
  bool OutputTXTsummary( const std::string& outFilePath );
  bool OutputTXTsummary();

  /**
   *   Output a listing of all valid tags 
   */
  bool OutputDictionary( const std::string& outFilePath );
  bool OutputDictionary();

  /**
   *  Parse an XML input file
   */
  void ParseConfigFile( const std::string& filePath );


  /**
   * Describe the current h3k reports configuration
   */
   void DescribeConfig ( char* cOptions[iParam_MAX*iChar_Len],
                         bool* bValues[iParam_MAX],
                         int* iCount);
   
  
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
   void ReportConfig ( std::string cParam, std::string &cValue );


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

  /**
   * Return the current status of h3k output file name (enabled/disabled)
   */
     bool UseResFilenameRoot();
        
  /**
   * Clean-up files
   */ 
     void Cleanup();
     
  /**
   * Increment vector count, and return new value.
   */
  int Increment_set_vector_count();
  
  /**
   * Read and write data from/to storage data base.
   */  
  bool Write_to_db_file(int var_index, double val, long step);
  double Read_from_db_file(int var_index, long step);
     
  /** Private methods: */
 private:
  /**
   * The constructor is private because we're a Singleton
   */
  TReportsManager(  );

  /**
   * 
   */
  void OutputTimestepData(  );
	
  /**
   * 
   */
  TTimeData GetTimeInfo(  );

  // search a vector for a string
  bool testForMatch( const std::vector<std::string>& txtlist, 
		     const std::string& search_text );


  /**
   * Update the module's simulation flags
   *
   */
  void SetFlags(  );

  // Interfaces to testForMatch that save the status of 
  // successful matches, and reduce the mumber of redundant
  // searches

  bool SearchVars( const std::vector<std::string>& txtlist, 
		   const std::string& search_text,
		   TVariableData& Variable, 
		   int mode );

  bool SearchAllVars( const std::vector<std::string>& txtlist1,
		      const std::vector<std::string>& txtlist2, 
		      const std::vector<std::string>& txtlist3, 
		      const std::string& search_text,
		      TVariableData& Variable);
		

  /**
   * This is a pointer to it's sole instance. 
   */    
  static TReportsManager* ptr_Instance;	
 
 protected:
  /**
   * This is the container that will store all the requested variables. This container
   * is populated by the AddReports() 
   */
		
  VariableDataMap m_variableDataList;

    /**
   * The total number of timestep in the simulation
   */

  unsigned int m_step_count; 
  int m_currentMonth;
  TTimeData m_currentTime;
  TTimeData m_rolloverTime;
  std::vector<TTimeDataRange> m_userDefinedTime;
	 
  std::string m_inputFilePath;
  std::map<std::string, std::string> m_params;
  // list of nodes to appear in out.xml
  std::vector<std::string> m_nodes;
  // list of columns to appear in out.csv
  std::vector<std::string> m_step_nodes;
  // list of token-value output to appear in out.summary
  std::vector<std::string> m_summary_nodes;
  // list of xsl styles sheet to be applied..in order.. 
  std::vector<std::string> m_stylesheet_list;

  // Stylesheets and transform files:
  map<std::string,std::string> m_StyleSheets;

  // list of transform targets.
  std::vector<std::string> m_xsl_targets;

  // vector indicating which timesteps contain monthly bins
  std::vector<int> m_month_bin_ts;
  
  /**
   * Flags for results post-processing options
   */
  bool bSaveToDisk;
  bool bFirstWrite;
  bool bReports_Enabled;
   
  bool bReportStartup; 
  bool bDumpEverything;
  bool bWildCardsEnabled;
  bool bTS_averaging;
  bool bDumpDictionary;
  bool bUseResFilenameRoot;

  bool bStyleSheetGood;
  bool bLinkStyleSheet;
  bool bTransformXMLRequested;
  
  long m_datasize;
  long m_ts_count;
  long m_first_step;
  
  void Open_db_file();
  void Close_db_file();
    
  int m_vector_count;
  
   /**
   * File object for I/O
   */    
   fstream diskDB;

};


#endif // TREPORTSMANAGER_H


