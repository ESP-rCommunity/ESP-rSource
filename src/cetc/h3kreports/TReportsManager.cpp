
#ifdef _WIN32
#pragma warning (disable: 4786) //to disable annoying "identifier was truncated to '255' characters in the debug information" in VC++
#endif

#include <iostream>
#include <fstream>
#include <iomanip>
#include <algorithm>
#include <string>
#include <cstdio>
#include <cstring>

#include "sys/stat.h"

#include "TReportsManager.h"
#include "TXMLAdapter.h"

#define DISKDB "h3kreports_DB.tmp"

#define DEBUG 0
#define DEBUG_1 0

#define SUMMARY     0
#define LOG         1
#define STEP        2
#define DUMPALLDATA 3
#define DUMPALLDATA 3


using namespace std;

std::string gString;


//Yee old trim function..
std::string trim(std::string const& source, char const* delims = " \t\r\n") {
  //cout << "trim_: " << std::endl;
  std::string result(source);
  std::string::size_type index = result.find_last_not_of(delims);
  if(index != std::string::npos)
    result.erase(++index);

  index = result.find_first_not_of(delims);
  if(index != std::string::npos)
    result.erase(0, index);
  else
    result.erase();

  return result;

}

extern "C"
{
  //These are the calls advertised to Fortran. Note that they are all
  //lowercase and end in an underscore. Calls from Fortran must omit
  //the trailing underscore

  /**
   *  Perform binning operations and move to next timestep
   */

  void rep_update__(long* step, float* hour, long* day, int* iStartup)
  {
    if(DEBUG) cout << "rep_update_: " << *step << "\t"
                   << *hour << "\t" << *day << endl;
    TReportsManager::Instance()->Update( *step, *hour, *day, *iStartup );
  }

  /**
   * Save data passed from bps
   *
   * note: sPassedValue and sPassedName are switched from the C++ call because
   * Fortran strings need to be at the end of a CALL - go figure
   *
   */
  void rep_report__(float *value, char *name, int sPassedName_length)
  {
    std::string passedName = std::string(name, sPassedName_length);

    if(DEBUG) cout << "rep_report_: " << sPassedName_length
                   << " \"" << *value << "\"\t\"" << passedName
                   << "\"" << endl;
    TReportsManager::Instance()->Report(passedName, *value);
  }


  /**
   * Text summary (obsolete)
   */
  void rep_summary__(  )
  {
    TReportsManager::Instance()->OutputSummary();
  }


  /**
   *  Output results in xml/csv formats
   */

  void rep_xml_summary__( char *sRootName, int iNameLength )
  {
    std::string sFileName;
    
    if ( TReportsManager::Instance()->UseResFilenameRoot() ) {
      // Turn the passed Fortran string into a C string
      std::string sRoot = std::string(sRootName, iNameLength);
    
      sFileName = sRoot + ".xml";
      TReportsManager::Instance()->OutputXMLSummary(sFileName);

      sFileName = sRoot + ".csv";
      TReportsManager::Instance()->OutputCSVData(sFileName);

      sFileName = sRoot + ".summary";
      TReportsManager::Instance()->OutputTXTsummary(sFileName);
    } else {
      // old "out.*" names
      // produce out.xml
      TReportsManager::Instance()->OutputXMLSummary();
      // produce out.csv
      TReportsManager::Instance()->OutputCSVData();

      // produce out.summary
      TReportsManager::Instance()->OutputTXTsummary();
    }

    // Leave these with "out.*" 
    // produce a dictionary of results
    TReportsManager::Instance()->OutputDictionary();

  }

  /**
   *  attach metadata to a variable
   */

  void rep_set_meta__(char *sVarName, char *sMetaName, char *sMetaValue,
                      int sVarNameLength, int sMetaNameLength, int sMetaValueLength)
  {
    
    
   std::string varName =std::string(sVarName, sVarNameLength);

   // (24-Jun-2011 Bart) Added dummy variable description to pass to SaveMetaItems.
   // Prior to the implementation of add_to_xml_reporting function, the functions 
   // rep_set_meta and rep_report were used instead. A variable description
   // was not passed until add_to_xml_reporting was implemented. 
   //
   // There are still many calls to the old rep_set_meta routine in esp-r plant code
   // that do not pass any variable description. As a result, XML reports assigns a bogus 
   // description to variables passed with rep_set_meta. This went
   // unnoticed until the optimized meta code, which proesses meta items only once, 
   // was implemented. 
   char* sDescription = "";
   int sDescriptionLength = 0;

   // Save meta items and do not convert to std strings here to avoid
   // computational overhead. These items are retrieved in function 
   // TReportsManager::SearchVars only once per variable. The meta data
   // is then converted to std::strings and then pushed to the m_metadata
   // storage map using function TReportsManager::SetMeta.
   TReportsManager::Instance()->SaveMetaItems( sMetaName,sMetaValue,sDescription,sMetaNameLength,sMetaValueLength,sDescriptionLength);  

  }

  /** 
   *  Set configuration file
   */
  void rep_set_config_file__(char *sFilePath, int sPathLength)
  {
   std::string str =std::string(sFilePath, sPathLength);

    TReportsManager::Instance()->ParseConfigFile(str);

  }

  /**
   *  Set a parameter
   */
   void rep_set_parameter__(char *sParamName,
                           char *sParamValue,
                           int sNameLength,
                           int sValueLength)
  {
    std::string paramName =std::string(sParamName, sNameLength);
    std::string paramValue =std::string(sParamValue, sValueLength);
    
    TReportsManager::Instance()->SetReportParameter(paramName, paramValue);
  
  }
  
  /**
   *  Return boolian indicating if output is enabled.
   */
  bool bh3k_rep_enabled__(){
    return TReportsManager::Instance()->ReportsEnabled();
  }

  /**
   *   Enable / disable output 
   */
  void h3k_enable_reports__( bool& bNewStatus ){
    TReportsManager::Instance()->EnableReports(bNewStatus);
    return;
  }
  
  /** 
   * return a boolian indicating H3kreports is active
   */
  bool rep_xmlstatus__(){
    return true;
  }

  /**
   * Return the current value of a configuration parameter
   *
   */
   void rep_report_config__(char* sValue, int iValLength, char *sParam, int iNameLength){


      short iPos;
      std::string paramValue;
      std::string paramName; 

      paramName = std::string(sParam, iNameLength);
      
      
      TReportsManager::Instance()->ReportConfig(paramName,paramValue);
   
      strcpy (sValue, paramValue.c_str());

      for (iPos = paramValue.size(); iPos<=iValLength; iPos++){
         sValue[iPos]=' ';
      }
   
      return;
   }
  /** 
   * Return the current status of an item in a vector 
   *
   */
   bool rep_report_list__(char *sType, char *sSheet,
                          int iTypeLength, int iSheetLength){

      std::string paramValue;
      std::string paramType;


      paramType  = std::string(sType, iTypeLength);
      paramValue = std::string(sSheet, iSheetLength);

      
      return TReportsManager::Instance()->ReportList(paramType,paramValue);
   
   }
   
   /**
    * Update the value of a configuration parameter
    *
    */
    void rep_update_config__(char* sParam, char* sValue, int iNameLength, int iValLength){

      std::string paramName = std::string(sParam, iNameLength);
      std::string paramValue = std::string(sValue, iValLength);

      TReportsManager::Instance()->UpdateConfig(paramName, paramValue);
      
    }
   /**
    * Toggle the status of an on-off configuration parameter
    */
    bool rep_toggle_config__(char* sParam, int iNameLength){

      std::string paramName = std::string(sParam, iNameLength);

      return TReportsManager::Instance()->ToggleConfig(paramName);

    }


    /**
     *   Write out configuration file with new options
     */
     
    void rep_update_config_file__(){

      TReportsManager::Instance()->UpdateConfigFile();
    
    }
    /**
     *  Clean up extra files
     */
    void rep_cleanup_files__(){
      
      TReportsManager::Instance()->Cleanup();
      
    }

/**
* This is a faster function to add a report for a value in ESP-r.
  There is no need  for LNBLNK to be used on any of the character arguments
  on the fortran side. This replaces rep_set_meta and rep_report..This will
  reduce simtulation times. Note there still has to be some major overhaul on
  the data structure itself (Like each zone, system and potentially surface
  entity has to be unique . This is problematic when trying to write genteric
  xslt transforms..So new functionality will be add as time goes on..So proper
  versioning of the xml file is required.

* @param value The float value to be stored and reported on.
* @param sVarName The Unique xml path location to where the data is to be stored.
* @param sMetaName The metatag to be added. (usually 'units' is used)
* @param sMetaValue The value for the above metatag (Usually the unit type, like
                    (W) for watts)
* @param sDescription A detailed description of the variable being reported on.
                    Will be outputted with the dictionary.
* @param *Length The length of the corrosponding char arrays.

**/

void add_to_xml_reporting__(float* value,
                        char* sVarName,
                        char* sMetaName,
                        char* sMetaValue,
                        char* sDescription,
                        int sVarNameLength,
                        int sMetaNameLength,
                        int sMetaValueLength,
                        int sDescriptionLength)
{
   std::string varName =std::string(sVarName, sVarNameLength);

   // Save meta items and do not convert to std strings here to avoid
   // computational overhead. These items are retrieved in function 
   // TReportsManager::SearchVars only once per varialbe. The meta data
   // is then converted to std::strings and then pushed to the m_metadata
   // storage map using function TReportsManager::SetMeta.
   TReportsManager::Instance()->SaveMetaItems( sMetaName,sMetaValue,sDescription,sMetaNameLength,sMetaValueLength,sDescriptionLength);  

   TReportsManager::Instance()->Report(varName, *value);

  }

  /**
    These interfaces to the above functions are needed to deal with
    dissimilar name mangling between GNU and sun compilers. (gcc
    adds two underscores to fortran function names containing
    embedded undescrores, while cc adds only one)

  **/

  void rep_update_(long* step, float* hour, long* day, int* iStartup)
  {
    rep_update__(step, hour, day, iStartup);
  }

  void rep_report_(float *value, char *name, int sPassedName_length)
  {
     rep_report__(value, name, sPassedName_length);
  }

  void rep_summary_(  )
  {
    rep_summary__();
  }

  void rep_xml_summary_( char *sRootName, int iNameLength )
  {
    rep_xml_summary__(sRootName,iNameLength);
  }

  void rep_set_meta_(char *sVarName, char *sMetaName, char *sMetaValue,
                      int sVarNameLength, int sMetaNameLength, int sMetaValueLength)
  {
       rep_set_meta__(sVarName, sMetaName, sMetaValue,
                      sVarNameLength, sMetaNameLength, sMetaValueLength);
  }


  void rep_set_config_file_(char *sFilePath, int sPathLength)
  {
     rep_set_config_file__(sFilePath, sPathLength);
  }


   void rep_set_parameter_(char *sParamName,
                           char *sParamValue,
                           int sNameLength,
                           int sValueLength)
  {
      rep_set_parameter__(sParamName, sParamValue, sNameLength, sValueLength);
  
  }

  bool bh3k_rep_enabled_(){
    return bh3k_rep_enabled__();
  }


  void h3k_enable_reports_( bool& bNewStatus )
  {
    h3k_enable_reports__( bNewStatus );
  }
  

  bool rep_xmlstatus_()
  {
    return rep_xmlstatus__();
  }


   void rep_report_config_(char* sValue, int iValLength, char *sParam, int iNameLength)
   {
      rep_report_config__(sValue, iValLength, sParam, iNameLength);
   }

   bool rep_report_list_(char *sType, char *sSheet,
                          int iTypeLength, int iSheetLength)
   {
      return rep_report_list__(sType, sSheet,iTypeLength, iSheetLength);
   }
   
   
    void rep_update_config_(char* sParam, char* sValue, int iNameLength, int iValLength)
    {
       rep_update_config__(sParam, sValue, iNameLength, iValLength);
    }

    bool rep_toggle_config_(char* sParam, int iNameLength)
    {
      return rep_toggle_config__(sParam, iNameLength);
    }



     
    void rep_update_config_file_()
    {
      rep_update_config_file__();
    }
    
  
    void add_to_xml_reporting_(float* value,
                          char* sVarName,
                          char* sMetaName,
                          char* sMetaValue,
                          char* sDescription,
                          int sVarNameLength,
                          int sMetaNameLength,
                          int sMetaValueLength,
                          int sDescriptionLength)
    {
       add_to_xml_reporting__(value,
                              sVarName,
                              sMetaName,
                              sMetaValue,
                              sDescription,
                              sVarNameLength,
                              sMetaNameLength,
                              sMetaValueLength,
                              sDescriptionLength);
    }

    void rep_cleanup_files_(){
      rep_cleanup_files__();
    }
}

// Prototypes
int Increment_vector_count();

bool testForMatch( const std::vector<std::string>& txtlist, const std::string& search_text);

bool SearchVars( const std::vector<std::string>& txtlist,
                 const std::string& search_text,
                 TVariableData& Variable,
                 int mode  );

bool SearchAllVars( const std::vector<std::string>& txtlist1,
                    const std::vector<std::string>& txtlist2,
                    const std::vector<std::string>& txtlist3,
                    const std::string& search_text,
                    TVariableData& Variable);

TReportsManager* TReportsManager::ptr_Instance = NULL;

bool TimeDataInRange(const TTimeData& data, const TTimeDataRange& range)
{
  if((range.begin.month < data.month) || (range.end.month > data.month))
    return false;

  if(range.begin.month == data.month)
    {
      if(range.begin.day > data.day) return false;
      else if(range.begin.day == data.day)
        {
          if(range.begin.hour > data.hour) return false;
          else if(range.begin.hour == data.hour)
            {
              if(range.begin.step > data.step) return false;
            }
        }
    }

  if(range.end.month == data.month)
    {
      if(range.end.day < data.day) return false;
      else if(range.end.day == data.day)
        {
          if(range.end.hour < data.hour) return false;
          else if(range.end.hour == data.hour)
            {
              if(range.end.step < data.step) return false;
            }
        }
    }

  return true;
}

TReportsManager* TReportsManager::Instance(  )
{
  if(ptr_Instance == NULL) //create a new instance
    {
      ptr_Instance = new TReportsManager();
    }
  return ptr_Instance;
}

// Constructor
TReportsManager::TReportsManager(  )
{
  m_currentMonth = 0;
  m_step_count = 0;
  m_currentTime.step = 0;
  m_currentTime.hour = 0;
  m_currentTime.day = 0;
  m_currentTime.month = 0;
  m_vector_count = 0;
  bFirstWrite = true;

  ParseConfigFile("input.xml"); //default input file
  
 
  SetFlags();                   //set config flags & defaults
  
  // Optionally, open up results storage on disk
  
  if ( bSaveToDisk ) Open_db_file();
    
}

/**
 * Update all data at the end of a timestep, and perform avg/bin operations
   operations as necessary
 */
 
bool TReportsManager::Update(long step, float hour, long day, int iStartup)
{

  if ( ! bReports_Enabled ) return false;

  static bool firstCallToUpdate = true;
  bool yearRollOver = false;
  VariableDataMap::iterator pos;
  // if simulation is still in start-up, and output of
  // start-up data not requested, return without doing anything.
  if ( iStartup == 1  && ! bReportStartup){return true;}

  if(DEBUG) cout << "Update step: " << step << "  hour: " << hour << "  day: "
                 << day << endl;

  if(firstCallToUpdate) //init on first call
    {
      m_first_step = step; 
    
      m_step_count = 0;
      if(DEBUG) cout << "First call" << endl;
      
      m_currentTime.step = step - 1; // -1 to force an update this call
      
      m_currentTime.hour = (int)hour;
      m_currentTime.day = day;

      //set month
      m_currentTime.month = 0;
      while(m_currentTime.day > kMonthlyTimesteps[m_currentTime.month])
        ++m_currentTime.month;

      firstCallToUpdate = false;
    }

  if(day - m_currentTime.day < 0) yearRollOver = true;



  //add new hour
  if((int)hour != m_currentTime.hour)
    {
      for(pos  = m_variableDataList.begin();
          pos != m_variableDataList.end(); ++pos)
        if(SearchAllVars(m_nodes,
                         m_step_nodes,
                         m_summary_nodes,
                         pos->first,
                         pos->second))
          
          // pos->second.UpdateHourly();

      m_currentTime.hour = (int)hour;

      if(DEBUG) cout << "Rollover hour: " << m_currentTime.hour << endl;
    }

  //add a new day
  if(day != m_currentTime.day)
    {
      for(pos  = m_variableDataList.begin();
          pos != m_variableDataList.end(); ++pos)
        if(SearchAllVars(m_nodes,
                        m_step_nodes,
                        m_summary_nodes,
                        pos->first,
                        pos->second))
          pos->second.UpdateDaily();

      m_currentTime.day = day;
      if(DEBUG) cout << "Rollover day: " << m_currentTime.day << endl;
    }

  //add a new month
  if((m_currentTime.day > kMonthlyTimesteps[m_currentTime.month]) || yearRollOver)
    {
      for(pos = m_variableDataList.begin();
          pos != m_variableDataList.end(); ++pos)
        if( SearchAllVars(m_nodes,
                         m_step_nodes,
                         m_summary_nodes,
                         pos->first,
                         pos->second))
          pos->second.UpdateMonthly(); 

      m_currentTime.month = (m_currentTime.month + 1)%12;

      //

      m_month_bin_ts.push_back(1);

      if(DEBUG) cout << "Rollover month: " << m_currentTime.month << endl;
    }
  else{

      m_month_bin_ts.push_back(0);

    }



  //add a new year - not used right now
  /*    if(m_currentTime.month == m_rolloverTime.month)
        {
        for(pos = m_variableDataList.begin(); pos != m_variableDataList.end(); ++pos)
        pos->second.UpdateAnnual();

        //m_currentTime.month = 0;
        }*/

  //check user-defined spans
  for(unsigned int i = 0; i < m_userDefinedTime.size(); i++)
    {
      if(TimeDataInRange(m_currentTime, m_userDefinedTime[i]))
        {

          for(pos  = m_variableDataList.begin();
              pos != m_variableDataList.end(); ++pos)
            if( SearchAllVars(m_nodes,
                              m_step_nodes,
                              m_summary_nodes,
                              pos->first,
                              pos->second))
              pos->second.UpdateUserDefined();
        }
    }

  /* check to see if the time step has changed, and if so,
     update data list. Note: these calls must come after
     the above roll-over code. Otherwise, the data will be
     assigned to the hour/day/month/year current bin, before
     rollover functions have determined of a new bin is required.
  */
  if(step != m_currentTime.step)
    {
      for(pos  = m_variableDataList.begin();
          pos != m_variableDataList.end(); ++pos)
        if( SearchAllVars(m_nodes,
                          m_step_nodes,
                          m_summary_nodes,
                          pos->first,
                          pos->second))
          pos->second.Update(bSaveToDisk, step);
      
      m_currentTime.step = step;
    
    }

  m_step_count++;

  return true;
}

void TReportsManager::AddUserDefinedTimeRange(TTimeDataRange range)
{
  m_userDefinedTime.push_back(range);
}

/**
 * Take data passed from bps and stuff it into the approprate vector
 */
void TReportsManager::Report( const std::string& sPassedName, const double& sPassedValue )
{
     if ( ! bReports_Enabled ) return;
        
     // Only set variable if it matches requested log/summary/step data.
     if ( SearchAllVars( m_nodes,
                         m_step_nodes,
                         m_summary_nodes,
                         sPassedName,
                         m_variableDataList[sPassedName]) ) {   
        
        m_variableDataList[sPassedName].Set(sPassedValue,
                                            bTS_averaging, 
                                            bSaveToDisk,
                                            m_step_count,
                                            m_month_bin_ts);
        
     }
}

/**
 * Set meta parameters, such as the units associated with a variable
 */
 
void TReportsManager::SetMeta(const std::string& sName, const std::string& sMetaName, const std::string& sMetaValue)
{

  if ( ! bReports_Enabled ) return;
  m_variableDataList[sName].SetMeta(sMetaName, sMetaValue);
  

}

/**
 * Function returing the current status of reporting output
 */
bool TReportsManager::ReportsEnabled(){
   return bReports_Enabled;
}

/**
 * Function returing the current status of reporting output file name
 */
bool TReportsManager::UseResFilenameRoot(){
   return bUseResFilenameRoot;
}

/**
 * Function setting the status of reporting output
 */
void TReportsManager::EnableReports( bool& ReportsStatus ){
   bReports_Enabled = ReportsStatus;
   return;
}


bool TReportsManager::OutputSummary()
{
  return OutputSummary("out.txt");
}

bool TReportsManager::OutputSummary( const std::string& outFilePath )
{

  if ( ! bReports_Enabled ) return false;

  if(m_inputFilePath == "") //by default, we dump everything
    {
      //Log();
    }

  return true;
}





// void TReportsManager::Log(  )
// {
// 
//   if ( ! bReports_Enabled ) return;
// 
//   VariableDataMap::iterator pos;
//   for( pos = m_variableDataList.begin(); pos != m_variableDataList.end(); ++pos)
//     {
//       cout << "Outputing data for: " << trim(pos->first) << endl;
//       pos->second.Log();
//     }
// }

void TReportsManager::OutputXMLSummary()
{

  if ( ! bReports_Enabled ) return;

  // Write out out.xml
  OutputXMLSummary("out.xml");
  std::string Target;

  // Check to see that i) a valid style sheet was provided and
  // ii) transforms were requested.
  if (  bStyleSheetGood &&  bTransformXMLRequested ) {

    TXMLAdapter XMLAdapter;

    // Synopsys :
    // WriteTransformedXML( source , target, stylesheet list )
    XMLAdapter.WriteTransformedXML( std::string("out.xml"),  m_StyleSheets );
  }

}

std::vector<std::string> PathComponents( std::string  path)
{
  vector<std::string> comp;
  size_t index, last_index;

  last_index = 0;
  if(path[path.size()-1] == '/') path.erase(path.size()-1); //remove trailing slash

  while((index = path.find("/", last_index + 1)) != std::string::npos)
    {
      if(path[last_index] == '/') ++last_index;
      comp.push_back(path.substr(last_index, index - last_index));
      last_index = index;
    }

  return comp;
}

///Streams report to an XML output file.

void TReportsManager::OutputXMLSummary(  const std::string& outFilePath )
{

  if ( ! bReports_Enabled ) return;
  
  //determine which variables to log
  bool useFlatHierarchy;
  // only dump xml if there's xml to dump!
  if(m_nodes.size() >> 0 || bDumpEverything ){

    if(m_params["hierarchy"].empty() || m_params["hierarchy"] == "tree")
      useFlatHierarchy = false;
    else
      useFlatHierarchy = true;

    TXMLAdapter doc;
    doc.AddNode(NULL, "system", "");

    // if style sheet provided and embedded link requested,
    // link style sheet.
    if(!m_stylesheet_list.empty() &&  bLinkStyleSheet )
    //set it to the first stylesheet in list only.
      doc.SetStylesheet(m_stylesheet_list[0]);

    doc.AddAttribute(doc.RootNode(), "version", "1.0");
    VariableDataMap::iterator pos;

    std::string currentPath = "";
    for(pos = m_variableDataList.begin(); pos != m_variableDataList.end(); ++pos)
      {
        if(bDumpEverything ||  SearchVars(m_nodes,
                                          pos->first,
                                          pos->second,
                                          LOG))
          {
            TXMLNode currentNode = doc.RootNode();

            if(useFlatHierarchy)
              {
                currentNode = doc.AddNode(currentNode, "parameter", "");
                doc.AddNode(currentNode, "name", trim(pos->first));
              }
            else
              {
                vector<std::string> comp = PathComponents(trim(pos->first));
                for(unsigned int i = 0; i < comp.size(); i++)
                  {
                    vector<TXMLNode> nextNodeVector = doc.GetChildren(currentNode, comp[i]);
                    if(nextNodeVector.size() == 0)
                      currentNode = doc.AddNode(currentNode, comp[i], "");
                    else
                      currentNode = nextNodeVector[0];
                  }

                currentNode = doc.AddNode(currentNode, trim(pos->first).substr(trim(pos->first).find_last_of("/") + 1), "");
                doc.AddNode(currentNode, "name", trim(pos->first).substr(trim(pos->first).find_last_of("/") + 1));
              }
            //tell it to output step data if required
            //if( testForMatch(m_step_nodes, pos->first) )
            //  SetMeta(pos->first, "StepData", "");
            //find(m_step_nodes.begin(), m_step_nodes.end(), pos->first) != m_step_nodes.end())


            pos->second.OutputXML(&doc, currentNode, m_params);
          }
      }

    doc.WriteToFile(outFilePath);
  }
}

/*
 * Output Dictionary simply dumps a listing of all valid tags
 * encountered during a simulation run.
 *
 */
bool TReportsManager::OutputDictionary()
{
  return OutputDictionary("out.dictionary");
}
bool TReportsManager::OutputDictionary( const std::string& outFilePath )
{

  if ( ! bReports_Enabled ) return false;

  VariableDataMap::iterator pos;

  ofstream dictionaryFile;
  if ( m_variableDataList.size() >> 0 && bDumpDictionary ){

    dictionaryFile.open(outFilePath.c_str());

    for(pos = m_variableDataList.begin(); pos != m_variableDataList.end(); ++pos) {
      dictionaryFile << "\""
                     << trim(pos->first)
                     << "\",\""
                     << pos->second.RetrieveMeta("description")
                     << "\",\""
                     << pos->second.RetrieveMeta("units")
                     << "\"\n";
    }
    dictionaryFile.close();
  }
  return true;
}


/**
 * OUTPUT summary is an alternative method for producing
   a token-value list of data without performing a transform
   on the xml output.
 *
 */
bool TReportsManager::OutputTXTsummary()
{
  return OutputTXTsummary("out.summary");
}

bool TReportsManager::OutputTXTsummary( const std::string& outFilePath )
{

  if ( ! bReports_Enabled ) return false;

  VariableDataMap::iterator pos;
  ofstream summaryFile;
  summaryFile.setf(ios::showpoint);
  summaryFile.precision(8);


  if (m_summary_nodes.size() >> 0){

    // open summary file
    summaryFile.open(outFilePath.c_str());

    // loop through all nodes, and check if summary data has been requested.
    for(pos = m_variableDataList.begin(); pos != m_variableDataList.end(); ++pos){
      if ( SearchVars(m_summary_nodes,
                      pos->first,
                      pos->second,
                      SUMMARY) ){
        // summary output has been requested. Open file, if necessary
        summaryFile <<  pos->second.OutputTXT( trim(pos->first) ,m_params );
      }
    }

    // close file
    summaryFile.close();
  }
  return true;
}
/**
 * OUTPUT CSV data is an alternative method for producing CSV
 * formatted output without transforming XML. Permits time-step
 * data to be produced from h3kreports on plantforms without
 * XSLT2.0 support.
 *
 */

bool TReportsManager::OutputCSVData()
{
  return OutputCSVData("out.csv");
}

bool TReportsManager::OutputCSVData( const std::string& outFilePath )
{

  if ( ! bReports_Enabled ) return false ;
  
  VariableDataMap::iterator pos;
  unsigned int curr_step,step_start;
  bool fileopen;

  std::string temp_text;

  ofstream csvFile;
  csvFile.setf(ios::showpoint);
  csvFile.precision(8);

  /* Check if time-step averaging has been set. If so, skip first (unaveraged)
   * step.
   *
   */

  if ( bTS_averaging ) {
    //skip first step
   step_start = 1;
  }else{
    step_start = 0;
  }

  fileopen = 0;

  // write out header row (names)
  for(pos = m_variableDataList.begin(); pos != m_variableDataList.end(); ++pos) {

    if( SearchVars(m_step_nodes,
                   pos->first,
                   pos->second,
                   STEP) ){
      // open file if necessary

      if (!fileopen) {
        csvFile.open(outFilePath.c_str());
        fileopen = 1;
      }


      // Add spaces to make header more "speadsheet friendly"
      temp_text = trim(pos->first);
      while ( temp_text.find("/") != string::npos ){
         temp_text.replace( temp_text.find("/"), 1, ":");
      }
      while ( temp_text.find("_") != string::npos ){
         temp_text.replace( temp_text.find("_"), 1, " ");
      }

      csvFile << temp_text;  //pos->first;

// write out unit
      csvFile << " ";
      csvFile << pos->second.RetrieveMeta("units");

      csvFile << ", ";
    }
  }
  csvFile << "\n";

/*
  // write out units
  for(pos = m_variableDataList.begin(); pos != m_variableDataList.end(); ++pos) {
    if( SearchVars(m_step_nodes,
                   pos->first,
                   pos->second,
                   STEP) ){
      csvFile << pos->second.RetrieveMeta("units");
      csvFile << ", ";
    }
  }
  csvFile << "\n";
*/

  // loop through all timesteps
  for (curr_step = step_start; curr_step <  m_step_count ; curr_step++){
    // loop through all variables
    for(pos = m_variableDataList.begin(); pos != m_variableDataList.end(); ++pos) {
      // check if time-step data has been requested for variable
      if( SearchVars(m_step_nodes,
                     pos->first,
                     pos->second,
                     STEP) ){

        // time-step data has been requested. Stream out.
        csvFile << pos->second.RetrieveValue(curr_step,
                                             m_first_step,
                                             bSaveToDisk);
        csvFile << ", ";

      }
    }
    // End of variable list. New line
    csvFile << "\n";
  }

  csvFile.close();

  return true;
}


/**
 * SetReportParameter: collect & store configuration data passed from bps
 *
 *
 */

void TReportsManager::SetReportParameter(const std::string& param, const std::string& value)
{

  if ( ! bReports_Enabled ) return;

  m_params[param] = value;
  // Is parameter esp-r's default timestep averaging setting?
  if ( param == "default_time_step_averaging" ) {
    if ( m_params["time_step_averaging"] == "true" ||
        m_params["time_step_averaging"] == "false"     ){
       // TS averaging has been set in H3K reports configuration. Do nothing.
    }else if ( strtol(m_params["default_time_step_averaging"].c_str(), NULL, 10) == 1 ){
      // TS averaging has not been set in H3KReports configuration. Use
      // ESP-r setting
      bTS_averaging = true;
    }else {
      // TS averaging has not been set in H3KReports configuration. Use
      // ESP-r default.
      bTS_averaging = false;
    }
  }
}


/**
 * Write out a configuration file using whatever options
 * that are in memory.
 */
void TReportsManager::UpdateConfigFile(){

   std::vector<std::string>::iterator sheet;
   std::vector<std::string>::iterator var;
   std::map<std::string, std::string>::iterator param;
   std::string sTemp;
   
   TXMLAdapter inputXML ;

   // Set defaults if required.
   SetFlags();

   inputXML.AddNode(NULL, "configuration", "");
   TXMLNode currentNode = inputXML.RootNode();

   // Loop through all defined parameters, add to xml document 
   for( param = m_params.begin(); param != m_params.end(); param++)
   {
      // Don't write out unset parameters
      if ( param->second != ""){
         inputXML.AddNode(currentNode,param->first,param->second);
      }
   }
   
   // Loop through stylesheets, and add to xml document 
   for (sheet  = m_stylesheet_list.begin();
        sheet != m_stylesheet_list.end();
        sheet++)
   {
    sTemp = *(sheet);
    inputXML.AddNode(currentNode,"style_sheet",sTemp.c_str());
  
   }

   // Loop through XSL transform targets
   for (sheet  = m_xsl_targets.begin();
        sheet != m_xsl_targets.end();
        sheet++)
   {
    sTemp = *(sheet);
    inputXML.AddNode(currentNode,"transform_destination_file",sTemp.c_str());
  
   }

   // Loop through output variables and add to document
   for ( var = m_nodes.begin(); var != m_nodes.end(); var++ ){
      sTemp = *(var);
      inputXML.AddNode(currentNode,"log_variable",sTemp.c_str());
   }
   
   // Loop through timestep output variables and add to document
   for ( var = m_step_nodes.begin(); var != m_step_nodes.end(); var++ ){
      sTemp = *(var);
      inputXML.AddNode(currentNode,"step_variable",sTemp.c_str());
   }

   // Loop through summary output variables and add to document
   for ( var = m_summary_nodes.begin(); var != m_summary_nodes.end(); var++ ){
      sTemp = *(var);
      inputXML.AddNode(currentNode,"summary_variable",sTemp.c_str());
   }

   // Write XML document ot disk
   inputXML.WriteToFile(m_inputFilePath);   
}


/**
 * Parse the configuration file (input.xml)
 *
 */

void TReportsManager::ParseConfigFile( const std::string& filePath  )
{
  
  
  TXMLAdapter inputXML(filePath);
  m_inputFilePath = filePath;

  
  
  if(inputXML.RootNode() == NULL)
  {
      bReports_Enabled = false;
      return;
  
  }else{
      
      bReports_Enabled = true;

  }

  // Nodes to appear in out.xml
  inputXML.GetNodeValues("log_variable", inputXML.RootNode(),m_nodes);

  // Nodes to appear in out.csv
  inputXML.GetNodeValues("step_variable", inputXML.RootNode(),m_step_nodes);

  // Nodes to appear in out.summary
  inputXML.GetNodeValues("summary_variable", inputXML.RootNode(),m_summary_nodes);

  // Sytlesheet list for multiple transforms.
  inputXML.GetNodeValues("style_sheet", inputXML.RootNode(),m_stylesheet_list);

  // Sytlesheet list for multiple transforms.
  inputXML.GetNodeValues("transform_destination_file", inputXML.RootNode(),m_xsl_targets);

  // Should style sheet be linked?
  m_params["link_style_sheet"] = inputXML.GetFirstNodeValue("link_style_sheet", inputXML.RootNode() );

  // XSLT transform requested
  m_params["apply_style_sheet"] = inputXML.GetFirstNodeValue("apply_style_sheet", inputXML.RootNode() );

  //hierarchy type
  m_params["hierarchy"] = inputXML.GetFirstNodeValue("hierarchy", inputXML.RootNode());

  // minutes per timestep - This data isn't read out of input.xml.
  // bps sets "minutes_per_step" instead.
  // m_params["minutes_per_step"] = inputXML.GetFirstNodeValue("minutes_per_step", inputXML.RootNode());

  //overide for time-step averaging
  m_params["time_step_averaging"] = inputXML.GetFirstNodeValue("time_step_averaging", inputXML.RootNode());
  
  // concise reporting mode
  m_params["dump_all_data"] = inputXML.GetFirstNodeValue("dump_all_data", inputXML.RootNode());

  // Wildcards
  m_params["enable_xml_wildcards"] = inputXML.GetFirstNodeValue("enable_xml_wildcards", inputXML.RootNode());

  // Report start up period data
  m_params["report_startup_period_data"] = inputXML.GetFirstNodeValue("report_startup_period_data", inputXML.RootNode());

  // Dictionary output
  m_params["output_dictionary"] = inputXML.GetFirstNodeValue("output_dictionary", inputXML.RootNode());

  // Output file name root = results file name root?
  m_params["use_resfilenameroot"] = inputXML.GetFirstNodeValue("use_resfilenameroot", inputXML.RootNode());
  
  // Save to disk
  m_params["save_to_disk"] = inputXML.GetFirstNodeValue("save_to_disk", inputXML.RootNode());
  
  return;
}

/**
 * Review configuration parameters and set flags & defaults, as necessary
 *
 */

void TReportsManager::SetFlags(){

  // Test all stylesheets for readability
  // (We won't perform an xml transform with it if it's not there!)

  ifstream Style_Sheet_test;
  std::vector<std::string>::iterator sheet;
  std::vector<std::string> erase_sheets;
  map<std::string, std::string>::iterator map_loc;
  int pos;

  // Check if style-sheet and xsl transform vectors are the same lenght;
  // otherwise, pad with none.


  pos = 0;
  for ( sheet = m_stylesheet_list.begin(); sheet != m_stylesheet_list.end(); sheet++ ){
    pos++;

    // Pad xsl targets with 'end'
    if ( m_xsl_targets.size() < pos ){
      m_xsl_targets.insert( m_xsl_targets.end(), "none" );
    }

    // Append stylesheet/target pair to map.
    m_StyleSheets.insert( pair<std::string,std::string>(*(sheet),m_xsl_targets.at(pos-1) ) );
  
  }



  // Loop through style sheets, and check if each is valid.

  for ( map_loc = m_StyleSheets.begin(); map_loc != m_StyleSheets.end(); map_loc++){

    std::string sTemp = map_loc->first;

    Style_Sheet_test.open( sTemp.c_str(), ifstream::in );
    Style_Sheet_test.close();

    if ( Style_Sheet_test.fail() ) {

      // sheet is not valid! Mark for deletion.
      erase_sheets.insert(erase_sheets.end(), sTemp);

      // Clear file I/O error:
      Style_Sheet_test.clear(ios::failbit);

    }

  }

  
  // Now delete the sheets that weren't found.
  for ( sheet = erase_sheets.begin(); sheet != erase_sheets.end(); sheet++ ){
    m_StyleSheets.erase(*(sheet));

  }

  if ( ! m_StyleSheets.empty() ) {
    bStyleSheetGood = true;
  }else{
    bStyleSheetGood = false;
  }

  if ( m_params["hierarchy"].empty() ) {
    m_params["hierarchy"] = "flat";
  }

  // Check if stylesheet should be linked
  
  if ( m_params["link_style_sheet"]  == "true" ){
    bLinkStyleSheet = true;
  }else{
    m_params["link_style_sheet"] = "false";
    bLinkStyleSheet = false;
  }

  // Check if an XSLT transform is requested
  
  if ( m_params["apply_style_sheet"] == "true" ) {
    bTransformXMLRequested = true;
  }else{
    m_params["apply_style_sheet"] = "false";
    bTransformXMLRequested = false;
  }

  //Conscise reporting mode
 
  if ( m_params["dump_all_data"]  == "true" ){
    bDumpEverything = true;
  }else{
    m_params["dump_all_data"] = "false";
    bDumpEverything = false;
  }
  // wildcards
 
  if ( m_params["enable_xml_wildcards"]  == "true" ){
    bWildCardsEnabled = true ;
  }else{
    m_params["enable_xml_wildcards"] = "false";
    bWildCardsEnabled = false;
  }
  // Report results from start-up period?
  if ( m_params["report_startup_period_data"] == "true" ){
    bReportStartup = true;
  }else{
    m_params["report_startup_period_data"] = "false";
    bReportStartup = false;
  }
  
  // Dictionary output?
  if ( m_params["output_dictionary"] == "true" ){
    bDumpDictionary = true;
  }else{
    m_params["output_dictionary"] = "false";
    bDumpDictionary = false;
  }

  // Output file name root = results file name root?
  if ( m_params["use_resfilenameroot"] == "true" ){
    bUseResFilenameRoot = true;
  }else{
    m_params["use_resfilenameroot"] = "false";
    bUseResFilenameRoot = false;
  }

  // Timestep averaging 
  if ( m_params["time_step_averaging"] == "false" ){
      bTS_averaging = false;
  }else if (m_params["time_step_averaging"] == "true" ){
      bTS_averaging = true;
  }
  
  // Optionally store data on disk
  if ( m_params["save_to_disk"] == "true" ){
     bSaveToDisk = true;
  }else{
    m_params["save_to_disk"] = "false";
    bSaveToDisk = false;
  }

  return;
}

/**
 *    Function to determine if a parameter has been
 *    defined 
 *
 */

bool TReportsManager::ReportList( std::string cType,
                                  std::string cValue){
   bool bFound;

   // Search list of style sheets
   if ( cType == "style_sheet" ) {

      if (std::find(m_stylesheet_list.begin(),
                    m_stylesheet_list.end(),
                    cValue) != m_stylesheet_list.end()){
         bFound = true;

      }else{
         
         bFound = false;

      }

   // Other types of searches added here
   }else{         

   }

   return bFound;
}

/**
 *   Return the value of a requested parameter
 *
 */
void TReportsManager::ReportConfig(std::string cParam,
                                   std::string &cValue){

   cValue = m_params[cParam];
   return;

}

/**
 *   Toggle value of requested parameter (true/false)
 *
 */
 

bool TReportsManager::ToggleConfig(std::string cParam){
   bool bSuccess;
   bSuccess = true;
   
   if ( m_params [ cParam ] == "true" ){
      m_params [ cParam ] = "false";
   }else{
      m_params [ cParam ] = "true";
   }
   
   return bSuccess; 
}

/**
 *   Update the value of a specified parameter
 *
 */
void TReportsManager::UpdateConfig(std::string cParam,
                                   std::string cValue ){
   
  
   // Add a style sheet to the stylesheet vector
   if ( cParam == "+style_sheet" &&
         std::find(m_stylesheet_list.begin(),m_stylesheet_list.end(), cValue)
         == m_stylesheet_list.end()){

        // empty the vector
        m_stylesheet_list.clear();
        // add new record
        m_stylesheet_list.push_back(cValue);

   // Drop a stylesheet from the vector
        
   }else if ( cParam == "-style_sheet" ){

         m_stylesheet_list.erase( std::find(m_stylesheet_list.begin(),
                                            m_stylesheet_list.end(),
                                            cValue) );

   // Otherwise, append value to m_params vector
   } else {
      m_params[cParam] = cValue;
   }

   // Reparse m_params and set flags used by TReportsManager
   SetFlags();
}

void TReportsManager::OutputTimestepData(  )
{

}


TTimeData TReportsManager::GetTimeInfo(  )
{
  return m_currentTime;
}

/**
 * Search through an array of strings, and check if any string matches search_text
 *
 */

bool TReportsManager::testForMatch(const std::vector<std::string>& txtlist,
                                   const std::string& search_text){

  Wildcard Cwildcard_engine;
  unsigned int txt;
  bool bResult = false;

  // Check if we've already flagged this data


  for (txt = 0; txt < txtlist.size() ; txt++ ) {

    // Wildcard searching
    if ( bWildCardsEnabled ){

      if ( Cwildcard_engine.wildcardfit(txtlist[txt].c_str(),search_text.c_str()) == 1) {
        bResult = true;
        return bResult;
      }

    } else if( txtlist[txt] == search_text ) {

      bResult = true;
      return bResult;

    }

  }

  return bResult;
}

/**
 * Search for a variable in an array of strings
 *
 */
bool TReportsManager::SearchVars( const std::vector<std::string>& txtlist,
                                  const std::string& search_text,
                                  TVariableData& Variable,
                                  int mode ){

  bool result;

  // Check if search has been performed perviously
  if ( ! Variable.QuerySearchStatus(mode) ){

    // run search
    if(bDumpEverything){// If all data has been requested, return *match*
      result = true;
    }
    else{
      result = testForMatch( txtlist,  trim(search_text));
    }

    // Save search result to ensure that we don't
    // have to run test-for-match again for this
    // variable!

    // update variable result
    Variable.UpdateSearchResult( mode, result);

    // Update search status
    Variable.UpdateSearchStatus( mode, true);

    // If processing variable the first time, set the meta data (units and description).
    // Now the saved meta data passed by add_to_xml_reporting is converted to 
    // std::strings so that it can be pushed onto the storage map using the SetMeta
    // function. This approach makes use of the existing search functionality and 
    // results in a 35-40% runtime reduction compared to the previous method. 
    std::string metaName =std::string(sMetaName_sv, sMetaNameLength_sv);
    std::string metaValue =std::string(sMetaValue_sv, sMetaValueLength_sv);
    std::string metaDesc =std::string(sDescription_sv, sDescriptionLength_sv);               
    SetMeta(search_text, metaName, metaValue);
    SetMeta(search_text, "description", metaDesc);

  }

  // return result
  return Variable.QuerySearchResult(mode);

}

/**
 * Search for a variable in three arrays of strings
 *
 */

bool TReportsManager::SearchAllVars(const std::vector<std::string>& txtlist1,
                                    const std::vector<std::string>& txtlist2,
                                    const std::vector<std::string>& txtlist3,
                                    const std::string& search_text,
                                    TVariableData& Variable){

  if ( bDumpEverything ){
    return SearchVars(m_dummy_list, search_text, Variable, DUMPALLDATA);
  }
  else{

    if ( SearchVars(txtlist1, search_text, Variable, LOG )){
      return true;
    }
    if ( SearchVars(txtlist2, search_text, Variable, STEP  )){
      return true;
    }
    if ( SearchVars(txtlist3, search_text, Variable, SUMMARY )){
      return true;
    }
  }
  return false;
}

/**
 * Save meta data passed from add_to_xml_reporting call
 *
 */
void TReportsManager::SaveMetaItems( char* sMetaName,
                        char* sMetaValue,
                        char* sDescription,
                        int sMetaNameLength,
                        int sMetaValueLength,
                        int sDescriptionLength)
{
      sMetaName_sv          = sMetaName;
      sMetaValue_sv         = sMetaValue;
      sDescription_sv       = sDescription;	
      sMetaNameLength_sv    = sMetaNameLength;
      sMetaValueLength_sv   = sMetaValueLength;
      sDescriptionLength_sv = sDescriptionLength;

      return;
}

/**
 * -----------------------------------------------------------------
 * 
 * The following routines will optionally save time-step data into 
 * a scratch file, instead of pushing it onto the heap.  The file is
 * formatted as a list-of-lists; for each variable reported by
 * h3kreports, the file contains all time-row data. Data for subsequent
 * variables are appended at the end of the file. 
 * For instance, a file containing N time row data from two variables 
 * would look like this:
 *
 * VARIABLE:  ___________ V 1______________   ____________ V 2 ___________
 *           /                             \ /                            \
 * TIME ROW: | __Ts1__  __Ts2__     __TsN__ | __Ts1__  __Ts2__     __TsN__ |
 *           |/       \/       \   /       \|/       \/       \   /       \|
 * FILE DATA:|---8B---|---8B---|...|---8B---|---8B---|---8B---|...|---8B---|....
 *
 * There are no delimiters in the file; each piece of data is stored
 * as a double (~8 Bytes on most systems)
 *
 * The facility does not need to know the total number of variables that
 * will be reported---it merely appends new variables on to the end of 
 * the current file. But it must know the total number of timesteps
 * expected in a simulaton, which prevents its use with variable 
 * timestep.
 */
 
/**
 * Delete temporary file
 */
void TReportsManager::Cleanup(){
  if (bSaveToDisk){  
    Close_db_file(); 
  }
  return;
}

/**
 * Open a database file for storing data on disk.
 */
void TReportsManager::Open_db_file(){
  struct stat file_stat;
  ofstream TempStream;
  // Delete file, if it exists.
  if ( stat ( DISKDB, &file_stat ) == 0 ){
    remove(DISKDB);
  }
  
  // For resons unknown, fstream cannot open a file for 
  // read/write if it does not already exist. Therefore, 
  // create an empty file. 
  TempStream.clear();
  TempStream.open(DISKDB, ios::binary);
  TempStream.close();
  
  diskDB.open ( DISKDB , ios::in | ios::out | ios::binary );
  
  if ( ! diskDB.is_open() ) {
    cout << "\n\n XML OUTPUT ERROR: could not open file " << DISKDB << " for writing.\n";
    cout << " Error flag: "<< diskDB.rdstate() <<"\n";
    exit(1);
  }
  return;
}
/**
 * Close database file and delete from disk.
 */
void TReportsManager::Close_db_file(){
  if ( DEBUG ) cout << "Closing DB file\n";
  diskDB.close();
  remove(DISKDB);
  return;
}

/**
 * Read data from disk
 */ 
double Read_from_db_file(int var_index, long step) {
    return TReportsManager::Instance()->Read_from_db_file(var_index, step);
}

double TReportsManager::Read_from_db_file( int var_index, long step){
   
  streampos start_byte; 
  double val; 
  
/* Compute the location of the current time row data, for the 
     specified variable.
  
     start-byte   = [  ( {Index of current vector} - 1 ) * { total number of timesteps }
                    
                       + ( {current timestep} - { first step } ) ] *  { bytes-per-redcord }
                       
  */
  
  start_byte = (   ( var_index - 1 ) * m_ts_count 
                        + ( step - m_first_step ) )  * m_datasize;
    
  if ( DEBUG) cout << "TS:"<<step<<" Reading "<< m_datasize << " bytes for var "
                    <<var_index 
                    << " Starting at byte " << start_byte;
  
  diskDB.seekg ( start_byte ); 
  diskDB.read ( (char*)&val, m_datasize );
  if ( ! diskDB ){
    if (DEBUG) cout << " ( ERROR! Read "<< diskDB.gcount() << "bytes, Val is " << val << ")\n";
  }else{
    if (DEBUG) cout << " ( Read "<< diskDB.gcount() << "bytes, Val is " << val << ")\n";
  }                  
  
  return val;
  
}

/**
 * Write data to disk
 */

bool Write_to_db_file(int var_index, double val, long step){
  return TReportsManager::Instance()->Write_to_db_file(var_index,val,step);
}

bool TReportsManager::Write_to_db_file(int var_index, double val, long step){
        
  streampos start_byte;     
  double val2;
  if ( bFirstWrite ) {
  
    // Determine size of double, in bites.
    m_datasize = sizeof(double);
  
    // Collect number of timesteps
    m_ts_count = int( strtod(m_params["number_of_timesteps"].c_str(), NULL) ); 
    
    bFirstWrite = false;
  }
  
  /* Compute the location of the current time row data, for the 
     specified variable.
  
     start-byte   = [  ( {Index of current vector} - 1 ) * { total number of timesteps }
                    
                       + ( {current timestep} - {first times step} ) ] *  { bytes-per-redcord }
                       
  */
  start_byte = (   ( var_index - 1 ) * m_ts_count 
                        + ( step - m_first_step ) ) * m_datasize;
  
  if (DEBUG) cout << "TS:"<<step<<" Writing "<< m_datasize << " bytes for var "
                    <<var_index << "(" << val <<")"
                    << " Starting at byte " << start_byte ;
                    
  // Now write data:
  diskDB.seekp( start_byte );
  
  if (DEBUG){
    diskDB.write( (char*)&val, m_datasize );
    diskDB.seekg( start_byte );
    diskDB.read( (char*)&val2, m_datasize );
    cout << " ( Val is " << val2 << ")\n";
  } 
  diskDB.seekp( start_byte );
  return diskDB.write( (char*)&val, m_datasize );

}

/**
 * Increment vector count.
 */
int Increment_set_vector_count(){
  return TReportsManager::Instance()->Increment_set_vector_count();
}

int TReportsManager::Increment_set_vector_count(){
  m_vector_count++;
  return m_vector_count;
}



