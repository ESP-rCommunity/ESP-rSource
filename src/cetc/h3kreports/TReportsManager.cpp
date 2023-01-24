#include "TReportsManager.h"

#define DEBUG 1
//#define TRACE_VARIABLE 120
/* ********************************************************************
** Tips for troubleshooting a variable:
** For tracing a variable define the TRACE_VARIABLE, run a simulation and
** look at the log_trace.txt that gets produced.  From there see wich
** variable you wish to trace and write that number beside the TRACE_VARIABLE
** then compile/run the simulation again.  Output of the variable will
** be appended at the end of the log_trace.txt.
** ***************************************************************** */
//#define TRACE_VARIABLE 157

/* ********************************************************************
** H3KReport main workflow
** notes: all the data between esp-r and the C++ code if funnel through
**        then h3kmodule.f90 and the TReportsmanager.cpp.
** 1. esrubps/bmatsv.F: sends then simulation information and initialize
**    the report variables (TReportManager::SetSimulationInfo) &
**    (TReportManager::AddToVariableInfoList)
** 2. TReportsManager stores the simulation information in class
**    variables and the report variable information in m_VariableInfoList
** 3. TReportsManager reads the input.xml for report configuration options
** 4. esru*: runs through a timestep, sending data for report variables
**    encountered to the TReportsManager::AddToReportDataList
** 5. TReportsManager stores the data into a map object of TReportData
**    The TReportData class will only store the data requested in the
**    input.xml file.  TReportData will use the TBinnedData.call class
**    to store bin data <log_variable>.
** 6. esrubps/bmatsv.F pushes the timestep forward and informs H3kReports
**    (TReportsManager::AddToTimeStepList)
** 7. Step 4-6 are repeated until the simulation is done.
** 8. esrubps/bmatsv.F request the output to be produced.
**    TReportsManager::GenerateOutput
** 9. For each requested output format the TReportManager will loop through
**    to collected results in TReportData and send to output.  The
**    DBManager and TXMLAdapter classes are used to send the results to
**    SQLite and xml.
** 10.esrubps/bmatsv.F terminated the simulation TReportsManager::Cleanup
** 11.TReportsManager destroys itselft.
**
** Alternate flow - Asynchronus save
** notes: the main workflow applies for the exception that when a
**        timestep is incremented (step 6) the TReportManager may trigger a
**        save routine to push the collected result to the requested output
**        format.
**
**        ***************************************************
**
** Class information
** H3Kmodule.f90: module in fortran that constains all the definitions
**    of all the possible report variables.  It also serves as a wrapper
**    between the h3kreports cpp and fortran code.
** TReportsManager.cpp: main driver for the H3K reports.  Used as
**    singleton this class will interact with all the other H3KReports
**    classes to store, calculate and the data it receives.
** TXMLAdapter.cpp: used by the TReportsManager to retrieve / populate
**    the configuration information from the input.xml and to generate
**    the out.xml file.
** TWildCards.cpp: used by the TReportsManager to perform wildcard
**    matching operations, a feature available when requesting specific
**    report variables in the input.xml file.
** TReportData.cpp: used by the TReportsManager to store all the data
**    for one variable.  The TReportsManager will maintain a map of
**    these class instances (one for each different variable).
** TBinnedData.cpp: used by the TReportData to store and calculate
**    one bin data.  The TReportData will store a vector of these bins.
**    one for each month + one for the annual data bin.
** DBManager.cpp: used by the TReportsManager to output the collected
**    data into an SQLite format.
** log.cpp: singleton class available to push messages and errors to
**    file.
** ***************************************************************** */

///convert int i to string
char* StringValue(char* sDestination, int i)
{
   #ifdef _WIN32
     _snprintf(sDestination, 255, "%i", i);
   #endif

   #ifndef _WIN32
     snprintf(sDestination, 255, "%i", i);
   #endif

   return sDestination;
}

///convert long i to string
char* StringValue(char* sDestination, long i)
{
   #ifdef _WIN32
     _snprintf(sDestination, 255, "%ld", i);
   #endif

   #ifndef _WIN32
     snprintf(sDestination, 255, "%ld", i);
   #endif

   return sDestination;
}

///convert double f to string
char* StringValue(char* sDestination, double f)
{
   #ifdef _WIN32
      _snprintf(sDestination, 255, "%f", f);
   #endif

   #ifndef _WIN32
      snprintf(sDestination, 255, "%f", f);
   #endif

  return sDestination;
}

///convert float f to string
char* StringValue(char* sDestination, float f)
{
   #ifdef _WIN32
      _snprintf(sDestination, 255, "%f", f);
   #endif

   #ifndef _WIN32
      snprintf(sDestination, 255, "%f", f);
   #endif

  return sDestination;
}

///check if a file exists
bool Exists(const char* sFilename)
{
   bool flag = false;
   fstream fin;
   fin.open(sFilename,ios::in);
   if( fin.is_open() )
      flag=true;
   fin.close();

   return flag;
}

/* ********************************************************************
** Method:   ReplaceChar()
** Purpose:  Custon routine, to inject a string at the position the
**           delimiter is found.  Method makes manipulation in memory
**           caller must ensure that the str is large enough.
** Scope:    Public method
** Params:   str - pointer of a string (must be large enough and
                                        nul terminated)
**           insert - pointer to a string(not necessarily nul term)
**           start - start position -of the inserted string
**           end - end position - of the inserted string
**           cDelimiter - delimiter to find and replace (only first occurance
                                                         is replace)
** Returns:  to pointer to str
** Author:   Claude Lamarche
** Mod Date: 2011-07-14
** ***************************************************************** */
char* ReplaceChar(char *str,const char *insert, int start, int end, char cDelimiter)
{
   int strlenght, pchlenght, insertlenght;
   char *pch;

   insertlenght = end - start;
   strlenght = strlen(str);

   //Get the delimiter address and it distance from the start of the string
   pch = (char*)memchr(str,cDelimiter,strlenght);

   pchlenght = pch-str;

   //create a space for the insert, sizeof of the insert
   memmove(pch+insertlenght-1,pch,strlenght-pchlenght+1);

   //insert the characters
   while(insertlenght-- >0)
   {
      str[pchlenght+insertlenght] = insert[start+insertlenght];
   }

   return str;
}



extern "C"
{
   //These are the calls advertised to Fortran. Note that they are all
   //lowercase and end in an underscore. Calls from Fortran must omit
   //the trailing underscore
   /* ********************************************************************
   ** Method:   rep_set_parameter__
   ** Purpose:  Called by the fortran code to set report parameters
   ** Params:   sParamName - param name
   **           sParamValue - param's value
   ** Returns:  Nothing
   ** ***************************************************************** */
   void rep_set_parameter__(char *sParamName,char *sParamValue,int sNameLength,int sValueLength)
   {
      std::string paramName =std::string(sParamName, sNameLength);
      std::string paramValue =std::string(sParamValue, sValueLength);

      TReportsManager::Instance()->SetReportParameter(paramName, paramValue);
   }
   //dummy routine see __ equivalent
   void rep_set_parameter_(char *sParamName,char *sParamValue,int sNameLength,int sValueLength)
   {
      rep_set_parameter__(sParamName, sParamValue, sNameLength, sValueLength);
   }


   /* ********************************************************************
   ** Method:   bh3k_rep_enabled
   ** Purpose:  Called by the fortran code to determine if this reporting
   **           output is enabled or not
   ** Params:   None
   ** Returns:  true/false
   ** ***************************************************************** */
   bool bh3k_rep_enabled__(){
      return TReportsManager::Instance()->ReportsEnabled();
   }
   bool bh3k_rep_enabled_(){return bh3k_rep_enabled__();}


   /* ********************************************************************
   ** Method:   h3k_enable_reports
   ** Purpose:  Called by the fortran code to enable/disable reporting
   ** Params:   true/false
   ** Returns:  N/A
   ** ***************************************************************** */
   void h3k_enable_reports__( bool& bNewStatus ){
       TReportsManager::Instance()->EnableReports(bNewStatus);
       return;
   }
   void h3k_enable_reports_( bool& bNewStatus )
   {h3k_enable_reports__( bNewStatus );}

  /**
   * return a boolean indicating H3kreports is active
   */
  bool rep_xmlstatus__(){
    return true;
  }
  bool rep_xmlstatus_()
  {
    return rep_xmlstatus__();
  }


   /* ********************************************************************
   ** Method:   rep_report_config
   ** Purpose:  Called by the fortran code to retrieve a report
   **           configuration parameter
   ** Params:   sValue - returned char*
   **           iValLength - returned char*'s length
   **           sParam - param to query
   **           iNameLength - param's length
   ** Returns:  the value of the passed configuration (sValue)
   ** Mod Date: 2011-09-09
   ** ***************************************************************** */
   void rep_report_config__(char* sValue, int iValLength, char *sParam, int iNameLength){
      int iPos;
      std::string paramValue;
      std::string paramName;

      paramName = std::string(sParam, iNameLength);

      paramValue = TReportsManager::Instance()->ReportConfig(paramName);

      strncpy (sValue, paramValue.c_str(),iValLength);

      for (iPos = paramValue.size(); iPos<=iValLength; iPos++){
         sValue[iPos]=' ';
      }

      return;
   }
   void rep_report_config_(char* sValue, int iValLength, char *sParam, int iNameLength)
   {rep_report_config__(sValue, iValLength, sParam, iNameLength); }


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
   bool rep_report_list_(char *sType, char *sSheet, int iTypeLength, int iSheetLength)
   {return rep_report_list__(sType, sSheet,iTypeLength, iSheetLength);}

   /**
    * Update the value of a configuration parameter
    *
    */
    void rep_update_config__(char* sParam, char* sValue, int iNameLength, int iValLength){

      std::string paramName = std::string(sParam, iNameLength);
      std::string paramValue = std::string(sValue, iValLength);

      TReportsManager::Instance()->UpdateConfig(paramName, paramValue);
    }
    void rep_update_config_(char* sParam, char* sValue, int iNameLength, int iValLength)
    {
       rep_update_config__(sParam, sValue, iNameLength, iValLength);
    }

   /**
    * Toggle the status of an on-off configuration parameter
    */
    bool rep_toggle_config__(char* sParam, int iNameLength){

      std::string paramName = std::string(sParam, iNameLength);

      return TReportsManager::Instance()->ToggleConfig(paramName);
    }
    bool rep_toggle_config_(char* sParam, int iNameLength)
    {
       return rep_toggle_config__(sParam, iNameLength);
    }

   /**
    * Report the status of an on-off configuration parameter
    */
    bool rep_bool_config__(char* sParam, int iNameLength){

      std::string paramName = std::string(sParam, iNameLength);

      return TReportsManager::Instance()->ReportBoolConfig(paramName);
    }
    bool rep_bool_config_(char* sParam, int iNameLength)
    {
      return rep_bool_config__(sParam, iNameLength);
    }

    /**
     *   Write out configuration file with new options
     */
    void rep_update_config_file__(){

      TReportsManager::Instance()->UpdateConfigFile();

    }
     void rep_update_config_file_()
    {
      rep_update_config_file__();
    }

    /**
     *  Clean up extra files
     */
    void rep_cleanup_files__(){

      TReportsManager::Instance()->Cleanup();
    }
    void rep_cleanup_files_(){
      rep_cleanup_files__();
    }

   /* ********************************************************************
   ** Method:   add_to_report
   ** Purpose:  Called by the fortran code to pass data value for a
   **           specified variable
   ** Params:   iIdentifier - pointer to a fortran integer*4
   **           fValue - pointer to a fortran real
   ** Returns:  N/A
   ** Author:   Claude Lamarche
   ** Mod Date: 2011-07-15
   ** ***************************************************************** */
   void add_to_report__(int* iIdentifier, float* fValue){
      TReportsManager::Instance()->AddToReportDataList(*iIdentifier,"",*fValue);
   }
   //Dummy routine see __ equivalent
   void add_to_report_(int* iIdentifier, float* fValue){
      add_to_report__(iIdentifier,fValue);
   }

   /* ********************************************************************
   ** Method:   add_to_report_wild3_
   ** Purpose:  Called by the Fortran code to pass data value for a
   **           specified variable
   ** Params:   iIdentifier - pointer to a fortran integer*4
   **           fValue - pointer to a fortran real
   **           sWild_ - pointers to fortran strings
   **           strLen_ - lenght of the passed in strings
   ** Note:     It's important to ensure that the strings are sent from
   **           Fortran without spaces (correctly allocated).  The trim
   **           routine could be added to the corresponding h3kmodule.f90
   **           wrapper if this is problematic.  This was left out for
   **           speed reasons
   ** Returns:  N/A
   ** Author:   Claude Lamarche
   ** Mod Auth: Achim Geissler
   ** Mod Date: 2014-05-12
   ** ***************************************************************** */
   void add_to_report_wild3__(int* iIdentifier,float* fValue,char* sWild1,
                          char* sWild2,char* sWild3,int strLen1,
                          int strLen2,int strLen3){
      char temp[strLen1+strLen2+strLen3+3];

      /* extract actual string length (non-NULL characters) for
        switching "zone_*" and "<<zone-name>>" output!
       */
      char * pch;
      int strlen_mod;

      strlen_mod=strLen1;
      pch=strchr(sWild1,' ');
      if(pch!=NULL) {
        strlen_mod=pch-sWild1;
      }

     /* strLen1 should only be shortened, never made longer!
      */
      if (strlen_mod < strLen1) {
        strLen1 = strlen_mod;
      }

      strlen_mod=strLen2;
      pch=strchr(sWild2,' ');
      if(pch!=NULL) {
        strlen_mod=pch-sWild2;
      }

     /* strLen2 should only be shortened, never made longer!
      */
      if (strlen_mod < strLen2) {
        strLen2 = strlen_mod;
      }

      strlen_mod=strLen3;
      pch=strchr(sWild3,' ');
      if(pch!=NULL) {
        strlen_mod=pch-sWild3;
      }

     /* strLen3 should only be shortened, never made longer!
      */
      if (strlen_mod < strLen3) {
        strLen3 = strlen_mod;
      }

      //Kludge: quick way to build a * delimited string with the params
      memcpy(temp,sWild1,strLen1);
      temp[strLen1] = '*';
      memcpy(temp+strLen1+1,sWild2,strLen2);
      temp[strLen1+strLen2+1] = '*';
      memcpy(temp+strLen1+strLen2+2,sWild3,strLen3);
      temp[strLen1+strLen2+strLen3+2] = '\0';

      TReportsManager::Instance()->AddToReportDataList(*iIdentifier,temp,*fValue);
   }
   void add_to_report_wild3_(int* iIdentifier,float* fValue,char* sWild1,char* sWild2,
                          char* sWild3,int strLen1,int strLen2,int strLen3){
      add_to_report_wild3__(iIdentifier,fValue,sWild1,sWild2,sWild3,strLen1,strLen2,strLen3);
   }

   /* ********************************************************************
   ** Method:   add_to_report_wild2_
   ** Purpose:  Called by the Fortran code to pass data value for a
   **           specified variable
   ** Params:   iIdentifier - pointer to a fortran integer*4
   **           fValue - pointer to a fortran real
   **           sWild_ - pointers to fortran strings
   **           strLen_ - lenght of the passed in strings
   ** Note:     It's important to ensure that the strings are sent from
   **           Fortran without spaces (correctly allocated).  The trim
   **           routine could be added to the corresponding h3kmodule.f90
   **           wrapper if this is problematic.  This was left out for
   **           speed reasons
   ** Returns:  N/A
   ** Author:   Claude Lamarche
   ** Mod Auth: Achim Geissler
   ** Mod Date: 2014-05-12
   ** ***************************************************************** */
   void add_to_report_wild2__(int* iIdentifier,float* fValue,char* sWild1,
                          char* sWild2,int strLen1,int strLen2){

      char temp[strLen1+strLen2+2];

     /* extract actual string length (non-NULL characters) for 
        switching "zone_*" and "<<zone-name>>" output!
      */
      char * pch;
      int strlen_mod;

      strlen_mod=strLen1;
      pch=strchr(sWild1,' ');
      if(pch!=NULL) {
        strlen_mod=pch-sWild1;
      }

     /* strLen1 should only be shortened, never made longer!
      */
      if (strlen_mod < strLen1) {
        strLen1 = strlen_mod;
      }

      strlen_mod=strLen2;
      pch=strchr(sWild2,' ');
      if(pch!=NULL) {
        strlen_mod=pch-sWild2;
      }

     /* strLen2 should also only be shortened, never made longer!
      */
      if (strlen_mod < strLen2) {
        strLen2 = strlen_mod;
      }

      //Kludge: quick way to build a * delimited string with the params
      memcpy(temp,sWild1,strLen1);
      temp[strLen1] = '*';
      memcpy(temp+strLen1+1,sWild2,strLen2);
      temp[strLen1+strLen2+1] = '\0';

      TReportsManager::Instance()->AddToReportDataList(*iIdentifier,temp,*fValue);
   }
   void add_to_report_wild2_(int* iIdentifier,float* fValue,char* sWild1,
                             char* sWild2,int strLen1,int strLen2){
      add_to_report_wild2__(iIdentifier,fValue,sWild1,sWild2,strLen1,strLen2);
   }


   /* ********************************************************************
   ** Method:   add_to_report_wild1_
   ** Purpose:  Called by the Fortran code to pass data value for a
   **           specified variable
   ** Params:   iIdentifier - pointer to a fortran integer*4
   **           fValue - pointer to a fortran real
   **           sWild_ - pointers to fortran strings
   **           strLen_ - lenght of the passed in strings
   ** Note:     It's important to ensure that the strings are send from
   **           Fortran without spaces (correctly allocated).  The trim
   **           routine could be added to the corresponding h3kmodule.f90
   **           wrapper if this is problematic.  This was left out for
   **           speed reasons
   ** Returns:  N/A
   ** Author:   Claude Lamarche
   ** Mod Auth: Achim Geissler
   ** Mod Date: 2014-05-12
   ** ***************************************************************** */
   void add_to_report_wild1__(int* iIdentifier,float* fValue,char* sWild1,int strLen1){
      char temp[strLen1+1];

     /* extract actual string length (non-NULL characters) for
        switching "zone_*" and "<<zone-name>>" output!
      */
      char * pch;
      int strlen_mod;

      strlen_mod=strLen1;
      pch=strchr(sWild1,' ');
      if(pch!=NULL) {
        strlen_mod=pch-sWild1;
      }

     /* strLen1 should only be shortened, never made longer!
      */
      if (strlen_mod < strLen1) {
        strLen1 = strlen_mod;
      }

      //Kludge: quick way to build a string with the params
      memcpy(temp,sWild1,strLen1);
      temp[strLen1] = '\0';

      TReportsManager::Instance()->AddToReportDataList(*iIdentifier,temp,*fValue);
   }
   void add_to_report_wild1_(int* iIdentifier,float* fValue,char* sWild1,int strLen1){
      add_to_report_wild1__(iIdentifier,fValue,sWild1,strLen1);
   }


   /* ********************************************************************
   ** Method:   add_to_report_details
   ** Purpose:  Enables fortran to set variable's metadata details.
   ** Params:   iIdentifier - pointer to a fortran integer*4
   **           cUnit - pointer to fortran unit string "units"
   **           cType - pointer to the fortran type string "(w)(V)(oC)..."
   **           cDescription - pointer to the fortran description string
   **           strLen1,2,3 - lenght of the passed in strings
   ** Note:     Only the first sent details to the specified variable will
   **           be kept.  These routines, are less
   **           efficient then when you specify the details in the h3kmodule.f90.
   **           ** Avoid the use of these methods where possible. **
   ** Returns:  N/A
   ** Author:   Claude Lamarche
   ** Mod Date: 2011-08-30
   ** ***************************************************************** */
   void add_to_report_details__(int* iIdentifier,char* cUnit,char* cType,char* cDescription,
                               int strLen1, int strLen2, int strLen3)
   {
      string sUnit, sType, sDescription;

      //only set the first time per variable, all after are ignore
      if(!TReportsManager::Instance()->IsReportDetailWildSet(*iIdentifier,""))
      {
         sUnit = string(cUnit, strLen1);
         sType = string(cType, strLen2);
         sDescription = string(cDescription, strLen3);

         TReportsManager::Instance()->AddToReportDetails(*iIdentifier,"",sUnit,sType,sDescription);
      }
   }
   void add_to_report_details_(int* iIdentifier,char* cUnit,char* cType,char* cDescription,
                               int strLen1, int strLen2, int strLen3)
   {
      add_to_report_details__(iIdentifier,cUnit,cType,cDescription,strLen1, strLen2, strLen3);
   }


   /* ********************************************************************
   ** Method:   add_to_report_details_wild1
   ** Purpose:  Enables fortran to set variable's metadata details.
   ** Params:   iIdentifier - pointer to a fortran integer*4
   **           cWild1 - pointer to fortran string, wild identifier
   **           cUnit - pointer to fortran unit string "units"
   **           cType - pointer to the fortran type string "(w)(V)(oC)..."
   **           cDescription - pointer to the fortran description string
   **           strLen1,2,3,4 - lenght of the passed in strings
   ** Note:     Only the first sent details to the specified variable will
   **           be kept.  These routines, are less efficient then when you
   **           specify the details in the h3kmodule.f90.
   **           ** Avoid the use of these methods where possible. **
   ** Returns:  N/A
   ** Author:   Claude Lamarche
   ** Mod Date: 2011-08-30
   ** ***************************************************************** */
   void add_to_report_details_wild1__(int* iIdentifier,char* cWild1, char* cUnit,char* cType,char* cDescription,
                               int strLen1, int strLen2, int strLen3,int strLen4)
   {
      char temp[strLen1+1];
      string sUnit, sType, sDescription;

      //Kludge: quick way to build a string with the params
      memcpy(temp,cWild1,strLen1);
      temp[strLen1] = '\0';

      //only set the first time per variable, all after are ignore
      if(!TReportsManager::Instance()->IsReportDetailWildSet(*iIdentifier,temp))
      {
         sUnit = string(cUnit, strLen2);
         sType = string(cType, strLen3);
         sDescription = string(cDescription, strLen4);

         TReportsManager::Instance()->AddToReportDetails(*iIdentifier,temp,sUnit,sType,sDescription);
      }
   }
   void add_to_report_details_wild1_(int* iIdentifier, char* cWild1, char* cUnit,char* cType,char* cDescription,
                               int strLen1, int strLen2, int strLen3, int strLen4)
   {
      add_to_report_details_wild1__(iIdentifier,cWild1,cUnit,cType,cDescription,strLen1, strLen2, strLen3,strLen4);
   }

   /* ********************************************************************
   ** Method:   add_to_report_details_wild2
   ** Purpose:  Enables fortran to set variable's metadata details.
   ** Params:   iIdentifier - pointer to a fortran integer*4
   **           cWild1 - pointer to fortran string, wild identifier
   **           cWild2 - pointer to fortran string, wild identifier
   **           cUnit - pointer to fortran unit string "units"
   **           cType - pointer to the fortran type string "(w)(V)(oC)..."
   **           cDescription - pointer to the fortran description string
   **           strLen1,2,3,4,5 - lenght of the passed in strings
   ** Note:     Only the first sent details to the specified variable will
   **           be kept.  These routines, are less efficient then when you
   **           specify the details in the h3kmodule.f90.
   **           ** Avoid the use of these methods where possible. **
   ** Returns:  N/A
   ** Author:   Claude Lamarche
   ** Mod Date: 2011-08-30
   ** ***************************************************************** */
   void add_to_report_details_wild2__(int* iIdentifier,char* cWild1,char* cWild2, char* cUnit,char* cType,char* cDescription,
                               int strLen1, int strLen2, int strLen3,int strLen4,int strLen5)
   {
      char temp[strLen1+strLen2+2];
      string sUnit, sType, sDescription;

      //Kludge: quick way to build a string with the params
      memcpy(temp,cWild1,strLen1);
      temp[strLen1] = '*';
      memcpy(temp+strLen1+1,cWild2,strLen2);
      temp[strLen1+strLen2+1] = '\0';

      //only set the first time per variable, all after are ignore
      if(!TReportsManager::Instance()->IsReportDetailWildSet(*iIdentifier,temp))
      {
         sUnit = string(cUnit, strLen3);
         sType = string(cType, strLen4);
         sDescription = string(cDescription, strLen5);

         TReportsManager::Instance()->AddToReportDetails(*iIdentifier,temp,sUnit,sType,sDescription);
      }
   }
   void add_to_report_details_wild2_(int* iIdentifier, char* cWild1, char* cWild2, char* cUnit,char* cType,char* cDescription,
                               int strLen1, int strLen2, int strLen3, int strLen4, int strLen5)
   {
      add_to_report_details_wild2__(iIdentifier,cWild1,cWild2,cUnit,cType,cDescription,strLen1, strLen2, strLen3,strLen4,strLen5);
   }


   /* ********************************************************************
   ** Method:   add_to_report_details_wild3
   ** Purpose:  Enables fortran to set variable's metadata details.
   ** Params:   iIdentifier - pointer to a fortran integer*4
   **           cWild1 - pointer to fortran string, wild identifier
   **           cWild2 - pointer to fortran string, wild identifier
   **           cWild3 - pointer to fortran string, wild identifier
   **           cUnit - pointer to fortran unit string "units"
   **           cType - pointer to the fortran type string "(w)(V)(oC)..."
   **           cDescription - pointer to the fortran description string
   **           strLen1,2,3,4,5,6 - lenght of the passed in strings
   ** Note:     Only the first sent details to the specified variable will
   **           be kept.  These routines, are less efficient then when you
   **           specify the details in the h3kmodule.f90.
   **           ** Avoid the use of these methods where possible. **
   ** Returns:  N/A
   ** Author:   Claude Lamarche
   ** Mod Date: 2011-08-30
   ** ***************************************************************** */
   void add_to_report_details_wild3__(int* iIdentifier,char* cWild1,char* cWild2, char* cWild3,char* cUnit,char* cType,char* cDescription,
                               int strLen1, int strLen2, int strLen3,int strLen4,int strLen5,int strLen6)
   {
      char temp[strLen1+strLen2+strLen3+3];
      string sUnit, sType, sDescription;

      //Kludge: quick way to build a string with the params
      memcpy(temp,cWild1,strLen1);
      temp[strLen1] = '*';
      memcpy(temp+strLen1+1,cWild2,strLen2);
      temp[strLen1+strLen2+1] = '*';
      memcpy(temp+strLen1+strLen2+2,cWild3,strLen3);
      temp[strLen1+strLen2+strLen3+2] = '\0';

      //only set the first time per variable, all after are ignore
      if(!TReportsManager::Instance()->IsReportDetailWildSet(*iIdentifier,temp))
      {
         sUnit = string(cUnit, strLen4);
         sType = string(cType, strLen5);
         sDescription = string(cDescription, strLen6);

         TReportsManager::Instance()->AddToReportDetails(*iIdentifier,temp,sUnit,sType,sDescription);
      }
   }
   void add_to_report_details_wild3_(int* iIdentifier, char* cWild1, char* cWild2, char* cWild3, char* cUnit,char* cType,char* cDescription,
                               int strLen1, int strLen2, int strLen3, int strLen4, int strLen5, int strLen6)
   {
      add_to_report_details_wild3__(iIdentifier,cWild1,cWild2,cWild3,cUnit,cType,cDescription,strLen1, strLen2, strLen3,strLen4,strLen5,strLen6);
   }


   /* ********************************************************************
   ** Method:   set_report_simulation_info
   ** Purpose:  Called by the fortran code to pass the simulation
   **           information
   ** Params:   iStartday - start day in a 365 day year format
   **                        (example:  Feb 1st --> 32) **no leap year **
   **           iEndDay -  end day in a 365 day year format
   **                        (example: Dec 31st --> 365) **no leap year **
   **           iTimeStep - number of time step per hour
   **                        (example: 12 --> 5 minute time step (60/12))
   ** Returns:  N/A
   ** Author:   Claude Lamarche
   ** Mod Date: 2011-07-20
   ** ***************************************************************** */
   void set_report_simulation_info__(int* iStartDay, int* iEndDay,int* iTimeStep){
      TReportsManager::Instance()->SetSimulationInfo(*iStartDay,*iEndDay,*iTimeStep);
   }
   void set_report_simulation_info_(int* iStartDay, int* iEndDay,int* iTimeStep){
      set_report_simulation_info__(iStartDay,iEndDay,iTimeStep);
   }


   /* ********************************************************************
   ** Method:   set_report_variable()
   ** Purpose:  Called by the fortran code to pass the Report Variable
   **           definition, id, description, to the C++ containers
   ** Params:   sVariableName - The Unique xml "path location" (the value of '%VariableName'
                                as defined in h3kmodule.f90) to where the data is to be stored.
                sMetaName -  The metatag to be added. (usually 'units' is used)
                sMetaValue - The value for the above metatag (Usually the unit type, like
                     (W) for watts)
                sDescription - A detailed description of the variable being reported on.
                     Will be outputted with the dictionary.
                *Length - The length of the corrosponding char arrays.
   ** Returns:  N/A, changes the information in memory
   ** Author:   Claude Lamarche
   ** Mod Date: 2011-06-27
   ** ***************************************************************** */
   void set_report_variable__(int * iIdentifier,bool* bEnabled,int* iVariableNameLength,
                          int* iMetatypeLength,int* iMetaValueLength,int* iDescriptionLength,
                          char* sVariableName,char* sMetaType,char* sMetaValue,
                          char* sDescription,int strLen1,int strLen2,int strLen3,
                          int strLen4){
      static int iVarIdentity = 1;

      //This will terminate the fortran string with NUL character
      //Making this character array in memory a valid c format.

      sVariableName[*iVariableNameLength] = '\0';
      sMetaType[*iMetatypeLength] = '\0';
      sMetaValue[*iMetaValueLength] = '\0';
      sDescription[*iDescriptionLength] = '\0';

      //Add variable into storage container
      TReportsManager::Instance()->AddToVariableInfoList(iVarIdentity,sVariableName,sMetaType,
                                                         sMetaValue,sDescription);

      //Kludge: dynamic generation of identifiers used on the c++ & fortran side
      *iIdentifier = iVarIdentity;

      //Kludge: this enabled the fortran side to see if a variable is enabled
      //*bEnabled = TReportsManager::Instance()->IsVariableEnable(iVarIdentity);

      //Increment static identifier
      iVarIdentity++;
   }

   //dummy call to set_report_variable__
   void set_report_variable_(int * iIdentifier,bool* bEnabled,int* iVariableNameLength,
                          int* iMetatypeLength,int* iMetaValueLength,int* iDescriptionLength,
                          char* sVariableName,char* sMetaType,char* sMetaValue,
                          char* sDescription,int strLen1,int strLen2,int strLen3,
                          int strLen4){
      set_report_variable__(iIdentifier,bEnabled,
            iVariableNameLength,iMetatypeLength,iMetaValueLength,iDescriptionLength,
            sVariableName, sMetaType, sMetaValue, sDescription,
            strLen1,strLen2,strLen3,strLen4);
   }


   /* ********************************************************************
   ** Method:   set_var_additional_info()
   ** Purpose:  Called by the Fortran code to sets extra flags for
   **           identifying variables.  This is only used by quickrun at the
   **           moment
   ** Params:   iIdentifier - variable identifier number
   **           iPropertyNum - hardcoded between fortran and c++ property
   **                         number, see table below
   **           bValue - value to set
   ** Returns:  N/A
   ** Author:   Claude Lamarche
   ** Mod Date:2012-02-03
   **
   **    Property Table **
   **    ___________________
   **   | 1   |   scale_htg |
   **   | 2   |   scale_clg |
   **   | 3   |   scale_gen |
   **    -------------------
   ** ***************************************************************** */
   void set_var_additional_info__(int *iIdentifier,int *iPropertyNum,bool *bValue)
   {
      TReportsManager::Instance()->SetVarAdditionalInfo(*iIdentifier,*iPropertyNum,*bValue);
   }
   void set_var_additional_info_(int *iIdentifier,int *iPropertyNum,bool *bValue)
   {
      set_var_additional_info__(iIdentifier,iPropertyNum,bValue);
   }


   /* ********************************************************************
   ** Method:   generate_output()
   ** Purpose:  Called by the fortran code to start the report generation
   ** Scope:    Public
   ** Params:   sRootName = root name of simulation results file
   **           iNameLength = length of root name
   ** Returns:  N/A
   ** Author:   Claude Lamarche
   ** Mod Date: 2012-02-03
   ** ***************************************************************** */
   void generate_output__( char *sRootName, int iNameLength )
   {
        TReportsManager::Instance()->GenerateOutput();
   }
   //dummy call to the generate_output__
   void generate_output_( char *sRootName, int iNameLength )
   {
        generate_output__(sRootName, iNameLength);
   }

   /* ********************************************************************
   ** Method:   set_report_filename()
   ** Purpose:  Called by the fortran code to set the report csv file name.
   ** Scope:    Public
   ** Params:   sRootName = root name of simulation results file
   **           iNameLength = length of root name
   ** Returns:  N/A
   ** Author:   Achim Geissler
   ** Mod Date: 2018-11-24
   ** ***************************************************************** */
   void set_report_filename__( char *sRootName, int iNameLength )
   {
        std::string sFileName;
        std::string sRoot = std::string(sRootName, iNameLength);
        sFileName = sRoot + ".csv";

        TReportsManager::Instance()->setCSVFileName(sFileName);
   }
   //dummy call to set_report_filename__
   void set_report_filename_( char *sRootName, int iNameLength )
   {
        set_report_filename__(sRootName, iNameLength);
   }


   /* ********************************************************************
   ** Method:   report_next_time_step()
   ** Purpose:  Called by the fortran code to indicate the start of the
   **           next time step and information about that timestep
   ** Scope:    Public
   ** Params:   iStep - step number
   **           iHour = TimeStep Hour
   **           iMinute = TimeStep Minute... may not be needed
   **           iDay = TimeStep Day
   **           iStartup = Startup indicator (logical in Fortran == int in C++)
   ** Returns:  N/A
   ** Author:   Claude Lamarche
   ** Mod Date: 2011-07-14
   ** Mod Date: 2013-13-16 - added iQrun to function parameters
   ** ***************************************************************** */
   void report_next_time_step__(int *iStep,int *iHour,int *iDay,int *iStartup, int *iQrun)
   {
      //Call ReportManager routine to store the time step data
      TReportsManager::Instance()->AddToTimeStepList((*iStartup)?true:false, *iStep, *iDay, *iHour, *iQrun);
   }
   //dummy routine, calls the __
   void report_next_time_step_(int *iStep,int *iHour,int *iDay,int *iStartup, int *iQrun)
   {
      report_next_time_step__(iStep,iHour,iDay,iStartup, iQrun);
   }


   /* ********************************************************************
   ** Method:   report_new_season()
   ** Purpose:  Called by the fortran code to indicate the start of
   **           a new season, used for QuickRun mode only
   ** Scope:    Public
   ** Params:   iSeason_index  - season index
   **           fHtgMultiplier - Htg Multiplier for season
   **           fClgMultiplier - Clg Multiplier for season
   **           fGenMultiplier - Gen Multiplier for season
   ** Returns:  N/A
   ** Author:   Claude Lamarche
   ** Mod Date: 2012-02-03
   ** ***************************************************************** */
   void report_new_season__(int* iSeason_index, float* fHtgMultiplier,
                        float* fClgMultiplier, float* fGenMultiplier )
   {
      TReportsManager::Instance()->AddNewSeason(*iSeason_index,*fHtgMultiplier,
                                                *fClgMultiplier, *fGenMultiplier);
   }
   void report_new_season_(int* iSeason_index, float* fHtgMultiplier,
                        float* fClgMultiplier, float* fGenMultiplier )
   {
      report_new_season__(iSeason_index,fHtgMultiplier,fClgMultiplier,fGenMultiplier);
   }


   /* ********************************************************************
   ** Method:   is_variable_enabled
   ** Purpose:  Called by the fortran code to query the report variables
   **           to see if the passed in pattern relates to at least one
   **           enabled variable.  This will b used by esp-r for avoiding
   **           sections of code, if h3k_reports does not report on these
   **           variables.  Comparison performs a string match between the
   **           passed string and variable name defined in the h3kmodule.
   **           The match is not a regular expression comparison.
   ** Scope:    Public
   ** Params:   sPatter - pattern to verify
   **           iLength - length of pattern string
   ** Returns:  true/false if enabled
   ** Author:   Claude Lamarche
   ** Mod Date: 2011-10-04
   ** ***************************************************************** */
   bool is_variable_enabled__(char* sPattern, int iLength)
   {
      std::string strPattern = std::string(sPattern, iLength);

      return true; //TReportsManager::Instance()->IsVariableEnable(strPattern.c_str());
   }
   bool is_variable_enabled_(char* sPattern, int iLength)
   {
      return is_variable_enabled__(sPattern, iLength);
   }
}




/* ********************************************************************
** Class:   TReportsManager
** Purpose: Main driver of the H3KReports.
** Used by: esrubps/bmatsv.F
** Error handling: to be implemented...
** ***************************************************************** */
TReportsManager* TReportsManager::ptr_Instance = NULL;
/* ********************************************************************
** Method:   Instance
** Scope:    Public
** Purpose:  Creates a singleton instance of the TReportManager
** Params:   N/A
** Returns:  N/A
** ***************************************************************** */
TReportsManager* TReportsManager::Instance(  )
{
   if(ptr_Instance == NULL) //create a new instance
      ptr_Instance = new TReportsManager();

   return ptr_Instance;
}

/* ********************************************************************
** Method:   TReportsManager
** Scope:    Private
** Purpose:  Constructor, initialize params
** Params:   N/A
** Returns:  N/A
** ***************************************************************** */
TReportsManager::TReportsManager(  )
{
   //Initialize members
   m_lCurrentStep = 0;
   m_bStartUp = true;
   m_lActiveSteps = 0;
   m_iActualStartDay = 0;
   m_iCurrentMonthIndex = 0;
   m_fMinutePerTimeStep = 0; //set by bps
   m_iTimeStepPerHour = 0; //set by bps
   m_lOutputStepCount = 0;
   m_iStartMonthIndex = 0;
   m_iStartSeasonIndex = 0;
   m_lSaveToDisk = 0; //set by input.xml
   m_AnnualBinStepCount = 0;
   m_iYearCount = 0;
   bReports_Enabled = false;
   m_bSeasonalRun = false;
   bUseResFilenameRoot = false;
   bUseZoneNames = false;
   bUseSurfaceNames = false;
   bOutputGeomDat = false;

   //remove the out.csv and out.db3 on init since the save_to_disk
   //option will append to file and database as the simulation runs
   remove("out.csv");
   remove("out.db3");

   ParseConfigFile("input.xml"); //default input file

   SetFlags();                   //set config flags & defaults


}

/* ********************************************************************
** Method:   ~TReportsManager
** Scope:    Private
** Purpose:  Desctructor, called by the Cleanup routine
** Params:   N/A
** Returns:  N/A
** ***************************************************************** */
//Destructor -- called by the Cleanup routine.
TReportsManager::~TReportsManager()
{
}

/* ********************************************************************
** Method:   SetSimulationInfo
** Scope:    public
** Purpose:  Calculates and stores the simulation data
** Params:   iStartday - start day in a 365 day year format
**                        (example:  Feb 1st --> 32) **no leap year **
**           iEndDay -  end day in a 365 day year format
**                        (example: Dec 31st --> 365) **no leap year **
**           iTimeStep - number of time step per hour
**                        (example: 12 --> 5 minute time step (60/12))
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-07-20
** ***************************************************************** */
void TReportsManager::SetSimulationInfo(int iStartDay,int iEndDay,int iTimeStep){
   //Calculate the total number of steps the simulation will encounter
   m_iStartDay = iStartDay;
   m_iEndDay = iEndDay;
   m_iTimeStepPerHour = iTimeStep;
   m_iExpectedTimeSteps = (iEndDay - iStartDay +1) * iTimeStep * 24;
   m_fMinutePerTimeStep = (float)1.0 / (float)m_iTimeStepPerHour * (float)60.0;

   //perform some vector optimization
   m_TimeStepList.reserve(m_iExpectedTimeSteps);

   //auto save to disk to avoid gigabytes in memory, when there are more then
   //SAVE_TO_DISK_TRIGGER timesteps the option is turned on.
   if(m_iExpectedTimeSteps > SAVE_TO_DISK_TRIGGER && bSaveToDisk == false)
   {
      bSaveToDisk = true;
      m_lSaveToDisk = SAVE_TO_DISK_MAX;
   }
}


/* ********************************************************************
** Method:   AddToTimeStepList()
** Scope:    public
** Purpose:  Store the timestep relevent data.
** Params:   bStartup - true/false, startup step
**           iStep - unique step number
**           iDay - day number
**           iHour - hour
** Returns:  N/A
** Comments: This method will not check for duplicate time step, it is
**           expected that the fortran code will only call this once
**           per timestep during the simulation loop.
**
**           If you needed to set the bin size to something else then
**           Monthly, this is where you would control this.
** Author:   Claude Lamarche
** Mod Date: 2011-07-14
** Mod Date: 2013-03-16 - added iQrun to function parameters
** ***************************************************************** */
void TReportsManager::AddToTimeStepList(bool bStartup, int iStep, int iDay, int iHour, int iQrun){
   struct stTimeStep ts;
   static bool bOutofStartup = false;
   static bool bLastSimulationStarted = false;
   bool bSimulationStarted = !bStartup;
   int iBinIndex;

   ReportDataMap::iterator itDataMap;
   int iModulus;

   //If save_to_disk mode true, send to output every m_lSaveToDisk Timestep.
   //At this point we know that m_iCurrentStep or m_lActiveSteps have completed
   //the data insertion.
   if(bSaveToDisk)
   {
      //Get the modulus of the
      if(bReportStartup)
         iModulus = m_lCurrentStep % m_lSaveToDisk;
      else
        if (m_lActiveSteps > 0)
          iModulus = m_lActiveSteps % m_lSaveToDisk;
        else iModulus = 1;

      //if it's time to call the save to disk routine
      if(iModulus == 0 && iStep > m_lSaveToDisk)
      {
         (bReportStartup)?GenerateStepOutput(m_lCurrentStep):GenerateStepOutput(m_lActiveSteps);
      }
   }

   //Get the upcomming step information
   ts.Startup = m_bStartUp = bStartup;
   ts.Step = m_lCurrentStep = iStep;
   ts.Day = iDay;
   ts.Hour = iHour;

   //A year change is indicate by the iDay being smaller then the last one sent
   if(m_TimeStepList.size() > 0)
   {
      int ttmp;
      if(iDay < m_TimeStepList.back().Day)
      {
         m_iYearCount++;
      }
   }

   //For quick run mode, check for a change in the stargup flag.
   //Simulation either left startup mode or re-entered.
   if (iQrun == 1)
   {
        if(bReportStartup && bLastSimulationStarted == false)
        {
            for(itDataMap = m_ReportDataList.begin(); itDataMap != m_ReportDataList.end(); itDataMap++)
            {
                itDataMap->second.SetSimulationStarted(true);
            }
            bLastSimulationStarted = true;
        }
        else if(bLastSimulationStarted != bSimulationStarted)
        {
            for(itDataMap = m_ReportDataList.begin(); itDataMap != m_ReportDataList.end(); itDataMap++)
            {
                itDataMap->second.SetSimulationStarted(bSimulationStarted);
            }
            bLastSimulationStarted = bSimulationStarted;
        }
    }
    else // otherwise, execute only once when the simulation first exits startup
    {
        if(!bStartup || bReportStartup)
        {
            if(!bOutofStartup)
            {
                //Set the report variable that have already been initiated to simulation started true
                for(itDataMap = m_ReportDataList.begin(); itDataMap != m_ReportDataList.end(); itDataMap++)
                {
                    itDataMap->second.SetSimulationStarted(true);
                }
                bOutofStartup = true;
            }
        }        
    }

   //an active step
   if(!bStartup || bReportStartup)
   {
      //Increment Active Step Counter
      m_lActiveSteps++;
      //Store the actual first day data was recorded
      if (m_lActiveSteps == 1){
         m_iActualStartDay = iDay;
         m_iStartMonthIndex = GetMonthIndex(iDay + (m_iYearCount*365));
         m_iStartSeasonIndex = m_iCurrentSeasonIndex;
      }     
      
      //Store the current Time Step's month index;, ignore startup steps.
      m_iCurrentMonthIndex = GetMonthIndex(iDay + (m_iYearCount*365));

   }
   
   //Store the Bin data's total time step count
   if(m_bSeasonalRun)
      iBinIndex = m_iCurrentSeasonIndex -m_iStartSeasonIndex;
   else
      iBinIndex = m_iCurrentMonthIndex -m_iStartMonthIndex;

   while(m_BinStepCount.size() < iBinIndex +1)
   {
      m_BinStepCount.push_back(0); //creates bin if it does not exist
   }

   if(bReportStartup || !bStartup)
   {
      m_BinStepCount.back() = m_BinStepCount[iBinIndex] + 1;

      //Store the annual bin data's total time step count
      m_AnnualBinStepCount++;
   }

   //Push on vector
   m_TimeStepList.push_back(ts);

}


/* ********************************************************************
** Method:   AddToVariableInfoList()
** Scope:    public
** Purpose:  Accessor method to add a variable to the m_VariableInfoList
** Params:   int id - identifier for the variable
**           sVarName - prepared statement created in h3kmodule.f90
**           sMetaType - metaType initiated in h3kmodule.f90
**           sMetaValue - metaValue initiated in h3kmodule.f90
**           sDescription - variable description initiated in h3kmodule.f90
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-07-13
** ***************************************************************** */
void TReportsManager::AddToVariableInfoList(int id,const char* sVarName, const char* sMetaType,
                                 const char* sMetaValue, const char* sDescription){
   stVariableInfo rv;

   rv.VarName = sVarName;
   rv.MetaType = sMetaType;
   rv.MetaValue = sMetaValue;
   rv.Description = sDescription;
   rv.Multiplier = MULT_NONE;

   //insert the value to the map object
   m_VariableInfoList.insert(make_pair(id,rv));
}


/* ********************************************************************
** Method:   AddToReportDetails()
** Scope:    public
** Purpose:  Populate meta variable data
** Note:     ** avoid the use of this method where possible ** this method
**           is not as efficient as declaring the variable descriptor
**           in the h3kmodule.f90
** Params:   int id - identifier for the variable
**           sDelimiter - the wild char (makes up the map key with id)
**           sUnit - the unit type
**           sType - the variable tytpe
**           sDescription - the variable description
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-30
** ***************************************************************** */
void TReportsManager::AddToReportDetails(int id,const char* sDelimiter, const string& sUnit,
                                         const string& sType,const string& sDescription)
{
   ReportDataMap::iterator it;

   //find the variable
   it = m_ReportDataList.find(stMapKey(id,string(sDelimiter)));
   if(it != m_ReportDataList.end())
   {
      //populate the variables unit,type,description.
      it->second.SetVariableDescriptionWild(sUnit,sType,sDescription);
   }

   //if not found ignore, maybe error report too?
}


/* ********************************************************************
** Method:   IsReportDetailWildSet()
** Scope:    public
** Purpose:  Return true/flase, if the passed in identifier maps and a
**           variable where the description was reported as wild.
** Note:     ** avoid the use of this technique when possible **
** Params:   int id - identifier for the variable
**           sDelimiter - the wild char (makes up the map key with id)
** Returns:  true/false
** Author:   Claude Lamarche
** Mod Date: 2011-08-30
** ***************************************************************** */
bool TReportsManager::IsReportDetailWildSet(int id, const char* sDelimiter)
{
   bool bRtn = false;
   ReportDataMap::iterator it;

   //fetch the variable,
   it = m_ReportDataList.find(stMapKey(id,string(sDelimiter)));
   if(it != m_ReportDataList.end())
   {
      bRtn = it->second.IsVariableDescriptionWild();
   }

   return bRtn;
}


/* ********************************************************************
** Method:   AddToReportDataList()
** Scope:    public
** Purpose:  Accessor method to add data to the m_ReportDataList
** Params:   int id - identifier for the variable
**           sDelimiter - the wild char (makes up the map key with id)
**           fValue - value to store
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-07-15
** ***************************************************************** */
void TReportsManager::AddToReportDataList(int id, const char* sDelimiter, float fValue){
   ReportDataMap::iterator it;
   VariableInfoMap::const_iterator itInfoMap;
   int iBinIndex;


   //Find if it exits
   it = m_ReportDataList.find(stMapKey(id,string(sDelimiter)));
   if(it == m_ReportDataList.end()){
      //create and retrieve the new member
      m_ReportDataList.insert(make_pair(stMapKey(id,string(sDelimiter)),TReportData(!m_bStartUp)));
      it = m_ReportDataList.find(stMapKey(id,string(sDelimiter)));

      //used for performance enhancements
      if(bSaveToDisk)
         it->second.SetExpectedTimeSteps(m_lSaveToDisk);
      else
         it->second.SetExpectedTimeSteps(m_iExpectedTimeSteps);

      //used to figure out the month passed values belong too.
      it->second.SetSimulationStartDay(m_iActualStartDay);

      //used to set if we'll report on the Startup period or not.
      it->second.SetReportStartupPeriod(bReportStartup);

      //used to set if we are to apply time step averaging
      it->second.SetTimeStepAveraging(bTS_averaging);

      //populate some startup information that is found in the VariableInfo dictionary
      itInfoMap = m_VariableInfoList.find(id);
      if(itInfoMap != m_VariableInfoList.end()){

         //Create the fully qualified variable name
         GetVariableName(itInfoMap->second.VarName,sDelimiter,it->second.sVarName);
         
         //Determine if output is requested 
         it->second.OutputType = GetOutputType(it->second.sVarName); 
         
         //set output type
         it->second.SetOutputType(it->second.OutputType); 
         
         (it->second.OutputType&0xFF)? it->second.Enabled = true : it->second.Enabled = false;
         if ( it->second.Enabled ) {
          
         }

         
      }

      // **** used for troubleshooting only, code block will normally not be compiled *****
      #ifdef TRACE_VARIABLE
         //print all the variable names
         char sTraceTemp[512];

         sprintf(sTraceTemp,"ID:%5d; Delimiters:%15s; h3kmodule:%60s; derived:%s;",
                id,sDelimiter,itInfoMap->second.VarName, it->second.sVarName);
         log::Instance()->writeTrace(sTraceTemp);
      #endif

   }

   // **** used for troubleshooting only, code block will normally not be compiled *****
   #ifdef TRACE_VARIABLE
   if(TRACE_VARIABLE == id)
   {
      char sTraceTemp2[512];

      sprintf(sTraceTemp2,"From BPS: id:%5d; Value:%13f; CurrentStep:%4ld; ActiveSteps:%4ld; OutputStepCount:%4ld",
              id,fValue,m_lCurrentStep,m_lActiveSteps,m_lOutputStepCount);
      log::Instance()->writeTrace(sTraceTemp2);
   }
   #endif

   //Choose the bin index Monthly or Seasonal
   if(m_bSeasonalRun)
      iBinIndex = m_iCurrentSeasonIndex - m_iStartSeasonIndex;
   else
      iBinIndex = m_iCurrentMonthIndex-m_iStartMonthIndex;

   //Push the value to the storage bins,
   if(bReportStartup)
      it->second.AddValue(m_lCurrentStep-m_lOutputStepCount,iBinIndex,fValue);
   else
      it->second.AddValue(m_lActiveSteps-m_lOutputStepCount,iBinIndex,fValue);
}


/* ********************************************************************
** Method:   AddNewSeason
** Scope:    public
** Purpose:  Called by the fortran code for Quick run calculations, this
**           method captures and stores the season multipliers
** Params:   iSeason_index - season index
**           fHtgMultiplier - Htg multiplier
**           fClgMultiplier - Clg multiplier
**           fGenMultiplier - Gen multiplier
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2012-02-07
** ***************************************************************** */
void TReportsManager::AddNewSeason(int iSeason_index,float fHtgMultiplier,float fClgMultiplier, float fGenMultiplier)
{
   ReportDataMap::iterator itDataMap;

   //calling this method indicates the run is Quickrun
   m_bSeasonalRun = true;

   //Store index
   m_iCurrentSeasonIndex = iSeason_index;

   //Size the vector, will only pass there once
   if(m_SeasonMultipliersList.size() <  12)
   {
      m_SeasonMultipliersList.resize(12);
   }

    // store seasonal multiplication factors (remember, c arrays start at zero!
   m_SeasonMultipliersList[iSeason_index-1].Htg = fHtgMultiplier;
   m_SeasonMultipliersList[iSeason_index-1].Clg = fClgMultiplier;
   m_SeasonMultipliersList[iSeason_index-1].Gen = fGenMultiplier;
}

/* ********************************************************************
 ** Method:   UseResFilenameRoot
 ** Scope:    public
 ** Purpose:  Returns state of boolean
 ** Params:   N/A
 ** Returns:  boolean
 ** Author:   Achim Geissler
 ** Mod Date: 2013-12-21
 ** ***************************************************************** */
bool TReportsManager::UseResFilenameRoot(){
   return bUseResFilenameRoot;
}

/* ********************************************************************
 ** Method:   UseZoneNames
 ** Scope:    public
 ** Purpose:  Returns state of boolean
 ** Params:   N/A
 ** Returns:  boolean
 ** Author:   Achim Geissler
 ** Mod Date: 2014-03-07
 ** ***************************************************************** */
bool TReportsManager::UseZoneNames(){
  return bUseZoneNames;
}

/* ********************************************************************
 ** Method:   UseSurfaceNames
 ** Scope:    public
 ** Purpose:  Returns state of boolean
 ** Params:   N/A
 ** Returns:  boolean
 ** Author:   Achim Geissler
 ** Mod Date: 2014-03-07
 ** ***************************************************************** */
bool TReportsManager::UseSurfaceNames(){
  return bUseSurfaceNames;
}

/* ********************************************************************
 ** Method:   OutputGeomDat
 ** Scope:    public
 ** Purpose:  Returns state of boolean
 ** Params:   N/A
 ** Returns:  boolean
 ** Author:   Achim Geissler
 ** Mod Date: 2019-04-05
 ** ***************************************************************** */
bool TReportsManager::OutputGeomDat(){
  return bOutputGeomDat;
}

/* ********************************************************************
 ** Method:   setCSVFileName
 ** Scope:    public
 ** Purpose:  Sets CSV file name string, defaults to "out.csv"
 ** Params:   sFileName = file name string
 ** Returns:  N/A
 ** Author:   Achim Geissler
 ** Mod Date: 2013-12-21
 ** ***************************************************************** */
void TReportsManager::setCSVFileName(const std::string& sFileName){
    if (bUseResFilenameRoot) {
      sCSVFileName=sFileName;
    } else {
      sCSVFileName="out.csv";
    }
}

/* ********************************************************************
** Method:   SetVarAdditionalInfo
** Scope:    public
** Purpose:  Called by the fortran code, this add a property flag for
**           a variable.  Only used for quickrun for now.
** Params:   iIdentifier - variable number identifier
**           iPropertyNum
**           bValue - ignore for now, only called when true
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2012-02-07
**    Property Table **
   **    ___________________
   **   | 1   |   scale_htg |
   **   | 2   |   scale_clg |
   **   | 3   |   scale_gen |
   **    -------------------
** ***************************************************************** */
void TReportsManager::SetVarAdditionalInfo(int iIdentifier,int iPropertyNum,bool bValue)
{
   VariableInfoMap::iterator itInfoMap;

   itInfoMap = m_VariableInfoList.find(iIdentifier);
   if(itInfoMap != m_VariableInfoList.end())
   {
      //found the variable to update, update the multiplier
      itInfoMap->second.Multiplier = iPropertyNum;
   }
}


/* ********************************************************************
** Method:   GetVariableName
** Scope:    public
** Purpose:  With the id and delimiter string the method will construct
**           a the full qualified string and insert into the
**           sDestination argument.
** Params:   id - variable id
**           sDelimiter - pointer to the delimiter string
**           sDestimation - pointer to the result string
** Returns:  by Ref - changes sDestination
** Author:   Claude Lamarche
** Mod Date: 2011-07-18
** ***************************************************************** */
void TReportsManager::GetVariableName(const char* sVarName, const char* sDelimiter, char *sDestination)
{
   //char *sReturn;
   int i,x;

   //copy string
   strcpy(sDestination, sVarName);

   if(sDelimiter[0]!='\0')
   {
      //iterator through tokens
      x = 0;
      for(i=0;i<strlen(sDelimiter);i++)
      {
         if(sDelimiter[i]=='*' || sDelimiter[i] == '\0')
         {
            sDestination = ReplaceChar(sDestination,sDelimiter,x,i,'*');
            x = i+1;
         }
      }

      //handle the last token
      sDestination = ReplaceChar(sDestination, sDelimiter,x,i,'*');
   }


   return;
}

/* ********************************************************************
** Method:   GetMonthIndex
** Scope:    private
** Purpose:  Return the month index of the passed in day
**             ex: 1-31 = 0, 32-59 = 1 ... (no leap year!)
**                 366 = 12...
** Params:   iDay - the day number
** Note:     Leap years are now a factor, based on 12 month per year,
**           365 days in a year.
** Returns:  month index (0-Jan,1-Feb...12-Jan,13-Feb ...)
** Author:   Claude Lamarche
** Mod Date: 2012-01-31
** ***************************************************************** */
int TReportsManager::GetMonthIndex(int iDay)
{
   const int DAYSINYEAR = 365; //365 is part of the year
   int iNumberYears = 0;
   int i;

   //count the number of years
   if(iDay > 0 )
   {
      iNumberYears = iDay / (DAYSINYEAR +1); //integer arithmetic
   }

   iDay = iDay - (iNumberYears * DAYSINYEAR);


   //Find the month index
   for(i=0;i<12;i++)
   {
      if(iDay <= kMonthlyTimesteps[i])
         break;
   }

   return i+(iNumberYears*12);
}


/* ********************************************************************
** Method:   IsVariableEnable
** Scope:    public
** Purpose:  Used to determine if a variable was requested from the
**           input.xml for any type of output.
** Params:   int id - identifier for the variable
** Returns:  true/false - false on Error
** Author:   Claude Lamarche
** Mod Date: 2011-07-14
** ***************************************************************** */
bool TReportsManager::IsVariableEnable(int iVarIdentifier)
{
//   bool bReturn = false;
// ReportDataMap::const_iterator it;
//  //find the member in m_VariableInfoList
//   it = m_ReportDataList.find(stMapKey(id,string(sDelimiter)));
//   if(it != m_ReportDataList.end()){
//      //found
//      bReturn = it->second.Enabled;
//   }

   return true; // bReturn;

}


/* ********************************************************************
** Method:   IsVariableEnable
** Purpose:  Method that looks if the patern passed matches one active
**           variable.  This will be use by esp-r to before performance
**           improvement by only reporting section of code if a variable
**           was requested in the output.
** Params:   const char* pattern - the pattern to look for an active
**           variable
** Returns:  true/false if pattern matchs an active variable
** Author:   Claude Lamarche
** Mod Date: 2011-10-04
** ***************************************************************** */
bool TReportsManager::IsVariableEnable(const char* cPattern)
{
   bool bReturn = true; 
   return bReturn;  
   
//   ReportDataMap::const_iterator it;
//   int i;
//
//   //loop through set varaibles
//   for(it = m_ReportDataList.begin(); it != m_ReportDataList.end(); it++)
//   {
//      //short-circuit evaluation left to right, the enable status will be evaluated before
//      //the expensive strstr operation is performed
//      if(it->second.Enabled && strstr(it->second.sVarName,cPattern)!=NULL)
//      {
//         bReturn = true;
//         break;
//      }
//   }
//
//   return bReturn;

}




/* ********************************************************************
** Method:   GenerateOutput()
** Scope:    public
** Purpose:  Method used to generate all the reports configures in the
**           input.xml
** Params:   N/A
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-07-14
** ***************************************************************** */
void TReportsManager::GenerateOutput(){
   ReportDataMap::iterator it;
   stSortedMapKeyRef sortedMapKeylist[m_ReportDataList.size()];
   TXMLAdapter XMLAdapter;
   DBManager *objDBManager;
   int i, iBinIndex;

   // Remove old .csv file if bUseResFilenameRoot is true
   if (bUseResFilenameRoot) {
     if(!bSaveToDisk) {
       remove(sCSVFileName.c_str());
     }
   }

   //Loop through all collection variables
   i = 0;

   //Choose the bin index Monthly or Seasonal
   if(m_bSeasonalRun)
      iBinIndex = m_iCurrentSeasonIndex-m_iStartSeasonIndex;
   else
      iBinIndex = m_iCurrentMonthIndex-m_iStartMonthIndex;


   for(it = m_ReportDataList.begin();it != m_ReportDataList.end(); it++)
   {
      //level out the steps and month containers, same size
      if(bReportStartup)
      {
         it->second.Finalize(m_lCurrentStep,iBinIndex, m_lOutputStepCount);
      }
      else
      {
         it->second.Finalize(m_lActiveSteps,iBinIndex, m_lOutputStepCount);
      }


      //Kludge: for performance reasons, we'll sort by the pointers to the variable
      //        name definitions and store pointer to MapKey object from the
      //        m_ReportDataList.  With this the output function can iterate
      //        through that array and find the corresponding Map object
      if(bSortOutput)
      {
         sortedMapKeylist[i].mapKey = &it->first;
         sortedMapKeylist[i++].cString = it->second.sVarName;
      }
   }

   //Use quick sort to sort the constructed structure of <char*, mapkey*>
   //** Retrieve maps for output with the sort will slow down the simulation **
   //          ** Guess slow down is 5% longers sim time. **
   if(bSortOutput)
      qsort(sortedMapKeylist,i, sizeof(struct stSortedMapKeyRef), cmp_by_string);


   //Print the dictionary
   if(bDumpDictionary)
      OutputDictionary("out.dictionary", sortedMapKeylist );


   //Print the XML Data
   if(bOutLogXML)
   {
       OutputXMLData("out.xml",sortedMapKeylist);

      // Check to see that i) a valid style sheet was provided and
      // ii) transforms were requested.
      if (  bStyleSheetGood &&  bTransformXMLRequested ) {
         // Synopsys :
         // WriteTransformedXML( source , target, stylesheet list )
         XMLAdapter.WriteTransformedXML("out.xml",  m_StyleSheets );
      }
   }


   if(bOutLogDB || bOutStepDB)
   {
      //Initialize the database file & table structure
      objDBManager = new DBManager("out.db3");

      //Send cerr to screen
      if(!objDBManager->isEnable())
      {
         cerr << "ESP-r was not built with the --SQLite option.  Database support is disable.\n";
         log::Instance()->writeError("ESP-r was not built with the --SQLite option.  Database support is disable.",-1);
      }

      //Output the database
      OutputSQLiteData(objDBManager);

      //create the views _ not a good way to flatten a table, better
      //let the user of the SQLite database flatten only the column he/she needs instead
      //objDBManager->createDataViews();


      //add the indexes, this operation takes a considerable amount of time
      //but searching on an unindex database would be greatly inaficient.
      if(bIndexDatabase)
      {
         cout<< "Indexing database...\n";
         objDBManager->indexDatabase();
      }


      //close the database, delete the object
      delete objDBManager;
   }



   //Print to CSV file
   if(bOutStepCSV)
   {
      if (bUseResFilenameRoot) {
        OutputCSVData(sCSVFileName.c_str(),sortedMapKeylist);
      } else {
        OutputCSVData("out.csv",sortedMapKeylist);
      }
   }

   //Print the summary data
   OutputTXTsummary("out.summary",sortedMapKeylist);
}


/* ********************************************************************
** Method:   GenerateStepOutput()
** Scope:    private
** Purpose:  Method used to push the step output in an incremental
**           fashion to avoid too large of a memory footprint.
**           This is controlled by the save_to_disk input.xml option
**           and triggers by the AddTimeStepList routine
** Notes:    Since the variable number may grow during a simulation
**           the sorting has to be done each time.  The OutputCSVData
**           will handle the proper insertion of the new entry if
**           required.
** Params:   The total step count in so far.
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-24
** ***************************************************************** */
void TReportsManager::GenerateStepOutput(unsigned long lStepCount){
   ReportDataMap::iterator itDataMap;
   stSortedMapKeyRef sortedMapKeylist[m_ReportDataList.size()];
   DBManager *objDBManager;
   int i;

   //Finalize (fill with zeros) the active step vectors up to the step count
   i = 0;
   for(itDataMap = m_ReportDataList.begin();itDataMap != m_ReportDataList.end(); itDataMap++)
   {
      //Kludge: for performance reasons, we'll sort by the pointers to the variable
      //        name definitions and store pointer to MapKey object from the
      //        m_ReportDataList.  With this the output function can iterate
      //        through that array and find the corresponding Map object
      if(bSortOutput)
      {
            sortedMapKeylist[i].mapKey = &itDataMap->first;
            sortedMapKeylist[i++].cString = itDataMap->second.sVarName;
      }

      if(itDataMap->second.IsOutStep())
      {
         //make the all steps containers even size
         itDataMap->second.FinalizeStepData(lStepCount-m_lOutputStepCount);
      }
   }

   //Use quick sort to sort the constructed structure of <char*, mapkey*>
   //** Retrieve maps for output with the sort will slow down the simulation **
   //          ** Guess slow down is 5% longer simulation time. **
   if(bSortOutput)
      qsort(sortedMapKeylist,i, sizeof(struct stSortedMapKeyRef), cmp_by_string);


   //Output the steps
   if(bOutStepCSV)
   {
      if (bUseResFilenameRoot) {
          OutputCSVData(sCSVFileName.c_str(),sortedMapKeylist);
      } else {
          OutputCSVData("out.csv",sortedMapKeylist);
      }
   }

   //Output to the database
   if(bOutStepDB)
   {
      //Initialize the database file & table structure
      objDBManager = new DBManager("out.db3");

      //Output the database
      OutputSQLiteStepData(objDBManager);

      //close the database, delete the object
      delete objDBManager;
   }


   //resize the vectors
   for(itDataMap = m_ReportDataList.begin(); itDataMap != m_ReportDataList.end(); itDataMap++)
   {
      if(itDataMap->second.IsOutStep())
      {
         //Remove the values that were outputted
         itDataMap->second.EraseSteps(m_lSaveToDisk);

         //Set the variable as already in output
         //this will be used to deal with new members that come in exsistance
         //after the first output was generated
         itDataMap->second.m_bStepOutput = true;
      }
   }

   m_lOutputStepCount = lStepCount;
}

/* ********************************************************************
** Method:   OutputSQLiteData()
** Scope:    private
** Purpose:  Outputs requested data from the input.xml
**           in an SQLite3 database format.
** Params:   objDBManager - pointer to an open database object
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-22
** ***************************************************************** */
void TReportsManager::OutputSQLiteData(DBManager *objDBManager)
{
   VariableInfoMap::const_iterator itInfoMap;
   ReportDataMap::iterator itDataMap;
   TimeStepVector::const_iterator itTimeStep;
   TBinnedData *ptrBin;
   string sIntegratedUnits;
   const char *sMetaValue;
   long lStep,lVariableID;
   int i,j, iVariableDescID, iMultiplierFlag;
   bool bIntegratedUnit,bIsWatt, bScaleOK;
   double dTotal, dAnnualTotal, dScaleFactor;

   //Populate the common variable descriptors
   for(itInfoMap = m_VariableInfoList.begin(); itInfoMap != m_VariableInfoList.end(); itInfoMap++)
   {
      objDBManager->addVariableDescriptor(itInfoMap->first, itInfoMap->second.VarName,itInfoMap->second.MetaType,
                                 itInfoMap->second.MetaValue,itInfoMap->second.Description);
   }


   //Populate the TimeStep info if step data requested
   if(bOutStepDB)
   {
      lStep = 1;
      for(itTimeStep = m_TimeStepList.begin();itTimeStep != m_TimeStepList.end();itTimeStep++)
      {
         //Skip over the startup steps unless ask to report on
         if(bReportStartup || !itTimeStep->Startup)
         {
            //The step value in the itTimeStep will not start at one,
            //use an incremental index instead
            objDBManager->addTimeStep(lStep++,itTimeStep->Hour,itTimeStep->Day, (int)itTimeStep->Startup);
         }

      }
   }

   //Send all the values
   j = 1;
   for(itDataMap = m_ReportDataList.begin();itDataMap != m_ReportDataList.end(); itDataMap++)
   {
      if(itDataMap->second.IsOutLog() || itDataMap->second.IsOutStep())
      {
         //If the variable was overwritten in the course of the simluation by the add_to_report_details
         //routines then we must send a new variable descriptor to the database with the overwritten info
         //usually-false.
         if(itDataMap->second.IsVariableDescriptionWild())
         {
            //get the next avail variable descriptor id
            iVariableDescID = m_VariableInfoList.size() + j++;
            //add the new variable descriptor
            objDBManager->addVariableDescriptor(iVariableDescID,
                                 itDataMap->second.sVarName, itDataMap->second.getVariableMetaType().c_str(),
                                 itDataMap->second.getVariableMetaValue().c_str(),
                                 itDataMap->second.getVariableDescription().c_str());
         }
         else
         {
            //for the exception of above the descriptor id is the database first identifier.
            iVariableDescID = itDataMap->first.identifier;
         }


         //Get the variable db id, if negative, populate the appropriate tables
         lVariableID = itDataMap->second.m_iDBVariableID;
         if(lVariableID < 0)
         {
            //add the variable name
            lVariableID = objDBManager->addVariableName(iVariableDescID,
                                                        itDataMap->first.delimiters.c_str(),
                                                        itDataMap->second.sVarName);
            //backfill zero for unitialize variable (will only occurs in save_to_disk mode).
            if(bOutStepDB)
            {
               for(i=1; i <= m_lOutputStepCount; i++)
               {
                  objDBManager->addValue(i,lVariableID,0.0);
               }
            }
         }
         else //name already set (during step save)
         {
            if(itDataMap->second.IsVariableDescriptionWild())
            {
               //Update the variable name to point to the new descriptor
               objDBManager->updateVariableName(lVariableID,iVariableDescID);
            }
         }


         //Push the step data
         if(itDataMap->second.IsOutStep() && bOutStepDB)
         {
            //Store the step data, outputStepCount is populate when "save_to_disk" is true
            lStep = m_lOutputStepCount + 1;

            itDataMap->second.GetNextStepValueReset();
            while(!itDataMap->second.GetNextStepValueEnd())
            {
               //push the value to the database.
               objDBManager->addValue(lStep++,lVariableID,itDataMap->second.GetNextStepValue());
            }
         }



         //Push the log Data
         if(itDataMap->second.IsOutLog() && bOutLogDB)
         {
            //Send the monthly binned data
            i = 0;
            itDataMap->second.GetNextLogBinReset();
            ptrBin = itDataMap->second.GetNextLogBin();
            while(ptrBin != NULL)
            {
               if(!m_bSeasonalRun)
               {
                  objDBManager->addBinData(lVariableID,i,(int)m_BinStepCount[i],(int)ptrBin->ActiveTimesteps(),
                                              ptrBin->Sum(), ptrBin->Max(), ptrBin->Min(), ptrBin->ActiveAverage(),
                                              ptrBin->TotalAverage(m_BinStepCount[i]),BIN_MONTH_TYPE);
               }
               else
               {
                  objDBManager->addBinData(lVariableID,i,(int)m_BinStepCount[i],(int)ptrBin->ActiveTimesteps(),
                                              ptrBin->Sum(), ptrBin->Max(), ptrBin->Min(), ptrBin->ActiveAverage(),
                                              ptrBin->TotalAverage(m_BinStepCount[i]),BIN_SEASONAL_TYPE);
               }

               //Get next
               ptrBin = itDataMap->second.GetNextLogBin();
               i++;
            }


            //Send the annual binned data
            ptrBin = itDataMap->second.GetAnnualBin();
            objDBManager->addBinData(lVariableID,0,(int)m_AnnualBinStepCount,(int)ptrBin->ActiveTimesteps(),
                                              ptrBin->Sum(), ptrBin->Max(), ptrBin->Min(), ptrBin->ActiveAverage(),
                                              ptrBin->TotalAverage(m_AnnualBinStepCount),BIN_ANNUAL_TYPE);


            //send the integrated data
            sIntegratedUnits = "";
            bIntegratedUnit = false;
            bIsWatt = false;

            //Get the metavalue
            itInfoMap = m_VariableInfoList.find(itDataMap->first.identifier);
            sMetaValue = itInfoMap->second.MetaValue;
            iMultiplierFlag = itInfoMap->second.Multiplier; //relevant for QuickRun mode
            //information in a different location when
            //add_to_report_details is used. usually - false
            if(itDataMap->second.IsVariableDescriptionWild())
            {
               sMetaValue = itDataMap->second.getVariableMetaValue().c_str();
            }


            //set the integrated types
            if(strcmp(sMetaValue,"(W)")==0)
            {
               sIntegratedUnits = "GJ";
               bIntegratedUnit = true;
               bIsWatt = true;
            }
            else if(strcmp(sMetaValue,"(kg/s)")==0)
            {
               sIntegratedUnits = "kg";
               bIntegratedUnit = true;
            }
            else if(strcmp(sMetaValue,"(kWh/s)")==0)
            {
               sIntegratedUnits = "kWh";
               bIntegratedUnit = true;
            }
            else if(strcmp(sMetaValue,"(l/s)")==0)
            {
               sIntegratedUnits = "l";
               bIntegratedUnit = true;
            }
            else if(strcmp(sMetaValue,"(m3/s)")==0)
            {
               sIntegratedUnits = "m3";
               bIntegratedUnit = true;
            }
            else if(strcmp(sMetaValue,"(tonne/s)")==0)
            {
               sIntegratedUnits = "tonne";
               bIntegratedUnit = true;
            }
            else if(strcmp(sMetaValue,"($/s)")==0)
            {
               sIntegratedUnits = "$";
               bIntegratedUnit = true;
            }

            if(bIntegratedUnit)
            {
               if(!m_bSeasonalRun)
               {
                  i = 0;
                  //Integrate monthly bin data
                  itDataMap->second.GetNextLogBinReset();
                  ptrBin = itDataMap->second.GetNextLogBin();
                  while(ptrBin != NULL)
                  {
                     //for W->J, convert result calculated in GJ
                     if (bIsWatt)
                        dTotal = (ptrBin->Sum() * m_fMinutePerTimeStep * 60. ) / 1.e09;
                     else
                        dTotal = ptrBin->Sum() * m_fMinutePerTimeStep * 60.;


                     objDBManager->addIntegratedData(lVariableID,i,sIntegratedUnits.c_str(),dTotal, BIN_MONTH_TYPE);

                     //Get Next bin
                     ptrBin = itDataMap->second.GetNextLogBin();
                     i++;
                  }

                  //Integrated annual bin data
                  ptrBin = itDataMap->second.GetAnnualBin();
                  if (bIsWatt)
                     dTotal = (ptrBin->Sum() * m_fMinutePerTimeStep * 60. ) / 1e09;
                  else
                     dTotal = ptrBin->Sum() * m_fMinutePerTimeStep * 60.;

                  objDBManager->addIntegratedData(lVariableID,0,sIntegratedUnits.c_str(),dTotal, BIN_ANNUAL_TYPE);
               }
               else
               {
                  //Integrated seasonal bin data
                  bScaleOK = false;
                  dAnnualTotal = 0.;
                  i = 0;

                  if(iMultiplierFlag != MULT_NONE)
                  {
                     bScaleOK = true;

                     itDataMap->second.GetNextLogBinReset();
                     ptrBin = itDataMap->second.GetNextLogBin();
                     while(ptrBin != NULL)
                     {
                        switch(iMultiplierFlag)
                        {
                           case MULT_HTG:
                              dScaleFactor =  m_SeasonMultipliersList[i].Htg;
                              break;
                           case MULT_CLG:
                              dScaleFactor = m_SeasonMultipliersList[i].Clg;
                              break;
                           case MULT_GEN:
                              dScaleFactor = m_SeasonMultipliersList[i].Gen;
                              break;
                           default:
                              dScaleFactor = 1.0;
                        }

                        //for W->J, convert result calculated in GJ
                        if (bIsWatt)
                           dTotal = (ptrBin->Sum() * m_fMinutePerTimeStep * 60. * dScaleFactor) / 1e09;
                        else
                           dTotal = ptrBin->Sum() * m_fMinutePerTimeStep * 60. * dScaleFactor;

                        dAnnualTotal += dTotal;

                        objDBManager->addIntegratedData(lVariableID,i,sIntegratedUnits.c_str(),dTotal, BIN_SEASONAL_TYPE);

                        //Get Next bin
                        ptrBin = itDataMap->second.GetNextLogBin();
                        i++;
                     }
                  }

                  if(bScaleOK)
                  {
                     //annual calculated seaonal data
                     objDBManager->addIntegratedData(lVariableID,0,sIntegratedUnits.c_str(),dAnnualTotal, BIN_ANNUAL_TYPE);
                  }
               }
            }
         }
      }
   }
}

/* ********************************************************************
** Method:   OutputSQLiteStepData()
** Scope:    private
** Purpose:  Used for incremental step output, will output only step
**           values to the database.  If a new variable came into existance
**           the method will backfill past outputted step to zero.
** Params:   sFileName - the file to create !will overwrite existing
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-26
** ***************************************************************** */
void TReportsManager::OutputSQLiteStepData(DBManager *objDBManager)
{
   ReportDataMap::iterator itDataMap;
   int i;
   int iVariableID;
   long lStep;

   //go through the variable list
   for(itDataMap = m_ReportDataList.begin();itDataMap != m_ReportDataList.end(); itDataMap++)
   {
      //check if it's a step data
      if(itDataMap->second.IsOutStep())
      {
         iVariableID = itDataMap->second.m_iDBVariableID;
         //get the variable id
         if(iVariableID < 0)
         {
            //Never initialized, generate an key
            iVariableID = objDBManager->addVariableName(itDataMap->first.identifier,
                                                         itDataMap->first.delimiters.c_str(),
                                                         itDataMap->second.sVarName);
            //If we need to backfill variable with zeros
            for(i=1; i <= m_lOutputStepCount; i++)
            {
               objDBManager->addValue(i,iVariableID,0.0);
            }

            //update the storage's db id
            itDataMap->second.m_iDBVariableID = iVariableID;
         }

         //Store the steps values in the vectors.
         lStep = m_lOutputStepCount + 1;
         itDataMap->second.GetNextStepValueReset();
         while(!itDataMap->second.GetNextStepValueEnd())
         {
            //push the value to the database.
            objDBManager->addValue(lStep++,iVariableID,itDataMap->second.GetNextStepValue());
         }
      }
   }
}


/* ********************************************************************
** Method:   OutputXMLData()
** Scope:    private
** Purpose:  Outputs <log_variable> requested data in the input.xml
**           in an XML format.
** Params:   sFileName - the file to create !will overwrite existing
**           sortedRef - an array that contains the mapkey in sorted order
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-08
** ***************************************************************** */
void TReportsManager::OutputXMLData(const char *sFileName, stSortedMapKeyRef sortedRef[])
{
   bool useFlatHierarchy, bIntegratedUnit, bIsWatt;
   ReportDataMap::iterator itDataMap;
   TXMLAdapter objXMLdoc;
   TXMLNode currentNode,tokenNode, integratedNode;
   VariableInfoMap::const_iterator itInfoMap;
   TBinnedData *ptrBin;
   const char *sMetaType, *sMetaValue, *sDescription;
   char *sTok, sTemp[256],sLastTok[256];
   vector<TXMLNode> nextNodeVector;
   string sIntegratedUnits;
   int i,j;
   double dTotal,dAnnualTotal;
   int iMultiplierFlag;
   double dScaleFactor;
   bool bScaleOK;

   //Get some of the input configs
   if(m_params["hierarchy"].empty() || m_params["hierarchy"] == "tree")
      useFlatHierarchy = false;
   else
      useFlatHierarchy = true;

   //Initialize the XML doc
   objXMLdoc.AddNode(NULL, "system", "");
   objXMLdoc.AddAttribute(objXMLdoc.RootNode(),"version","1.0");

   //if the sort_order is requester, we retreive the report map differently
   j = 0;
   if(bSortOutput)
   {
      if(j < m_ReportDataList.size())
         itDataMap = m_ReportDataList.find(*sortedRef[j++].mapKey);
      else
         itDataMap = m_ReportDataList.end();
   }
   else
      itDataMap = m_ReportDataList.begin();

   while(itDataMap != m_ReportDataList.end())
   {
      //only output if it was set as <log_variable>
      if(itDataMap->second.IsOutLog())
      {
         //for testing
         //itDataMap->second.PrintLogData();
         currentNode = objXMLdoc.RootNode();

         if(useFlatHierarchy)
         {
            currentNode = objXMLdoc.AddNode(currentNode, "parameter", "");
            objXMLdoc.AddNode(currentNode, "name", itDataMap->second.sVarName);
         }
         else
         {
            //copy the string loop through the tokens
            strcpy(sTemp,itDataMap->second.sVarName);
            sTok = strtok(sTemp,"/");
            while(sTok != NULL)
            {
               //save the last valid token to push to xml
               strcpy(sLastTok,sTok);

               //build the XML node tree
               nextNodeVector = objXMLdoc.GetChildren(currentNode,sTok);
               if(nextNodeVector.size() == 0)
                  currentNode = objXMLdoc.AddNode(currentNode,sTok,"");
               else
                  currentNode = nextNodeVector[0];

               sTok = strtok(NULL,"/");
            }

            //Store where the actual token node is, since currentNode will walk back/forth
            tokenNode = currentNode;

            //START add description
            //Fetch the description for this node in the case it's overwritten by the
            itInfoMap = m_VariableInfoList.find(itDataMap->first.identifier);
            sMetaType = itInfoMap->second.MetaType;
            sMetaValue= itInfoMap->second.MetaValue;
            sDescription = itInfoMap->second.Description;
            iMultiplierFlag = itInfoMap->second.Multiplier; //relevant data when Quick run mode is on

            //add_to_report_details routines fetch from a different store.
            if(itDataMap->second.IsVariableDescriptionWild())
            {
               sMetaValue = itDataMap->second.getVariableMetaValue().c_str();
               sMetaType = itDataMap->second.getVariableMetaType().c_str();
               sDescription = itDataMap->second.getVariableDescription().c_str();
            }

            //Add to XML
            objXMLdoc.AddNode(currentNode,"name",sLastTok);
            objXMLdoc.AddNode(currentNode,"description",sDescription);
            objXMLdoc.AddNode(currentNode,sMetaType,sMetaValue);
            //END add description

            //START add monthly bin
            i = 0;
            itDataMap->second.GetNextLogBinReset();
            ptrBin = itDataMap->second.GetNextLogBin();
            while(ptrBin != NULL)
            {
               currentNode = objXMLdoc.AddNode(tokenNode,"binned_data","");

               if(m_bSeasonalRun)
                  objXMLdoc.AddAttribute(currentNode, "type", "seasonal");
               else
                  objXMLdoc.AddAttribute(currentNode, "type", "monthly");

               objXMLdoc.AddNode(currentNode, "index", StringValue(sTemp,i));
               objXMLdoc.AddNode(currentNode, "steps", StringValue(sTemp,(int)m_BinStepCount[i]));
               objXMLdoc.AddNode(currentNode, "active_steps", StringValue(sTemp,(int)ptrBin->ActiveTimesteps()));
               objXMLdoc.AddNode(currentNode, "sum", StringValue(sTemp,ptrBin->Sum()));
               objXMLdoc.AddNode(currentNode, "max", StringValue(sTemp,ptrBin->Max()));
               objXMLdoc.AddNode(currentNode, "min", StringValue(sTemp,ptrBin->Min()));
               objXMLdoc.AddNode(currentNode, "active_average", StringValue(sTemp,ptrBin->ActiveAverage()));
               objXMLdoc.AddNode(currentNode, "total_average", StringValue(sTemp,ptrBin->TotalAverage(m_BinStepCount[i])));

               //Get next
               ptrBin = itDataMap->second.GetNextLogBin();
               i++;
            }
            //END add monthly bin

            //START annual bin to xml
            ptrBin = itDataMap->second.GetAnnualBin();
            currentNode = objXMLdoc.AddNode(tokenNode,"binned_data","");
            objXMLdoc.AddAttribute(currentNode,"type","annual");
            objXMLdoc.AddNode(currentNode,"steps",StringValue(sTemp,m_AnnualBinStepCount));
            objXMLdoc.AddNode(currentNode,"active_steps",StringValue(sTemp,(int)ptrBin->ActiveTimesteps()));
            objXMLdoc.AddNode(currentNode,"sum",StringValue(sTemp,ptrBin->Sum()));
            if(ptrBin->ActiveTimesteps() > 0) //these are only valid if the variable is active
            {
               objXMLdoc.AddNode(currentNode, "max", StringValue(sTemp,ptrBin->Max()));
               objXMLdoc.AddNode(currentNode, "min", StringValue(sTemp,ptrBin->Min()));
            }
            else
            {
               objXMLdoc.AddNode(currentNode, "max", "NaN");
               objXMLdoc.AddNode(currentNode, "min", "NaN");
            }
            objXMLdoc.AddNode(currentNode, "active_average", StringValue(sTemp,ptrBin->ActiveAverage()));
            objXMLdoc.AddNode(currentNode, "total_average", StringValue(sTemp,ptrBin->TotalAverage(m_AnnualBinStepCount)));
            //END annual bin

            //START Custom manipulation: Integrate mass/energy flow
            sIntegratedUnits = "";
            bIntegratedUnit = false;
            bIsWatt = false;
            if(strcmp(sMetaValue,"(W)")==0)
            {
               sIntegratedUnits = "GJ";
               bIntegratedUnit = true;
               bIsWatt = true;
            }
            else if(strcmp(sMetaValue,"(kg/s)")==0)
            {
               sIntegratedUnits = "kg";
               bIntegratedUnit = true;
            }
            else if(strcmp(sMetaValue,"(kWh/s)")==0)
            {
               sIntegratedUnits = "kWh";
               bIntegratedUnit = true;
            }
            else if(strcmp(sMetaValue,"(l/s)")==0)
            {
               sIntegratedUnits = "l";
               bIntegratedUnit = true;
            }
            else if(strcmp(sMetaValue,"(m3/s)")==0)
            {
               sIntegratedUnits = "m3";
               bIntegratedUnit = true;
            }
            else if(strcmp(sMetaValue,"(tonne/s)")==0)
            {
               sIntegratedUnits = "tonne";
               bIntegratedUnit = true;
            }
            else if(strcmp(sMetaValue,"($/s)")==0)
            {
               sIntegratedUnits = "$";
               bIntegratedUnit = true;
            }

            if(bIntegratedUnit)
            {
               //Integrate bin data
               if(!m_bSeasonalRun)
               {
                  integratedNode = objXMLdoc.AddNode(tokenNode, "integrated_data", "");
                  objXMLdoc.AddAttribute(integratedNode,"units",sIntegratedUnits.c_str());

                  //Monthly bins
                  i = 0;
                  itDataMap->second.GetNextLogBinReset();
                  ptrBin = itDataMap->second.GetNextLogBin();
                  while(ptrBin != NULL)
                  {
                     //for W->J, convert result calculated in GJ
                     if (bIsWatt)
                        dTotal = (ptrBin->Sum() * m_fMinutePerTimeStep * 60. ) / 1e09;
                     else
                        dTotal = ptrBin->Sum() * m_fMinutePerTimeStep * 60.;

                     currentNode = objXMLdoc.AddNode(integratedNode,"bin",StringValue(sTemp,dTotal));
                     objXMLdoc.AddAttribute(currentNode, "number", StringValue(sTemp,i));
                     if(m_bSeasonalRun)
                        objXMLdoc.AddAttribute(currentNode, "type", "seasonal");
                     else
                        objXMLdoc.AddAttribute(currentNode, "type", "monthly");

                     //Get Next bin
                     ptrBin = itDataMap->second.GetNextLogBin();
                     i++;
                  }

                  // And integrate annual data
                  ptrBin = itDataMap->second.GetAnnualBin();
                  if (bIsWatt)
                     dTotal = (ptrBin->Sum() * m_fMinutePerTimeStep * 60. ) / 1e09;
                  else
                     dTotal = ptrBin->Sum() * m_fMinutePerTimeStep * 60.;
                  currentNode = objXMLdoc.AddNode(integratedNode,"bin",StringValue(sTemp,dTotal));
                  objXMLdoc.AddAttribute(currentNode, "type", "annual");
               }
               else
               {
                  //seasonal bins
                  bScaleOK = false;
                  dAnnualTotal = 0.;
                  i = 0;

                  //C.L. if not specified with seasonal mult don't display integrated_data
                  if(iMultiplierFlag != MULT_NONE)
                  {
                     bScaleOK = true;
                     integratedNode = objXMLdoc.AddNode(tokenNode, "integrated_data", "");
                     objXMLdoc.AddAttribute(integratedNode,"units",sIntegratedUnits.c_str());

                     itDataMap->second.GetNextLogBinReset();
                     ptrBin = itDataMap->second.GetNextLogBin();
                     while(ptrBin != NULL)
                     {
                        switch(iMultiplierFlag)
                        {
                           case MULT_HTG:
                              dScaleFactor =  m_SeasonMultipliersList[i].Htg;
                              break;
                           case MULT_CLG:
                              dScaleFactor = m_SeasonMultipliersList[i].Clg;
                              break;
                           case MULT_GEN:
                              dScaleFactor = m_SeasonMultipliersList[i].Gen;
                              break;
                           default:
                              dScaleFactor = 1.0;
                        }


                        dTotal = ptrBin->Sum() * m_fMinutePerTimeStep * 60. *  dScaleFactor;

                        if(strcmp(sMetaValue,"(W)")==0){dTotal = dTotal / 1e09;}
                        dAnnualTotal = dAnnualTotal + dTotal;

                        currentNode = objXMLdoc.AddNode(integratedNode, "bin", StringValue(sTemp,dTotal));
                        objXMLdoc.AddAttribute(currentNode, "number", StringValue(sTemp,i));
                        objXMLdoc.AddAttribute(currentNode, "type", "seasonal");
                        objXMLdoc.AddAttribute(currentNode, "scale", StringValue(sTemp,(float)dScaleFactor));

                        //Get Next bin
                        ptrBin = itDataMap->second.GetNextLogBin();
                        i++;
                     }
                  }
                  //C.L. 2012-02-22 - if bScaleOK is false we should not display the annual integrated bin since its not calculated.
                  // And integrate annual data
                  if(bScaleOK)
                  {
                     currentNode = objXMLdoc.AddNode(integratedNode,"bin",StringValue(sTemp,dAnnualTotal));
                     objXMLdoc.AddAttribute(currentNode, "type", "annual");
                  }
               }
            }
            //END Custom manipulation: Integrate mass/energy flow
         }
      }

      //Get the next ReportData map iterator
      if(bSortOutput)
      {
         if(j < m_ReportDataList.size())
            itDataMap = m_ReportDataList.find(*sortedRef[j++].mapKey);
         else
            itDataMap = m_ReportDataList.end();
      }
      else
         itDataMap = ++itDataMap;
   }
   objXMLdoc.WriteToFile(sFileName);
}


/* ********************************************************************
** Method:   OutputTXTsummary()
** Scope:    private
** Purpose:  Output <summary_variable> requested data in the input.xml
** Params:   sFileName - the file to create !will overwrite existing
**           sortedRef - an array that contains the mapkey in sorted order
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-09-09
** ***************************************************************** */
void TReportsManager::OutputTXTsummary(const char *sFileName, stSortedMapKeyRef sortedRef[])
{
   VariableInfoMap::const_iterator itInfoMap;
   ReportDataMap::iterator itDataMap;
   TBinnedData *ptrBin;
   bool isEmpty = true;
   char *sVarName;
   const char *sMetaValue;
   char buffer[255];
   double gj,kg;
   int j;

   //Open the file
   ofstream summaryFile(sFileName,ios::trunc);
   summaryFile.setf(ios::showpoint);
   summaryFile.precision(8);

   if(summaryFile.is_open())
   {
      //if the sort_order is requester, we retreive the report map differently
      j = 0;
      if(bSortOutput)
      {
         if(j < m_ReportDataList.size())
            itDataMap = m_ReportDataList.find(*sortedRef[j++].mapKey);
         else
            itDataMap = m_ReportDataList.end();
      }
      else
         itDataMap = m_ReportDataList.begin();

      while(itDataMap != m_ReportDataList.end())
      {
         if(itDataMap->second.IsOutSummary())
         {
            //Get the overwritten variable meta information if overwritten during the
            //course of the simulation with the add_to_report_details routines.  Usually-false
            if(itDataMap->second.IsVariableDescriptionWild())
            {
               sMetaValue = itDataMap->second.getVariableMetaValue().c_str();
            }
            else
            {
               itInfoMap = m_VariableInfoList.find(itDataMap->first.identifier);
               sMetaValue = itInfoMap->second.MetaValue;
            }
            ptrBin = itDataMap->second.GetAnnualBin();
            sVarName = itDataMap->second.sVarName;

            if(bOutputGeomDat)
            {
               summaryFile << sVarName << "," << StringValue(buffer,ptrBin->TotalAverage(m_AnnualBinStepCount)) << "," << sMetaValue << "\n";
            }
            else
            {
               //push to output stream
               summaryFile << sVarName << "::Total_Average " << StringValue(buffer,ptrBin->TotalAverage(m_AnnualBinStepCount)) << " " << sMetaValue << "\n";
               summaryFile << sVarName << "::Active_Average " << StringValue(buffer,ptrBin->ActiveAverage()) << " " << sMetaValue << "\n";
   
               if(ptrBin->ActiveTimesteps() > 0)
               {
                  summaryFile << sVarName << "::Maximum " << StringValue(buffer,ptrBin->Max()) << " " << sMetaValue << "\n";
                  summaryFile << sVarName << "::Minimum " << StringValue(buffer,ptrBin->Min()) << " " << sMetaValue << "\n";
               }
               else
               {
                  summaryFile << sVarName << "::Maximum NaN " << sMetaValue << "\n";
                  summaryFile << sVarName << "::Minimum NaN " << sMetaValue << "\n";
               }
   
               if(strcmp(sMetaValue,"(W)") == 0)
               {
                  gj = ptrBin->Sum() * (float)m_fMinutePerTimeStep * 60 / 1e09;
                  summaryFile << sVarName << "::AnnualTotal " << StringValue(buffer,gj) << " (GJ)\n";
               }
   
               if(strcmp(sMetaValue,"(kg/s)") == 0)
               {
                  kg = ptrBin->Sum() * (float)m_fMinutePerTimeStep * 60;
                  summaryFile << sVarName << "::AnnualTotal " << StringValue(buffer,kg) << " (kg)\n";
               }
            }
            isEmpty = false;
         }

         //Get next ReportData Map iterator
         if(bSortOutput)
         {
            if(j < m_ReportDataList.size())
               itDataMap = m_ReportDataList.find(*sortedRef[j++].mapKey);
            else
               itDataMap = m_ReportDataList.end();
         }
         else
            itDataMap = ++itDataMap;
      }
   }


   // close file
   summaryFile.close();

   //remove the file if nothing was written
   if(isEmpty)
      remove(sFileName);

   return;
}


/* ********************************************************************
** Method:   OutputCSVData()
** Scope:    private
** Purpose:  Output <step_variable> requested data in the input.xml
** Params:   sFileName - the file to create !will overwrite existing
**           sortedRef - an array that contains the mapkey in sorted order
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-23
** ***************************************************************** */
void TReportsManager::OutputCSVData(const char *sFileName, stSortedMapKeyRef sortedRef[])
{
   VariableInfoMap::const_iterator itInfoMap;
   ReportDataMap::iterator itDataMap;
   static bool isInit = false;
   int i,j, iSteps=0, iPos=0;
   string sTemp;


   //open the file, append mode
   ofstream outfile(sFileName,ios::app);
   outfile.setf(ios::showpoint);
   outfile.precision(8);

   if(outfile.is_open())
   {
      //if the sort_order is requester, we retreive the report map differently
      j = 0;
      if(bSortOutput)
      {
         if(j < m_ReportDataList.size())
            itDataMap = m_ReportDataList.find(*sortedRef[j++].mapKey);
         else
            itDataMap = m_ReportDataList.end();
      }
      else
         itDataMap = m_ReportDataList.begin();

      while(itDataMap != m_ReportDataList.end())
      {
         if(itDataMap->second.IsOutStep())
         {
            //Store the number of steps stored (since they are all the same size it 
            //does not matter if the counter is overwritten by a different variable)
            iSteps = itDataMap->second.GetStepCount();

            //Increment/store the current position (column) in the csv
            iPos++;

            //** note: may perform better if the MetaValue was part of the m_ReportDataList **
            itInfoMap = m_VariableInfoList.find(itDataMap->first.identifier);

            //Make the header more readable
            sTemp = itDataMap->second.sVarName;

            for(i=0;i<sTemp.length();i++)
            {
               if(sTemp[i] == '/')
               {
                  sTemp[i] = ':';
               }
               if(sTemp[i] == '_')
               {
                  sTemp[i] = ' ';
               }
            }
            sTemp+= " ";

            if(itDataMap->second.IsVariableDescriptionWild())
               sTemp+= itDataMap->second.getVariableMetaValue();
            else
               sTemp+= itInfoMap->second.MetaValue;
            sTemp+= ", "; // trailing "," should be avoided (how?) ag@20131221

            if(isInit)
            {
               if(!itDataMap->second.m_bStepOutput)
               {
                  outfile.close();
                  //new variable brought came alive, must add new column to CSV
                  InjectVariableToCSV(sFileName,sTemp.c_str(),iPos);
                  //InjectValueToCSV(sFileName,1,4,itDataMap->second.sVarName);

                  outfile.open(sFileName,ios::app);
                  outfile.setf(ios::showpoint);
                  outfile.precision(8);
               }
            }
            else
            {
               //push to output stream
               outfile << sTemp;
            }
         }

         //Get the next ReportData map iterator
         if(bSortOutput)
         {
            if(j < m_ReportDataList.size())
               itDataMap = m_ReportDataList.find(*sortedRef[j++].mapKey);
            else
               itDataMap = m_ReportDataList.end();
         }
         else
            itDataMap = ++itDataMap;
      }

      if(!isInit)
         outfile << '\n';

      //output step data
      for(i=0;i<iSteps;i++)
      {
         //if the sort_order is requester, we retreive the report map differently
         j = 0;
         if(bSortOutput)
         {
            if(j < m_ReportDataList.size())
               itDataMap = m_ReportDataList.find(*sortedRef[j++].mapKey);
            else
               itDataMap = m_ReportDataList.end();
         }
         else
            itDataMap = m_ReportDataList.begin();


         while(itDataMap != m_ReportDataList.end())
         {
            if(itDataMap->second.IsOutStep())
            {
               outfile << itDataMap->second.GetStepValue(i) << ", ";
            }

            //Get the next ReportData map iterator
            if(bSortOutput)
            {
               if(j < m_ReportDataList.size())
                  itDataMap = m_ReportDataList.find(*sortedRef[j++].mapKey);
               else
                  itDataMap = m_ReportDataList.end();
            }
            else
               itDataMap = ++itDataMap;
         }
         outfile << '\n';
      }
   }

   isInit = true;

   //Close file
   outfile.close();
}


/* ********************************************************************
** Method:   InjectVariableToCSV()
** Scope:    private
** Purpose:  Used only when save_to_disk is true, this method will insert
**           and initialise a variable that came into existance
**           after the first write occured.
** Params:   sFileName - the file to create - will append to the file
**           sVarName - the formated name to insert
**           iPosition - the column location to insert the variable
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-29
** ***************************************************************** */
void TReportsManager::InjectVariableToCSV(const char *sFileName, const char *sVarName, int iPosition)
{
   ofstream objOutFile;
   ifstream objInFile;
   string sTempFileName, sBuffer, sVarToInsert;
   int iLineNumber = 0;
   int iFoundPos = 0;
   int iPos = iPosition;

   //Create a temp outfile
   sTempFileName = sFileName;
   sTempFileName+= ".tmp";

   //Open the files streams
   objOutFile.open(sTempFileName.c_str(),ios::trunc);
   objInFile.open(sFileName, ios::in);

   //Create the column name to insert
   sVarToInsert = sVarName;

   if(objInFile.is_open() && objOutFile.is_open())
   {
      //get the first line
      getline(objInFile,sBuffer);
      while(!objInFile.eof())
      {
         //Find the position to insert
         iFoundPos = 0;
         while(iPos-- > 1)
         {
            iFoundPos = sBuffer.find(",", iFoundPos + 1, 1);
         }

         //modify the line accordingly
         if(iLineNumber == 0)
            (iFoundPos > 1)?sBuffer.insert(iFoundPos + 2, sVarToInsert):sBuffer.insert(iFoundPos, sVarToInsert);
         else
            (iFoundPos > 1)?sBuffer.insert(iFoundPos + 2,"0.0000000, "):sBuffer.insert(iFoundPos,"0.0000000, ");

         //push to new file
         objOutFile << sBuffer.c_str() << '\n';
         getline(objInFile,sBuffer);
         iLineNumber++;
         iPos = iPosition;
      }
   }


   //Delete the file, rename the temp
   objInFile.close();
   objOutFile.close();
   remove(sFileName);
   rename(sTempFileName.c_str(),sFileName);
}



/* ********************************************************************
** Method:   OutputDictionary()
** Scope:    private
** Purpose:  Outputs all possible variables from a simulation.  The
**           current logic is to output only the variables it encounters
**           with there fully describes name (rendered during simulation).
**           Depending on the use of the information, an output of all possible
**           variables with the wildcard in may be more desirable.
**           ** To confirm **
** Params:   sFileName - the file to create !will overwrite existing
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-08
** ***************************************************************** */
void TReportsManager::OutputDictionary(const char* sFileName, stSortedMapKeyRef sortedRef[])
{
   VariableInfoMap::const_iterator itInfoMap;
   ReportDataMap::iterator itDataMap;
   const char *sMetaValue, *sDescription, *sVarName;
   FILE *pFile;

   pFile = fopen(sFileName,"w");
   int j;

   //Code that loops and outputs all the variable with wildcards
   //and their information.
   if(pFile!=NULL)
   {
      //apply a sort algorithm
      j = 0;
      if(bSortOutput)
      {
         if(j < m_ReportDataList.size())
            itDataMap = m_ReportDataList.find(*sortedRef[j++].mapKey);
         else
            itDataMap = m_ReportDataList.end();
      }
      else
         itDataMap = m_ReportDataList.begin();

      //get the data
      while(itDataMap != m_ReportDataList.end())
      {
         itInfoMap =  m_VariableInfoList.find(itDataMap->first.identifier);
         sVarName = itDataMap->second.sVarName; //variable name without *

         //Get the information differently if variable's information was overwritten
         //during the simulation by the add_to_report_details routines. usually-false
         if(itDataMap->second.IsVariableDescriptionWild())
         {
            sMetaValue = itDataMap->second.getVariableMetaValue().c_str();
            sDescription = itDataMap->second.getVariableDescription().c_str();
         }
         else
         {
            sMetaValue = itInfoMap->second.MetaValue;
            sDescription = itInfoMap->second.Description;
         }

         //format print
         //fprintf(pFile,"%s\n  %s%s\",\"%s\"\n",
         //        sVarName,itDataMap->first.delimiters.c_str(),
         //        itInfoMap->second.Description, itInfoMap->second.MetaValue);
         //fprintf(pFile,"%s \n  [%s, Units: %s]\n\n",

         fprintf(pFile,"%s [%s, Units: %s]\n",
                 sVarName,
                 itInfoMap->second.Description, 
                 itInfoMap->second.MetaValue);


         //Get next ReportData Map iterator
         if(bSortOutput)
         {
            if(j < m_ReportDataList.size())
               itDataMap = m_ReportDataList.find(*sortedRef[j++].mapKey);
            else
               itDataMap = m_ReportDataList.end();
         }
         else
            itDataMap = ++itDataMap;
      }
   }
   fclose(pFile);
}


/**
 * Function returing the current status of reporting output
 */
bool TReportsManager::ReportsEnabled(){
   return bReports_Enabled;
}

/**
 * Function setting the status of reporting output
 */
void TReportsManager::EnableReports( bool& ReportsStatus ){
   bReports_Enabled = ReportsStatus;
   return;
}

/**
 * SetReportFilename: ... basically a dummy function.
 *
 *
 */
void TReportsManager::SetReportFilename(const std::string& sRootName)
{

//  cout << "TReportsManager::SetReportFilename: " << sRootName << '\n';
  return;
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
   std::set<std::string>::iterator it;
   std::map<std::string, std::string>::iterator param;
   std::string sTemp,sTemp2;

   TXMLAdapter inputXML;

   // Set defaults if required.
   SetFlags();

   inputXML.AddNode(NULL, "configuration", "");
   TXMLNode currentNode = inputXML.RootNode();

   // Loop through all defined parameters, add to xml document
   for( param = m_params.begin(); param != m_params.end(); param++)
   {
      // Don't write out unset parameters
      // Don't write out the every attribute on the save_to_disk
      sTemp = param->first;
      sTemp2 = param->second;
      if ( param->second != "" && param->second !="save_to_disk_every"){
         inputXML.AddNode(currentNode,sTemp.c_str(),sTemp2.c_str());
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
   for (it=m_nodes.begin() ; it !=m_nodes.end(); it++ ){
      inputXML.AddNode(currentNode,"log_variable",(*it).c_str());
   }

   // Loop through timestep output variables and add to document
   for (it=m_step_nodes.begin() ; it !=m_step_nodes.end(); it++ ){
      inputXML.AddNode(currentNode,"step_variable",(*it).c_str());
   }

   // Loop through summary output variables and add to document
   for (it=m_summary_nodes.begin() ; it !=m_summary_nodes.end(); it++ ){
      inputXML.AddNode(currentNode,"summary_variable",(*it).c_str());
   }

   // Write XML document ot disk
   inputXML.WriteToFile(m_inputFilePath.c_str());
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
  inputXML.GetNodeValuesSet("log_variable", inputXML.RootNode(),m_nodes);

  // Nodes to appear in out.csv
  inputXML.GetNodeValuesSet("step_variable", inputXML.RootNode(),m_step_nodes);

  // Nodes to appear in out.summary
  inputXML.GetNodeValuesSet("summary_variable", inputXML.RootNode(),m_summary_nodes);

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

  // Output zone names?
  m_params["use_zonenames"] = inputXML.GetFirstNodeValue("use_zonenames", inputXML.RootNode());

  // Output file name root = results file name root?
  m_params["use_surfacenames"] = inputXML.GetFirstNodeValue("use_surfacenames", inputXML.RootNode());

  // Output geometric data for embedded energy?
  m_params["output_geomdat"] = inputXML.GetFirstNodeValue("output_geomdat", inputXML.RootNode());

  // Save to disk, save_to_disk max attribute
  m_params["save_to_disk"] = inputXML.GetFirstNodeValue("save_to_disk", inputXML.RootNode());
  m_params["save_to_disk_every"] = inputXML.GetFirstAttributeValue("save_to_disk","every").c_str();


  //Get step variable output mode
  m_params["step_output_format"] = inputXML.GetFirstNodeValue("step_output_format", inputXML.RootNode());

  //Get log variable output mode
  m_params["log_output_format"] = inputXML.GetFirstNodeValue("log_output_format", inputXML.RootNode());

  // Sort the output flag
  m_params["sort_output"] = inputXML.GetFirstNodeValue("sort_output", inputXML.RootNode());

  // Index the database files
  m_params["index_database"] = inputXML.GetFirstNodeValue("index_database",inputXML.RootNode());

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

  // Dictionary output: true/false(default)
  if ( m_params["output_dictionary"] == "true" ){
     bDumpDictionary = true;
  }
  else
  {
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

  // Output zone names
  if ( m_params["use_zonenames"] == "true" ){
    bUseZoneNames = true;
  }else{
  //  m_params["use_zonenames"] = "false";  set always to true
  //  bUseZoneNames = false;
    m_params["use_zonenames"] = "true";
    bUseZoneNames = true;
  }

  // Output surface names
  if ( m_params["use_surfacenames"] == "true" ){
    bUseSurfaceNames = true;
  }else{
  //  m_params["use_surfacenames"] = "false";  set always to true
  //  bUseSurfaceNames = false;
    m_params["use_surfacenames"] = "true";
    bUseSurfaceNames = true;
  }

  // Output geometric data
  if ( m_params["output_geomdat"] == "true" ){
    bOutputGeomDat = true;
  }else{
    m_params["output_geomdat"] = "false";
    bOutputGeomDat = false;
  }

  // Timestep averaging
  if ( m_params["time_step_averaging"] == "false" ){
      bTS_averaging = false;
  }else if (m_params["time_step_averaging"] == "true" ){
      bTS_averaging = true;
  }

   // Optionally store data on disk (push step every x)
   // default - false
   if ( m_params["save_to_disk"] == "true" )
      bSaveToDisk = true;
   else
      bSaveToDisk = false;

   //Optional size to store, depending on the amount of variable
   //for the step data store, the default 10000 should keep
   //the memory footprint below 200MB, a 100 for example will
   //keep is below 50MB.  The ideal setting depends on the
   //system running the engine, the goal is to stay out of
   //virtual memory.
   m_lSaveToDisk = atoi(m_params["save_to_disk_every"].c_str());
   if(m_lSaveToDisk == 0)
      m_lSaveToDisk = SAVE_TO_DISK_DEFAULT;
   else if(m_lSaveToDisk < SAVE_TO_DISK_MIN)
      m_lSaveToDisk = SAVE_TO_DISK_MIN;
   else if(m_lSaveToDisk > SAVE_TO_DISK_MAX)
      m_lSaveToDisk = SAVE_TO_DISK_MAX;

  // Optional sort output data, default true
  if (m_params["sort_output"] == "false")
    bSortOutput = false;
  else
    bSortOutput = true;

   // Optional index database, default true for legacy support
   if (m_params["index_database"] == "false")
      bIndexDatabase = false;
   else
      bIndexDatabase = true;

   //Log output mode: default XML
   bOutLogXML = bOutLogDB = false;
   if(m_params["log_output_format"] == "all")
      bOutLogXML = bOutLogDB = true;
   else if(m_params["log_output_format"] == "db")
      bOutLogDB = true;
   else if(m_params["log_output_format"] == "xml")
      bOutLogXML = true;
   else
      bOutLogXML = true;


   //Step output mode: default CSV
   bOutStepDB = bOutStepCSV = false;
   if(m_params["step_output_format"] == "all")
      bOutStepDB = bOutStepCSV = true;
   else if(m_params["step_output_format"] == "db")
      bOutStepDB = true;
   else if(m_params["step_output_format"] == "csv")
      bOutStepCSV = true;
   else
     bOutStepCSV = true;

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
string TReportsManager::ReportConfig(std::string cParam){
   return m_params[cParam];
}

/**
 *   Toggle value of requested parameter (true/false)
 *
 */
bool TReportsManager::ToggleConfig(std::string cParam){
   bool bSuccess;
   bSuccess = true;


   //boolean type settings, true/false
   if ( m_params [ cParam ] == "true" ){
      m_params [ cParam ] = "false";
   }else{
      m_params [ cParam ] = "true";
   }

   return bSuccess;
}

/**
 *   Report boolean value of requested parameter (true/false)
 *
 */
bool TReportsManager::ReportBoolConfig(std::string cParam){
  bool bReturn;

  //boolean type settings, true/false
  if ( m_params [ cParam ] == "true" ){
    bReturn = true;
  }else{
    bReturn = false;
  }

  return bReturn;
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


/* ********************************************************************
** Method:   GetOutputType()
** Purpose:  Return the output type requested for a passed in variable
** Params:   variabled name
** Returns:  unsigned char, bitwize representation of the output type
** Author:   Claude Lamarche
** Mod Date: 2011-07-12
** ***************************************************************** */
unsigned char TReportsManager::GetOutputType(const char* search_text){
   Wildcard Cwildcard_engine;
   unsigned char cOutputType = 0x00;
   set<string>::iterator it;
   string sStr;

   if ( bDumpEverything ){
    //Bitwise or
    cOutputType = cOutputType | OUT_ALL;
   }
   else{
      if(bWildCardsEnabled)
      {
         //for each vector loop and test for match and set the return bit
         for(it = m_nodes.begin(); it != m_nodes.end(); it++)
         {
            sStr = *it;
            if(Cwildcard_engine.wildcardfit(sStr.c_str(),search_text) == 1)
            {
               cOutputType = cOutputType | OUT_LOG;
               break;
            }
         }
         for(it = m_step_nodes.begin(); it != m_step_nodes.end(); it++)
         {
            sStr = *it;
            if(Cwildcard_engine.wildcardfit(sStr.c_str(),search_text) == 1)
            {
               cOutputType = cOutputType | OUT_STEP;
               break;
            }
         }
         for(it = m_summary_nodes.begin(); it != m_summary_nodes.end(); it++)
         {
            sStr = *it;
            if(Cwildcard_engine.wildcardfit(sStr.c_str(),search_text) == 1)
            {
               cOutputType = cOutputType | OUT_SUMMARY;
               break;
            }
         }
      }
      else
      {
         //Bitset the output type for return
         if(m_nodes.find(search_text) != m_nodes.end())
            cOutputType = cOutputType | OUT_LOG;
         if(m_step_nodes.find(search_text) != m_step_nodes.end())
            cOutputType = cOutputType | OUT_STEP;
         if(m_summary_nodes.find(search_text) != m_summary_nodes.end())
            cOutputType = cOutputType | OUT_SUMMARY;
      }
   }
   return cOutputType;
}


/**
 * Delete temporary file
 */
void TReportsManager::Cleanup(){

   //This will trigger the destructor
   delete Instance();
   delete log::Instance();

   return;
}
