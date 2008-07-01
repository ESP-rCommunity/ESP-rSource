
#ifndef TVARIABLEDATA_H
#define TVARIABLEDATA_H

#ifdef _WIN32
#pragma warning (disable: 4786) //to disable annoying "identifier was truncated to '255' characters in the debug information" in VC++
#endif

#include "TBinnedData.h"
#include "TXMLAdapter.h"
#include <vector>
#include <string>
#include <iostream>
#include <fstream>
#include <map>




typedef  std::map<std::string, std::string> TMetadataMap;

struct TTimeData
{
        int month;
        int day;
        int hour;
        int step;

        //default constructor
        TTimeData(int m = -1, int d = -1, int h = -1, int s =-1)
        : month(month), day(day), hour(hour), step(step)
        {}

        //copy constructor
        TTimeData(const TTimeData& t)
        : month(t.month), day(t.day), hour(t.hour), step(t.step)
        {}

};

//const int kMonthlyTimesteps[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
const int kMonthlyTimesteps[] = {31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365};


/**
* class TVariableData
TVariable Data contains the composite data for fro a particular variables. It stores all data (timestep and binned for hourly, daily, monthly and annually).

 */

class TVariableData
{

        /** Public methods: */
public:

  TVariableData();

  ///Sets the current value of the variable. This method gets called often, so it should be fast.
    void Set(const double& val, 
             const bool& bTS_averaging, 
             const bool& bSaveToDisk,
             const int& timestep,
             std::vector<int>& m_month_ts_bin);
             
    void SetMeta(const std::string& sMetaName, const std::string& sMetaValue);

    // Push current values onto vector
    void Update( const bool& bSaveToDisk, const long& TimeRow );
    
    
    void UpdateHourly(  );
    void UpdateDaily(  );
    void UpdateMonthly(  );
    void UpdateAnnual(  );
    void UpdateUserDefined(  );

    // Recover value i from time step data
    double RetrieveValue ( unsigned int i, 
                           const unsigned int& first_step,
                           const bool& bSaveToDisk );

    // Query search status
    bool QuerySearchStatus(int& i);

    // Update the status of variable's searches
    void UpdateSearchStatus(int& i, bool bStatus);

    // Query search results
    bool QuerySearchResult(int& i);

    // Update past search result
    void UpdateSearchResult(int& i, bool bResult);

    // Recover data from meta array
    std::string RetrieveMeta(const std::string&  RequestMeta );

    TBinnedData GetDataForDate(const TTimeData& date);

    void Log();
    void OutputXML(TXMLAdapter *doc,const TXMLNode& parentNode, std::map<std::string, std::string>& params);
    std::string OutputTXT( const std::string& prefex, std::map<std::string, std::string>& params );

   
 protected:
    
    int set_vector_index;
    
    std::vector<double> m_steps;
    std::vector<TBinnedData> m_hourly;
    std::vector<TBinnedData> m_daily;
    std::vector<TBinnedData> m_monthly;
    TBinnedData m_annual;
    TBinnedData m_userDefined;

    bool bNewValueSentThisTimestep;

    TMetadataMap m_metadata;
    double m_currentValue;
    double m_oldValue;
    double m_newValue;
    bool bFirstCall;
    bool bTS_averaging;
    bool bNotSet; 

    // Status of serches
    bool bSummarySearchStatus;
    bool bLogSearchStatus;
    bool bStepSearchStatus;
    

    // Results of serches
    bool bSummarySearchResult;
    bool bLogSearchResult;
    bool bStepSearchResult;

};

#endif // TVARIABLEDATA_H
