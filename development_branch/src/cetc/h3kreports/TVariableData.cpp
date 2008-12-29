
/*
 *  TVariableData.cpp
 *  H3KReports
 *
 *  Created by Shawn Henry on Tue Mar 09 2004.
.*  CETC Natural Resources Canada
 *
 */
#ifdef _WIN32
#pragma warning (disable: 4786) //to disable annoying "identifier was truncated to '255' characters in the debug information" in VC++
#endif

#include <algorithm>
#include <cstdlib>
#include "TVariableData.h"

#define DEBUG 0

// Named constants
#define SUMMARY 0
#define LOG     1
#define STEP    2

// Flag to enable timstep data storage in xml output. Note: XML'd time-step data
// is presently suppressed, since time-step data is dumped to ascii csv format
// instead.
#define XMLtimestep 0

using namespace std;

//Prototypes
int Increment_set_vector_count();
bool Write_to_db_file(int var_index, double val, long step);
double Read_from_db_file(int var_index, long step);

//hack for VC++ not having std::max and std::min
namespace h3kreports {

  template <class T>
  inline const T& min(const T& a, const T& b) {
    return b < a ? b : a;
  }

  template <class T>
  inline const T& max(const T& a, const T& b) {
    return a < a ? b : a;
  }
}

///convert integer i to string
std::string StringValue(int i)
{
  char buffer[256];

#ifdef _WIN32
  _snprintf(buffer, 255, "%i", i);
#endif

#ifndef _WIN32
  snprintf(buffer, 255, "%i", i);
#endif

  return string(buffer);
}

///convert double f to string
std::string StringValue(double f)
{
  char buffer[256];

#ifdef _WIN32
  _snprintf(buffer, 255, "%f", f);
#endif

#ifndef _WIN32
  snprintf(buffer, 255, "%f", f);
#endif

  return string(buffer);
}

TVariableData::TVariableData()
{
  bFirstCall = true;
  bNewValueSentThisTimestep = false;

  // Search status flags.
  bSummarySearchStatus = false;
  bLogSearchStatus = false;
  bStepSearchStatus = false;
  bNotSet = true; 

  UpdateHourly();
  UpdateDaily();
  UpdateMonthly();
  UpdateAnnual();
  if (DEBUG) cout << " init: " << bFirstCall << "\n";



  //    m_steps.reserve(200000);
}


/*
  Take passed value, and apply time-step averaging if necessary.
*/
void TVariableData::Set(const double& val,
                        const bool& bTS_averaging,
                        const bool& bSaveToDisk,
                        const int& timestep,
                        std::vector<int>& m_month_bin_ts)
{

  // The first time this variable is set, 
  // increment vector count, and save new value 
  // as this vector's index
  if ( bNotSet ){
     set_vector_index = Increment_set_vector_count();
     bNotSet = false;
  }
  
  

  int ii;

  if (DEBUG) cout << " run: " << bFirstCall << "\n";

  // If this is the first time this variable has been passed
  // to h3kreports and this is *not* the first timestep in the
  // simulation, we'll need to 'zero' the proceeding timestep
  // records for this variable.
  //
  // To do so, call update once for each timestep that has
  // already lapsed.

  if ( timestep > 0 && bFirstCall ){
      for (ii=0;ii<timestep;ii++){

        //Possibly bin a previous month's worth of data
        if ( m_month_bin_ts.at(ii) == 1 ){ UpdateMonthly();}
      
        Update( bSaveToDisk, ii );
        
      }
  }

  // If timestep averaging is requested and this is the first
  // time step of the simulation, there is no 'old value' to
  // average with. Use current value instead.
  if ( bTS_averaging && bFirstCall ){
    m_oldValue = val;
  }
  // apply timestep averaging, if necessary
  if ( bTS_averaging ) {
    m_newValue = val;
    m_currentValue = ( m_oldValue + m_newValue ) / 2.0;
  }else{
    m_currentValue = val;
  }

  if(DEBUG) cout << " avg:  "
                 << " tsavg " << bTS_averaging
                 << " fst c " << bFirstCall
                 << " old   " << m_oldValue
                 << " new   " << m_newValue  << "\n";
  bNewValueSentThisTimestep = true;
  bFirstCall = false;
}

void TVariableData::SetMeta(const std::string& sMetaName, const std::string& sMetaValue)
{

  //too slow in VC++
  //m_metadata[sMetaName] = sMetaValue;

  TMetadataMap::iterator pos;

  //if sMetaName not found
  if((pos = m_metadata.find(sMetaName)) == m_metadata.end())
    {
      m_metadata.insert(make_pair(sMetaName, sMetaValue)); //create and insert a new pair
    }

}
/**
 * Take current value, and i) save it to a vector, or ii) save it to disk
 */
 
void TVariableData::Update( const bool& bSaveToDisk, const long& TimeRow )
{

  double SaveValue;
  
  // If new data was sent, use it.  
  if(bNewValueSentThisTimestep == true)
    {
      // update old for timestep averaging
      m_oldValue = m_newValue;
      
      SaveValue = m_currentValue;
      
      // Add current value to running hourly/daily/monthly/annual bins
      m_hourly.back().AddValue(m_currentValue);
      m_daily.back().AddValue(m_currentValue);
      m_monthly.back().AddValue(m_currentValue);
      m_annual.AddValue(m_currentValue);
       
    }
  // Otherwise, push a value of zero onto the stack...
  else
    {

      // set old value to zero for timestep averaging
      m_oldValue = 0.0;
      
      // push empty value on to vector.
      
      SaveValue = 0.0;
      
    }

  if ( bSaveToDisk ) {
    // Save file to database
    if ( ! Write_to_db_file(set_vector_index,SaveValue,TimeRow) ){
      cout << "Error: Could not write to file\n";
    }
  }else{
    // push value on to vector. 
    m_steps.push_back(SaveValue);
  }

  m_hourly.back().Increment();
  m_daily.back().Increment();
  m_monthly.back().Increment();
  m_annual.Increment();
  bNewValueSentThisTimestep = false;
}



void TVariableData::UpdateHourly(  )
{
  m_hourly.push_back(TBinnedData());
}

void TVariableData::UpdateDaily(  )
{
  m_daily.push_back(TBinnedData());
}

void TVariableData::UpdateMonthly(  )
{
  m_monthly.push_back(TBinnedData());
}


void TVariableData::UpdateAnnual(  )
{

}


void TVariableData::UpdateUserDefined(  )
{

 //m_userDefined.AddValue(m_currentValue);
 //m_userDefined.Increment();

}



/*
 * ReturnSearchStatus:
 * Checks to see if a search has been performed, and
 * returns true if one has. Returns false otherwise.
 *
 */


bool TVariableData::QuerySearchStatus(int& i){

  if ( i == SUMMARY ){
    return bSummarySearchStatus;
  }else if ( i == LOG ){
    return bLogSearchStatus;
  }else{
    return bStepSearchStatus;
  }
}


/*
 * update search status
 * updates the search status flags to indicate that
 * a search was completed.
 *
 */

void TVariableData::UpdateSearchStatus(int& i, bool bStatus){

  if ( i == SUMMARY ){
    bSummarySearchStatus = bStatus;
  }
  if ( i == LOG ){
    bLogSearchStatus = bStatus;
  }
  if ( i == STEP ){
    bStepSearchStatus = bStatus;
  }

  return;
}

/*
 * Query past matches:
 * Returns the result from the last time the
 * search was performed.
 *
 */


bool TVariableData::QuerySearchResult(int& i){

  if ( i == SUMMARY ){
    return bSummarySearchResult;
  }
  else if ( i == LOG ){
    return bLogSearchResult;
  }
  else{
    return bStepSearchResult;
  }
}


/*
 * Update serarch result
 * updates the search status flags to indicate that
 * a search was completed.
 *
 */

void TVariableData::UpdateSearchResult(int& i, bool bResult){

  if ( i == SUMMARY ){
    bSummarySearchResult = bResult;
  }
  if ( i == LOG ){
    bLogSearchResult = bResult;
  }
  if ( i == STEP ){
    bStepSearchResult = bResult;
  }

  return;
}



/* RetrieveValue:
 * Returns value i for object containing step variable
 *
 */

double TVariableData::RetrieveValue(unsigned int i, 
                                    const unsigned int& first_step, 
                                    const bool& bSaveToDisk){
  if ( bSaveToDisk ){
    // pull value from disk 
    return Read_from_db_file ( set_vector_index, i + first_step ); 
  }else{
    // pop value from vector. 
    return m_steps[i];
  }

}

std::string TVariableData::RetrieveMeta(const std::string& RequestMeta ){
  TMetadataMap::iterator pos;
  std::string result;
  result = "";
  for(pos = m_metadata.begin(); pos != m_metadata.end(); ++pos){
    if ( pos->first == RequestMeta ){
      result =  pos->second;
    }
  }
  return result;
}

void TVariableData::OutputXML(TXMLAdapter *doc,
                              const TXMLNode& parentNode,
                              map<std::string, std::string>& params
                              )
{
  unsigned int i;
  int aBin[10]; //annual binned data
  int mBin[12][10]; //monthly binned data
  float minsPerStep;


  if(params["minutes_per_step"] != ""){
    minsPerStep = float( strtod(params["minutes_per_step"].c_str(), NULL) );
  }
  else{
    minsPerStep = 5; //default 5 mins per step
  }

  //output metadata
  TMetadataMap::iterator pos;
  std::string Units;
  for(pos = m_metadata.begin(); pos != m_metadata.end(); ++pos)
    {
      if ( pos->first == "units"){
        // save units
        Units = pos->second;
      }
      doc->AddNode(parentNode, pos->first, pos->second);
    }

  //PLR data
  if(m_metadata.find("PLRD") != m_metadata.end())
    {

      unsigned int j;
      double denominator = strtod(m_metadata["PLRD"].c_str(), (char **)NULL);
      memset(aBin, 0, sizeof(aBin));
      memset(mBin, 0, sizeof(mBin));
      for(j = 0; j < m_daily.size(); j++)
        {
          int binIndex = int(m_daily[j].TotalAverage() * 10.0 / denominator);
          h3kreports::min(binIndex, 9); h3kreports::max(binIndex, 0);

          //monthly bins
          if(j < (unsigned int)kMonthlyTimesteps[0])
            mBin[0][binIndex] +=1;
          else
            {
              unsigned int k;
              for(k = 0; k < 11; k++)
                if((j > (unsigned int)kMonthlyTimesteps[k]) && (j < (unsigned int)kMonthlyTimesteps[k+1]))
                  mBin[k+1][binIndex] +=1;
            }

          aBin[binIndex] += 1;
        }
    }

  //output monthly data
  for(i = 0; i < m_monthly.size(); i++)
    {
      TXMLNode binnedNode = doc->AddNode(parentNode, "binned_data", "");
      doc->AddAttribute(binnedNode, "type", "monthly");
      doc->AddNode(binnedNode, "index", StringValue((int)i));
      doc->AddNode(binnedNode, "steps", StringValue((int)m_monthly[i].Timesteps()));
      doc->AddNode(binnedNode, "active_steps", StringValue((int)m_monthly[i].ActiveTimesteps()));
      doc->AddNode(binnedNode, "sum", StringValue(m_monthly[i].Sum()));
      doc->AddNode(binnedNode, "max", StringValue(m_monthly[i].Max()));
      doc->AddNode(binnedNode, "min", StringValue(m_monthly[i].Min()));
      doc->AddNode(binnedNode, "active_average", StringValue(m_monthly[i].ActiveAverage()));
      doc->AddNode(binnedNode, "total_average", StringValue(m_monthly[i].TotalAverage()));

      //Monthly PLR data
      if(m_metadata.find("PLRD") != m_metadata.end())
        {
          unsigned int j;
          TXMLNode plrNode = doc->AddNode(binnedNode, "plrdata", "");
          for(j = 0; j < 10; j++)
            {
              TXMLNode tNode = doc->AddNode(plrNode, "bin", StringValue(mBin[i][j]));
              doc->AddAttribute(tNode, "number", StringValue((int)j*10));
            }
        }

    }

  //Annual data
  TXMLNode binnedNode = doc->AddNode(parentNode, "binned_data", "");
  doc->AddAttribute(binnedNode, "type", "annual");
  doc->AddNode(binnedNode, "steps", StringValue((int)m_annual.Timesteps()));
  doc->AddNode(binnedNode, "active_steps", StringValue((int)m_annual.ActiveTimesteps()));
  doc->AddNode(binnedNode, "sum", StringValue(m_annual.Sum()));
  if(m_annual.ActiveTimesteps() > 0) //these are only valid if the variable is active
    {
      doc->AddNode(binnedNode, "max", StringValue(m_annual.Max()));
      doc->AddNode(binnedNode, "min", StringValue(m_annual.Min()));
    }
  else
    {
      doc->AddNode(binnedNode, "max", "NaN");
      doc->AddNode(binnedNode, "min", "NaN");
    }
  doc->AddNode(binnedNode, "active_average", StringValue(m_annual.ActiveAverage()));
  doc->AddNode(binnedNode, "total_average", StringValue(m_annual.TotalAverage()));

  //Annual PLR data
  if(m_metadata.find("PLRD") != m_metadata.end())
    {
      unsigned int j;
      TXMLNode plrNode = doc->AddNode(binnedNode, "plrdata", "");
      for(j = 0; j < 10; j++)
        {
          TXMLNode tNode = doc->AddNode(plrNode, "bin", StringValue(aBin[j]));
          doc->AddAttribute(tNode, "number", StringValue((int)j*10));
        }
    }


  //Custom manipulation: Integrate mass/energy flows
  if(Units == "(W)"       ||
     Units == "(kg/s)"    ||
     Units == "(kWh/s)"   ||
     Units == "(l/s)"     ||
     Units == "(m3/s)"    ||
     Units == "(tonne/s)" )
    {
      unsigned int j;
      double maxTotal = 0;
      TXMLNode currentNode;
      // Add node to hold integrated data
      currentNode = doc->AddNode(parentNode, "integrated_data", "");
      if (Units == "(W)"      ){ doc->AddAttribute(currentNode, "units", "GJ"    );}
      if (Units == "(kg/s)"   ){ doc->AddAttribute(currentNode, "units", "kg"    );}
      if (Units == "(kWh/s)"  ){ doc->AddAttribute(currentNode, "units", "kWh"   );}
      if (Units == "(l/s)"    ){ doc->AddAttribute(currentNode, "units", "l"     );}
      if (Units == "(m3/s)"   ){ doc->AddAttribute(currentNode, "units", "m3"    );}
      if (Units == "(tonne/s)"){ doc->AddAttribute(currentNode, "units", "tonne" );}

      // Integrate monthly bin data
      for(j = 0; j < m_monthly.size(); j++)
        {
          int tSteps;
          if(j > 0) tSteps = (kMonthlyTimesteps[j] - kMonthlyTimesteps[j-1]);
          else tSteps = kMonthlyTimesteps[0];
          // Integrate data over month
          double total = m_monthly[j].Sum() * minsPerStep * 60. ;
          // for W->J, convert result to GJ
          if ( Units == "(W)"){ total = total / 1e09;}
          TXMLNode tNode = doc->AddNode(currentNode, "bin", StringValue(total));
          doc->AddAttribute(tNode, "number", StringValue((int)j));
          doc->AddAttribute(tNode, "type", "monthly");
        }
      // And integrate annual data 
      double total = m_annual.Sum() * minsPerStep * 60.;
      if ( Units == "(W)"){ total = total / 1e09;}
      TXMLNode tNode = doc->AddNode(currentNode, "bin", StringValue(total));
      doc->AddAttribute(tNode, "type", "annual");
    }



  // Output timestep data: presently disabled, as this feature and the
  // ascii csv output are redundant, and the ascii output is compatable with
  // windows platforms.
  if(m_metadata.find("StepData") != m_metadata.end() && XMLtimestep )
    {
      int k,step_start;
      if (bTS_averaging ) {
        //skip first step
        step_start = 1;
      }else{
        step_start = 0;
      }
      TXMLNode binnedNode = doc->AddNode(parentNode, "binned_data", "");
      doc->AddAttribute(binnedNode, "type", "step");
      // loop through each time step. Note: first time step
      // will be skipped if 'timestep_averaging' has been
      // requested.
      for(i = step_start; i < m_steps.size(); i++)
        {
          // temp vbl k is required to prevent overloading of
          // StringValue(k) function
          k = i;
          // build time step value leaf
          TXMLNode step_value = doc->AddNode(binnedNode, "value", StringValue(m_steps[i]));
          // attribute timestep value leaf with index integer.
          // speeds up xslt manipulation
          doc->AddAttribute(step_value, "ts", StringValue(k));
        }
    }
}

/*
 * OutputTXT:
 *
 * Returns a string containing summary data for the current object
 *
 */

std::string TVariableData::OutputTXT( const std::string& prefex, map<std::string, std::string>& params )
{

  std::string Result;
  std::string Units;
  float minsPerStep;
  TMetadataMap::iterator pos;

  if(params["minutes_per_step"] != ""){
    minsPerStep = float(strtod(params["minutes_per_step"].c_str(), NULL));
  }
  else{
    minsPerStep = 5.0; //default 5 mins per step
  }

  // get units
  pos = m_metadata.find("units");
  if ( pos != m_metadata.end()){
    Units = pos->second;
  }

  /*
   * output annual data
   * -> perhaps a future improvement would permit monthly data
   *    to be requested via a flag in input.xml?
   *
   */

  Result = prefex + "::Total_Average " + StringValue(m_annual.TotalAverage()) + " " + Units + " \n";
  Result = Result + prefex + "::Active_Average " + StringValue(m_annual.ActiveAverage()) + " " + Units + "\n";
  if(m_annual.ActiveTimesteps() > 0){
    //these are only valid if the variable is active
    Result = Result + prefex + "::Maximum " + StringValue(m_annual.Max()) +" " + Units + "\n";
    Result = Result + prefex + "::Minimum " + StringValue(m_annual.Min()) +" " + Units + "\n";
  }else{
    Result = Result + prefex + "::Maximum " + "NaN " + Units + "\n";
    Result = Result + prefex + "::Minimum " + "NaN " + Units + "\n";
  }
  if( Units == "(W)"){
    double gj = m_annual.Sum() * minsPerStep * 60. / 1e09;
    Result = Result + prefex + "::AnnualTotal " + StringValue(gj) + " (GJ) \n";
  }
  if( Units == "(kg/s)"){
    double kg = m_annual.Sum() * minsPerStep * 60.;
    Result = Result + prefex + "::AnnualTotal " + StringValue(kg) + " (kg) \n";
  }

  return Result;
}

