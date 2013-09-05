#define DEBUG 0

#include "TReportData.h"

/* ********************************************************************
** Method:   Default constructor
** Scope:    public
** Purpose:  Initialized class members
** Params:   N/A
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-07-22
** ***************************************************************** */
TReportData::TReportData()
{
   //Call overloaded constructor
   TReportData(false);
   //printf("RepData Constructor at: %p\n",this);
}


/* ********************************************************************
** Method:   Overloaded constructor
** Scope:    public
** Purpose:  Initialized class members
** Params:   bStarted, true/false if the simulation is out of startup mode
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-15
** ***************************************************************** */
TReportData::TReportData(bool bStarted)
{
   //set default to a 365 day, 1 hour timestep
   m_lExpectedTimeSteps = 8760;

   //set the init bit to nothing
   m_cInitBit = 0x00;

   //used for iterating through output
   m_iNextMonthlyBinIndex = 0;
   m_lNextStepIndex = 0;
   m_itNextMonthlyBinIndex = m_MonthlyBinData.begin();
   m_bSimulationStarted = bStarted; //is the sim out of startup?
   m_bReportStartupPeriod = false; //do we report the startup values
   m_fLastValue = 0.0; //initialize the first last value to 0.0
   m_lLastTimeStep = -1; //stores the last time stepped pushed
   m_firstpass = true;
   m_bStepOutput = false; //used for incremental save to file
   m_bTS_averaging = false; //set by the set_time_step_averaging
   m_cOutputType = 0x00; // set by SetOutputType
   m_iDBVariableID = -1;

   //only used when meta variable description is passed
   m_sMetaType.clear();
   m_sVariableType.clear();
   m_sDescription.clear();
}


/* ********************************************************************
** Method:   Default destructor
** Scope:    public
** Purpose:  Destroy initialized class members
** Params:   N/A
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-07-22
** ***************************************************************** */
TReportData::~TReportData()
{
   //printf("RepData Destructor at: %p\n",this);
}


/* ********************************************************************
** Method:   SetSimulationStartDay
** Scope:    public
** Purpose:  Set the simulation's actual start day
** Params:   iSimulationStartDay - number representation of the day
**           ex: december 31st is - 365 --no leap year
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-07-22
** ***************************************************************** */
void TReportData::SetSimulationStartDay(int iSimulationStartDay)
{
   m_iSimulationStartDay = iSimulationStartDay;
}


/* ********************************************************************
** Method:   SetExpectedTimeSteps
** Scope:    public
** Purpose:  Set the expected time steps, this will be used for
**           performance management
** Params:   lExpectedTimeSteps - number of steps expected in the
**           simulation
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-07-22
** ***************************************************************** */
void TReportData::SetExpectedTimeSteps(unsigned long lExpectedTimeSteps)
{
   m_lExpectedTimeSteps = lExpectedTimeSteps;
}


/* ********************************************************************
** Method:   SetOutputType
** Scope:    public
** Purpose:  Set the variable's output type requested
** Params:   cOutType - binary representation of the variable's requested
**                ouput, use defined constants OUT_ALL, OUT_STEP, OUT_LOG
**                OUT_SUMMARY.  To determined the output requested
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-07-22
** ***************************************************************** */
void TReportData::SetOutputType(unsigned char cOutType){
   m_cOutputType = cOutType;

   //Do some vector optimization
   if(m_cOutputType & OUT_STEP || m_cOutputType & OUT_ALL)
      m_StepData.reserve(m_lExpectedTimeSteps);
   if(m_cOutputType & OUT_LOG || m_cOutputType & OUT_ALL)
      m_MonthlyBinData.reserve(12); //max one year, fairly small anyways
}


/* ********************************************************************
** Method:   AddValue
** Scope:    public
** Purpose:  Accessor method to push a value to the storage bins.
** Params:   lTimeStep - the time step attached to the value
**           iMonth - the month index attached to the value
**           fValue - the value to push onto the bins
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-07-22
** ***************************************************************** */
void TReportData::AddValue(const unsigned long &lTimeStep, const int &iMonthIndex,const float &fValue)
{
   // If timestep averaging is requested and this is the first
   // time step of the simulation, there is no 'old value' to
   // average with. Use current value instead.
   if(m_bTS_averaging && lTimeStep >= 0)
   {
      if(m_firstpass)
      {

         m_fLastValue = (fValue + fValue) / 2.0;
         m_firstpass = false;
      }
      else
      {
         m_fLastValue = (fValue + m_fLastValue) / 2.0;
      }
   }


   //only store if the sim is started (startup mode false) or we report on the startup mode
   //and only store if the time step is not the last (handle duplicates)
   //do not store the timestep 0 ~ current fonctionality
   if((m_bSimulationStarted || m_bReportStartupPeriod) && lTimeStep != m_lLastTimeStep && lTimeStep != 0)
   {
      //store step data
      if(m_cOutputType & OUT_STEP || m_cOutputType & OUT_ALL)
         AddToStepData(lTimeStep,m_fLastValue);

      //store log data
      if(m_cOutputType & OUT_LOG || m_cOutputType & OUT_ALL)
         AddToLogData(iMonthIndex,m_fLastValue);

      //store summary data
      if(m_cOutputType & OUT_SUMMARY || m_cOutputType & OUT_ALL)
         AddToSummaryData(m_fLastValue);
   }

   //Current functionality, simulation records value one step prior the start and
   //end one steps prior the end.
   m_fLastValue = fValue;

   //When multiple values are pushed for the same time step we'll handle
   m_lLastTimeStep = lTimeStep;
}


/* ********************************************************************
** Method:   Finalize
** Scope:    public
** Purpose:  Call before printing results, ensure bins are all the same
**           size.  Fills empties with 0.0, push the last value when
**           reporting startup mode.
** Params:   lTimeStep - the last time step
**           iMonth - the last month index
**           lOutputStepCount - number of steps that are already in output files
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-24
** ***************************************************************** */
void TReportData::Finalize(unsigned int lTimeStep, unsigned int iMonthIndex, unsigned long lOutputStepCount)
{
   //push the last value in memory onto the storage bins
   //when reporting the full simulation startup include
   if(m_bReportStartupPeriod)
   {
      //store step data
      if(m_cOutputType & OUT_STEP || m_cOutputType & OUT_ALL)
      {
         AddToStepData(lTimeStep-lOutputStepCount,m_fLastValue);
      }

      //store log data
      if(m_cOutputType & OUT_LOG || m_cOutputType & OUT_ALL)
         AddToLogData(iMonthIndex,m_fLastValue);

      //store summary data
      if(m_cOutputType & OUT_SUMMARY || m_cOutputType & OUT_ALL)
         AddToSummaryData(m_fLastValue);
   }

   //if not last step, fill with 0.0
   FinalizeStepData(lTimeStep - lOutputStepCount);


   //store Monthly bin, fill
   if(m_cOutputType & OUT_LOG || m_cOutputType & OUT_ALL)
   {
      while(m_MonthlyBinData.size() < iMonthIndex+1)
      {
         m_MonthlyBinData.push_back(TBinnedData());
         m_MonthlyBinData.back().Increment();
      }
   }

   //When timestep averaging is on, remove the last step
   if(m_bTS_averaging)
      m_StepData.erase(m_StepData.end() -1 );


}


/* ********************************************************************
** Method:   FinalizeStepData
** Scope:    public
** Purpose:  Call before printing step data to ensure step bins are of the
**           same size.  Fills empties with 0.0
** Params:   lTimeStep - The computed size it should be
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-24
** ***************************************************************** */
void TReportData::FinalizeStepData(unsigned long lTimeStep)
{
   //if not last step, fill with 0.0
   if(m_cOutputType & OUT_STEP || m_cOutputType & OUT_ALL)
   {
      while(m_StepData.size() < lTimeStep)
      {
         m_StepData.push_back(0.0);
      }
   }
}

/* ********************************************************************
** Method:   AddToStepData
** Scope:    private
** Purpose:  push a value to the step data bin
** Params:   lTimeStep - the step number
**           fValue - the variable's value
** Note:     This method will ensure that the bin is the correct size
**           and push 0.0 for the unspecified step.
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-07-22
** ***************************************************************** */
void TReportData::AddToStepData(unsigned long lTimeStep, const double &fValue){
   //push values until same size as timestep
   while(m_StepData.size() < lTimeStep)
   {
      m_StepData.push_back(0.0);
   }
   //handle duplicate - overwrite last everytime
   m_StepData.at(lTimeStep-1) = fValue;
}


/* ********************************************************************
** Method:   AddToSummaryData
** Scope:    private
** Purpose:  push a value to the summary data bin
** Params:   fValue - the variable's value
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-07-22
** ***************************************************************** */
void TReportData::AddToSummaryData(const double &fValue){
   m_AnnualData.AddValue(fValue);

   //Increment step count
   m_AnnualData.Increment();
}


/* ********************************************************************
** Method:   AddToLogData
** Scope:    private
** Purpose:  push a value to the monthly storage bins
** Params:   iMonthIndex - The monthindex the value is attached to.
**           fValue - the variable's value
** Note:     This method will ensure that the bin is the correct size
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-07-22
** ***************************************************************** */
void TReportData::AddToLogData(const int &iMonthIndex, const double &fValue){
   //The TBinnedData should be float and not double
   //for the exception of the sum **to do**

   //push values until the same size as the month index
   while(m_MonthlyBinData.size() < iMonthIndex+1)
   {
      m_MonthlyBinData.push_back(TBinnedData());

   }

   m_MonthlyBinData.back().AddValue(fValue);
   m_MonthlyBinData.back().Increment();

   //The Summary Reporting already tracks the annual bin,
   //do not add a value to the annual bin in that case.
   if(!(m_cOutputType & OUT_SUMMARY || m_cOutputType & OUT_ALL))
      m_AnnualData.AddValue(fValue);
}


/* ********************************************************************
** Method:   IsOutStep
** Scope:    public
** Purpose:  Evaluate if the OUT_STEP is enabled.
** Params:   N/A
** Returns:  true/false
** Author:   Claude Lamarche
** Mod Date: 2011-07-22
** ***************************************************************** */
bool TReportData::IsOutStep(){
   if(m_cOutputType & OUT_STEP || m_cOutputType & OUT_ALL )
      return true;
   else
      return false;
}


/* ********************************************************************
** Method:   IsOutLog
** Scope:    public
** Purpose:  Evaluate if the OUT_LOG is enabled.
** Params:   N/A
** Returns:  true/false
** Author:   Claude Lamarche
** Mod Date: 2011-07-22
** ***************************************************************** */
bool TReportData::IsOutLog(){
   if(m_cOutputType & OUT_LOG || m_cOutputType & OUT_ALL )
      return true;
   else
      return false;
}


/* ********************************************************************
** Method:   IsOutSummary
** Scope:    public
** Purpose:  Evaluate if the OUT_SUMMARY is enabled.
** Params:   N/A
** Returns:  true/false
** Author:   Claude Lamarche
** Mod Date: 2011-07-22
** ***************************************************************** */
bool TReportData::IsOutSummary(){
   if(m_cOutputType & OUT_SUMMARY || m_cOutputType & OUT_ALL )
      return true;
   else
      return false;
}


/* ********************************************************************
** Method:   IsVariableDescriptionWild
** Scope:    public
** Purpose:  True/false if the wildcards for the description, unit
**           and type are set.  Will only evaluate the description
**           to make this determination.  Most of the time this
**           routine will return false.
** Params:   N/A
** Returns:  true/false
** Author:   Claude Lamarche
** Mod Date: 2011-08-30
** ***************************************************************** */
bool TReportData::IsVariableDescriptionWild()
{
   if(m_sDescription.size()==0)
      return false;
   else
      return true;
}

/* ********************************************************************
** Method:   SetVariableDescriptoinWild
** Scope:    public
** Purpose:  Set the variable's dynamicly generated variable description.
**           The description is normally static and not set here.  This
**           is to handle a few cases where ESP-r determines the variable
**           description at run time.
** Params:   sMetaType - The meta type "units"
**           sVariableType - the variable type "(W),(oC)..."
**           sDescription - the text description
** Returns:  true/false
** Author:   Claude Lamarche
** Mod Date: 2011-08-30
** ***************************************************************** */
bool TReportData::SetVariableDescriptoinWild(const string& sMetaType,
                                             const string& sVariableType,
                                             const string& sDescription)
{
   m_sMetaType = sMetaType;
   m_sVariableType = sVariableType;
   m_sDescription = sDescription;
}

//Getter's, variables only have values when fortran calls AddToReportDetails
//routines, this should be the exception and not the norm.
string& TReportData::getVariableDescription(){return m_sDescription;}
string& TReportData::getVariableMetaValue(){return m_sVariableType;}
string& TReportData::getVariableMetaType(){return m_sMetaType;}



/* ********************************************************************
** Method:   GetNextStepValue
** Scope:    public
** Purpose:  consuming routine, return the next value, when -1.0 check
**           with GetNextStepValueEnd() to see if end was reached.
**           Use GetNextStepValueReset() to restart the index.
** Params:   N/A
** Returns:  float value and the next step index.
** Author:   Claude Lamarche
** Mod Date: 2011-08-08
** ***************************************************************** */
double TReportData::GetNextStepValue()
{
   double rtn = -1.0;

   //Check that it's not the end
   if(m_lNextStepIndex < m_StepData.size())
   {
      rtn = m_StepData.at(m_lNextStepIndex++);
      if(m_lNextStepIndex == m_StepData.size())
      {
         m_bNextStepIndexEnd = true;
      }
   }
   else
   {
      m_bNextStepIndexEnd = true;
   }

   return rtn;
}


/* ********************************************************************
** Method:   GetNextStepValueEnd
** Scope:    public
** Purpose:  Indicator to see if the GetNextStepValue has reach the end
** Params:   N/A
** Returns:  true/false is GetNextStepValue reached end
** Author:   Claude Lamarche
** Mod Date: 2011-08-08
** ***************************************************************** */
bool TReportData::GetNextStepValueEnd()
{
   return m_bNextStepIndexEnd;
}


/* ********************************************************************
** Method:   GetNextStepValueReset
** Scope:    public
** Purpose:  reset the GetNextStep index and end flag
** Params:   N/A
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-08
** ***************************************************************** */
void TReportData::GetNextStepValueReset()
{
   m_lNextStepIndex = 0;
   m_bNextStepIndexEnd = false;
}


/* ********************************************************************
** Method:   GetStepValue
** Scope:    public
** Purpose:  return the value at a specified index
** Params:   index - the position
** Note:     This is note a safe routine it expects the calling
**           method to ensure, the value exists.
** Returns:  the value at passed in index
** Author:   Claude Lamarche
** Mod Date: 2011-07-22
** ***************************************************************** */
double TReportData::GetStepValue(unsigned long index)
{
   return m_StepData.at(index);
}

/* ********************************************************************
** Method:   EraseSteps
** Scope:    public
** Purpose:  Remove the first x number of data from the step container
** Params:   index - the position
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-24
** ***************************************************************** */
void TReportData::EraseSteps(unsigned long lNumberOfSteps)
{
   if(m_StepData.size() >= lNumberOfSteps)
   {
      m_StepData.erase(m_StepData.begin(),m_StepData.begin()+lNumberOfSteps);
      m_lLastTimeStep-=lNumberOfSteps;
   }
}


/* ********************************************************************
** Method:   GetAnnualBin
** Scope:    public
** Purpose:  gets the annual bin and returns a pointer to it.
** Params:   N/A
** Returns:  Pointer to the m_AnnualData bin
** Kludge:   Access to the TBinnedData should be wrapped but this
**           makes coding allot quicker.  Since we can trust the
**           calling method not to change anything in the Bin, this
**           should not cause any issues.
** Author:   Claude Lamarche
** Mod Date: 2011-07-22
** ***************************************************************** */
TBinnedData* TReportData::GetAnnualBin()
{
   return &m_AnnualData;
}


/* ********************************************************************
** Method:   GetNextMonthlyBin
** Scope:    public
** Purpose:  consuming routine, return the next pointer to the monthly
**           bin, NULL is return when end is reached.  Sister method
**           GetNextMonthlyBinReset will enable to start loop over.
** Params:   N/A
** Returns:  pointer to TBinnedData object, NULL when end is there
** Author:   Claude Lamarche
** Mod Date: 2011-08-11
** ***************************************************************** */
TBinnedData* TReportData::GetNextMonthlyBin()
{
   //return the next iterator
   if(m_itNextMonthlyBinIndex == m_MonthlyBinData.end())
      return NULL;
   else
      return &*m_itNextMonthlyBinIndex++;
}


/* ********************************************************************
** Method:   GetNextMonthlyBinReset
** Scope:    public
** Purpose:  Reset the count to enabel GetNextMonthlyBin to start over
** Params:   N/A
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-11
** ***************************************************************** */
void TReportData::GetNextMonthlyBinReset()
{
   m_iNextMonthlyBinIndex = 0;
   m_itNextMonthlyBinIndex = m_MonthlyBinData.begin();
}


/* ********************************************************************
** Method:   GetStepCount
** Scope:    public
** Purpose:  Returns the number of stored steps in the m_StepData vector
** Params:   N/A
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-15
** ***************************************************************** */
unsigned long TReportData::GetStepCount()
{
   return m_StepData.size();
}


/* ********************************************************************
** Method:   SetSimulationStarted
** Scope:    public
** Purpose:  true/false if the simulation is startup, sets the
** Params:   bStarted - true/flase
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-15
** ***************************************************************** */
void TReportData::SetSimulationStarted(bool bStarted)
{
   m_bSimulationStarted = bStarted;
}


/* ********************************************************************
** Method:   SetReportStartupPeriod
** Scope:    public
** Purpose:  true/false if we are to report the startup or not
** Params:   bReportStartup - true/false
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-22
** ***************************************************************** */
void TReportData::SetReportStartupPeriod(bool bReportStartup)
{
   m_bReportStartupPeriod = bReportStartup;
}


/* ********************************************************************
** Method:   SetTimeStepAveraging
** Scope:    public
** Purpose:  true/false if time step averaging is on
** Params:   bTS_averaging - true/false
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-22
** ***************************************************************** */
void TReportData::SetTimeStepAveraging(bool bTS_averaging)
{
   m_bTS_averaging = bTS_averaging;
}


/* ********************************************************************
** Method:   PrintLogData
** Scope:    private
** Purpose:  For testing only
** Params:   N/A
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-07-22
** ***************************************************************** */
void TReportData::PrintLogData()
{
   BinnedDataVector::iterator it;

   for ( it=m_MonthlyBinData.begin() ; it < m_MonthlyBinData.end(); it++ )
   {
     printf("%s: Steps:%d\n",sVarName,(int)it->ActiveTimesteps());
   }
}


/* ********************************************************************
** Method:   printAll
** Scope:    public
** Purpose:  Testing routine
** Params:   N/A
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-07-22
** ***************************************************************** */
void TReportData::printAll()
{
   StepDataVector::const_iterator it;
   int i = 1;

   for(it = m_StepData.begin();it<m_StepData.end();it++)
   {
      printf("Step: %d; Value: %f\n",i++,*it);
   }
}
