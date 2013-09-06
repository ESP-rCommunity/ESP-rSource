#ifndef TREPORTDATA_H
#define TREPORTDATA_H

#ifdef _WIN32
#pragma warning (disable: 4786) //to disable annoying "identifier was truncated to '255' characters in the debug information" in VC++
#endif

#include <cstdio>
#include <cstdlib>
#include <vector>
#include <string>
#include "TBinnedData.h"
#include "log.h"

#include <stdexcept>

//Should build a common include for common functions/structures and defines.
#define OUT_ALL     0x01  // 0000 0001
#define OUT_SUMMARY 0x02  // 0000 0010
#define OUT_LOG     0x04  // 0000 0100
#define OUT_STEP    0x08  // 0000 1000
#define OUT_ANY     0xFF  // 0000 0000


using namespace std;

typedef std::vector<double> StepDataVector;
typedef std::vector<TBinnedData> BinnedDataVector;

class TReportData
{
   public:
      TReportData();
      TReportData(bool bStartup);
      ~TReportData();

      unsigned char GetOutputType();
      void SetOutputType(unsigned char OutputType);
      void AddValue(const unsigned long &lTimeStep, const int &iLogIndex,const float &fValue);
      void Finalize(unsigned int iTimeStep, unsigned int iLogIndex, unsigned long lOutputStepCount);
      void FinalizeStepData(unsigned long lTimeStep);

      void SetExpectedTimeSteps(unsigned long lExpectedTimeStep);
      void SetSimulationStartDay(int iSimulationStartDay);
      void SetSimulationStarted(bool bStarted);
      void SetReportStartupPeriod(bool bReportStartup);
      void SetTimeStepAveraging(bool bTS_averaging);

      //methods to retreive the step data (m_StepData)
      double GetStepValue(unsigned long index);
      double GetNextStepValue();
      bool GetNextStepValueEnd();
      void GetNextStepValueReset();
      void EraseSteps(unsigned long lNumberOfSteps);

      //methods to retreive the log bin data (m_LogBinData)
      void GetNextLogBinReset();
      TBinnedData* GetNextLogBin();

      unsigned long GetStepCount();
      TBinnedData* GetAnnualBin();

      void PrintLogData();


      bool IsOutStep();
      bool IsOutLog();
      bool IsOutSummary();

      //Routine that checks if the variable meta data is wild (dynamic).
      bool IsVariableDescriptionWild();
      void SetVariableDescriptionWild(const string& sMetaType,
                                      const string& sVariableType,
                                      const string& sDescription);
      string& getVariableDescription();
      string& getVariableMetaValue();
      string& getVariableMetaType();

      //These probably should not be public
      //could be more efficient if we populate the sVarName
      //only when required.
      char sVarName[256];
      
      bool Enabled; 
      unsigned char OutputType;


      //This boolean is to indicate if a step was previously outputted or not.
      //Initialize to false, set by calling class for outputting purpose.
      bool m_bStepOutput;

      //Kludge: This value is used to store the generated identifier when
      //sending the variable in the database for storage.  Since the
      //save mode permits incremental send it's more efficient to store
      //the generated id here, then fetching it from the database.
      int m_iDBVariableID;

      void printAll();

   protected:
   private:
      unsigned char m_cOutputType; //binary flag to indicate the output type
      unsigned long m_lExpectedTimeSteps; //used for optimization only
      bool m_bNextStepIndexEnd; //used to flag the end in the GetNextRoutine
      unsigned long m_lNextStepIndex; //used to track where the step index is at (for output only)
      int m_iNextLogBinIndex; //used to track where the log bin index is at (for output only)
      bool m_bReportStartupPeriod; //used to flag if we keep track of the startup period or not
      bool m_bSimulationStarted; //used to flag if the variable in startup mode or not
      double m_fLastValue; //last value passed to the AddValue() method
      long m_lLastTimeStep; //stores the last timestep that was pushed
      int m_iLastLogIndex; //stores the last log index a value was push too.
      bool m_bTS_averaging; //Time step averging mode on/off.
      bool m_firstpass; //the first call has special code (ts_averaging)

      BinnedDataVector::iterator m_itNextLogBinIndex;


      int m_iSimulationStartDay; //the day number the simulation started

      void AddToStepData(unsigned long lTimeStep, const double &fValue);
      void AddToSummaryData(const double &fValue);
      void AddToLogData(const int &iLogIndex, const double &fValue);

      StepDataVector m_StepData;
      BinnedDataVector m_LogBinData;
      TBinnedData m_AnnualData;

      //Only used when a meta variable description is passed.
      string m_sMetaType;
      string m_sVariableType;
      string m_sDescription;
};

#endif // TREPORTDATA_H

