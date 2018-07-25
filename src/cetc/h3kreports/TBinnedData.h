#ifndef TBINNEDDATA_H
#define TBINNEDDATA_H

#ifdef _WIN32
#pragma warning (disable: 4786) //to disable annoying "identifier was truncated to '255' characters in the debug information" in VC++
#endif

#include <cstdio>

/**
Class TBinnedData

Holds the aggregate data for a variable over its time period.
*/
class TBinnedData
{

public:

	TBinnedData();
	~TBinnedData();

	/**
	AddValue(double val)
	@param val new value

	Stores val, calculates the running sum, average, min, max, etc. Does *not* increment the total number of timesteps, for that, call Increment()

	*/
	void AddValue(double val); /// Does not call Increment()

   void RemoveValue(double val);

	///getters
	double ActiveTimesteps();
	double Sum();
	double Max();
	double Min();
	double ActiveAverage();
	double TotalAverage(long lBinStepCount);
protected:
  int m_activeTimesteps;
  double m_sum;
  double m_maxValue;
  double m_minValue;
  double m_activeAverage;

	 double m_lastValue;
	 bool m_isInit;
};

#endif // TBINNEDDATA_H
