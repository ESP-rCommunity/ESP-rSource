#include <iostream>
#include <limits>

#include "TBinnedData.h"

using namespace std;

TBinnedData::TBinnedData()
{
	m_activeTimesteps = 0;
	m_activeAverage = 0.0;
	m_sum = 0.0;

	//set the initial min/max
	m_maxValue = -numeric_limits<double>::max();
	m_minValue = numeric_limits<double>::max();

	m_isInit = false;
}

TBinnedData::~TBinnedData()
{

}

void TBinnedData::AddValue(double val)
{
	m_lastValue = val;
	m_activeTimesteps++;
	m_sum += m_lastValue;

	if(m_lastValue > m_maxValue) m_maxValue = m_lastValue;
	if(m_lastValue < m_minValue) m_minValue = m_lastValue;

   m_activeAverage = m_sum / m_activeTimesteps;

	m_isInit = true;

}

void TBinnedData::RemoveValue(double val)
{

}

double TBinnedData::ActiveTimesteps() {return m_activeTimesteps;}
double TBinnedData::Sum() {return m_sum;}
double TBinnedData::Max() {return m_maxValue;}
double TBinnedData::Min() {return m_minValue;}
double TBinnedData::ActiveAverage() {return m_activeAverage;}

//params: lTimeStepsBin - the number of timesteps in that bin
//          since the bins are only populated for active values
//          the count of timestep per-bins is done in the
//          tReportsManager
double TBinnedData::TotalAverage(long lTimeStepsBin){
   //prevent division by zero
   if(lTimeStepsBin > 0 )
      return m_sum/lTimeStepsBin;
   else
      return 0.0;
}
