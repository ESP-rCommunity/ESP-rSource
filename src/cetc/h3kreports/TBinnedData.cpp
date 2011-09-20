
#include <iostream>
#include <limits>

#include "TBinnedData.h"

using namespace std;

TBinnedData::TBinnedData()
{
	m_activeTimesteps = 0;
	m_activeAverage = 0;
	m_timesteps = 0;
	m_sum = 0;

	//set the initial min/max
	m_maxValue = -numeric_limits<double>::max();
	m_minValue = numeric_limits<double>::max();

	m_isInit = false;

	//printf("Construct:Bin %p\n",this);
}
TBinnedData::~TBinnedData()
{
   //printf("Destruct:Bin %p\n",this);
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

void TBinnedData::Increment()
{
	m_timesteps++;
}

double TBinnedData::Timesteps() {return m_timesteps;}
double TBinnedData::ActiveTimesteps() {return m_activeTimesteps;}
double TBinnedData::Sum() {return m_sum;}
double TBinnedData::Max() {return m_maxValue;}
double TBinnedData::Min() {return m_minValue;}
double TBinnedData::ActiveAverage() {return m_activeAverage;}
double TBinnedData::TotalAverage(){

   //prevent division by zero
   if(m_timesteps > 0 )
      return m_sum/m_timesteps;
   else
      return 0.0;
}


void TBinnedData::Log()
{
	if(m_isInit)
	{
		cout << "Last: " << m_lastValue << "\tSteps: " << m_timesteps
			 << "\tASteps: " << m_activeTimesteps
			<< "\tSum: " << m_sum << "\tAAvg: " << m_activeAverage << "\tTAvg: "
			<< this->TotalAverage() << "\tMin: " << m_minValue << "\tMax: " << m_maxValue
			<< endl;
	}
	else
	{
		cout << "[EMPTY]" << endl;
	}
}
