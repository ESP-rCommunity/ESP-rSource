
#include <iostream>
#include <limits>
#include "TBinnedData.h"

using namespace std;

TBinnedData::TBinnedData()
{
	m_activeTimesteps = 0;
	m_timesteps = 0;
	m_sum = 0;
	
	//set the initial min/max
	m_maxValue = -numeric_limits<double>::max();
	m_minValue = numeric_limits<double>::max();
	
	m_isInit = false;
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

void TBinnedData::Increment()
{
	m_timesteps++;
	
	m_totalAverage = m_sum / m_timesteps;
}

double TBinnedData::Timesteps() {return m_timesteps;}
double TBinnedData::ActiveTimesteps() {return m_activeTimesteps;}
double TBinnedData::Sum() {return m_sum;}
double TBinnedData::Max() {return m_maxValue;}
double TBinnedData::Min() {return m_minValue;}
double TBinnedData::ActiveAverage() {return m_activeAverage;}
double TBinnedData::TotalAverage() {return m_totalAverage;}


void TBinnedData::Log()
{
	if(m_isInit)
	{
		cout << "Last: " << m_lastValue << "\tSteps: " << m_timesteps 
			 << "\tASteps: " << m_activeTimesteps
			<< "\tSum: " << m_sum << "\tAAvg: " << m_activeAverage << "\tTAvg: " 
			<< m_totalAverage << "\tMin: " << m_minValue << "\tMax: " << m_maxValue 
			<< endl;
	}
	else
	{
		cout << "[EMPTY]" << endl;	
	}
}
