#ifndef DBMANAGER_STUB_H
#define DBMANAGER_STUB_H

//Hardcoded available bin type (annual/monthly)
#define BIN_MONTH_TYPE     0
#define BIN_ANNUAL_TYPE    1
#define BIN_SEASONAL_TYPE  2

#ifdef _WIN32
#pragma warning (disable: 4786) //to disable annoying "identifier was truncated to '255' characters in the debug information" in VC++
#endif

#include <stdio.h>

/* ********************************************************************
** Class:   DBManager (stub)
** Purpose: Stub of the DBmanager class.  When esp-r is built without
**          the --SQLite option this file will be used instead of the
**          DBManager.cpp
** Used by: TReportsManager
** ***************************************************************** */
class DBManager
{
   public:
      DBManager();
      DBManager(const char *sFileName);
      bool isEnable();
      void addValue(long lStepID, long lVariableID, double fValue);
      void addTimeStep(long lStep, int iHour, int iDay, int bStartup);
      void addVariableDescriptor(int identifier,const char *sVarName,const char *sMetaType,const char *sMetaValue,const char *sDescription);
      long addVariableName(int iVariableDescriptorID,const char *sDelimiters,const char *sVariableName);
      void addBinData(int iVariableID,int iIndex, int iStep, int iActiveStep, double dSum, double dMax, double dMin, double dActiveAverage,
                            double dTotalAverage, int iType);
      void addIntegratedData(int iVariableID, int iIndex, const char *sIntegratedUnits,double dTotal, int sType);
      void createTableStructure();
      void indexDatabase();
      void createDataViews();
      void updateVariableName(int iVariableId, int iNewDescriptorID);
   private:
   protected:
};
#endif // DBMANAGER_STUB_H

DBManager::DBManager(){}
DBManager::DBManager(const char *sFileName){}
bool DBManager::isEnable(){return false;} //Database not enable
void DBManager::addValue(long lStepID, long lVariableID, double fValue){}
void DBManager::addTimeStep(long lStep, int iHour, int iDay, int bStartup){}
void DBManager::addVariableDescriptor(int identifier,const char *sVarName,const char *sMetaType,const char *sMetaValue,const char *sDescription){}
long DBManager::addVariableName(int iVariableDescriptorID,const char *sDelimiters,const char *sVariableName){return 0;}
void DBManager::addBinData(int iVariableID,int iIndex, int iStep, int iActiveStep, double dSum, double dMax, double dMin, double dActiveAverage,
                            double dTotalAverage, int iType){}
void DBManager::addIntegratedData(int iVariableID, int iIndex, const char *sIntegratedUnits,double dTotal, int sType){}
void DBManager::createTableStructure(){}
void DBManager::indexDatabase(){}
void DBManager::createDataViews(){};
void DBManager::updateVariableName(int iVariableId, int iNewDescriptorID){}
