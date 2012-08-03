#ifndef DBMANAGER_H
#define DBMANAGER_H

#ifdef _WIN32
#pragma warning (disable: 4786) //to disable annoying "identifier was truncated to '255' characters in the debug information" in VC++
#endif

#include <stdio.h>
#include <sqlite3.h>
#include <stdlib.h>
#include "log.h"

#define MAX_TRANSACTIONS 50000
#define MAX_INSERTSTATEMENT 10
#define MAX_UPDATESTATEMENT 1

//Hardcoded available bin type (annual/monthly/seasonal)
#define BIN_MONTH_TYPE     0
#define BIN_ANNUAL_TYPE    1
#define BIN_SEASONAL_TYPE  2

/* ******************************************************************
** SQLite3 Return codes (http://www.sqlite.org/)
** *************************************************************** */
#define SQLITE_OK           0   /* Successful result */
/* beginning-of-error-codes */
#define SQLITE_ERROR        1   /* SQL error or missing database */
#define SQLITE_INTERNAL     2   /* Internal logic error in SQLite */
#define SQLITE_PERM         3   /* Access permission denied */
#define SQLITE_ABORT        4   /* Callback routine requested an abort */
#define SQLITE_BUSY         5   /* The database file is locked */
#define SQLITE_LOCKED       6   /* A table in the database is locked */
#define SQLITE_NOMEM        7   /* A malloc() failed */
#define SQLITE_READONLY     8   /* Attempt to write a readonly database */
#define SQLITE_INTERRUPT    9   /* Operation terminated by sqlite3_interrupt()*/
#define SQLITE_IOERR       10   /* Some kind of disk I/O error occurred */
#define SQLITE_CORRUPT     11   /* The database disk image is malformed */
#define SQLITE_NOTFOUND    12   /* Unknown opcode in sqlite3_file_control() */
#define SQLITE_FULL        13   /* Insertion failed because database is full */
#define SQLITE_CANTOPEN    14   /* Unable to open the database file */
#define SQLITE_PROTOCOL    15   /* Database lock protocol error */
#define SQLITE_EMPTY       16   /* Database is empty */
#define SQLITE_SCHEMA      17   /* The database schema changed */
#define SQLITE_TOOBIG      18   /* String or BLOB exceeds size limit */
#define SQLITE_CONSTRAINT  19   /* Abort due to constraint violation */
#define SQLITE_MISMATCH    20   /* Data type mismatch */
#define SQLITE_MISUSE      21   /* Library used incorrectly */
#define SQLITE_NOLFS       22   /* Uses OS features not supported on host */
#define SQLITE_AUTH        23   /* Authorization denied */
#define SQLITE_FORMAT      24   /* Auxiliary database format error */
#define SQLITE_RANGE       25   /* 2nd parameter to sqlite3_bind out of range */
#define SQLITE_NOTADB      26   /* File opened that is not a database file */
#define SQLITE_ROW         100  /* sqlite3_step() has another row ready */
#define SQLITE_DONE        101  /* sqlite3_step() has finished executing */
/* ****** End of SQLite3 Return codes ****** */

class DBManager
{
   public:
      DBManager();
      DBManager(const char *sFileName);
      bool isEnable();
      ~DBManager();
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
    protected:
        sqlite3 *db;
    private:
        int m_iInsertCount;
        int m_iVariableCount;
        bool bOpenTrans;
        sqlite3_stmt *insertStatements[MAX_INSERTSTATEMENT];
        sqlite3_stmt *updateStatements[MAX_UPDATESTATEMENT];

        void beginTran();
        void commitTran();
        void addBinTypes();
};

#endif // DBMANAGER_H
