#include "DBManager.h"

//*************************** "TRACE_VARIABLE" **********************************
//*** Use to trace a variable.  The varname can be taken for the ***
//*** log_trace.txt generated in the TReportsManager.cpp to turn on go to the ***
//*** top of the TReportsManager.cpp ***
//*************************** "TRACE_VARIABLE" **********************************


/* ********************************************************************
** Class:   DBManager
** Purpose: Design to abstract the interface with SQLite3 and the rest
**          of the application
** Used by: TReportsManager
** Error handling: to be implemented...
** ***************************************************************** */
/* ********************************************************************
** Method:   DBManager()
** Purpose:  Default constructor for DBManager class, will intanciate
**           the database and initial structure.  By default the
**           database will only be in memory.
** Params:   N/A
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-06-06
** ***************************************************************** */
DBManager::DBManager()
{
   //call overloaded constructor
   DBManager("out_default.db3");
}


/* ********************************************************************
** Method:   DBManager()
** Purpose:  Overloaded constructor for DBManager class, will intanciate
**           the database and initial structure.
** Params:   sFileName - desired database name
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-08
** ***************************************************************** */
DBManager::DBManager(const char *sFileName)
{
   int errCode;
   bool bIsInit;

   #ifdef TRACE_VARIABLE
      log::Instance()->writeTrace("DBManager::Overloaded Constructor");
   #endif

   //Used for transaction control
   bOpenTrans = false;
   m_iInsertCount = 0;

   //Counts the number of defined variables in the database
   m_iVariableCount = 0;

   //open/create the database file
   sqlite3_open(sFileName,&db);

   //Set database PRAGMAs
   sqlite3_exec(db,"PRAGMA locking_mode = EXCLUSIVE;",0,0,0); //Locking mode prevents other of accessing the database until it is closed.
   sqlite3_exec(db,"PRAGMA journal_mode = OFF;",0,0,0); //Disables Rollbacks
   sqlite3_exec(db,"PRAGMA synchronous = OFF;",0,0,0); //One user database, synchrosnous not required, SQLITE will not wait for disk to finish, when commit

   //Check if the structure already exists.  Returns error if not found
   errCode = sqlite3_exec(db,"SELECT 1 FROM BinTypes;",0,0,0);
   (errCode)?bIsInit=false:bIsInit=true;

   if(!bIsInit)
      createTableStructure();

   //Create the prepared insert statements
   sqlite3_prepare_v2(db, "INSERT INTO StepDataValues values(?,?,?);", -1, &insertStatements[0], NULL);
   sqlite3_prepare_v2(db, "INSERT INTO TimeSteps values(?,?,?,?);", -1, &insertStatements[1], NULL);
   sqlite3_prepare_v2(db, "INSERT INTO Variables values(null,?,?,?);", -1, &insertStatements[2], NULL);
   sqlite3_prepare_v2(db, "INSERT INTO VariableDescriptors values(?,?,?,?,?);", -1, &insertStatements[3], NULL);
   sqlite3_prepare_v2(db, "INSERT INTO BinDataValues values(?,?,?,?,?,?,?,?,?,?);", -1, &insertStatements[4], NULL);
   sqlite3_prepare_v2(db, "INSERT INTO BinTypes values(?,?);", -1, &insertStatements[5], NULL);
   sqlite3_prepare_v2(db, "INSERT INTO IntegratedDataValues values(?,?,?,?,?);", -1, &insertStatements[6], NULL);

   //Create the prepared update statements
   sqlite3_prepare_v2(db, "UPDATE Variables SET VariableDescriptorID = ? WHERE ID = ?;", -1, &updateStatements[0], NULL);

   //Initialize bin type values
   if(!bIsInit)
      addBinTypes();
}


/* ********************************************************************
** Method:   ~DBManager()
** Purpose:  Destructor of the DBManager class
** Params:   N/A
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-09
** ***************************************************************** */
DBManager::~DBManager()
{
   #ifdef TRACE_VARIABLE
      log::Instance()->writeTrace("DBManager::Destructor");
   #endif

   //Destroy the prepared statement
   sqlite3_finalize(insertStatements[0]);
   sqlite3_finalize(insertStatements[1]);
   sqlite3_finalize(insertStatements[2]);
   sqlite3_finalize(insertStatements[3]);
   sqlite3_finalize(insertStatements[4]);
   sqlite3_finalize(insertStatements[5]);
   sqlite3_finalize(insertStatements[6]);

   sqlite3_finalize(updateStatements[0]);

   //ensure all is commited
   commitTran();

   //close the database
   sqlite3_close(db);
}

/* ********************************************************************
** Method:   isEnable()
** Purpose:  Method to determine if the SQLite is enabled or not.
**           During built a stub will be compile in it's place always
**           returning false .
** Params:   N/A
** Returns:  true
** Author:   Claude Lamarche
** Mod Date: 2011-09-15
** ***************************************************************** */
bool DBManager::isEnable()
{
   return true;
}

/* ********************************************************************
** Method:   createTableStructure()
** Purpose:  Creates the database tables sturcture, indexes are
**           only create after the data was inserted (performance)
** Scope:    Public
** Params:   None
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-22
** ***************************************************************** */
void DBManager::createTableStructure()
{
   #ifdef TRACE_VARIABLE
      log::Instance()->writeTrace("DBManager::createTableStructure");
   #endif

   //Create the tables
   //Primary key was intentionally not put on the certain tables for performance reasons
   sqlite3_exec(db,"CREATE TABLE StepDataValues(VariableID INTEGER, TimeStepID INTEGER, Value FLOAT);",0,0,0);
   sqlite3_exec(db,"CREATE TABLE TimeSteps(ID INTEGER PRIMARY KEY,Hour INTEGER,Day INTEGER, Startup INTEGER);",0,0,0);
   sqlite3_exec(db,"CREATE TABLE Variables(ID INTEGER PRIMARY KEY AUTOINCREMENT,VariableDescriptorID INTEGER, WildCard TEXT, Name TEXT);", 0,0,0);
   sqlite3_exec(db,"CREATE TABLE VariableDescriptors(ID INTEGER PRIMARY KEY, MetaName TEXT, MetaType TEXT, MetaValue TEXT, Description TEXT);", 0,0,0);

   //Key should be variable, index combination, binType
   sqlite3_exec(db,"CREATE TABLE BinDataValues(VariableID INTEGER, BinIndex INTEGER, Step INTEGER, ActiveStep INTEGER, Sum FLOAT, Max FLOAT, \
                Min FLOAT, ActiveAverage FLOAT, TotalAverage FLOAT, BinType INTEGER);",0,0,0);
   sqlite3_exec(db,"CREATE TABLE BinTypes(ID INTEGER PRIMARY KEY,Description TEXT);",0,0,0);
   sqlite3_exec(db,"CREATE TABLE IntegratedDataValues(VariableID INTEGER, BinIndex INTEGER, Unit TEXT, Value FLOAT, BinType INTEGER);", 0,0,0);


   //indexes are created before closing the database for performance reasons

}


/* ********************************************************************
** Method:   indexDatabase()
** Purpose:  Creates the indexes for the database, for performance
**           reasons this is done after the data was inserted.
** Scope:    Public
** Params:   None
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-22
** ***************************************************************** */
void DBManager::indexDatabase()
{
   #ifdef TRACE_VARIABLE
      log::Instance()->writeTrace("DBManager::indexDatabase");
   #endif

   //Create the indexes before closing the database, index create takes time but
   //dramatically increase the search speed.  If indexes aren't required the removal
   //of these would increase simulation performance
   sqlite3_exec(db,"CREATE INDEX idxVariableName on Variables (Name);",0,0,0);
   sqlite3_exec(db,"CREATE INDEX idxMetaName on VariableDescriptors (MetaName);",0,0,0);

   //The lack of a primary key on the following was intentional, index for quick search instead
   sqlite3_exec(db,"CREATE INDEX idxStepDataValuesVars on StepDataValues (VariableID, TimeStepID);",0,0,0);
   sqlite3_exec(db,"CREATE INDEX idxStepDataValuesTime on StepDataValues (TimeStepID, VariableID);",0,0,0);
   sqlite3_exec(db,"CREATE INDEX idxBinDataValues on BinDataValues (VariableID);",0,0,0);
   sqlite3_exec(db,"CREATE INDEX idxIntegratedDataValues on IntegratedDataValues (VariableID);",0,0,0);
}


/* ********************************************************************
** Method:   beginTran()
** Purpose:  Transaction are managed by the class since we do not want
**           to commit at every value passed.  Method begins a
**           transaction.
** Scope:    Private
** Params:   None
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-09
** ***************************************************************** */
void DBManager::beginTran()
{
    if(bOpenTrans == false)
    {
        sqlite3_exec(db, "BEGIN TRANSACTION;", 0, 0, 0);
        bOpenTrans = true;
    }
}

/* ********************************************************************
** Method:   commitTran()
** Purpose:  Transaction are managed by the class since we do not want
**           to commit at every value passed, method commits the
**           transaction.
** Scope:    Private
** Params:   None
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-09
** ***************************************************************** */
void DBManager::commitTran()
{
   if(bOpenTrans)
   {
      sqlite3_exec(db, "COMMIT TRANSACTION;", 0, 0, 0);
      bOpenTrans = false;
      m_iInsertCount = 0;
   }
}

/* ********************************************************************
** Method:   addVariableDescriptor()
** Purpose:  Inserts a Variable Description into the database
** Scope:    Public
** Params:   identifier - the variable's number used in the simulation
**           sVarName - the variable's meta name (* characters in the name)
**           sMetaType - the variable's type
**           sMetaValue - the variable's meta value ((W)att,(C)elcius,...)
**           sDescription - full text description of the variable.
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-09
** ***************************************************************** */
void DBManager::addVariableDescriptor(int identifier,const char *sVarName,const char *sMetaType,const char *sMetaValue,const char *sDescription)
{
   if(!bOpenTrans)beginTran();

   sqlite3_bind_int(insertStatements[3], 1, identifier);
   sqlite3_bind_text(insertStatements[3], 2, sVarName,-1,NULL);
   sqlite3_bind_text(insertStatements[3], 3, sMetaType,-1,NULL);
   sqlite3_bind_text(insertStatements[3], 4, sMetaValue,-1,NULL);
   sqlite3_bind_text(insertStatements[3], 5, sDescription,-1,NULL);


   sqlite3_step(insertStatements[3]);



   sqlite3_clear_bindings(insertStatements[3]);
   sqlite3_reset(insertStatements[3]);
   m_iInsertCount++;


   //Commit transaction only every MAX_TRANSACTIONS
   if(m_iInsertCount >= MAX_TRANSACTIONS)commitTran();

   return;
}

/* ********************************************************************
** Method:   addVariableName()
** Purpose:  Inserts a Variable name into the database
** Scope:    Public
** Params:   iVariableDescriptorID - link to the VariableDescriptors table
**           sDelimiters - * seperated list of the wildcards
**           sVariableName - the computed variable name (wildcards in full
**                           variable description)
** Returns:  The key number that was generated
** Author:   Claude Lamarche
** Mod Date: 2011-08-09
** Note:     There is some overhead to compute and store the sVariableName,
**           if not required, you could remove to increase simulation seed
** ***************************************************************** */
long DBManager::addVariableName(int iVariableDescriptorID,const char *sDelimiters,const char *sVariableName)
{
   if(!bOpenTrans)beginTran();

   sqlite3_bind_int(insertStatements[2], 1, iVariableDescriptorID);
   sqlite3_bind_text(insertStatements[2], 2, sDelimiters,-1,NULL);
   sqlite3_bind_text(insertStatements[2], 3, sVariableName,-1,NULL);

   sqlite3_step(insertStatements[2]);

   sqlite3_clear_bindings(insertStatements[2]);
   sqlite3_reset(insertStatements[2]);
   m_iInsertCount++;

   //Commit transaction only every MAX_TRANSACTIONS
   if(m_iInsertCount >= MAX_TRANSACTIONS)commitTran();

   m_iVariableCount++;

   return (long)sqlite3_last_insert_rowid(db);
}

/* ********************************************************************
** Method:   addValue()
** Purpose:  Inserts a Value ito the database (most used call)
** Scope:    Public
** Params:   iStepID - link the the TimeSteps table
**           lVariableId - link to the Variables Tables
**           fValue - the value to store
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-09
** ***************************************************************** */
void DBManager::addValue(long lStepID, long lVariableID, double fValue)
{
   if(!bOpenTrans)beginTran();

   sqlite3_bind_int(insertStatements[0],1,lVariableID);
   sqlite3_bind_int64(insertStatements[0],2,lStepID);
   sqlite3_bind_double(insertStatements[0],3, fValue);


   sqlite3_step(insertStatements[0]);


   sqlite3_clear_bindings(insertStatements[0]);
   sqlite3_reset(insertStatements[0]);
   m_iInsertCount++;


   //Commit transaction only every MAX_TRANSACTIONS
   if(m_iInsertCount >= MAX_TRANSACTIONS)commitTran();

   return;
}

/* ********************************************************************
** Method:   addTimeStep()
** Purpose:  Inserts a time step data
** Scope:    Public
** Params:   lStep - step number
**           iHour - hour
**           iDay - day
**           bStartup - 1 - true, 0 false
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-09
** ***************************************************************** */
void DBManager::addTimeStep(long lStep, int iHour, int iDay, int bStartup)
{
   if(!bOpenTrans)beginTran();

   sqlite3_bind_int(insertStatements[1], 1, lStep);
   sqlite3_bind_int(insertStatements[1], 2, iHour);
   sqlite3_bind_int(insertStatements[1], 3, iDay);
   sqlite3_bind_int(insertStatements[1], 4, bStartup);

   sqlite3_step(insertStatements[1]);

   sqlite3_clear_bindings(insertStatements[1]);
   sqlite3_reset(insertStatements[1]);
   m_iInsertCount++;


   //Commit transaction only every MAX_TRANSACTIONS
   if(m_iInsertCount >= MAX_TRANSACTIONS)commitTran();

   return;
}


/* ********************************************************************
** Method:   addBinData()
** Purpose:  Inserts a monthly bin data
** Scope:    Public
** Params:   iVariableID - link to the variable table
**           iIndex - the bin index
**           iStep - start step number
**           iActiveStep - number of active steps in bin
**           dSum - sum
**           dMax - max value
**           dMin - min value
**           dActiveAverage - calculated active average
**           dTotalAverage - calculated total average
**           iType - link to the BinTypes table
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-19
** ***************************************************************** */
void DBManager::addBinData(int iVariableID,int iIndex, int iStep,
                                  int iActiveStep, double dSum, double dMax,
                                  double dMin, double dActiveAverage,
                                  double dTotalAverage, int iType)
{
   if(!bOpenTrans)beginTran();

   sqlite3_bind_int(insertStatements[4],1,iVariableID);
   sqlite3_bind_int(insertStatements[4],2,iIndex);
   sqlite3_bind_int(insertStatements[4],3,iStep);
   sqlite3_bind_int(insertStatements[4],4,iActiveStep);
   sqlite3_bind_double(insertStatements[4],5, dSum);
   sqlite3_bind_double(insertStatements[4],6, dMax);
   sqlite3_bind_double(insertStatements[4],7, dMin);
   sqlite3_bind_double(insertStatements[4],8, dActiveAverage);
   sqlite3_bind_double(insertStatements[4],9, dTotalAverage);
   sqlite3_bind_double(insertStatements[4],10, iType);

   sqlite3_step(insertStatements[4]);

   sqlite3_clear_bindings(insertStatements[4]);
   sqlite3_reset(insertStatements[4]);
   m_iInsertCount++;



   //Commit transaction only every MAX_TRANSACTIONS
   if(m_iInsertCount >= MAX_TRANSACTIONS)commitTran();

   return;
}


/* ********************************************************************
** Method:   addIntegratedData()
** Purpose:  Inserts into the integrated data table
** Scope:    Public
** Params:   - iVariableId - link to the variable table
**           - iIndex - the bin index generated during simulation
**           - sIntegratedUnits - the integrated unit
**           - dTotal - integrated total calculated during simulation
**           - sType - The
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-19
** ***************************************************************** */
void DBManager::addIntegratedData(int iVariableID, int iIndex, const char *sIntegratedUnits,double dTotal, int iType)
{
   if(!bOpenTrans)beginTran();

   sqlite3_bind_int(insertStatements[6],1,iVariableID);
   sqlite3_bind_int(insertStatements[6],2,iIndex);
   sqlite3_bind_text(insertStatements[6], 3, sIntegratedUnits,-1,NULL);
   sqlite3_bind_double(insertStatements[6],4, dTotal);
   sqlite3_bind_int(insertStatements[6], 5, iType);

   sqlite3_step(insertStatements[6]);

   sqlite3_clear_bindings(insertStatements[6]);
   sqlite3_reset(insertStatements[6]);
   m_iInsertCount++;

   //Commit transaction only every MAX_TRANSACTIONS
   if(m_iInsertCount >= MAX_TRANSACTIONS)commitTran();

   return;
}


/* ********************************************************************
** Method:   addBinTypes()
** Purpose:  Initializes the bin types values
** Scope:    Private
** Params:   N/A
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-08-22
** ***************************************************************** */
void DBManager::addBinTypes()
{
   if(!bOpenTrans)beginTran();

   //Insert Monthly
   sqlite3_bind_int(insertStatements[5],1,BIN_MONTH_TYPE);
   sqlite3_bind_text(insertStatements[5], 2, "Monthly",-1,NULL);
   sqlite3_step(insertStatements[5]);
   sqlite3_clear_bindings(insertStatements[5]);
   sqlite3_reset(insertStatements[5]);
   m_iInsertCount++;

   //Insert Seasonal
   sqlite3_bind_int(insertStatements[5],1,BIN_SEASONAL_TYPE);
   sqlite3_bind_text(insertStatements[5], 2, "Seasonal",-1,NULL);
   sqlite3_step(insertStatements[5]);
   sqlite3_clear_bindings(insertStatements[5]);
   sqlite3_reset(insertStatements[5]);
   m_iInsertCount++;


   //Insert Annual
   sqlite3_bind_int(insertStatements[5],1,BIN_ANNUAL_TYPE);
   sqlite3_bind_text(insertStatements[5], 2, "Annual",-1,NULL);
   sqlite3_step(insertStatements[5]);
   sqlite3_clear_bindings(insertStatements[5]);
   sqlite3_reset(insertStatements[5]);
   m_iInsertCount++;


   //Commit transaction only every MAX_TRANSACTIONS
   if(m_iInsertCount >= MAX_TRANSACTIONS)commitTran();

   return;
}


/* ********************************************************************
** Method:   updateVariableName()
** Purpose:  Updates the Variable table's VariableDescriptor to the new
**           passed in value
** Scope:    Public
** Params:   iVariableID - ID of table Variable
**           iNewDescriptorId - ID of the new VariableDescriptorID
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-09-07
** ***************************************************************** */
void DBManager::updateVariableName(int iVariableId, int iNewDescriptorID)
{
   //Before an update, commit outstanding trans
   beginTran();

   sqlite3_bind_int(updateStatements[0],1,iNewDescriptorID);
   sqlite3_bind_int(updateStatements[0],2,iVariableId);
   sqlite3_step(updateStatements[0]);
   sqlite3_clear_bindings(updateStatements[0]);
   sqlite3_reset(updateStatements[0]);

   //Commit after update
   commitTran();

   return;
}

/* ********************************************************************
** Method:   createDataViews
** Purpose:  Execute a statement to create a view of the
**           pivot_stepdatavalues table.
** Scope:    Public
** Params:   N/A
** Returns:  N/A
** Author:   Claude Lamarche
** Mod Date: 2011-10-03
** ***************************************************************** */

void DBManager::createDataViews()
{
   char *sPivotTable;
   char buffer[5]; //max acceptable variables 99999
   int i;

   //create statement for a flatten view of the stepdatavalues
   if(m_iVariableCount > 0)
   {
     sPivotTable = (char*) malloc ((m_iVariableCount*65)+150);

      strcpy(sPivotTable,"create view pivot_stepdatavalues as select TimeStepID ");
      for(i=1;i<=m_iVariableCount;i++)
      {
         sprintf(buffer,"%d",i);
         strcat(sPivotTable,",max(case when VariableID=");
         strcat(sPivotTable,buffer);
         strcat(sPivotTable," then value end) as Variable");
         strcat(sPivotTable,buffer);
      }
      strcat(sPivotTable," from stepdatavalues group by TimeStepID;");

      //create the view
      sqlite3_exec(db,sPivotTable,0,0,0);


      free(sPivotTable);
   }
}
