#include "log.h"

//This is just a crude error and trace log class.
//Only used at this stage for the H3KReport
//This call could probably be easily extended to esp-r if needs be.
//Because the class is a singleton the calls to use it are a bit
//difference then the norm.  This class does not need to be
//instanciated by the calling class.  Just add to the include and
//use away.

//Example to call:
//   log::Instance()->writeTrace("DBManager::Overloaded Constructor")

//Don't forget to destroy
//   delete log::Instance()


//Singleton variable and class
log* log::ptr_Instance = NULL;
log* log::Instance(){
   if(ptr_Instance == NULL) //create a new instance
      ptr_Instance = new log();

   return ptr_Instance;
}


//constructor
log::log()
{
   this->setErrorFileName("log_errors.txt");
   this->setTraceFileName("log_trace.txt");
   this->setLogFileName("log.txt");

   m_bIsErrOpen = m_bIsTraceOpen = m_bIsLogOpen = false;
}

//destructor
log::~log()
{
   //destructor
   //close files
   if(m_bIsErrOpen)
   {
      fprintf(m_pErrFile,"------------------- END LOG INSTANCE ------------------------\n\n");
      fclose(m_pErrFile);
   }
   if(m_bIsTraceOpen)
   {
      fprintf(m_pTraceFile,"------------------- END LOG INSTANCE ------------------------\n\n");
      fclose(m_pTraceFile);
   }

   if(m_bIsLogOpen)
   {
      fprintf(m_pLogFile,"------------------- END LOG INSTANCE ------------------------\n\n");
      fclose(m_pLogFile);
   }
}

//public method to overwrite filename
void log::setErrorFileName(const char* cFileName)
{
   m_sErrFileName = cFileName;
}

//public method to overwrite filename
void log::setTraceFileName(const char* cFileName)
{
   m_sTraceFileName = cFileName;
}

//public method to overwrite filename
void log::setLogFileName(const char* cFileName)
{
   m_sLogFileName = cFileName;
}

//public method to write to a file
void log::writeError(const char* cString, int iErrorCode)
{
   time_t rawtime;
   struct tm * timeinfo;

   //get today's system time
   time ( &rawtime );
   timeinfo = localtime ( &rawtime );

   //open file if not openned
   if(!m_bIsErrOpen)
   {
      m_pErrFile = OpenFile(m_pErrFile,m_sErrFileName.c_str());
      m_bIsErrOpen = true;
   }

   //write to file
   if(m_pErrFile != NULL)
   {
      fprintf(m_pErrFile,"%02d:%02d:%02d: Error(%d) %s\n",timeinfo->tm_hour,timeinfo->tm_min,timeinfo->tm_sec,iErrorCode,cString);
   }
}

//public method to write to a file
void log::writeTrace(const char* cString, const char* cString2)
{
   string sTemp = cString;
   sTemp += cString2;

   this->writeTrace(sTemp.c_str());
}


void log::writeTrace(const char* cString)
{
   time_t rawtime;
   struct tm * timeinfo;

   //get today's system time
   time ( &rawtime );
   timeinfo = localtime ( &rawtime );



   //open file if not openned
   if(!m_bIsTraceOpen)
   {
      m_pTraceFile = OpenFile(m_pTraceFile,m_sTraceFileName.c_str());
      m_bIsTraceOpen = true;
   }

   //write to file
   if(m_pTraceFile != NULL)
   {
      fprintf(m_pTraceFile,"%02d:%02d:%02d: %s\n",timeinfo->tm_hour,timeinfo->tm_min,timeinfo->tm_sec,cString);
   }
}

//public method to write to a file
void log::writeLog(const char* cString)
{
   time_t rawtime;
   struct tm * timeinfo;

   //get today's system time
   time ( &rawtime );
   timeinfo = localtime ( &rawtime );

   //open file if not openned
   if(!m_bIsLogOpen)
   {
      m_pLogFile = OpenFile(m_pLogFile,m_sLogFileName.c_str());
      m_bIsLogOpen = true;
   }

   //write to file
   if(m_pLogFile != NULL)
   {
      fprintf(m_pLogFile,"%02d:%02d:%02d: %s\n",timeinfo->tm_hour,timeinfo->tm_min,timeinfo->tm_sec,cString);
   }
}

//private method to open a file in append mode
FILE* log::OpenFile(FILE* pfile, const char* cFileName)
{
   //open in append mode.
   pfile = fopen(cFileName,"a");
   fprintf(pfile,"------------------ START LOG INSTANCE -----------------------\n");

   return pfile;

}

