#ifndef LOG_H
#define LOG_H

#ifdef _WIN32
#pragma warning (disable: 4786) //to disable annoying "identifier was truncated to '255' characters in the debug information" in VC++
#endif

#include <cstdio>
#include <cstring>
#include <string>
#include <time.h>

using namespace std;

class log
{
   public:
      //return a singleton instance
      static log* Instance();

      void setErrorFileName(const char* cFileName);
      void setTraceFileName(const char* cFileName);
      void setLogFileName(const char* cFileName);

      void writeError(const char* cString, int iErrorCode);
      void writeTrace(const char* cString);
      void writeTrace(const char* cString, const char* cString2);
      void writeLog(const char* cString);

      ~log();
   protected:
      //protect the constructor , force a singleton instance instead
      log();

   private:
      static log* ptr_Instance;
      //methods
      FILE* OpenFile(FILE* pfile, const char* cFileName);

      //members
      FILE *m_pErrFile,*m_pTraceFile, *m_pLogFile;

      string m_sErrFileName;
      string m_sTraceFileName;
      string m_sLogFileName;

      bool m_bIsErrOpen;
      bool m_bIsTraceOpen;
      bool m_bIsLogOpen;
};

#endif // LOG_H
