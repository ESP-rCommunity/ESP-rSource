/*C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre 
C Natural Resources Canada, Government of Canada
C 2004. Please Contact Ian Beausoliel-Morrison for details 
C concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.
*/


/* This subroutine will kill esp-r after iTime amount of seconds
have elapsed. The method that it does this is by creating a child thread to keep 
track of time while the parent thread continues to run ESP-r. The child thread wakes 
up after iTime seconds and will commit patricide. The iTime is passed via the 
command line using the switch -timeout <timeout in seconds> for bps.
*/
/*
#include <iostream>
#include <fstream>
#include <iomanip>
#include <algorithm>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <signal.h> 
*/

void timeout(int iTime)
{
/*  pid_t parent;

  parent = getpid();
  switch (fork()) {
  case -1:
    //can't fork;
  case 0:
    
    // execution in child process  
    
    //The child sleeps while the simulator runs..
    sleep(iTime);
    
    //Wakes up to commit patricide if still running.commmit.
    std::cout << "\nExceeded timeout period of " << iTime << "seconds.\n";
    std::cout << "Killing Program\n";
       kill(parent ,SIGKILL);
    exit(0);
    
  default:
    parent = getpid(); 
   //do nothing and let the thread continue on in the simulation.
  }
*/
}

//External reference for fortran.
extern "C"{

void timeout_espr__(char* cTime, int iTimeLen)
{
/* std::string sTime =std::string(cTime, iTimeLen);
 int iTime = atoi(sTime.c_str());
timeout(iTime);
*/
}

}
