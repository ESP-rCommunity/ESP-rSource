/*
 This file is part of the ESP-r system.
 Copyright Energy Systems Research Unit, University of
 Strathclyde, Glasgow Scotland, 2001.

 ESP-r is free software.  You can redistribute it and/or
 modify it under the terms of the GNU General Public
 License as published by the Free Software Foundation
 (version 2 or later).

 ESP-r is distributed in the hope that it will be useful
 but WITHOUT ANY WARRANTY; without even the implied
 warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 PURPOSE. See the GNU General Public License for more
 details.

 You should have received a copy of the GNU General Public
 License along with ESP-r. If not, write to the Free
 Software Foundation, Inc., 59 Temple Place, Suite 330,
 Boston, MA 02111-1307 USA.

 FMIc.c contains C functions for the FMI implementation.
 Many of these functions are essentially interfaces from
 the ESP-r Fortran code to the FMILibrary C functions. All
 of these functions relate to simulation of FMUs (i.e. only
 called by functions in FMIsim.F).
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fmilib.h>

fmi_import_context_t *context;
fmi1_import_t *fmu[82]; /* array limit defined in FMI.h (MFMUIS) */
jm_status_enu_t fmierr;
fmi1_status_t fmistat;

/* ********* step_finished ******* */
/* This function is a dummy.
 */
void step_finished() {
}

/* ********* FMIC_INITFMI_ ******* */
/* This function initialises the FMI.
 */
void fmic_initfmi_() {
  jm_callbacks *jmcPointer;

  jmcPointer=jm_get_default_callbacks();

  context=fmi_import_allocate_context(jmcPointer);
}

/* ********* FMIC_FREEFMI_ ******* */
/* This function frees memory allocated to the FMI.
 */
void fmic_freefmi_() {
  fmi_import_free_context(context);
}

/* ********* FMIC_INITFMU_ ******* */
/* This function instantiates an FMU.
   Note that multiple instances of an FMU are not currently supported
   with FMILibrary, so this is accomplished by unpacking the archive
   into a seperate folder for each instance (i.e. considers each instance
   as a seperate FMU).
 */
void fmic_initfmu_(float *in_timeout, int *in_instanceIndex, char *in_filename, char *in_absInstance, char *in_relInstance, int len1, int len2, int len3) {
  fmi_version_enu_t version;
  fmi1_callback_functions_t callBackFunctions;
  int registerGlobally;
  fmi1_string_t relInstance;
  fmi1_real_t timeout;
  fmi1_boolean_t visible = fmi1_true;
  fmi1_boolean_t interactive = fmi1_false;
  int instanceIndex;
  fmi1_string_t MIMEtype = "application/x-fmu-sharedlibrary";
  char *locat_s;
  fmi1_string_t locat;
  locat_s=malloc(50);
  locat_s=strcpy(locat_s,"./");
  locat_s=strcat(locat_s,in_relInstance);
  locat=locat_s;

  instanceIndex=*in_instanceIndex-1;
  callBackFunctions.logger=fmi1_default_callback_logger;
  callBackFunctions.allocateMemory=calloc;
  callBackFunctions.freeMemory=free;
  callBackFunctions.stepFinished=step_finished;
  registerGlobally=0;
  relInstance=in_relInstance;
  timeout=*in_timeout;

  /*printf("instanceIndex %i\n",instanceIndex);*/
  /*printf("in_absInstance %s\n",in_absInstance);*/
  /*printf("relInstance %s\n",relInstance);*/

  version=fmi_import_get_fmi_version(context, in_filename, in_relInstance);
  /*printf("fmi version done\n");*/
  fmu[instanceIndex]=fmi1_import_parse_xml(context, in_absInstance);
  /*printf("parse xml done\n");*/
  fmierr=fmi1_import_create_dllfmu(fmu[instanceIndex], callBackFunctions, registerGlobally);
  /*printf("create dll done\n");*/
  fmierr=fmi1_import_instantiate_slave(fmu[instanceIndex],relInstance,locat,MIMEtype,timeout,visible,interactive);
  /*printf("instantiate slave done\n");  */
}

/* ********* FMIC_FREEFMU_ ******* */
/* This function destroys an FMU.
   This destroys both the instance and the FMU as these are currently
   synonymous; see comments for function FMIC_INITFMU_.
 */
void fmic_freefmu_(int *in_instanceIndex) {
  int instanceIndex;

  instanceIndex=*in_instanceIndex-1;

  fmi1_import_free_slave_instance(fmu[instanceIndex]);
  fmi1_import_destroy_dllfmu(fmu[instanceIndex]);
  fmi1_import_free(fmu[instanceIndex]);
}

/* ********* FMIC_SIMSTART_ ******* */
/* This function signals to an FMU instance that simulation has begun.
 */
void fmic_simstart_(int *in_instanceIndex, float *in_tStart, float *in_tStop) {
  fmi1_real_t tStart;
  fmi1_real_t tStop;
  fmi1_boolean_t tStopDef = fmi1_true;
  int instanceIndex;

  instanceIndex=*in_instanceIndex-1;
  tStart=*in_tStart;
  tStop=*in_tStop;

  fmistat=fmi1_import_initialize_slave(fmu[instanceIndex],tStart,tStopDef,tStop);
}

/* ********* FMIC_SIMSTOP_ ******* */
/* This function signals to an FMU instance that simulation has finished.
 */
void fmic_simstop_(int *in_instanceIndex) {
  int instanceIndex;

  instanceIndex=*in_instanceIndex-1;

  fmistat=fmi1_import_terminate_slave(fmu[instanceIndex]);
}

/* ********* FMIC_SIMSTEP_ ******* */
/* This function transfers input variables to an FMU, simulates it, and
   retrieves output variables.

 */
void fmic_simstep_(int *in_instanceIndex, float *in_tCurrent, float *in_tStepSize, float in_inVals[], int in_inRefs[], int *in_numIns, float out_outVals[], int in_outRefs[], int *in_numOuts) {
  fmi1_real_t tCurrent;
  fmi1_real_t tStepSize;
  fmi1_boolean_t newStep = fmi1_true;
  int instanceIndex;
  size_t numIns = *in_numIns;
  size_t numOuts = *in_numOuts;
  int i;
  fmi1_value_reference_t inRefs[numIns];
  fmi1_real_t inVals[numIns];
  fmi1_value_reference_t outRefs[numOuts];
  fmi1_real_t outVals[numOuts];

  instanceIndex=*in_instanceIndex-1;
  tCurrent=*in_tCurrent;
  tStepSize=*in_tStepSize;  
  for (i=0;i<numIns;i++) {
    inRefs[i]=in_inRefs[i];
    inVals[i]=in_inVals[i];
  }
  for (i=0;i<numOuts;i++) {
    outRefs[i]=in_outRefs[i];
  }

  fmistat=fmi1_import_set_real(fmu[instanceIndex],inRefs,numIns,inVals);
  fmistat=fmi1_import_do_step(fmu[instanceIndex],tCurrent,tStepSize,newStep);
  fmistat=fmi1_import_get_real(fmu[instanceIndex],outRefs,numOuts,outVals);

  for (i=0;i<numOuts;i++) {
    out_outVals[i]=outVals[i];
  }
}
