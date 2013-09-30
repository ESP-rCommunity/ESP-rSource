
/* SHOCC Dummy stub to allow ESP-r to compile without SHOCC. */
void initprj_( int* nbzones ){}
void loadlib_( char* filename, int* filelngth ){}
void loadzone_( char* filename, int* filelngth, int* icomp ){}
void blindcheck_( int* icomp, char* sname, int* lngth, int* blindindx ){}
void updatedaily_( int* idwp, float* btimef ){}
void updateoccupancy_( int* icomp, float* btimep, float* btimef ){}
void updateequipment_(int* icomp){}
void fetchloads_( int* icomp, char* lodlabel, int* lodlen, float* shsenc, float* shsenr, float* shlat ){}
void updatelighting_( int* icomp, int* lightzonedx, float* daylight, float* btimep, float* btimef ){}
void updateblind_( int* icomp, int* blndindx, float* angle, float* direct, int* state, float* btimep, float* btimef ){}

