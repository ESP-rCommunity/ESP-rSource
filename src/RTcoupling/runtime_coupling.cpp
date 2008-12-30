extern "C"
{
  //These are the calls advertised to Fortran. Note that they are all
  //lowercase and end in an underscore. Calls from Fortran must omit
  //the trailing underscore

  /**
   *  Flag for RTC support 
   */

  bool bRTCsupported__()
  {
    return true;
  }
}