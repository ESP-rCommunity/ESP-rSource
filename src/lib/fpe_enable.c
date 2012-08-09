#define _GNU_SOURCE 1
#include <fenv.h>
void trapfpe_ ()
{
  /* Enable some exceptions.  At startup all exceptions are masked.  */

  feenableexcept (FE_INVALID|FE_DIVBYZERO|FE_OVERFLOW);

  return;
}
