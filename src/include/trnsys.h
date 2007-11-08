C trnsys.h



C Global constants from trnsys
C Trnsys types normally use the module TrnsysConstants
C Since the module feature is not supported by fortran 77, the 
C referred constants in trnsys types are moved here.
C ONLY referred constants are declared here. So, these
C constants may need to be updated if other trnsys types
C are coupled in the future.
      INTEGER  maxMessageLength, nMaxErrorMessages

C Maximum length of notices, warnings and error messages in TRNSYS
      PARAMETER (maxMessageLength=800)
C Maximum number of standard error messages in TRNSYS
      PARAMETER (nMaxErrorMessages=1000)


C Global constants delcared in esp-r code to couple trnsys types
      INTEGER  MTRNCOM, MTRNPARV, MTRNXIN, MTRNDER , MTRNOUT

C Maximum number of TRNSYS components in the plant network 
      PARAMETER (MTRNCOM=3)
C Maximum number of parameters for a TRNSYS type that will be 
C changed by esp-r
      PARAMETER (MTRNPARV=5)
C Maximum number of variables for a TRNSYS type 
      PARAMETER (MTRNXIN=50)
C Maximum number of derivatives for a TRNSYS type
      PARAMETER (MTRNDER=100)
C Maximum number of outputs for a TRNSYS type
      PARAMETER (MTRNOUT=100)
C

