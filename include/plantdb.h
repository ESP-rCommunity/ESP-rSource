
C ESRUpdb Plant DataBase management program, PARAMETERs & COMMONs

C Library record length. If changed, update ESRUbps (mzpmsu) !!
      PARAMETER (IRECLN = 20)

C Max no library entries = MHEADR-2 * IRECLN
      PARAMETER (MAXPC = 460)

C Max no nodes = IRECLN
      PARAMETER (MAXNOD = 20)

C Max no matrix coefficients = n * IRECLN
      PARAMETER (MAXMTX = 80)

C Max no items misc data = n * IRECLN
      PARAMETER (MAXMSC = 120)

C Max header records in database = 2+MAXPC/IRECLN
      PARAMETER (MHEADR = 25)

C Max no components in Meta component.
      PARAMETER (MAXCMP = 50)

C Max no connenctions in a component.
      PARAMETER (MAXCON = 50)

C Max no of additional output.
      PARAMETER (MXAOUT = 20)

C Max no. of TRNSYS types in a generic component.
      PARAMETER (MXTSYS = 20)

C Max no. of supplementary data for mass flow components.
      PARAMETER (MAXFLW = 17)

