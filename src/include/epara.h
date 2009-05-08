C epara.h
C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2009.

C For page control of the menus.
      integer NONE,CREATE,PREV,NEXT,EDIT,LIST
      PARAMETER (NONE=0,CREATE=1,PREV=3,NEXT=6,EDIT=20,LIST=22)

C Variables used in paging menus:
      integer mhead  ! number of static menu items above the scrolling portion
      integer mctl   ! number of static menu items below the scrolling portion
      integer mifull ! max size of the scrolling portion
      integer mfull  ! is mifull plus mhead plus mctl
      integer ist    ! is the start index of the array for the current page
      integer ilen   ! total number of items to include in scrolling portion
      integer ipm,mpm ! the index of the current page and last page
      integer ipflg  ! ipflg greater than zero signals multi-page
      common/pmenu/mhead,mctl,mifull,mfull,ist,ilen,ipm,mpm,ipflg
