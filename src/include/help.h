C help.h
C Data structures for contexual help in ESP-r.
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp         ! number of help lines found (will be non-zero)
      common/pophelp/h(60)   ! array of strings
      character h*72

