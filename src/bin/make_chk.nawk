#  make_chk.nawk
#
#  For sorting ESP-r makfiles into alphabetical order 
#  and checking correct file lists given for linking  
#  and  cleaning.  Works on all esru???/Makefile
#
#  USAGE:  awk -f make_chk.nawk Makefile
#
#  NB on unix use nawk, can use gawk on linux.
#
BEGIN{PROG=0; SRCnumber=0; OBJ=0; nobjs=0; nlink=0}
{if (PROG==0) {
  if (substr($1,1,6) != "SRCdir") {
  SRCnumber++
#  print NR, SRCnumber, $0 # debug line
  SRCname[SRCnumber]=$0}
  }
}
{if (substr($1,1,4) == "PROG") {
  PROG=1
  SRCnumber--
  PROGRAM=$NF
  PROGxxx=substr(PROGRAM,1,3)
  }
}
{if (substr($1,1,7)=="OBJECTS" || OBJ==1) 
  {if (NF==0) 
    OBJ=0
  else
    OBJ=1
    for (ij=1; ij <= NF; ij++) 
# Create a list of objects, ignoring progxxx.o, remove suffix.
    if (substr($ij,1,7) != "OBJECTS" && $ij != "\\" && $ij != "=" && $ij != PROGxxx".o")
     {nobjs++; OBJnames[nobjs]=prefix($ij,".")}
  }
}
{if ($1=="ln") {
# Create a list of links.
    temp=linktext($3)
    temp=prefix(temp,"=")
    found=chkobj(temp, OBJnames, nobjs)
    if (found==1) {
      nlink++
      LINK[nlink]=linktext($3)}
    }
}
END {
  print "SRCdir = `pwd`/.."
  qsort(SRCname,1,SRCnumber)
  for (i=1; i <= SRCnumber; i++) print SRCname[i]
  printf("PROG = %s\n\n.SUFFIXES: .o .F\n.F.o:\n\t$(MFC) $(FFLAGS) -c $<\n\n",PROGRAM)
  qsort(OBJnames,1,nobjs)
  OUTS="OBJECTS = "PROGxxx".o"
  i=0
  while (i <= nobjs) {
    {if (length(OUTS" "OBJnames[i]".o") < 70)
      {i++; OUTS=OUTS" "OBJnames[i]".o"}
    else
      {if (substr(OUTS,1,7)=="OBJECTS")
         {printf("%s \\\n", OUTS); OUTS=" "}
       else
         {printf("\t%s \\\n", OUTS); OUTS=" "}
      }
    }
    if (i==nobjs)
      {printf("\t%s\n\n", OUTS); i++}
  }
  printf("$(PROG): $(OBJECTS)\n")
  printf("\t$(MFC) $(LD_FLAGS) -o $(PROG) $(FFLAGS) $(OBJECTS) $(ULIBS)\n\n")
# print linking information. << need to check files and directories>>
  qsort(LINK,1,nlink)
  i=0
  while (i < nlink) {
    i++
    LinkF=prefix(LINK[i],"=")
    LinkD=suffix(LINK[i],"=")
    printf("%s.F:\n\trm -f %s.F\n\tln -s $(%s)/%s.F .\n",LinkF,LinkF,LinkD,LinkF)
  }
  printf("\ninstall:\n")
  printf("\tstrip $(PROG); cp $(PROG) $(ESPbin)\n\n")
  printf("clean:\n")
  printf("\trm -f core $(PROG) $(OBJECTS) \\\n")
  OUTS=""
  i=0
  while (i <= nlink) {
    {LinkF=prefix(LINK[i+1],"=")
    if (length(OUTS LinkF".F ") < 70)
      {i++; OUTS=OUTS LinkF".F "}
    else
      {printf("\t%s\\\n", OUTS); OUTS=""}
    }
    if (i==nlink)
      {printf("\t%s\n", OUTS)}
  }
}
function qsort(A,left,right,i,last) {
  if (left >= right) # do nothing if array contains less that two elements
    return
  swap(A,left,left+int((right-left+1)*rand()))
  last=left  # A[left] is now partition element
  for (i = left+1; i <= right; i++)
    if (A[i] < A[left])
      swap(A, ++last, i)
  swap(A,left,last)
  qsort(A,left,last-1)
  qsort(A,last+1,right)
}
function swap(A,i,j,t) {
  t=A[i]; A[i]=A[j]; A[j]=t
}
function prefix(s,sep) {
  return substr(s,1,index(s, sep)-1)
}
function suffix(s,sep) {
  return substr(s,index(s, sep)+1)
}
function linktext(s, text) {
  text=substr(s,index(s,"/")+1)
  text=substr(text,1,length(text)-2)
  text=text"="substr(s,3,index(s,")")-1)
  text=substr(text,1,length(text)-2)
  return text
}
function chkobj(link, objects, nobj, i) {
  i=1
  while (link!=objects[i]){
    i++
    if (i>nobj) return 0}
  return 1
}
