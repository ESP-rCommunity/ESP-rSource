BEGIN{printf("<HTML>\n<BODY>\n<PRE>\n")}
{ref=toupper($1)
 if (ref=="VOID"||ref=="SUBROUTINE") 
   {print $0,"<A name="toupper(subname($2))">"}
 else {
  sup=toupper($0)
  ind=match(sup, "CALL")
  {if (ind==0){
     ind=match(sup, "COMMON")
    {if (ind==0 || comlist=="FALSE")
       {print $0}
     else{
       ref=substr($0,ind+7)
       ref=comname(ref)
       refup=toupper(ref)
      {if (length(ref)==0){ 
         print $0}
       else{
         subst="<A HREF=../common_list.html#" refup ">"ref"</A>"
         sub(ref, subst, $0)
         print $0}
      }}
      }
   }
   else{
   
#
# Strip the name of the subroutine called and create link to definition.
#
     ref=substr($0,ind+5)
     ref=subname(ref)
     refup=toupper(subname(ref))
     filenm=""
#
#  Check if we already know where routine is defined, if not then use 
#  grep to search source files.
#
     for (i=1; i<=NSUBS; i++){
       if (knsub[i]==refup)
         {filenm=knfile[i]}
     }
#
#  Check fortran subroutines first
#
     if (filenm=="") {
       ("fgrep -i ' "ref" ' /tmp/esp-r.subroutines | cut -d ':' -f 1") | getline filenm
       NSUBS++
       knsub[NSUBS]=refup
       knfile[NSUBS]=filenm
     }
#
#  Now check c functions
#
     if (filenm=="") {
       ("fgrep -i ' "ref"_ ' /tmp/esp-r.subroutines | cut -d ':' -f 1") | getline filenm
       NSUBS++
       knsub[NSUBS]=refup
       knfile[NSUBS]=filenm
     }
     {if (filenm=="") 
        {print "Cannot find definition for routine:",ref > "/dev/stderr"
         print $0}
      else{
        subst="<A HREF=../"filenm".html#"refup">"ref"</A>"
        sub(ref, subst, $0)
        print $0}
     }}
  }}
}

END{printf("</PRE>\n")}

function subname(s) {
  ind=index(s, "(")-1
  {if (ind<0)
     return s
   else
     return substr(s,1,ind)
  }
}

function comname(s) {
  ind=index(s, "/")-1
  {if (ind<0)
     return ""
   else
     return substr(s,1,ind)
  }
}
