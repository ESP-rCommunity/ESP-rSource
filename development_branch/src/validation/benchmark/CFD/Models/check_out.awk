BEGIN{MaxRelAllow=0.05; 
      MaxAbsAllow[1]=0.001;
      MaxAbsAllow[2]=0.001;
      MaxAbsAllow[3]=0.001;
      MaxAbsAllow[4]=0.01;
      MaxAbsAllow[5]=0.01;
      MaxAbsAllow[6]=0.01;
      MaxAbsAllow[7]=0.1;
      MaxAbsAllow[8]=0.01;
      MaxAbsAllow[9]=0.00001;
      MaxAbsAllow[10]=0.00001}
($1=="Checking") {TEST=$3}
($1=="Relative") {
  getline
  PAR=0
  while (NF==3) {
    getline
    PAR++
    RMAX=$1
    FAIL[PAR]=0
    if ( RMAX>MaxRelAllow ) {
#      print NR, $0, "FAIL1",PAR
      FAIL[PAR]=1}
    if ( RMAX<MaxRelAllow ) {
#      print NR, $0, "FAIL2",PAR
      FAIL[PAR]=1}
  }
  PARMAX=PAR
#  for (i=1;i<PARMAX;i++) {print FAIL[i]}
}
($1=="Absolute") {
  getline
  PAR=0
  while (NF==3) {
    getline
    PAR++
    RMAX=$1
    if ( RMAX>MaxAbsAllow[PAR] ) {
#      print NR, $0, "FAIL3",PAR
    FAIL[PAR]++}
  }
  PARMAX=PAR
  print "Checking ",TEST
  for (i=1;i<PARMAX;i++) {
    if (FAIL[i]>1) print " FAIL on parameter",i
  }
  print "-----------------------"
}
