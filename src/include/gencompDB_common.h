C ----------------------------------------------------------------------
C Common Blocks - Data Read by cdb from the Components 
C ---------------------------------------------------------------------- 
C Database structural information.
      COMMON/CGDB1/cdbversion
      COMMON/CGDB2/Comp_DBase_Unit,nbdomain,nbcat(Max_Domain_Types),
     &nbitem(Max_Domain_Types,Max_Categories)
      COMMON/CGDB3/Comp_DBase_Name,domaintag(Max_Domain_Types),	
     &domainmenu(Max_Domain_Types),
     &cattag(Max_Domain_Types,Max_Categories),
     &catmenu(Max_Domain_Types,Max_Categories),
     &itemtag(Max_Domain_Types,Max_Categories,Max_Items),
     &itemmenu(Max_Domain_Types,Max_Categories,Max_Items),
     &itempointer(Max_Domain_Types,Max_Categories,Max_Items)  

C List of component data attributes.
      COMMON/CGDB4/natribs(Max_DB_Items),
     &tagatr(Max_DB_Items,Max_parameters,6),
     &atrib(Max_DB_Items,Max_parameters,3),
     &menuatr(Max_DB_Items,Max_parameters),
     &item_text_desc(Max_DB_Items,Max_text_Desc),
     &n_text_desc(Max_DB_Items)

C List of component graphics data data. 
      COMMON/CGDB5/nbvert(Max_DB_Items),
     &vert(Max_DB_Items,Max_vert,2),
     &nbedge(Max_DB_Items),
     &iedge(Max_DB_Items,Max_edge,5),
     &nbdot(Max_DB_Items),
     &idot(Max_DB_Items,Max_dots,4),
     &nbarc(Max_DB_Items),
     &iarc(Max_DB_Items,Max_dots,7),
     &nblabel(Max_DB_Items),
     &ilabel(Max_DB_Items,Max_dots,4),
     &nbatt(Max_DB_Items),
     &iatt(Max_DB_Items,Max_conn_p,2)

      COMMON/CGDB6/labeltx(Max_DB_Items,Max_dots)

C List of specific component data attributes returned by an item search. 
      COMMON/CGDB7/s_natribs,s_tagatr(Max_parameters,6),
     &s_atrib(Max_parameters,3),
     &s_menuatr(Max_parameters),
     &s_item_text_desc(Max_text_Desc),
     &s_n_text_desc
      COMMON/CGDB8/s_vert(Max_vert,2),
     &s_nedge,s_iedge(Max_edge,5),
     &s_ndot,s_idot(Max_dots,4),
     &s_narc,s_iarc(Max_dots,7),
     &s_nlabel,s_ilabel(Max_dots,4),
     &s_natt,s_iatt(Max_conn_p,2)

      COMMON/CGDB9/s_item_tag,s_item_menu,s_labeltx(Max_dots)
      COMMON/CGDB10/s_item_dom,s_item_cat

      COMMON/CDB2P/s_NNODE,s_NMATX,s_MISCD,s_NXT,s_MATPOS,s_ICODE,
     &s_NDPOS(100)
  
      INTEGER nbdomain,nbcat,nbitem,Comp_DBase_Unit,itempointer
      INTEGER nbedge,nbdot,nbvert,nbarc,nblabel,nbatt
      INTEGER iedge,idot,iarc,ilabel,iatt,s_iedge,s_idot,s_iarc
      INTEGER s_ilabel,s_iatt,natribs,s_nlabel,s_natt,s_narc,s_ndot
      INTEGER s_nedge,s_n_text_desc,n_text_desc,s_natribs
      INTEGER s_NNODE,s_NMATX,s_MISCD,s_NXT,s_MATPOS,s_ICODE,s_NDPOS
      REAL cdbversion,vert,s_vert

      CHARACTER*4 labeltx,s_labeltx
      CHARACTER s_item_menu*72,s_item_tag*16,s_item_dom*32,s_item_cat*32

      CHARACTER tagatr*12,menuatr*32,atrib*32,item_text_desc*72
      CHARACTER s_tagatr*12,s_menuatr*32,s_atrib*32,s_item_text_desc*72

      CHARACTER*16 domaintag,cattag,itemtag
      CHARACTER*32 domainmenu,catmenu
      CHARACTER*64 itemmenu
      CHARACTER*144 Comp_DBase_Name


