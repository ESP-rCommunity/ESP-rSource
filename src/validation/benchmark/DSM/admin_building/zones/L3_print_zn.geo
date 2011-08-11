*Geometry 1.1,GEN,L3_print_zn # tag version, format, zone name
*date Thu Aug 11 07:30:21 2011  # latest file modification 
L3_print_zn describes the print zone on L3 at srv facade
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,24.03439,35.70000,56.35000  #   1
*vertex,27.69180,35.70000,56.35000  #   2
*vertex,27.69180,40.70000,56.35000  #   3
*vertex,27.12900,40.70000,56.35000  #   4
*vertex,26.12900,40.70000,56.35000  #   5
*vertex,24.12938,40.70000,56.35000  #   6
*vertex,24.03439,35.70000,59.49300  #   7
*vertex,27.69180,35.70000,59.49300  #   8
*vertex,27.69180,40.70000,59.49300  #   9
*vertex,27.12900,40.70000,59.49300  #  10
*vertex,26.12900,40.70000,59.49300  #  11
*vertex,24.12938,40.70000,59.49300  #  12
*vertex,26.12900,40.70000,58.60000  #  13
*vertex,27.12900,40.70000,58.60000  #  14
*vertex,27.12900,40.70000,57.46000  #  15
*vertex,26.12900,40.70000,57.46000  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,8,7  #  1
*edges,4,2,3,9,8  #  2
*edges,6,3,4,15,14,10,9  #  3
*edges,4,15,16,13,14  #  4
*edges,6,5,6,12,11,13,16  #  5
*edges,4,6,1,7,12  #  6
*edges,6,7,8,9,10,11,12  #  7
*edges,6,1,6,5,4,3,2  #  8
*edges,4,14,13,11,10  #  9
*edges,4,4,5,16,15  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_pr_op,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   1 ||< not yet defined
*surf,ptn_cel,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,26,25  #   2 ||< not yet defined
*surf,vent_d,VERT,-,-,-,stone_vent,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,glaz_a,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   4 ||< external
*surf,stone_a,VERT,-,-,-,stone_gyp,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,ptn_social,VERT,-,-,-,gyp_aco_gyp,OPAQUE,UNKNOWN,0,0  #   6 ||< not yet defined
*surf,ceiling_pz,CEIL,-,-,-,slab_275,OPAQUE,UNKNOWN,0,0  #   7 ||< not yet defined
*surf,floor_pz,FLOR,-,-,-,flr_ov_pln,OPAQUE,UNKNOWN,0,0  #   8 ||< not yet defined
*surf,upper_gla_a,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   9 ||< external
*surf,low_gla_a,VERT,-,-,-,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  10 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,    18.05 0  # zone base list
