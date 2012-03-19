*Geometry 1.1,GEN,L4_printz # tag version, format, zone name
*date Sun Feb 12 08:17:17 2012  # latest file modification 
L4_printz describes printing room on level four as srv facade
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,24.03439,35.70000,60.25000  #   1
*vertex,27.69180,35.70000,60.25000  #   2
*vertex,27.69180,40.70000,60.25000  #   3
*vertex,27.12900,40.70000,60.25000  #   4
*vertex,26.12900,40.70000,60.25000  #   5
*vertex,24.12938,40.70000,60.25000  #   6
*vertex,24.03439,35.70000,63.39300  #   7
*vertex,27.69180,35.70000,63.39300  #   8
*vertex,27.69180,40.70000,63.39300  #   9
*vertex,27.12900,40.70000,63.39300  #  10
*vertex,26.12900,40.70000,63.39300  #  11
*vertex,24.12938,40.70000,63.39300  #  12
*vertex,26.12900,40.70000,62.50000  #  13
*vertex,27.12900,40.70000,62.50000  #  14
*vertex,27.12900,40.70000,61.36000  #  15
*vertex,26.12900,40.70000,61.36000  #  16
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
*surf,ptn_pr_op,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,56,32  #   1 ||< ptn_pr_op:L4_open_s
*surf,ptn_cel,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,55,25  #   2 ||< ptn_pz:L4_cell_srv
*surf,vent_d,VERT,-,WALL,-,stone_vent,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,glaz_a,VERT,-,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   4 ||< external
*surf,stone_a,VERT,-,WALL,-,stone_gyp,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,ptn_social,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,63,03  #   6 ||< ptn_print:L4_admin
*surf,ceiling_pz,CEIL,-,ROOF,FLAT,roof_0.2,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,floor_pz,FLOR,-,-,-,top_pln,OPAQUE,ANOTHER,80,03  #   8 ||< floor_pz:L4_srv_void
*surf,upper_gla_a,VERT,-,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   9 ||< external
*surf,low_gla_a,VERT,-,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #  10 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable   6 # list of surfs
  3  4  5  7  9 10
# 
*insol_calc,all_applicable   3 # insolation sources
  4  9 10
# 
*base_list,1,8,    18.05 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start, 20 20 # geometric blocks
*obs,27.100,40.750,51.690,1.000,1.000,13.450,0.000,1.00,vnt_c,stone_eldon  # block   1
*obs,13.250,50.750,48.000,36.000,1.000,13.450,0.000,1.00,other_bld,stone_eldon  # block   2
*end_block
