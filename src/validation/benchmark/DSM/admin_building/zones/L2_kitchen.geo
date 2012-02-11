*Geometry 1.1,GEN,L2_kitchen # tag version, format, zone name
*date Sat Feb 11 17:22:29 2012  # latest file modification 
L2_kitchen describes L2 kitchenette on service road facade
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,41.37145,35.70000,52.45000  #   1
*vertex,44.69636,35.70000,52.45000  #   2
*vertex,44.74386,40.70000,52.45000  #   3
*vertex,44.22137,40.70000,52.45000  #   4
*vertex,42.17893,40.70000,52.45000  #   5
*vertex,41.32395,40.70000,52.45000  #   6
*vertex,41.37145,35.70000,55.59300  #   7
*vertex,44.69636,35.70000,55.59300  #   8
*vertex,44.74386,40.70000,55.59300  #   9
*vertex,44.22137,40.70000,55.59300  #  10
*vertex,42.17893,40.70000,55.59300  #  11
*vertex,41.32395,40.70000,55.59300  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,8,7  #  1
*edges,4,2,3,9,8  #  2
*edges,4,3,4,10,9  #  3
*edges,4,4,5,11,10  #  4
*edges,4,5,6,12,11  #  5
*edges,4,6,1,7,12  #  6
*edges,6,7,8,9,10,11,12  #  7
*edges,6,1,6,5,4,3,2  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-1,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,27,11  #   1 ||< Wall-11:L2_coridor
*surf,Wall-2,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,28,09  #   2 ||< Wall-11:L2_cor_cell
*surf,Wall-3,VERT,-,WALL,-,stone_gyp,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,glazing,VERT,-,C-WINDOW,CLOSED,db_lpasol,DSF4554_06nb,EXTERIOR,0,0  #   4 ||< external
*surf,Wall-5,VERT,-,WALL,-,stone_vent,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,Wall-6,VERT,-,-,-,gyp_aco_gyp,OPAQUE,ANOTHER,29,10  #   6 ||< ptn_stair:L2_cell_srv
*surf,Top-7,CEIL,-,-,-,slab_275,OPAQUE,ANOTHER,50,25  #   7 ||< Top-7:L3_ufloor
*surf,Base-8,FLOR,-,-,-,top_pln,OPAQUE,ANOTHER,17,57  #   8 ||< Base-8:L2_uf_plen
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,all_applicable   3 # list of surfs
  3  4  5
# 
*insol_calc,all_applicable   1 # insolation sources
  4
# 
*base_list,1,8,    16.86 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start, 20 20 # geometric blocks
*obs,40.750,40.750,51.690,1.000,1.000,13.450,0.000,1.00,vnt_e,stone_eldon  # block   1
*obs,13.250,50.750,48.000,36.000,1.000,13.450,0.000,1.00,other_bld,stone_eldon  # block   2
*end_block
