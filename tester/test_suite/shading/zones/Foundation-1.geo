*Geometry 1.1,GEN,Foundation-1 # tag version, format, zone name
*date Fri Apr 30 11:21:35 2021  # latest file modification 
simplified basement elevation
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,-1.00000,0.75000,-1.82000  #   1
*vertex,-1.00000,0.75000,0.53000  #   2
*vertex,-10.74600,0.75000,0.53000  #   3
*vertex,-10.74600,0.75000,-1.82000  #   4
*vertex,-10.74600,10.49600,0.53000  #   5
*vertex,-10.74600,10.49600,-1.82000  #   6
*vertex,-1.00000,10.49600,0.53000  #   7
*vertex,-1.00000,10.49600,-1.82000  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,3,4  #   1
*edges,4,4,3,5,6  #   2
*edges,4,6,5,7,8  #   3
*edges,4,8,7,2,1  #   4
*edges,4,1,4,6,8  #   5
*edges,4,7,5,3,2  #   6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,bsm_Mainflr2,VERT,-,-,-,foundation,OPAQUE,BASESIMP,001,011  #   1 ||< BASESIMP config type   1
*surf,bsm_Mainflr1,VERT,-,-,-,foundation,OPAQUE,BASESIMP,001,011  #   2 ||< BASESIMP config type   1
*surf,bsm_Mainflr4,VERT,-,-,-,foundation,OPAQUE,BASESIMP,001,011  #   3 ||< BASESIMP config type   1
*surf,bsm_Mainflr3,VERT,-,-,-,foundation,OPAQUE,BASESIMP,001,011  #   4 ||< BASESIMP config type   1
*surf,slab,FLOR,-,-,-,slab_floor,OPAQUE,BASESIMP,001,057  #   5 ||< BASESIMP config type   1
*surf,to_main,CEIL,-,-,-,floors_r,OPAQUE,ANOTHER,001,005  #   6 ||< to_bsm:Main
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,    94.98 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start,  5  5 # geometric blocks
*obs,-3.7500,-0.0000,2.0470,0.7500,4.0000,0.0100,-270.00,1.00,obstr_1 NONE  # block   1
*obs,-10.7460,4.7500,2.3970,4.0000,1.0000,0.0100,-270.00,1.00,obstr_2 NONE  # block   2
*obs,-5.2460,10.4960,2.6300,0.5000,0.5000,0.0100,-270.00,1.00,obstr_3 NONE  # block   3
*obs,-0.0000,4.0620,2.5970,1.2000,1.0000,0.0100,-270.00,1.00,obstr_4 NONE  # block   4
*end_block
