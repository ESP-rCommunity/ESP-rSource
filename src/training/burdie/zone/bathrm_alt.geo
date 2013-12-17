*Geometry 1.1,GEN,bathroom # tag version, format, zone name
*date Sun Dec  8 13:54:50 2013  # latest file modification 
bathroom describes the lower level toilet
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,4.70000,0.00000  #   1
*vertex,2.10000,4.70000,0.00000  #   2
*vertex,2.10000,6.40000,0.00000  #   3
*vertex,0.00000,6.40000,0.00000  #   4
*vertex,0.00000,4.70000,2.70000  #   5
*vertex,2.10000,4.70000,2.70000  #   6
*vertex,2.10000,6.40000,2.70000  #   7
*vertex,0.00000,6.40000,2.70000  #   8
*vertex,2.00000,6.40000,0.70000  #   9
*vertex,0.90000,6.40000,0.70000  #  10
*vertex,0.90000,6.40000,2.30000  #  11
*vertex,2.00000,6.40000,2.30000  #  12
*vertex,0.00000,6.33600,2.70000  #  13
*vertex,2.10000,6.33600,2.70000  #  14
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,5,2,3,7,14,6  #  2
*edges,10,3,4,8,7,3,9,12,11,10,9  #  3
*edges,5,4,1,5,13,8  #  4
*edges,4,5,6,14,13  #  5
*edges,4,4,3,2,1  #  6
*edges,4,9,10,11,12  #  7
*edges,4,13,14,7,8  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,hall_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,02,03  #   1 ||< bathrm_r:hall
*surf,kitch_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,04,07  #   2 ||< bathrm_r:kitchen
*surf,bath_ex_n,VERT,-,-,-,ext_wall_low,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,bath_ex_w,VERT,-,-,-,ext_wall_low,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,bed_1_r,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,09,07  #   5 ||< bath_r:bed_3
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,n_glaz,VERT,-,-,-,glazing,DC_8074_04nb,EXTERIOR,0,0  #   7 ||< external
*surf,air_gap_r,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,13,11  #   8 ||< xair_gap_r:bed_3_air_g
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     3.57 0  # zone base list
