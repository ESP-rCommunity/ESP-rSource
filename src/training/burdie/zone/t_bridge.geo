*Geometry 1.1,GEN,t_bridge # tag version, format, zone name
*date Sun Dec  8 13:46:26 2013  # latest file modification 
t_bridge describes a thermal bridge
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,6.33600,4.92500  #   1
*vertex,3.30000,6.33600,4.92500  #   2
*vertex,3.30000,6.40000,4.92500  #   3
*vertex,0.00000,6.40000,4.92500  #   4
*vertex,0.00000,6.33600,5.10000  #   5
*vertex,3.30000,6.33600,5.10000  #   6
*vertex,3.30000,6.40000,5.10000  #   7
*vertex,0.00000,6.40000,5.10000  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,bed_3_r,VERT,-,-,-,t_bridge_r,OPAQUE,ANOTHER,09,10  #   1 ||< loft_n_r:bed_3
*surf,bed_2_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,08,12  #   2 ||< xbed_2_r:bed_2
*surf,loft_n_r,VERT,-,-,-,air_gap_ext,OPAQUE,ANOTHER,11,08  #   3 ||< bed3_sof:loft
*surf,west_ext,VERT,-,-,-,ext_wall_up,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,loft_up,CEIL,-,-,-,ceil_loft,OPAQUE,ANOTHER,11,15  #   5 ||< t_bridge:loft
*surf,bedroom,FLOR,-,-,-,steel_sheet,OPAQUE,ANOTHER,13,04  #   6 ||< xbedroom:bed_3_air_g
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     0.21 0  # zone base list
