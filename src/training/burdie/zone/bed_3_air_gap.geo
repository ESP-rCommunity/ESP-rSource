*Geometry 1.1,GEN,bed_3_air_g # tag version, format, zone name
*date Sun Dec  8 13:46:26 2013  # latest file modification 
bed_3_air_g is the air gap on north face of bedroom 3
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,6.33600,2.70000  #   1
*vertex,0.00000,6.40000,2.70000  #   2
*vertex,3.30000,6.33600,2.70000  #   3
*vertex,0.00000,6.33600,4.92500  #   4
*vertex,3.30000,6.33600,4.92500  #   5
*vertex,2.10000,6.33600,3.40000  #   6
*vertex,2.10000,6.33600,4.90000  #   7
*vertex,1.10000,6.33600,4.90000  #   8
*vertex,1.10000,6.33600,3.40000  #   9
*vertex,3.30000,6.40000,2.70000  #  10
*vertex,2.10000,6.40000,3.40000  #  11
*vertex,1.10000,6.40000,3.40000  #  12
*vertex,1.10000,6.40000,4.90000  #  13
*vertex,2.10000,6.40000,4.90000  #  14
*vertex,3.30000,6.40000,4.92500  #  15
*vertex,0.00000,6.40000,4.92500  #  16
*vertex,2.10000,6.33600,2.70000  #  17
*vertex,2.10000,6.40000,2.70000  #  18
# 
# tag, number of vertices followed by list of associated vert
*edges,4,17,18,10,3  #  1
*edges,11,3,6,9,8,7,6,3,5,4,1,17  #  2
*edges,11,10,11,14,13,12,11,10,18,2,16,15  #  3
*edges,4,16,4,5,15  #  4
*edges,4,7,8,13,14  #  5
*edges,4,2,1,4,16  #  6
*edges,4,6,11,12,9  #  7
*edges,4,11,6,7,14  #  8
*edges,4,13,8,9,12  #  9
*edges,4,15,5,3,10  # 10
*edges,4,17,1,2,18  # 11
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,first,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,04,10  #   1 ||< xfirst:kitchen
*surf,xbed3_ex_n,VERT,-,-,-,air_gap_intr,OPAQUE,ANOTHER,09,03  #   2 ||< bed3_ex_n:bed_3
*surf,xxbed3_ex_n,VERT,-,-,-,air_gap_ext,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,xbedroom,CEIL,-,-,-,steel_sheet,OPAQUE,ANOTHER,10,06  #   4 ||< bedroom:t_bridge
*surf,Surf-5,FLOR,-,-,-,steel_sheet,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,gap_ext_w,VERT,-,-,-,ext_wall_up,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,Surf-7,CEIL,-,-,-,steel_sheet,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,Surf-8,VERT,-,-,-,steel_sheet,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,Surf-9,VERT,-,-,-,steel_sheet,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,Surf-10,VERT,-,-,-,partition,OPAQUE,ANOTHER,08,11  #  10 ||< xSurf-10:bed_2
*surf,xair_gap_r,FLOR,-,-,-,ceiling_rev,OPAQUE,ANOTHER,03,08  #  11 ||< air_gap_r:bathroom
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,3,1,5,11,     0.28 0  # zone base list
