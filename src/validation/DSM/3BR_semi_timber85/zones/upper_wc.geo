*Geometry 1.1,GEN,upper_wc # tag version, format, zone name
*date Mon Mar 19 11:02:48 2012  # latest file modification 
upper_wc describes WC at top of landing
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,5.50000,2.40000,2.80000  #   1
*vertex,6.50000,2.40000,2.80000  #   2
*vertex,6.50000,4.90000,2.80000  #   3
*vertex,5.50000,4.90000,2.80000  #   4
*vertex,5.50000,4.40000,2.80000  #   5
*vertex,5.50000,3.60000,2.80000  #   6
*vertex,5.50000,2.50000,2.80000  #   7
*vertex,5.50000,2.40000,5.60000  #   8
*vertex,6.50000,2.40000,5.60000  #   9
*vertex,6.50000,4.90000,5.60000  #  10
*vertex,5.50000,4.90000,5.60000  #  11
*vertex,5.50000,3.60000,5.60000  #  12
*vertex,5.50000,2.50000,5.60000  #  13
*vertex,5.50000,4.40000,5.60000  #  14
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,9,8  #  1
*edges,4,2,3,10,9  #  2
*edges,4,7,1,8,13  #  3
*edges,7,1,7,6,5,4,3,2  #  4
*edges,4,3,4,11,10  #  5
*edges,4,6,7,13,12  #  6
*edges,4,5,6,12,14  #  7
*edges,4,4,5,14,11  #  8
*edges,7,10,11,14,12,13,8,9  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_br1,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,07,07  #   1 ||< ptn_br1:up_br_one
*surf,facade,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,ptn_br1_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,07,08  #   3 ||< ptn_br1_a:up_br_one
*surf,wc_floor,FLOR,-,-,-,Floorti_up,OPAQUE,ANOTHER,02,24  #   4 ||< ceil_wc:entry
*surf,ptn_wc_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,05,01  #   5 ||< ptn_wc_a:upstair_pa
*surf,ptn_wc_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,05,09  #   6 ||< ptn_wc_b:upstair_pa
*surf,door_wc_b,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,05,10  #   7 ||< door_wc_b:upstair_pa
*surf,ptn_wc_c,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,05,11  #   8 ||< ptn_wc_c:upstair_pa
*surf,wc_ceiling,CEIL,-,-,-,Ceilti_up,OPAQUE,ANOTHER,10,03  #   9 ||< wc_ceiling:roof_space
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,9,4,     5.00 0  # zone base list
