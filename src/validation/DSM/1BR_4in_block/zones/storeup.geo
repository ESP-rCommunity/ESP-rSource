*Geometry 1.1,GEN,storeup # tag version, format, zone name
*date Mon Mar 12 10:09:09 2012  # latest file modification 
storeup describes upper flat storage adj to entrance
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,2.80000,0.30000,2.60000  #   1
*vertex,4.50000,0.30000,2.60000  #   2
*vertex,4.50000,2.60000,2.60000  #   3
*vertex,4.30000,2.60000,2.60000  #   4
*vertex,3.50000,2.60000,2.60000  #   5
*vertex,2.80000,2.60000,2.60000  #   6
*vertex,2.80000,2.70000,2.60000  #   7
*vertex,2.60000,2.70000,2.60000  #   8
*vertex,2.60000,1.90000,2.60000  #   9
*vertex,2.80000,1.90000,2.60000  #  10
*vertex,2.80000,0.30000,4.80000  #  11
*vertex,4.50000,0.30000,4.80000  #  12
*vertex,4.50000,2.60000,4.80000  #  13
*vertex,4.30000,2.60000,4.80000  #  14
*vertex,3.50000,2.60000,4.80000  #  15
*vertex,2.80000,2.60000,4.80000  #  16
*vertex,2.80000,2.70000,4.80000  #  17
*vertex,2.60000,2.70000,4.80000  #  18
*vertex,2.60000,1.90000,4.80000  #  19
*vertex,2.80000,1.90000,4.80000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,12,11  #  1
*edges,4,2,3,13,12  #  2
*edges,4,3,4,14,13  #  3
*edges,4,4,5,15,14  #  4
*edges,4,5,6,16,15  #  5
*edges,4,6,7,17,16  #  6
*edges,4,7,8,18,17  #  7
*edges,4,8,9,19,18  #  8
*edges,4,9,10,20,19  #  9
*edges,4,10,1,11,20  # 10
*edges,10,11,12,13,14,15,16,17,18,19,20  # 11
*edges,10,1,10,9,8,7,6,5,4,3,2  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,fr_facade,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,ptn_bath,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,09,09  #   2 ||< ptn_store:bathup
*surf,ptn_hall_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,11,13  #   3 ||< ptn_stor_d:hall_up
*surf,door_hall,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,11,12  #   4 ||< door_stoor:hall_up
*surf,ptn_hall_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,11,11  #   5 ||< ptn_stor_c:hall_up
*surf,ptn_hall_c,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,11,10  #   6 ||< ptn_stor_b:hall_up
*surf,ptn_hall_d,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,11,09  #   7 ||< ptn_stor_a:hall_up
*surf,ptn_stair,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,01,04  #   8 ||< ptn_stor:stairs
*surf,left_fac,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,left_fac_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,stor_ceil,CEIL,-,-,-,Ceilti_up,OPAQUE,ANOTHER,14,01  #  11 ||< ovr_store:roof
*surf,stor_floor,FLOR,-,-,-,Floorti_intc,OPAQUE,ANOTHER,02,11  #  12 ||< stor2_ceil:store2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,12,     4.07 0  # zone base list
