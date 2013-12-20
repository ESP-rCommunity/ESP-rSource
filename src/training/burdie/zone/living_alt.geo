*Geometry 1.1,GEN,living_room # tag version, format, zone name
*date Sun Dec  8 13:54:50 2013  # latest file modification 
living_room is the ground level lounge
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,2.10000,0.00000,0.00000  #   1
*vertex,6.90000,0.00000,0.00000  #   2
*vertex,6.90000,3.70000,0.00000  #   3
*vertex,2.10000,3.70000,0.00000  #   4
*vertex,2.10000,0.00000,2.70000  #   5
*vertex,6.90000,0.00000,2.70000  #   6
*vertex,6.90000,3.70000,2.70000  #   7
*vertex,2.10000,3.70000,2.70000  #   8
*vertex,2.10000,3.10000,2.70000  #   9
*vertex,3.30000,3.10000,2.70000  #  10
*vertex,3.30000,2.10000,2.70000  #  11
*vertex,2.30000,2.10000,2.70000  #  12
*vertex,2.30000,0.00000,2.70000  #  13
*vertex,3.30000,3.70000,2.70000  #  14
*vertex,5.20000,3.70000,0.00000  #  15
*vertex,5.20000,3.70000,2.70000  #  16
*vertex,6.90000,3.10000,2.70000  #  17
*vertex,3.40000,0.00000,0.40000  #  18
*vertex,5.60000,0.00000,0.40000  #  19
*vertex,5.60000,0.00000,2.30000  #  20
*vertex,3.40000,0.00000,2.30000  #  21
# 
# tag, number of vertices followed by list of associated vert
*edges,11,1,2,6,13,5,1,18,21,20,19,18  #  1
*edges,5,2,3,7,17,6  #  2
*edges,5,15,4,8,14,16  #  3
*edges,5,4,1,5,9,8  #  4
*edges,4,9,10,14,8  #  5
*edges,5,4,15,3,2,1  #  6
*edges,6,9,5,13,12,11,10  #  7
*edges,6,11,12,13,6,17,10  #  8
*edges,4,3,15,16,7  #  9
*edges,5,16,14,10,17,7  # 10
*edges,4,18,19,20,21  # 11
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,liv_ex_s,VERT,-,-,-,ext_wall_low,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,liv_sim_e,VERT,-,-,-,intern_wall,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,kitch_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,04,01  #   3 ||< liv_r:kitchen
*surf,hall_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,02,02  #   4 ||< livrm_r:hall
*surf,bed_3_r,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,09,09  #   5 ||< liv_r:bed_3
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,hall_up_r,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,06,09  #   7 ||< liv_r:hall_up
*surf,bed_1_r,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,07,08  #   8 ||< liv_r:bed_1
*surf,store_r,VERT,-,-,-,partition,OPAQUE,ANOTHER,05,01  #   9 ||< liv_r:store
*surf,bed_2_r,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,08,06  #  10 ||< liv_r:bed_2
*surf,s_glaz,VERT,-,-,-,glazing,DC_8074_04nb,EXTERIOR,0,0  #  11 ||< external
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    17.76 0  # zone base list
