*Geometry 1.1,GEN,below_str # tag version, format, zone name
*date Thu Apr  5 11:12:11 2012  # latest file modification 
below_str describes storage below the stairs
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.50000,4.90000,0.20000  #   1
*vertex,5.50000,4.90000,0.20000  #   2
*vertex,6.50000,6.10000,0.20000  #   3
*vertex,6.50000,7.00000,0.20000  #   4
*vertex,4.50000,7.00000,0.20000  #   5
*vertex,4.50000,6.10000,0.20000  #   6
*vertex,4.50000,4.90000,1.20000  #   7
*vertex,5.50000,4.90000,1.20000  #   8
*vertex,5.50000,6.10000,1.20000  #   9
*vertex,6.50000,6.10000,1.20000  #  10
*vertex,6.50000,7.00000,1.20000  #  11
*vertex,4.50000,7.00000,1.20000  #  12
*vertex,4.50000,6.10000,1.20000  #  13
*vertex,6.50000,4.90000,0.20000  #  14
*vertex,5.50000,4.90000,2.60000  #  15
*vertex,4.50000,4.90000,2.60000  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,6,1,2,8,15,16,7  #  1
*edges,5,14,3,4,11,10  #  2
*edges,4,4,5,12,11  #  3
*edges,4,5,6,13,12  #  4
*edges,5,6,1,7,16,13  #  5
*edges,5,9,10,11,12,13  #  6
*edges,7,1,6,5,4,3,14,2  #  7
*edges,4,2,14,10,9  #  8
*edges,4,9,13,16,15  #  9
*edges,4,2,9,15,8  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,door_hall,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,02,07  #   1 ||< door_und_st:entry
*surf,ext_right,VERT,-,WALL,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,ext_bk,VERT,-,WALL,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,Wall-6,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,03,15  #   4 ||< lower_ptn:living
*surf,Wall-7,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,03,16  #   5 ||< ptn_below:living
*surf,Top-8,CEIL,-,-,-,Ceilti_lowc,OPAQUE,ANOTHER,02,14  #   6 ||< landing:entry
*surf,Base-9,FLOR,-,-,-,Floorti_low,OPAQUE,ANOTHER,11,04  #   7 ||< Base-9:crawl
*surf,steps,SLOP,-,-,-,Ceilti_lowc,OPAQUE,ANOTHER,02,15  #   8 ||< steps:entry
*surf,up_steps,SLOP,-,-,-,Ceilti_lowc,OPAQUE,ANOTHER,02,16  #   9 ||< up_steps:entry
*surf,low_banist,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,02,23  #  10 ||< low_banist:entry
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,7,     4.20 0  # zone base list
