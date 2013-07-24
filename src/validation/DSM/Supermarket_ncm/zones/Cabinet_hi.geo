*Geometry 1.1,GEN,Cabinet_hi # tag version, format, zone name
*date Mon Feb 13 15:05:45 2012  # latest file modification 
Cabinet_hi describes a high temperature chilled cabinet
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,77.50000,42.50000,0.00000  #   1
*vertex,93.50000,42.50000,0.00000  #   2
*vertex,93.50000,62.50000,0.00000  #   3
*vertex,77.50000,62.50000,0.00000  #   4
*vertex,77.50000,42.50000,2.50000  #   5
*vertex,93.50000,42.50000,2.50000  #   6
*vertex,93.50000,62.50000,2.50000  #   7
*vertex,77.50000,62.50000,2.50000  #   8
*vertex,77.71000,42.71000,1.50000  #   9
*vertex,93.29000,42.71000,1.50000  #  10
*vertex,93.29000,62.29000,1.50000  #  11
*vertex,77.71000,62.29000,1.50000  #  12
*vertex,77.71000,42.71000,1.00000  #  13
*vertex,93.29000,42.71000,1.00000  #  14
*vertex,93.29000,62.29000,1.00000  #  15
*vertex,77.71000,62.29000,1.00000  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
*edges,4,9,10,11,12  #  7
*edges,4,10,9,12,11  #  8
*edges,4,13,14,15,16  #  9
*edges,4,14,13,16,15  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,cab_front,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,02,06  #   1 ||< low_cab_fr:Refrig
*surf,cab_right,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,02,07  #   2 ||< low_cab_ri:Refrig
*surf,cab_back,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,02,08  #   3 ||< low_cab_bk:Refrig
*surf,cab_left,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,02,09  #   4 ||< low_cab_lft:Refrig
*surf,cab_top,CEIL,-,-,-,fictitious,SC_fictit,ANOTHER,02,10  #   5 ||< low_cab_top:Refrig
*surf,cab_base,FLOR,-,FLOOR,EXTGRND,SM_grnd_cab,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,up_TM_fu,CEIL,-,-,-,mass_part,OPAQUE,ANOTHER,11,08  #   7 ||< up_TM_fd:Cabinet_hi
*surf,up_TM_fd,FLOR,-,-,-,mass_part,OPAQUE,ANOTHER,11,07  #   8 ||< up_TM_fu:Cabinet_hi
*surf,low_TM_fu,CEIL,-,-,-,mass_part,OPAQUE,ANOTHER,11,10  #   9 ||< low_TM_fd:Cabinet_hi
*surf,low_TM_fd,FLOR,-,-,-,mass_part,OPAQUE,ANOTHER,11,09  #  10 ||< low_TM_fu:Cabinet_hi
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,   320.00 2  # zone base list
