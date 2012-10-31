*Geometry 1.1,GEN,bath # tag version, format, zone name
*date Thu Aug 23 18:13:58 2007  # latest file modification 
bath describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.75700,9.99300,2.40000  #   1
*vertex,2.45100,9.37500,2.40000  #   2
*vertex,1.36900,9.08500,2.40000  #   3
*vertex,1.92800,7.00000,2.40000  #   4
*vertex,5.01900,7.82800,2.40000  #   5
*vertex,5.31600,7.90800,2.40000  #   6
*vertex,2.45100,9.37500,3.46600  #   7
*vertex,1.36900,9.08500,3.46600  #   8
*vertex,1.80200,7.47200,4.60000  #   9
*vertex,1.92800,7.00000,4.26200  #  10
*vertex,4.75700,9.99300,4.80000  #  11
*vertex,1.36900,9.08500,4.80000  #  12
*vertex,1.92800,7.00000,4.80000  #  13
*vertex,5.01900,7.82800,4.80000  #  14
*vertex,5.31600,7.90800,4.80000  #  15
*vertex,4.69900,9.97700,3.60000  #  16
*vertex,3.54000,9.66700,3.60000  #  17
*vertex,3.54000,9.66700,4.50000  #  18
*vertex,4.69900,9.97700,4.50000  #  19
*vertex,2.53000,9.39600,3.87000  #  20
*vertex,1.92200,9.23300,3.87000  #  21
*vertex,1.92200,9.23300,4.49900  #  22
*vertex,2.53000,9.39600,4.49900  #  23
# 
# tag, number of vertices followed by list of associated vert
*edges,18,1,2,7,8,21,20,23,22,21,8,12,11,1,16,19,18,17,16  #  1
*edges,4,2,3,8,7  #  2
*edges,5,3,4,10,9,8  #  3
*edges,5,8,9,10,13,12  #  4
*edges,5,4,5,14,13,10  #  5
*edges,4,5,6,15,14  #  6
*edges,4,6,1,11,15  #  7
*edges,5,11,12,13,14,15  #  8
*edges,6,6,5,4,3,2,1  #  9
*edges,4,16,17,18,19  # 10
*edges,4,20,21,22,23  # 11
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,n_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,u_exl_bth,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,11,04  #   2 ||< exl_bath:ext_lobby
*surf,gar_bth,VERT,-,-,-,ext_wall,OPAQUE,ANOTHER,14,08  #   3 ||< gar_bath:garage
*surf,w_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,bth_b1,VERT,-,-,-,int_part_2,OPAQUE,ANOTHER,06,01  #   5 ||< p_bath_b1:bedroom_1
*surf,bth_uhb,VERT,-,-,-,int_part_2,OPAQUE,ANOTHER,10,03  #   6 ||< prt_btha:upper_hall
*surf,bth_uha,VERT,-,-,-,int_part_2,OPAQUE,ANOTHER,10,02  #   7 ||< prt_bth:upper_hall
*surf,ceil,CEIL,-,-,-,1st_ceiling,OPAQUE,ANOTHER,13,07  #   8 ||< ceil_bath:loft
*surf,flr_kit,FLOR,-,-,-,grd_ceil_rev,OPAQUE,ANOTHER,04,08  #   9 ||< ceil_k_bth:kitchen
*surf,winda,VERT,-,-,-,double_glaz,DCF7672_06nb,EXTERIOR,0,0  #  10 ||< external
*surf,windb,VERT,-,-,-,double_glaz,DCF7672_06nb,EXTERIOR,0,0  #  11 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,9,     7.57 0  # zone base list
