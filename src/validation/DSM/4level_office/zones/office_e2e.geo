*Geometry 1.1,GEN,office_e2e # tag version, format, zone name
*date Mon Apr  2 13:39:34 2012  # latest file modification 
office_e2e is the east perimeter offices of level 2
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,40.00000,8.00000,8.05000  #   1
*vertex,40.00000,26.00000,8.05000  #   2
*vertex,40.00000,8.00000,10.85000  #   3
*vertex,40.00000,26.00000,10.85000  #   4
*vertex,40.00000,12.00000,8.85000  #   5
*vertex,40.00000,15.00000,8.85000  #   6
*vertex,40.00000,15.00000,10.25000  #   7
*vertex,40.00000,12.00000,10.25000  #   8
*vertex,40.00000,20.00000,8.85000  #   9
*vertex,40.00000,20.00000,10.25000  #  10
*vertex,40.00000,17.30000,8.85000  #  11
*vertex,40.00000,17.30000,10.25000  #  12
*vertex,40.00000,16.00000,8.05000  #  13
*vertex,40.00000,16.00000,10.85000  #  14
*vertex,35.00000,13.00000,8.05000  #  15
*vertex,35.00000,13.00000,10.85000  #  16
*vertex,35.00000,21.00000,8.05000  #  17
*vertex,35.00000,21.00000,10.85000  #  18
*vertex,39.90000,16.00000,8.15000  #  19
*vertex,35.10000,16.00000,8.15000  #  20
*vertex,35.10000,16.00000,10.75000  #  21
*vertex,39.90000,16.00000,10.75000  #  22
*vertex,40.00000,12.08000,8.93000  #  23
*vertex,40.00000,14.92000,8.93000  #  24
*vertex,40.00000,14.92000,10.17000  #  25
*vertex,40.00000,12.08000,10.17000  #  26
*vertex,40.00000,17.38000,8.93000  #  27
*vertex,40.00000,19.92000,8.93000  #  28
*vertex,40.00000,19.92000,10.17000  #  29
*vertex,40.00000,17.38000,10.17000  #  30
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,13,14,3,1,5,8,7,6,5  #  1
*edges,5,3,14,4,18,16  #  2
*edges,5,15,17,2,13,1  #  3
*edges,10,5,6,7,8,5,23,26,25,24,23  #  4
*edges,10,11,9,10,12,11,27,30,29,28,27  #  5
*edges,10,13,2,4,14,13,11,12,10,9,11  #  6
*edges,4,15,1,3,16  #  7
*edges,4,2,17,18,4  #  8
*edges,4,17,15,16,18  #  9
*edges,4,19,20,21,22  # 10
*edges,4,20,19,22,21  # 11
*edges,4,23,24,25,26  # 12
*edges,4,27,28,29,30  # 13
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,east,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,top,CEIL,-,-,-,ceiling_dr,OPAQUE,ANOTHER,18,16  #   2 ||< over_e2e:ceiling_2
*surf,bottom,FLOR,-,-,-,inter_flrev,OPAQUE,ANOTHER,17,16  #   3 ||< under_off_e:ceiling_1
*surf,frm_est_1,VERT,east,-,-,gl_frame,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,frm_est_2,VERT,east_b,-,-,gl_frame,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,east_b,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,se_ptn,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,12,18  #   7 ||< se_ptn:office_2_sth
*surf,ne_ptn,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,11,18  #   8 ||< ne_ptn:office_2_nor
*surf,corid_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,15,08  #   9 ||< corid_b:passage_2
*surf,ff_ptn_e,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,13,11  #  10 ||< ff_ptn_ee:office_e2e
*surf,ff_ptn_ee,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,13,10  #  11 ||< ff_ptn_e:office_e2e
*surf,gl_est_1,VERT,frm_est_1,C-WINDOW,CLOSED,double_glz,TRAN,EXTERIOR,0,0  #  12 ||< external
*surf,gl_est_2,VERT,frm_est_2,C-WINDOW,CLOSED,double_glz,TRAN,EXTERIOR,0,0  #  13 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,all_applicable   2 # insolation sources
  4  5
# 
*base_list,1,3,    65.00 0  # zone base list
