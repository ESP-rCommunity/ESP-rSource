*Geometry 1.1,GEN,storage # tag version, format, zone name
*date Wed Apr 18 18:08:49 2012  # latest file modification 
storage describes core storage between BR and lounge
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,3.40000,3.80000,0.20000  #   1
*vertex,4.10000,3.80000,0.20000  #   2
*vertex,4.10000,5.10000,0.20000  #   3
*vertex,4.10000,6.60000,0.20000  #   4
*vertex,3.40000,6.60000,0.20000  #   5
*vertex,3.40000,5.10000,0.20000  #   6
*vertex,3.40000,3.80000,2.40000  #   7
*vertex,4.10000,3.80000,2.40000  #   8
*vertex,4.10000,5.10000,2.40000  #   9
*vertex,4.10000,6.60000,2.40000  #  10
*vertex,3.40000,6.60000,2.40000  #  11
*vertex,3.40000,5.10000,2.40000  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,8,7  #  1
*edges,4,2,3,9,8  #  2
*edges,4,3,4,10,9  #  3
*edges,4,4,5,11,10  #  4
*edges,4,5,6,12,11  #  5
*edges,4,6,1,7,12  #  6
*edges,6,7,8,9,10,11,12  #  7
*edges,6,1,6,5,4,3,2  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_hall,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,07  #   1 ||< ptn_stor:hall
*surf,ptn_liv_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,11  #   2 ||< ptn_stor_b:livingdin
*surf,ptn_liv_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,10  #   3 ||< ptn_stor_a:livingdin
*surf,bk_facade,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ptn_br1_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,04,06  #   5 ||< ptn_store_b:BR_one
*surf,ptn_br1_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,04,05  #   6 ||< ptn_store_a:BR_one
*surf,stor_ceil,CEIL,-,-,-,Ceiltk_up,OPAQUE,ANOTHER,09,05  #   7 ||< stor_ceil:roof
*surf,stor_floor,FLOR,-,-,-,Floorti_low,OPAQUE,ANOTHER,10,05  #   8 ||< stor_floor:crawl
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,     1.96 0  # zone base list
