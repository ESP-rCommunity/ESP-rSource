*Geometry 1.1,GEN,main # tag version, format, zone name
*date Sun May  2 07:44:17 2021  # latest file modification 
this the main floor zone
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,10.00000,0.00000,0.00000  #   2
*vertex,10.00000,9.00000,0.00000  #   3
*vertex,5.00000,9.00000,0.00000  #   4
*vertex,5.00000,15.00000,0.00000  #   5
*vertex,0.00000,15.00000,0.00000  #   6
*vertex,0.00000,0.00000,2.70000  #   7
*vertex,10.00000,0.00000,2.70000  #   8
*vertex,10.00000,9.00000,2.70000  #   9
*vertex,5.00000,9.00000,2.70000  #  10
*vertex,5.00000,15.00000,2.70000  #  11
*vertex,0.00000,15.00000,2.70000  #  12
*vertex,0.25000,0.00000,0.00000  #  13
*vertex,2.25000,0.00000,0.00000  #  14
*vertex,2.25000,0.00000,2.00000  #  15
*vertex,0.25000,0.00000,2.00000  #  16
*vertex,4.00000,0.00000,0.50000  #  17
*vertex,8.00000,0.00000,0.50000  #  18
*vertex,8.00000,0.00000,2.30000  #  19
*vertex,4.00000,0.00000,2.30000  #  20
*vertex,4.50000,15.00000,0.00000  #  21
*vertex,3.00000,15.00000,0.00000  #  22
*vertex,3.00000,15.00000,2.00000  #  23
*vertex,4.50000,15.00000,2.00000  #  24
*vertex,5.00000,9.50000,0.00000  #  25
*vertex,5.00000,10.50000,0.00000  #  26
*vertex,5.00000,10.50000,2.00000  #  27
*vertex,5.00000,9.50000,2.00000  #  28
*vertex,1.50000,15.00000,0.50000  #  29
*vertex,0.30000,15.00000,0.50000  #  30
*vertex,0.30000,15.00000,2.00000  #  31
*vertex,1.50000,15.00000,2.00000  #  32
# 
# tag, number of vertices followed by list of associated vert
*edges,14,1,13,16,15,14,2,8,7,1,17,20,19,18,17  #   1
*edges,4,2,3,9,8  #   2
*edges,4,3,4,10,9  #   3
*edges,8,4,25,28,27,26,5,11,10  #   4
*edges,14,5,21,24,23,22,6,12,11,5,29,32,31,30,29  #   5
*edges,4,6,1,7,12  #   6
*edges,6,7,8,9,10,11,12  #   7
*edges,12,1,6,22,21,5,26,25,4,3,2,14,13  #   8
*edges,4,13,14,15,16  #   9
*edges,4,17,18,19,20  #  10
*edges,4,21,22,23,24  #  11
*edges,4,25,26,27,28  #  12
*edges,4,29,30,31,32  #  13
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Southwall,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,000,000  #   1 ||< external
*surf,eastwall,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,000,000  #   2 ||< external
*surf,garge-s,VERT,-,-,-,internl_wal,OPAQUE,ANOTHER,002,001  #   3 ||< garage-s:garage
*surf,garge-w,VERT,-,-,-,internl_wal,OPAQUE,ANOTHER,002,004  #   4 ||< garage-w:garage
*surf,northwall,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,000,000  #   5 ||< external
*surf,westwall,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,000,000  #   6 ||< external
*surf,ceiling,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,003,005  #   7 ||< floor-main:attic
*surf,floor,FLOR,-,-,-,slabONgrade,OPAQUE,GROUND,000,001  #   8 ||< user def grnd profile  1
*surf,win-patio,VERT,-,-,-,d_glz,DCF7671_06nb,EXTERIOR,000,000  #   9 ||< external
*surf,win-mbr,VERT,-,-,-,d_glz,DCF7671_06nb,EXTERIOR,000,000  #  10 ||< external
*surf,ext-door,VERT,-,-,-,ext-door,OPAQUE,EXTERIOR,000,000  #  11 ||< external
*surf,garage-idor,VERT,-,-,-,int_doors,OPAQUE,ANOTHER,002,008  #  12 ||< garage-idor:garage
*surf,win-bedr,VERT,-,-,-,d_glz,DCF7671_06nb,EXTERIOR,000,000  #  13 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,   120.00 0  # zone base list
