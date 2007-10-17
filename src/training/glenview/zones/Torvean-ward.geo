*Geometry 1.1,GEN,Torvean-war # tag version, format, zone name
*date Thu Aug 23 18:04:04 2007  # latest file modification 
Torvean-war describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,11.82506,8.46459,0.00000  #   1
*vertex,8.89993,5.73629,0.00000  #   2
*vertex,8.89993,5.73629,2.60000  #   3
*vertex,10.36249,7.10044,3.21146  #   4
*vertex,11.82506,8.46459,2.60000  #   5
*vertex,8.08144,6.61383,0.00000  #   6
*vertex,11.00657,9.34213,0.00000  #   7
*vertex,11.00657,9.34213,2.60000  #   8
*vertex,8.08144,6.61383,2.60000  #   9
*vertex,6.18011,4.84044,0.00000  #  10
*vertex,-2.68686,14.34711,-0.00001  #  11
*vertex,20.30000,18.01000,0.00000  #  12
*vertex,6.18000,4.84000,2.60000  #  13
*vertex,-2.68700,14.34700,2.60000  #  14
*vertex,20.30000,18.01000,2.60000  #  15
*vertex,1.74659,9.59341,6.60000  #  16
*vertex,8.86635,8.70452,3.21146  #  17
*vertex,20.30000,35.78723,0.00000  #  18
*vertex,20.30000,35.78700,2.60000  #  19
*vertex,20.30000,26.89854,6.60000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,4,11,10,13,14  #  1
*edges,4,12,18,19,15  #  2
*edges,4,18,11,14,19  #  3
*edges,4,10,6,9,13  #  4
*edges,4,6,2,3,9  #  5
*edges,8,10,11,18,12,7,1,2,6  #  6
*edges,5,2,1,5,4,3  #  7
*edges,4,7,12,15,8  #  8
*edges,4,5,8,17,4  #  9
*edges,4,9,3,4,17  # 10
*edges,4,1,7,8,5  # 11
*edges,7,13,9,17,8,15,20,16  # 12
*edges,4,19,14,16,20  # 13
*edges,3,14,13,16  # 14
*edges,3,15,19,20  # 15
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-v-s2,VERT,-,-,-,Ward-wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,Wall-v-n3,VERT,-,-,-,Ward-wall,OPAQUE,ANOTHER,10,02  #   2 ||< Wall-v-n3:Drumoss-war
*surf,Wall-v-n4,VERT,-,-,-,Ward-wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,Wall-v-s3,VERT,-,-,-,Ward-wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,Wall-v-s4,VERT,-,-,-,Ward-wall,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,Floor,FLOR,-,-,-,Ward-wall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,Wall-v-s5,VERT,-,-,-,Ward-wall,OPAQUE,ANOTHER,01,04  #   7 ||< Fictive-sim:West_1
*surf,Wall-v-s6,VERT,-,-,-,Ward-wall,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,Roof-n1,SLOP,-,-,-,Ward-roof,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,Roof-s1,SLOP,-,-,-,Ward-roof,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,Wall-v-n1,VERT,-,-,-,Ward-wall,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,Roof-s2,SLOP,-,-,-,Ward-roof,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,Roof-n2,SLOP,-,-,-,Ward-roof,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,Wall-v-s1,VERT,-,-,-,Ward-wall,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,Wall-v-n2,VERT,-,-,-,Ward-wall,OPAQUE,ANOTHER,10,15  #  15 ||< Wall-v-n2:Drumoss-war
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,   334.62 0  # zone base list
