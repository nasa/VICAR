////////////////////////////////////////////////////////////////
// RotationDefs.h 
//
////////////////////////////////////////////////////////////////
#ifndef ROTATIONDEFS_H
#define ROTATIONDEFS_H

enum    RotationType    { ROTATE_NO, ROTATE_CW, ROTATE_CCW,
                          FLIP_NW_SE, FLIP_NE_SW, ROTATE_FULL };

#define ROT_NO_SWAP(type) (((type)==ROTATE_NO) || ((type)==ROTATE_FULL))

// Define X resource names

#define XvicNrotation             "rotation"
#define XvicCRotation             "Rotation"
#define XvicRRotationType         "RotationType"

#endif
