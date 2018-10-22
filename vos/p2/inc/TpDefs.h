////////////////////////////////////////////////////////////
// TpDefs.h: General definitions for tp program.
////////////////////////////////////////////////////////////

#ifndef TPDEFS_H
#define TPDEFS_H

//const int TP_MAX_IMAGES = 12;
const int TP_MAX_IMAGES = 25;
const int TP_MAX_DISPLAYS = 6;

typedef unsigned char TpAutofindMode;
#define MANUAL 0
#define AFFINE 1
#define SPICE  2
#define XtpRAutofindMode "TpAutofindMode"

typedef unsigned char TpMatchMode;
#define AUTO_CORR     0
#define AFFINE_ONLY   1
#define XtpRMatchMode "TpMatchMode"

typedef unsigned char TpPointSymbolShapeType;
#define CrossWithDot                0
#define Rectangle                   1
#define Dot                         2
#define Cross45                     3
#define CrossWithHole45             4
#define RectangleWithCrossesWithDot 5
#define XtpRPointSymbolShapeType "TpPointSymbolShapeType"

typedef unsigned char TpTagPosition;
#define NorthEast 0
#define NorthWest 1
#define SouthEast 2
#define SouthWest 3
#define Center 4

#define FULL "FULL"
#define REAL "REAL"
#define TEXT "A256"
 
#define BLANK_FULL -1
#define BLANK_REAL (float)-1.0
#define BLANK_TEXT "NONE"

#ifndef STRDUP_SIM_DEFINED
#define STRDUP_SIM_DEFINED
#include <string.h>
 
inline char *sdup(const char *str)
{
    // not robust, but neither is strdup
    char *newStr = new char[strlen(str) + 1];
    strcpy(newStr, str);
    return (newStr);
}
#endif

#endif
