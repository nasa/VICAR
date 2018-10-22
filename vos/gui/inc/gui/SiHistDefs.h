///////////////////////////////////////////////////////////////////
// SiHistDefs.h: Type definitions for histogram components.
///////////////////////////////////////////////////////////////////
#ifndef SIHISTDEFS_H
#define SIHISTDEFS_H

typedef unsigned char MethodType;
#define STACKED 0
#define BLEND 1
#define POPUP 2
typedef unsigned char PopupDirectionType;
#define ROW 0
#define COLUMN 1
typedef unsigned char VerAxisDirType;
#define ASC 0
#define DESC 1
typedef unsigned char OrientType;
#define HORIZONTAL 0
#define VERTICAL 1

// Define X resource names

#define XvicNmethod		"method"
#define XvicCMethod		"Method"
#define XvicRMethodType		"MethodType"

#define XvicNorientation	"histOrientation"
#define XvicCOrientation	"HistOrientation"
#define XvicROrientType         "OrientType"

#define XvicNpopupDirection	"popDirection"
#define XvicCPopupDirection	"PopDirection"
#define XvicRPopupDirectionType "PopupDirectionType,"

#define XvicNshowAxis           "showAxis"
#define XvicCShowAxis           "ShowAxis"
#define XvicRShowAxisType       "Boolean"

#define XvicNshowStat           "showStat"
#define XvicCShowStat           "ShowStat"
#define XvicRShowAxisType       "Boolean"

#define XvicNshowHist           "showHist"
#define XvicCShowHist           "ShowHist"
#define XvicRShowHistType       "Boolean"

#define XvicNspike              "spike"
#define XvicCSpike              "Spike"
#define XvicRSpikeType          "Int"

#define XvicNverAxisDir		"verAxisDir"
#define XvicCVerAxisDir		"VerAxisDir"
#define XvicRVerAxisDirType	"VerAxisDirType"

#endif
