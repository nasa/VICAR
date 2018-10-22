///////////////////////////////////////////////////////////////////
// HistDefs.h:
///////////////////////////////////////////////////////////////////
#include "GuiDefs.h"

#ifndef HISTDEFS_H
#define HISTDEFS_H

enum MethodType { STACKED, BLEND, POPUP };
enum PopupDirectionType { ROW, COLUMN };
enum VerAxisDirType { ASC, DESC };

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
