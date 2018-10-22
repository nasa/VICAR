//////////////////////////////////////////////////////////////
// HistBtnInterface.cc: A "push button" interface to a Cmd object.
///////////////////////////////////////////////////////////////
#include "HistBtnInterface.h"
#include "Histogram.h"
#include "HistGraphView.h"
#include <Xm/Frame.h>

HistBtnInterface::HistBtnInterface ( Widget parent, Cmd *cmd, 
		Histogram *histR, Histogram *histG, Histogram *histB ) 
	: SgDrawAreaInterface ( parent, cmd )
{
   HistGraphView *histGraphView = new HistGraphView(_w, "histGraphView",
                                        histR, histG, histB,
                                        BLEND, HORIZONTAL, ASC);
   histGraphView->manage();

   _drawingAreaWidget = histGraphView->baseWidget();

   XtAddCallback(histGraphView->baseWidget(),
		 XmNinputCallback, 
		 &CmdInterface::executeCmdCallback,
		 (XtPointer) this );
}
