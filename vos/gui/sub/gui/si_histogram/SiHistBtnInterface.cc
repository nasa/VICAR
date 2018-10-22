//////////////////////////////////////////////////////////////
// SiHistBtnInterface.cc: A "push button" interface to a Cmd object.
///////////////////////////////////////////////////////////////
#include "SiHistBtnInterface.h"
#include "SiHistogram.h"
#include "SiHistGraph.h"

SiHistBtnInterface::SiHistBtnInterface ( Widget parent, Cmd *cmd,
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB )
	: SgDrawAreaInterface ( parent, cmd )
{
   SiHistGraph *histGraphView = new SiHistGraph ( _w, "histGraphView",
		histR, histG, histB );
   histGraphView->manage();

   setDrawingAreaWidget ( histGraphView->baseWidget() );
}

