//////////////////////////////////////////////////////////////
// LutBtnInterface.cc: A "push button" interface to a Cmd object
//////////////////////////////////////////////////////////////
#include "LutBtnInterface.h"
#include "Lut.h"
#include "LutGraphView.h"

LutBtnInterface::LutBtnInterface ( Widget parent, Cmd *cmd, 
		Lut *lutR, Lut *lutG, Lut *lutB ) 
	: SgDrawAreaInterface ( parent, cmd )
{
   LutGraphView *lutGraphView = new LutGraphView (_w, "lutGraphView",
                                        lutR, lutG, lutB);
   lutGraphView->manage();

   _drawingAreaWidget = lutGraphView->baseWidget();
    
   XtAddCallback ( lutGraphView->baseWidget(),
		   XmNinputCallback, 
		   &CmdInterface::executeCmdCallback,
		   (XtPointer) this );  
}
