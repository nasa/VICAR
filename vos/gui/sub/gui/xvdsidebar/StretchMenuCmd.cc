///////////////////////////////////////////////////////////
// StretchMenuCmd.cc: Command class to execute the STRETCH
//                    command from the Image Menu
//////////////////////////////////////////////////////////
#include "StretchMenuCmd.h"
#include <Xm/Xm.h>
#include <iostream>
using namespace std;

////////////////////////////////////////////////////////////
// StretchMenuCmd:
////////////////////////////////////////////////////////////
StretchMenuCmd::StretchMenuCmd ( char *name, int active, 
	Lut *lutR, Lut *lutG, Lut *lutB ) : NoUndoCmd ( name, active )
{
    _created = FALSE;
    _lutR = lutR;
    _lutG = lutG;
    _lutB = lutB;
}

////////////////////////////////////////////////////////////
// doit:
////////////////////////////////////////////////////////////
void StretchMenuCmd::doit()
{
//  deactivate();
  if (!_created) {
//    _histWindow = new HistWindow( _histR, _histG, _histB, "HistWindow" );
//    _histWindow->initialize();
//    XtVaSetValues(_histWindow->baseWidget(), 
//		XmNdeleteResponse, XmUNMAP, 
//		NULL);

    _created = TRUE;
  }
//  _histWindow->manage();
}      
