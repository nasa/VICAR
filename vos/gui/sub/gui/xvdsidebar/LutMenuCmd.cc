///////////////////////////////////////////////////////////
// LutMenuCmd.cc: Lookup table (LUT) command class for
//                Lookup Table View command from Image Menu
//////////////////////////////////////////////////////////
#include "LutMenuCmd.h"
#include "LutWindow.h"

//////////////////////////////////////////////////////////////
// LutMenuCmd: Saves pointers to the images histgram and
//         lookup table.  Might not need the histogram
//         pointers.
//////////////////////////////////////////////////////////////
LutMenuCmd::LutMenuCmd ( const char *name, int active, 
                Lut *lutR, Lut *lutG, Lut *lutB ) : NoUndoCmd ( name, active )
{
    _created = FALSE;

    _lutR = lutR;
    _lutG = lutG;
    _lutB = lutB;

}

//////////////////////////////////////////////////////////////
// doit: Execute upon command activation.
//        Create lookup table window just once, set the Close
//        button to the UNMAP state and display the window by
//        managing it.
//////////////////////////////////////////////////////////////
void LutMenuCmd::doit()
{
  if (!_created) {
    _lutWindow = new LutWindow( "LutWindow", _lutR, _lutG, _lutB );
    _lutWindow->initialize();
    _created = TRUE;
    XtVaSetValues(_lutWindow->baseWidget(), 
		XmNdeleteResponse, XmUNMAP, 
		NULL);
  }
  _lutWindow->manage();
}      



