///////////////////////////////////////////////////////////
// SiHistMenuCmd.cc: Command to bring up the histogram display
//////////////////////////////////////////////////////////
#include "SiHistMenuCmd.h"
#include "SiHistWindow.h"
#include "SiHistogram.h"

SiHistMenuCmd::SiHistMenuCmd ( const char *name, const char* titleName,
		int active, 
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB ) 
	: NoUndoCmd ( name, active )
{
    _created = FALSE;

    _histR = histR;
    _histG = histG;
    _histB = histB;

    _title = titleName;
}

void SiHistMenuCmd::doit()
{
    // Execute the following upon button activation.
    // Create histogram Window only once and then display it.
    // Set the Close button to the UNMAP state so the Window
    // is only unmanaged when it is closed and can therefore
    // be managed again when the user hits the command button.

    if (!_created) {                 // Do only once
	_histWindow = new SiHistWindow ( _title, _histR, _histG, _histB );
	_histWindow->initialize();
	XtVaSetValues ( _histWindow->baseWidget(), 
			XmNdeleteResponse, XmUNMAP, 
			NULL);

	_created = TRUE;
    }
    _histWindow->manage();
}      
