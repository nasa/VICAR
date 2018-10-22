///////////////////////////////////////////////////////////
// HistMenuCmd.cc: Example, dummy command class
//////////////////////////////////////////////////////////
#include "HistMenuCmd.h"
#include "HistWindow.h"
#include "Histogram.h"
#include <iostream>
using namespace std;

HistMenuCmd::HistMenuCmd ( char *name, char* titleName, int active, 
	Histogram *histR, Histogram *histG, Histogram *histB ) : 
	NoUndoCmd ( name, active )
{
    //  Save the histogram pointers and set value to create
    //  histogram once.

    _created = FALSE;
    _histR = histR;
    _histG = histG;
    _histB = histB;
    _title = titleName;
}

void HistMenuCmd::doit()
{
  // Execute the following upon button activation.
  // Create histogram Window only once and then display it.
  //  Set the Close button to the UNMAP state so the Window
  //  is only unmanaged when it is closed and can therefore
  //  be managed again when the user hits the command button.

  if (!_created) {                 // Do only once
    _histWindow = new HistWindow ( _histR, _histG, _histB, _title);
    _histWindow->initialize();
    XtVaSetValues(_histWindow->baseWidget(), 
		XmNdeleteResponse, XmUNMAP, 
		NULL);

    _created = TRUE;
  }
  _histWindow->manage();
}      
