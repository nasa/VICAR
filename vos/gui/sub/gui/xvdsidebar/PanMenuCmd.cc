///////////////////////////////////////////////////////////
// PanMenuCmd.cc: Example, dummy command class
//////////////////////////////////////////////////////////
#include "PanMenuCmd.h"
#include "Lut.h"

PanMenuCmd::PanMenuCmd ( const char *name, int active, 
	ImageData *imageData, Widget widget,
	Lut *rlut, Lut *glut, Lut *blut, Lut *rplut, Lut *gplut, Lut *bplut)
	: NoUndoCmd ( name, active )
{
    _imageData = imageData;
    _imageViewWidget = widget;
    _created = FALSE;
    _rlut = rlut;
    _glut = glut;
    _blut = blut;
    _rplut = rplut;
    _gplut = gplut;
    _bplut = bplut;
}

void PanMenuCmd::doit()
{
  if (!_created) {
    _panToolWindow = new PanToolWindow("PanToolWindow", _imageData,
		_imageViewWidget,
		_rlut, _glut, _blut, _rplut, _gplut, _bplut );
    _panToolWindow->initialize();
    XtVaSetValues(_panToolWindow->baseWidget(), 
		XmNdeleteResponse, XmUNMAP, 
		NULL);
    _created = TRUE;
  }
  _panToolWindow->manage();
}

