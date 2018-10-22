////////////////////////////////////////////////////////////////
// PseudoColorView.cc
////////////////////////////////////////////////////////////////
#include "PseudoColorView.h"
#include "PseudoValue.h"
#include "ColorModel.h"

PseudoColorView::PseudoColorView( Widget parent, const char *name, 
	PseudoValue *pseudoValue) :  WedgeOverlayView(parent, name)
{
	_pseudoValue = pseudoValue;

	XtVaSetValues(_iw, 
		XvicNlutType, XvicPSEUDO_ONLY,
		NULL);
	XvicImageSetColorLUT(_iw, 
		_pseudoValue->getRedAsArray(),
		_pseudoValue->getGrnAsArray(),
		_pseudoValue->getBluAsArray());
}

///////////////////////////////////////////////////////////////
//      update
///////////////////////////////////////////////////////////////
void PseudoColorView::update (PseudoValue *value)
{
	_pseudoValue = value;
	XvicImageSetColorLUT(_iw,
                _pseudoValue->getRedAsArray(),
                _pseudoValue->getGrnAsArray(),
                _pseudoValue->getBluAsArray());
}
