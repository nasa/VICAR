///////////////////////////////////////////////////////
// TpAutofindResults
///////////////////////////////////////////////////////
#include "TpAutofindRes.h"
#include "TpMatchManager.h"
#include "KeyinView.h"
#include <Xm/RowColumn.h>
#include <stdio.h>

TpAutofindResults::TpAutofindResults(Widget parent, const char *name, 
				     TpMatchManager *mm)
	: UIComponent(name)
{
    _matchManager = mm;

    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent,
		XmNorientation, XmVERTICAL,
		XmNnumColumns, 2,
		XmNpacking, XmPACK_COLUMN,
		NULL);
    installDestroyHandler();

    for (int i = 0; i < 6; i++) {
	char buf[16];
	sprintf(buf, "affPar%d", i);
	_labels[i] = new KeyinView(_w, buf);
	_labels[i]->manage();
    }
}

TpAutofindResults::~TpAutofindResults()
{
    // Empty
}

void TpAutofindResults::setValues(double a[6])
{
    for (int i = 0; i < 6; i++) {
	char buf[16];
	sprintf(buf, "%f", a[i]);
	_labels[i]->setFieldValue(buf);
    }
}
