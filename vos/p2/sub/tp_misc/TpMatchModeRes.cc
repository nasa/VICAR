///////////////////////////////////////////////////////
// TpMatchModeResults
///////////////////////////////////////////////////////
#include "TpMatchModeRes.h"
#include "TpMatchManager.h"
#include "KeyinView.h"
#include <Xm/RowColumn.h>
#include <Xm/ToggleB.h>
#include <stdio.h>

TpMatchModeResults::TpMatchModeResults(Widget parent, const char *name, 
				       TpMatchManager *mm)
	: UIComponent(name)
{
    _matchManager = mm;

    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent,
		XmNorientation, XmVERTICAL,
		XmNnumColumns, 1,
		XmNpacking, XmPACK_COLUMN,
		NULL);
    installDestroyHandler();

    for (int i = 0; i < 5; i++) {
	char buf[8];
	sprintf(buf, "erg%d", i);
	_labels[i] = new KeyinView(_w, buf);
	_labels[i]->manage();
    }

    // This is cheating... should use a CheckBoxInterface.  But we just check
    // this for printing, we don't really execute a command, and it's easier
    // this way...

    _dumpStdout = XtVaCreateManagedWidget("dump stdout",
			xmToggleButtonWidgetClass, _w,
			XmNindicatorType, XmN_OF_MANY,
			XmNset, False,
			NULL);

}

TpMatchModeResults::~TpMatchModeResults()
{
    // Empty
}

void TpMatchModeResults::setValues(float a[5])
{
    for (int i = 0; i < 5; i++) {
	char buf[32];
	sprintf(buf, "%f", a[i]);
	_labels[i]->setFieldValue(buf);
    }
}

Boolean TpMatchModeResults::isDumpToStdout()
{
    if (XmToggleButtonGetState(_dumpStdout))
	return True;
    return False;
}

