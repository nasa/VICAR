//////////////////////////////////////////////////////////////////////////////
// TpMatchModeResultsDialog.h: Dialog containing matchMode results values.
//////////////////////////////////////////////////////////////////////////////
#include "TpMatchModeResultsDialog.h"
#include "TpMatchManager.h"
#include "TpMatchModeRes.h"
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <stdio.h>

TpMatchModeResultsDialog::TpMatchModeResultsDialog(const char *name, 
						 TpMatchManager *mm)
    : CustomDialog(name, Default, Visible, Visible, Visible, Visible)
{
    _matchManager = mm;
    _matchModeResults = NULL;
}

Widget TpMatchModeResultsDialog::createWorkArea(Widget parent)
{
    Widget rc = XtVaCreateWidget("workArea", 
				 xmRowColumnWidgetClass, parent, 
				 XmNorientation, XmVERTICAL,
				 XmNnumColumns, 1,
				 NULL);

    XtVaCreateManagedWidget("matchModeResultsLabel", 
			    xmLabelWidgetClass, rc, 
			    NULL);

    _matchModeResults = new TpMatchModeResults(rc, "matchModeResults",
					     _matchManager);
    _matchModeResults->manage();

    float a[5];
    for (int i = 0; i < 5; i++)
	a[i] = _matchManager->getMatchModeResult(i);

    _matchModeResults->setValues(a);

    return rc;
}

void TpMatchModeResultsDialog::setValues(float a[5])
{
    if (_matchModeResults)
	_matchModeResults->setValues(a);
}

Boolean TpMatchModeResultsDialog::isDumpToStdout()
{
    if (_matchModeResults)
	return _matchModeResults->isDumpToStdout();
    return False;
}

