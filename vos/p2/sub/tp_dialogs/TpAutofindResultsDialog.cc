//////////////////////////////////////////////////////////////////////////////
// TpAutofindResultsDialog.h: Dialog containing autofind results values.
//////////////////////////////////////////////////////////////////////////////
#include "TpAutofindResultsDialog.h"
#include "TpMatchManager.h"
#include "TpAutofindRes.h"
#include <Xm/Label.h>
#include <Xm/RowColumn.h>
#include <stdio.h>

TpAutofindResultsDialog::TpAutofindResultsDialog(const char *name, 
						 TpMatchManager *mm)
    : CustomDialog(name, Default, Visible, Visible, Visible, Visible)
{
    _matchManager = mm;
    _autofindResults = NULL;
}

Widget TpAutofindResultsDialog::createWorkArea(Widget parent)
{
    Widget rc = XtVaCreateWidget("workArea", 
				 xmRowColumnWidgetClass, parent, 
				 XmNorientation, XmVERTICAL,
				 XmNnumColumns, 1,
				 NULL);

    XtVaCreateManagedWidget("autofindResultsLabel", 
			    xmLabelWidgetClass, rc, 
			    NULL);

    _autofindResults = new TpAutofindResults(rc, "autofindResults",
					     _matchManager);
    _autofindResults->manage();

    double a[6];
    for (int i = 0; i < 6; i++)
	a[i] = _matchManager->getAutofindResult(i);

    _autofindResults->setValues(a);

    return rc;
}

void TpAutofindResultsDialog::setValues(double a[6])
{
    if (_autofindResults)
	_autofindResults->setValues(a);
}
