///////////////////////////////////////////////////////
// TpImageReference
///////////////////////////////////////////////////////
#include "TpImageReference.h"
#include "ButtonInterface.h"
#include "TpDefs.h"
#include <Xm/RowColumn.h>
#include <Xm/Label.h>
#include <stdio.h>

TpImageReference::TpImageReference(Widget parent, const char *name)
	: UIComponent(name)
{
    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent,
		XmNorientation, XmVERTICAL,
		XmNnumColumns, TP_MAX_IMAGES,
		XmNpacking, XmPACK_COLUMN,
		XmNspacing, 1,
		NULL);
    installDestroyHandler();

    _fgLoaded = sdup("blue");
    _bgVisible = sdup("maroon");
    XtVaGetValues(_w,
                  XmNforeground, &_fg,
                  XmNbackground, &_bg,
                  NULL);

    _labels1 = new Widget [TP_MAX_IMAGES];
    _labels2 = new Widget [TP_MAX_IMAGES];

    for (int i = 0; i < TP_MAX_IMAGES; i++) {
	char buf[3];
	sprintf(buf, "%d", i+1);
//	_labels1[i] = XtVaCreateManagedWidget(buf, 
	_labels1[i] = XtVaCreateWidget(buf, 
			xmLabelWidgetClass, _w, 
			NULL);
//	_labels2[i] = XtVaCreateManagedWidget(buf,
//			xmLabelWidgetClass, _w,
//			NULL);
    }
}

TpImageReference::~TpImageReference()
{
    // Empty
}

// Visible, Invisible, Referenced

void TpImageReference::setAllToInvisible()
{
    for (int i = 0; i < TP_MAX_IMAGES; i++)
	setToInvisible(_labels1[i]);
}

void TpImageReference::setToVisible(int i)
{
    setToVisible(_labels1[i]);
}

void TpImageReference::setReferencedImage(int)
{
    // setTo
}

void TpImageReference::setToVisible(Widget w)
{
    XtVaSetValues(w, XtVaTypedArg, 
		  XmNbackground, XmRString,
		  _bgVisible, (strlen(_bgVisible) + 1),
		  NULL);
}

void TpImageReference::setToInvisible(Widget w)
{
    XtVaSetValues(w, XmNbackground, _bg, NULL);
}

void TpImageReference::indicateLoadedImage(int i)
{
    indicateLoadedImage(_labels1[i]);
}

void TpImageReference::indicateLoadedImage(Widget w)
{
    XtVaSetValues(w, XtVaTypedArg, 
		  XmNforeground, XmRString, 
		  "green", (strlen("green") + 1),
		  NULL);
    XtVaGetValues(_w,
                  XmNforeground, &_fg,
                  XmNbackground, &_bg,
                  NULL);
    XtManageChild(w);

}

