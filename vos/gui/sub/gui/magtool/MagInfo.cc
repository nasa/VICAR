////////////////////////////////////////////////////////////////
// MagInfo.cc
///////////////////////////////////////////////////////////////
#include "MagInfo.h"
#include "KeyinView.h"
#include <Xm/RowColumn.h>
#include <Xm/Frame.h>
#include <Xm/LabelG.h>
#include <stdio.h>

String MagInfo::_defaults[] = {
    (char *)"*frameLabel.labelString: 		Magnifier", 
    (char *)"*rc*shadowThickness: 		0",
    (char *)"*field.editable:			False",
    (char *)"*field.cursorPositionVisible: 	False",
    (char *)"*field.traversalOn: 		False",
    (char *)"*field.columns:			8",
    (char *)"*magSize.label.labelString:	Size:",
    (char *)"*magRatio.label.labelString:	Ratio:",
    NULL,
};

MagInfo::MagInfo(Widget parent, const char *name) : UIComponent(name)
{
    setDefaultResources ( parent, _defaults );

    _w = XtVaCreateWidget(_name,
		xmFrameWidgetClass, parent,
		NULL);
    XtVaCreateManagedWidget ("frameLabel", 
		xmLabelGadgetClass, _w, 
		XmNchildType, XmFRAME_TITLE_CHILD,
		XmNchildVerticalAlignment, XmALIGNMENT_CENTER,
		NULL );
    Widget rc = XtVaCreateManagedWidget("rc", 
		xmRowColumnWidgetClass, _w, 
		NULL);
    installDestroyHandler();

    _magRatio = new KeyinView(rc, "magRatio");
    _magSize = new KeyinView(rc, "magSize");

    _magRatio->manage();
    _magSize->manage();
}

MagInfo::~MagInfo()
{
    // Empty
}

void MagInfo::printSize(int w, int h)
{
    String string = new char[32];
    sprintf(string, "%dx%d", w, h);
    XtVaSetValues(_magSize->getField(), 
		XmNcolumns, strlen(string),
		NULL);
    _magSize->setFieldValue(string);
    delete [] string;
}

void MagInfo::printRatio(float ratioX, float ratioY)
{
    String string = new char[32];

    if (ratioX == ratioY)
	sprintf(string, "x%.1f", ratioX);
    else 
	sprintf(string, "x%.1fx%.1f", ratioX, ratioY);

    XtVaSetValues(_magRatio->getField(),
		XmNcolumns, strlen(string),
		NULL);
    _magRatio->setFieldValue(string);
    delete [] string;
}
