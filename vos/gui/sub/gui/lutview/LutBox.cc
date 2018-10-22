////////////////////////////////////////////////////////////////////
// LutBox.cc
///////////////////////////////////////////////////////////////////
#include "LutBox.h"
#include "LutGraphView.h"
#include "LutHorAxisView.h"
#include "LutVerAxisView.h"
#include "Lut.h"
#include <Xm/Form.h>
#include <iostream>
using namespace std;
#include <stdlib.h>

LutBox::LutBox(Widget parent, 
		const char *name,
		Lut *lut1, Lut *lut2, Lut *lut3 ) : UIComponent(name)
{
	// Create a form to hold the other widgets
	_w = XtVaCreateWidget ( _name, 
				xmFormWidgetClass, parent, 
				NULL);
	installDestroyHandler();

	LutGraphView *lutGraphView = new LutGraphView (_w, "lutGraphView", lut1, lut2, lut3);
	LutAxisView *horAxisView = new LutHorAxisView (_w, "haxis", lut1, lut2, lut3);
	LutAxisView *verAxisView = new LutVerAxisView (_w, "vaxis", lut1, lut2, lut3); 

	XtVaSetValues   ( verAxisView->baseWidget(),
			XmNtopAttachment,       XmATTACH_FORM,
			XmNleftAttachment,      XmATTACH_FORM,
			XmNrightAttachment,     XmATTACH_NONE,
			XmNbottomAttachment,    XmATTACH_WIDGET,
			XmNbottomWidget,        horAxisView->baseWidget(),
			NULL );

	XtVaSetValues   ( lutGraphView->baseWidget(),
                        XmNtopAttachment,	XmATTACH_FORM,
                        XmNleftAttachment, 	XmATTACH_WIDGET,
			XmNleftWidget,		verAxisView->baseWidget(),
                        XmNrightAttachment,	XmATTACH_FORM,
                        XmNbottomAttachment,	XmATTACH_WIDGET,
			XmNbottomWidget,	horAxisView->baseWidget(),
                        NULL );

	XtVaSetValues   ( horAxisView->baseWidget(),
			XmNtopAttachment,     	XmATTACH_NONE,
			XmNleftAttachment,    	XmATTACH_FORM,
			XmNrightAttachment,   	XmATTACH_FORM,
			XmNbottomAttachment,  	XmATTACH_FORM,
			XmNleftOffset,		40,
			NULL );


	lutGraphView->manage();
	horAxisView->manage();
	verAxisView->manage();
}
