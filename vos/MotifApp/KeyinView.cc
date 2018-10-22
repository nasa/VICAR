///////////////////////////////////////////////////////
// KeyinView.C:
////////////////////////////////////////////////////////
#include "KeyinView.h"
#include <Xm/Form.h>
#include <Xm/TextF.h>
#include <Xm/Label.h>
#include <Xm/Frame.h>

KeyinView::KeyinView ( Widget parent, 
		     const char *name ) : UIComponent (name)
{

	// A RowColumn widget manages  a 1 by 2 grid of 
	// labels and text widgets

	_w  = XtVaCreateWidget ( _name,
                        xmFormWidgetClass, 	parent,
			NULL );
	installDestroyHandler ();

	Widget frame  = XtVaCreateManagedWidget ( "frame",
			xmFrameWidgetClass, _w,
			XmNtopAttachment,       XmATTACH_FORM,
                        XmNbottomAttachment,    XmATTACH_FORM,
                        XmNrightAttachment,     XmATTACH_FORM,
			NULL );
	_field = XtVaCreateManagedWidget ( "field",
			xmTextFieldWidgetClass, frame,
			NULL );

        _label = XtVaCreateManagedWidget ( "label",
                        xmLabelWidgetClass, 	_w,
                        XmNtopAttachment,       XmATTACH_FORM,
			XmNrightAttachment,     XmATTACH_WIDGET,
			XmNrightWidget, 	frame,
                        XmNbottomAttachment,    XmATTACH_FORM,
                        NULL );
}

void KeyinView::installCallback(void (*fun)(Widget, XtPointer, XtPointer),
				XtPointer obj)
			// fun = updateCallback, obj = (XtPointer)NULL
{
	if (obj == NULL)
		obj = (XtPointer)this;

        XtAddCallback ( _field, XmNlosingFocusCallback, fun, obj );
        XtAddCallback ( _field, XmNactivateCallback, fun, obj );
}

void KeyinView::setFieldValue(char *text) 
{ 
	XmTextFieldSetString(_field, text); 
}

char* KeyinView::getFieldValue() 
{ 
	return XmTextFieldGetString(_field); 
}

void KeyinView::updateCallback ( Widget,
                                 XtPointer clientData,
                                 XtPointer callData )
{
	KeyinView *obj;
	obj = ( KeyinView * ) clientData;
	if (obj != NULL)
		obj->update(callData);
}
