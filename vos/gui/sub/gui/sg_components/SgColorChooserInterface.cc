//////////////////////////////////////////////////////////////////////////////
// SgColorChooserInterface.h:  A command interface that allows user to select 
// a color by either typing the value or using color chooser.  It then 
// executes command with value passed as a string (either standard X color
// name, like 'red', or hex value in the form #RRGGBB.
//////////////////////////////////////////////////////////////////////////////
#include "SgColorChooserInterface.h"
#include "ColorChooser.h"
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include <Xm/RowColumn.h>
#include <stdio.h>

static String resources[] = {
    (char *)"*button.labelString: Select Color",
    NULL,
};

SgColorChooserInterface::SgColorChooserInterface(Widget parent, Cmd *cmd)
	: CmdInterface(cmd)
{
    _oldValue = NULL;

    setDefaultResources(parent, resources);

    _w = XtVaCreateWidget(_name, xmRowColumnWidgetClass, parent, 
			  XmNorientation, XmHORIZONTAL,
			  NULL);
    installDestroyHandler();

    _text = XtVaCreateManagedWidget("text", xmTextFieldWidgetClass, _w,
				    NULL);

    _button = XtVaCreateManagedWidget("button", xmPushButtonWidgetClass, _w, 
				      NULL);
    XtAddCallback(_button,
		  XmNactivateCallback,
		  &SgColorChooserInterface::pickColorCallback,
		  (XtPointer)this);
    _colorChooser = new ColorChooser(_w, "colorChooser");

    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.
    
    if ( _active )
	activate();     
    else
	deactivate();   

    XtAddCallback(_text, XmNlosingFocusCallback, 
		  &CmdInterface::executeCmdCallback,
		  (XtPointer)this);
    XtAddCallback(_text, XmNactivateCallback, 
		  &CmdInterface::executeCmdCallback,
		  (XtPointer)this);
}

void SgColorChooserInterface::pickColorCallback(Widget, XtPointer clientData, 
						XtPointer)
{
    SgColorChooserInterface *obj = (SgColorChooserInterface *)clientData;
    ColorChooser *colorChooser = obj->_colorChooser;
    
    // Make color chooser to post a dialog and register a callback to return 
    // the results

    colorChooser->pickColor(&SgColorChooserInterface::colorSelectedCallback, 
			    NULL, clientData);
}

void SgColorChooserInterface::colorSelectedCallback(int red, int green, 
						    int blue,
						    void *clientData)
{
    SgColorChooserInterface *obj = (SgColorChooserInterface *)clientData;

    unsigned int c = (red << 16) | (green << 8) | blue;
    char *buf = new char[16];
    sprintf(buf, "#%06x", c);
    XmTextFieldSetString(obj->_text, buf);
    obj->runCmd((CmdValue) buf);
}

////////////////////////////////////////////////////////////////

void SgColorChooserInterface::executeCmd(XtPointer)
{
    char *string;

    string = XmTextFieldGetString(_text);

    if (_oldValue && strcmp(string, _oldValue) == 0) {
        // Duplicate value, so don't execute the command again
        XtFree(string);
    }
    else {
        copyOldValue(string);
        runCmd((CmdValue) string);
    }
}

////////////////////////////////////////////////////////////////

void SgColorChooserInterface::setValue(CmdValue value)
{
    CmdInterface::setValue(value);	// Removes cmd from deferred list

    XmTextFieldSetString(_text, (char *)value);
    copyOldValue((char *)value);

    XColor xcolor;
    XParseColor(XtDisplay(_w), DefaultColormapOfScreen(XtScreen(_w)), 
		(char *)value, &xcolor);
    _colorChooser->setColor((unsigned)(xcolor.red >> 8), 
			    (unsigned)(xcolor.green >> 8), 
			    (unsigned)(xcolor.blue >> 8));
}

///////////////////////////////////////////////////////////////////
//  Allow execution of command after value is set.
///////////////////////////////////////////////////////////////////
void SgColorChooserInterface::setValueAndRun(CmdValue value)
{
    CmdInterface::setValue(value);	// Removes cmd from deferred list

    XmTextFieldSetString(_text, (char *)value);
    executeCmd();
}

////////////////////////////////////////////////////////////////

void SgColorChooserInterface::copyOldValue(char *string)
{
    if (_oldValue)
        delete _oldValue;
    if (string)
        _oldValue = strdup(string);
    else
        _oldValue = NULL;
}

