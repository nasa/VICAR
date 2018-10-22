///////////////////////////////////////////////////////////////
// BlinkControl.cc
///////////////////////////////////////////////////////////////


#ifdef ENABLE_SAGE
#include "SageApplication.h"
#else
#include "Application.h"
#endif
#include "BlinkControl.h"
#include "BlinkImageCmd.h"
#include "ImageWindowBlink.h"
#include "BasicImageView.h"
#include "ImageToBlinkGlue.h"
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/Scale.h>
#include <stdio.h>

BlinkControl *theBlinkControl = NULL;

XtResource BlinkControl::_resources[] = {
  {
    (char *)"numBlinks",
    (char *)"NumBlinks",
    XmRInt,
    sizeof(int),
    XtOffsetOf(BlinkControl, _initial_num_blinks),
    XmRImmediate,
    (XtPointer) 2,
  }
};

///////////////////////////////////////////////////////////////

BlinkControl::BlinkControl(const char *name) : MainWindow(name)
{
    _cur_blink = 0;
    _num_blinks = 0;
    theBlinkControl = this;

}

///////////////////////////////////////////////////////////////
// Create the main window, including most commands
///////////////////////////////////////////////////////////////

Widget BlinkControl::createWorkArea( Widget parent)
{
    int i;
    // Get application resources
    XtGetApplicationResources(theApplication->baseWidget(), (XtPointer)this,
			_resources, XtNumber(_resources), NULL, 0);

    Widget form = XtCreateManagedWidget( _name, 
				 xmFormWidgetClass, parent, 
				 NULL, 0);

    // Set up the CmdList before the IWB's so the filename updates can happen
    // properly

    _blinkCmdList = new CmdList();
    for (i=0; i < _initial_num_blinks; i++) {
	char name[100];
	sprintf(name, "Image %d", i);
	_blinkCmds[i] = new BlinkImageCmd(strdup(name), True,
						_blinkCmdList, this, i);
    }
    _blinkCmdBox = new BlinkRadioCmdBox(form, "image list", _blinkCmdList);

    for (i=0; i < _initial_num_blinks; i++) {
	ImageWindowBlink *iwb = new ImageWindowBlink("XVd");
	iwb->initialize();
	iwb->setExitOnDestroy();
	iwb->manage();
    }

    Widget lbl = XtVaCreateManagedWidget("List of Images",
		xmLabelWidgetClass, form,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		NULL);

    XtVaSetValues(_blinkCmdBox->baseWidget(),
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, lbl,
		NULL);
    _blinkCmdBox->manage();

    Widget button = XtVaCreateManagedWidget("push me", xmPushButtonWidgetClass,
			form,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, _blinkCmdBox->baseWidget(),
			NULL);
    XtAddCallback(button, XmNactivateCallback,
			&BlinkControl::buttonPressedCallback,
			(XtPointer)this);

    Widget start = XtVaCreateManagedWidget("start", xmPushButtonWidgetClass,
			form,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, button,
			XmNleftAttachment, XmATTACH_FORM,
			NULL);
    XtAddCallback(start, XmNactivateCallback,
			&BlinkControl::startBlinkingCallback,
			(XtPointer)this);

    Widget stop = XtVaCreateManagedWidget("stop", xmPushButtonWidgetClass,
			form,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, button,
			XmNleftAttachment, XmATTACH_WIDGET,
			XmNleftWidget, start,
			NULL);
    XtAddCallback(stop, XmNactivateCallback,
			&BlinkControl::stopBlinkingCallback,
			(XtPointer)this);

    Widget lbl2 = XtVaCreateManagedWidget("interval label", xmLabelWidgetClass,
			form,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, stop,
			XmNleftAttachment, XmATTACH_FORM,
			NULL);

    _intervalWidget = XtVaCreateManagedWidget("interval", xmScaleWidgetClass,
			form,
			XmNminimum, 100,
			XmNmaximum, 3000,
			XmNorientation, XmHORIZONTAL,
			XmNshowValue, True,
			XmNvalue, 500,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, lbl2,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			NULL);

    return form;

}

///////////////////////////////////////////////////////////////
// Control which window is active
///////////////////////////////////////////////////////////////

void BlinkControl::nextBlink()
{
    int next = _cur_blink+1;
    if (next >= _num_blinks)
	next = 0;

    makeActiveWindow(next);
}

void BlinkControl::makeActiveWindow(int which)
{
    if (which >= _num_blinks) which = _num_blinks-1;
    if (which < 0) which = 0;

    if (_cur_blink == which)
	return;				// avoid recursion

    _cur_blink = which;

    Widget w = _blinks[_cur_blink]->baseWidget();
    if (XtIsRealized(w)) {
        XRaiseWindow(XtDisplay(w), XtWindow(w));

	// Activate this window's colormap, just to make sure.
	// This will NOT WORK with hardware overlays!  (one reason they're
	// disabled in the resource file).

	Widget list[2];
	list[0] = ImageWindowBlink::getShell();
	list[1] = _blinks[_cur_blink]->getImageView()->getWidget();
	XtSetWMColormapWindows(list[0], list, 2);
    }

    // Notify the list of the new current widget

    _blinkCmds[_cur_blink]->execute((CmdValue)True);

}

///////////////////////////////////////////////////////////////

void BlinkControl::addBlinkWindow(ImageWindowBlink *iwb)
{
    if (_num_blinks >= (int)(sizeof(_blinks)/sizeof(ImageWindowBlink *))) {
	printf("Too many blinks!\n");
	return;
    }
    _blinks[_num_blinks] = iwb;
    new ImageToBlinkGlue(iwb->getImageData(), this, _num_blinks);
    _num_blinks++;

}

///////////////////////////////////////////////////////////////

void BlinkControl::setImageFilename(int which, char *name)
{
    XmString str = XmStringCreateSimple(name);
    Widget w = _blinkCmdBox->getInterface(which)->baseWidget();
    if (w != NULL)
        XtVaSetValues(w, XmNlabelString, str, NULL);
    XmStringFree(str);
}

///////////////////////////////////////////////////////////////
// Callbacks for the controls.  We really should use MotifApp
// Cmd's and such, but this is a quick hack.
///////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////////////
// Button to cycle to the next view
///////////////////////////////////////////////////////////////
void BlinkControl::buttonPressedCallback(Widget,
			XtPointer clientData, XtPointer callData)
{
    BlinkControl *obj = (BlinkControl *)clientData;
    obj->buttonPressed();
}

void BlinkControl::buttonPressed()
{
    nextBlink();
}

///////////////////////////////////////////////////////////////
// Start blinking button
///////////////////////////////////////////////////////////////
void BlinkControl::startBlinkingCallback(Widget,
			XtPointer clientData, XtPointer callData)
{
    BlinkControl *obj = (BlinkControl *)clientData;
    obj->startBlinking();
}

void BlinkControl::startBlinking()
{
    nextBlink();			// Start off with a blink
    fireTimer();
}

///////////////////////////////////////////////////////////////
// Stop blinking button
///////////////////////////////////////////////////////////////
void BlinkControl::stopBlinkingCallback(Widget,
			XtPointer clientData, XtPointer callData)
{
    BlinkControl *obj = (BlinkControl *)clientData;
    obj->stopBlinking();
}

void BlinkControl::stopBlinking()
{
    cancelTimer();
}

///////////////////////////////////////////////////////////////
// Timer management
///////////////////////////////////////////////////////////////

void BlinkControl::fireTimer()
{
    unsigned long interval;
    int interval_int;

    if (_timer_active)
	cancelTimer();

    // Get the interval from the widget.  Since we look every time, there's
    // no need to bother with an event.

    XmScaleGetValue(_intervalWidget, &interval_int);
    interval = interval_int;

    _timer_id = XtAppAddTimeOut(theApplication->appContext(), interval,
		&BlinkControl::timerCallback, (XtPointer)this);
    _timer_active = True;
}

void BlinkControl::cancelTimer()
{
    if (_timer_active)
	XtRemoveTimeOut(_timer_id);
    _timer_active = False;
}

void BlinkControl::timerCallback(XtPointer clientData, XtIntervalId *)
{
    BlinkControl *obj = (BlinkControl *)clientData;
    obj->timerExpired();
}

void BlinkControl::timerExpired()
{
    _timer_active = False;
    nextBlink();			// Display the next image
    fireTimer();			// do it again
}


