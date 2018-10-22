////////////////////////////////////////////////////////////////
// BlinkControl.h
////////////////////////////////////////////////////////////////
#ifndef BLINKCONTROL_H
#define BLINKCONTROL_H
#include "MainWindow.h"
#include "BlinkRadioCmdBox.h"
#include "CmdList.h"

#define MAX_BLINKS 10

class ImageWindowBlink;

class BlinkControl : public MainWindow {
private:
	static XtResource _resources[];

protected:
	int _cur_blink;		// current image being displayed
	int _num_blinks;	// number of blinks
	ImageWindowBlink *_blinks[MAX_BLINKS];	// array of windows to blink

	// List of images
	CmdList *_blinkCmdList;
	Cmd *_blinkCmds[MAX_BLINKS];
	BlinkRadioCmdBox *_blinkCmdBox;

	Widget _intervalWidget;

	// Timer controls

	Boolean _timer_active;
	XtIntervalId _timer_id;

	// Application resources

	int _initial_num_blinks;

// UI management

	static void buttonPressedCallback(Widget, XtPointer, XtPointer);
	void buttonPressed();

	static void startBlinkingCallback(Widget, XtPointer, XtPointer);
	void startBlinking();

	static void stopBlinkingCallback(Widget, XtPointer, XtPointer);
	void stopBlinking();

// Timer stuff

	void fireTimer();
	void cancelTimer();
	static void timerCallback(XtPointer, XtIntervalId *);
	void timerExpired();

public:

	BlinkControl(const char *name);

	virtual Widget createWorkArea(Widget parent);

	virtual void addBlinkWindow(ImageWindowBlink *iwb);

	virtual void nextBlink();		// cycle to next image
	virtual void makeActiveWindow(int which);	// make window active

	virtual void setImageFilename(int which, char *name);
};

// Create pointer to single global instance of BlinkControl class

extern BlinkControl *theBlinkControl;

#endif
