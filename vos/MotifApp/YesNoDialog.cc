///////////////////////////////////////////////////////////////
// YesNoDialog.h: Motif Question dialog that also contains No
// button.  Note that this dialog is not cached, it is created 
// at every request and destroys itself when user presses any of 
// the buttons.
////////////////////////////////////////////////////////////////
#include "YesNoDialog.h"
#include "Cmd.h"
#include "Application.h"
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <stdio.h>

String YesNoDialog::_defaults[] = {
    (char *)"*okLabelString:	Yes",
    (char *)"*no.labelString:	No",
    NULL,
};

#define DEFAULTQUESTION "Do you want to save changes?"

YesNoDialog::YesNoDialog (const char *name, void *clientData,
		const char *question,
		DialogCallback yes, 
		DialogCallback no, 
		DialogCallback cancel, 
		DialogCallback help)
	: UIComponent (name)
{
    _clientData = clientData;
    _yes = yes;
    _no = no;
    _cancel = cancel;
    _help = help;

    if (question && strlen(question))
	_question = strdup(question);
    else 
	_question = strdup(DEFAULTQUESTION);

    XmString msgStr = XmStringCreateLocalized (_question);

    Arg args[5];
    int n = 0;
    XtSetArg (args[n], XmNautoUnmanage, False); n++;
    XtSetArg (args[n], XmNmessageString, msgStr); n++;
    setDefaultResources ( theApplication->baseWidget(), _defaults );
    Widget dialog = XmCreateQuestionDialog (theApplication->baseWidget(), 
					_name, args, n);
    XtVaSetValues (dialog, 
			XmNdialogStyle, XmDIALOG_FULL_APPLICATION_MODAL,
			NULL);
    if (help == NULL)
	XtUnmanageChild (XmMessageBoxGetChild (dialog, XmDIALOG_HELP_BUTTON));

    _w = XtParent (dialog);
    installDestroyHandler();
    XtAddCallback (dialog, XmNokCallback, 
			YesNoDialog::yesCallback, 
			(XtPointer) this);
    XtAddCallback (dialog, XmNcancelCallback, 
			YesNoDialog::cancelCallback,
			(XtPointer) this);

    XmStringFree (msgStr);
    
    Widget noButton = XtVaCreateManagedWidget ("no", 
			xmPushButtonWidgetClass, dialog, 
			NULL);
    XtAddCallback (noButton, XmNactivateCallback,
			YesNoDialog::noCallback, 
			(XtPointer) this);

    XtManageChild(dialog);
    manage();
}

YesNoDialog::~YesNoDialog()
{
    XtPopdown (_w);
}

void YesNoDialog::manage()
{
    UIComponent::manage();
    XtPopup (_w, XtGrabNone);
}

void YesNoDialog::yesCallback (Widget, XtPointer clientData, 
				XtPointer)
{
    YesNoDialog *obj = (YesNoDialog *) clientData;

    // Call the base class execute() member function 
    // to do all the usual processing of the command

    obj->yes();

    delete obj;
}

void YesNoDialog::yes()
{
    (*_yes)(_clientData);
}

void YesNoDialog::noCallback (Widget, XtPointer clientData, 
				XtPointer)
{
    YesNoDialog *obj = (YesNoDialog *) clientData;

    obj->no();

    delete obj;
}

void YesNoDialog::no()
{
    (*_no)(_clientData);
}

void YesNoDialog::cancelCallback (Widget, XtPointer clientData,
				XtPointer)
{
    YesNoDialog *obj = (YesNoDialog *) clientData;

    delete obj;
}

void YesNoDialog::helpCallback (Widget, XtPointer clientData,
                                XtPointer)
{
    YesNoDialog *obj = (YesNoDialog *) clientData;

    obj->help();
}   

void YesNoDialog::help()
{
    (*_help)(_clientData);
}
