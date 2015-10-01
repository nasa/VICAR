////////////////////////////////////////////////////////////////////////
// SingleFileSelWindow.cc: Component that creates a popup window containing 
// a file selection box.
////////////////////////////////////////////////////////////////////////
#include "SingleFileSelWindow.h"
#include "SingleFileSelBox.h"
#include "Cmd.h"
#include <Xm/Form.h>

SingleFileSelWindow::SingleFileSelWindow(const char *name, Cmd *loadFileCmd)
		: MainWindow(name)
{
    _loadFileCmd = loadFileCmd;
}

Widget SingleFileSelWindow::createWorkArea(Widget parent)
{
    // Tell the Shell not to destroy us
    XtVaSetValues(_w, XmNdeleteResponse, XmUNMAP, NULL);

    _form = XtVaCreateWidget(_name, xmFormWidgetClass, parent,
		NULL);

    _fileSelBox = new SingleFileSelBox(_form, _loadFileCmd, this);

    XtVaSetValues(_fileSelBox->baseWidget(),
		XmNtopAttachment,    XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment,   XmATTACH_FORM,
		XmNrightAttachment,  XmATTACH_FORM,
		NULL);
    _fileSelBox->manage();
    if (_loadFileCmd->getValue())	// start value (file name) exist
        _fileSelBox->setValue(_loadFileCmd->getValue());

    return _form;
}

