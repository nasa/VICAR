////////////////////////////////////////////////////////////////////////
// Compenent that creates a popup window containing a file selection box.
////////////////////////////////////////////////////////////////////////
#include "FileSelWindow.h"
#ifdef ENABLE_SAGE
#include "SptParamMultiFileSel.h"
#else
#include "FileSelBox.h"
#endif
#include <Xm/Form.h>

FileSelWindow::FileSelWindow(const char *name, Cmd *loadFileCmd)
		: MainWindow(name)
{
   _loadFileCmd = loadFileCmd;
}

Widget FileSelWindow::createWorkArea(Widget parent)
{
   // Tell the Shell not to destroy us
   XtVaSetValues(_w, XmNdeleteResponse, XmUNMAP, NULL);

   _form = XtVaCreateWidget(_name, xmFormWidgetClass, parent,
		NULL);

#ifdef ENABLE_SAGE
   _fileSelBox = SptParamMultiFileSel::create(_form, "filename", _loadFileCmd,
						NULL_ID, this);
#else
   _fileSelBox = new FileSelBox(_form, "FileSelBox", _loadFileCmd,this);
#endif

   XtVaSetValues(_fileSelBox->baseWidget(),
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		NULL);
   _fileSelBox->manage();

   return _form;
}

