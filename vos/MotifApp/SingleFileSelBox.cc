////////////////////////////////////////////////////////////////////////
// SingleFileSelBox.cc: Component that implements a file selection box.
////////////////////////////////////////////////////////////////////////
#include "SingleFileSelBox.h"
#include "HelpBrowser.h"
#include <Xm/FileSB.h>

////////////////////////////////////////////////////////////////////////
// Since the Motif FileSelectionBox provides OK/Cancel buttons for us,
// we do the dialog stuff a little differently here.  When OK is hit, we
// explicitly execute the interface's Cmd object (rather than using a
// deferred list).  Also, the popdown action of OK/Cancel is done by
// calling mainWindow->unmanage().  It is assumed that mainWindow is a
// subclass of MainWindow that contains this object, but it can in reality
// be any UIComponent (NULL is okay too).
////////////////////////////////////////////////////////////////////////

SingleFileSelBox::SingleFileSelBox(Widget parent,
				Cmd *loadFileCmd,
				UIComponent *mainWindow)
				// = NULL
		: CmdInterface(loadFileCmd)
{
    _mainWindow = mainWindow;
    _loadFileCmd = loadFileCmd;

    _w = XtVaCreateWidget(_name, 
	xmFileSelectionBoxWidgetClass, parent,
	NULL);
    installDestroyHandler();

    XtAddCallback(_w, XmNokCallback,
		&SingleFileSelBox::okButtonCallback, (XtPointer) this);

    XtAddCallback(_w, XmNcancelCallback,
		&SingleFileSelBox::cancelButtonCallback, (XtPointer)this);

    XtAddCallback(_w, XmNhelpCallback,
		&SingleFileSelBox::helpButtonCallback, (XtPointer)this);
}

////////////////////////////////////////////////////////////////////////
// Returns the path of the given file name (filename) and stores it
// in variable "dir". "filename" contains file name and path before
// executing this function. After executing "filename" contains
// only the file name without path information. "dir" must be long
// enough to store the path string, no memory allocation will be
// performed.
////////////////////////////////////////////////////////////////////////
void SingleFileSelBox::getPath(char *filename, char *dir)
{
    char *value;
    int lenPath, lenFileAndPath;

    strcpy(dir, filename);
    lenFileAndPath = strlen(filename);

#ifdef __VMS					// VMS version
    value = strrchr(filename,':');
    if (value != NULL)
        strcpy(filename,value+1);
    value = strrchr(filename,']');
    if (value != NULL)
        strcpy(filename,value+1);
#else						// UNIX version
    value = strrchr(filename,'/');
    if (value != NULL)
        strcpy(filename,value+1);
#endif

    lenPath = lenFileAndPath - strlen(filename);
    dir[lenPath] = '\0';
}

////////////////////////////////////////////////////////////////////////
// Sets a file name with path for the selection box.
// The resources XmNdirectory and XmNdirSpec will be set by this
// function by using the given value, which contains the file name
// with path information.
////////////////////////////////////////////////////////////////////////
void SingleFileSelBox::setValue(CmdValue value)
{
    XmString strFile;
    XmString strPath;
    char *file;
    char *dir;

    file = (char *)value;
    strFile = XmStringCreateSimple(file);
    dir = new char[strlen(file)+1];
    getPath(file, dir);
    strPath = XmStringCreateSimple(dir);

    if (_w) {
        XtVaSetValues(_w, XmNdirectory, strPath, NULL);
	// Note: dirSpec is full path, not just name
        XtVaSetValues(_w, XmNdirSpec, strFile, NULL);
    }

    XmStringFree(strFile);
    XmStringFree(strPath);
    delete[] dir;
}

////////////////////////////////////////////////////////////////////////
// Callback called when the OK button is hit.
////////////////////////////////////////////////////////////////////////

void SingleFileSelBox::okButtonCallback(Widget w, XtPointer clientData,
					    XtPointer callData)
{
    SingleFileSelBox *obj = (SingleFileSelBox *)clientData;

    obj->okButton(w, callData);
}

void SingleFileSelBox::okButton(Widget, XtPointer callData)
{
    XmFileSelectionBoxCallbackStruct *cbs =
        (XmFileSelectionBoxCallbackStruct *) callData;

    // Get the string typed in char* format
    char *file;
    if (!XmStringGetLtoR (cbs->value, XmFONTLIST_DEFAULT_TAG, &file) )
        return;

    // Execute the command passing the string as a CmdValue
    runCmd (file);

    if (_mainWindow)
         _mainWindow->unmanage();
}

////////////////////////////////////////////////////////////////////////
// Callback called when the Cancel button is hit.
////////////////////////////////////////////////////////////////////////

void SingleFileSelBox::cancelButtonCallback(Widget w, XtPointer clientData, 
					XtPointer callData)
{
    SingleFileSelBox *obj = (SingleFileSelBox *)clientData;

    obj->cancelButton(w, callData);
}

void SingleFileSelBox::cancelButton(Widget, XtPointer)
{
    if (_mainWindow)
      _mainWindow->unmanage();
}

////////////////////////////////////////////////////////////////////////
// Callback called when the Help button is hit.
////////////////////////////////////////////////////////////////////////

void SingleFileSelBox::helpButtonCallback(Widget w, XtPointer clientData,
					XtPointer callData)
{
    SingleFileSelBox *obj = (SingleFileSelBox *)clientData;

    obj->helpButton(w, callData);
}

void SingleFileSelBox::helpButton(Widget w, XtPointer)
{
    theHelpBrowser->run(XmFileSelectionBoxGetChild(w, XmDIALOG_HELP_BUTTON));
}
