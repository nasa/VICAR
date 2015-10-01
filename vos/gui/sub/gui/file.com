$!****************************************************************************
$!
$! Build proc for MIPL module file
$! VPACK Version 1.9, Monday, December 07, 2009, 15:57:23
$!
$! Execute by entering:		$ @file
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module file ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to file.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("file.imake") .nes. ""
$   then
$      vimake file
$      purge file.bld
$   else
$      if F$SEARCH("file.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake file
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @file.bld "STD"
$   else
$      @file.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create file.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack file.com -mixed -
	-s FileSelBox.cc FileSelWindow.cc FileTextInterface.cc LoadFileCmd.cc -
	   LoadMenuCmd.cc ReloadFileCmd.cc ImageToReloadGlue.cc -
	-i file.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create FileSelBox.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// Compenent that implements a file selection box.
////////////////////////////////////////////////////////////////////////

#include "FileSelBox.h"
#include "FileTextInterface.h"
#include "Cmd.h"
#include "HelpBrowser.h"
#include <Xm/FileSB.h>
#include <Xm/Text.h>
#include <Xm/PushB.h>

////////////////////////////////////////////////////////////////////////
// Since the Motif FileSelectionBox provides OK/Cancel buttons for us,
// we do the dialog stuff a little differently here.  When OK is hit, we
// explicitly execute the interface's Cmd object (rather than using a
// deferred list).  Also, the popdown action of OK/Cancel is done by
// calling mainWindow->unmanage().  It is assumed that mainWindow is a
// subclass of MainWindow that contains this object, but it can in reality
// be any UIComponent (NULL is okay too).
////////////////////////////////////////////////////////////////////////

FileSelBox::FileSelBox(Widget parent, const char *name, Cmd *loadFileCmd,
			UIComponent *mainWindow)
			// = NULL
		: UIComponent(name)
{
   _mainWindow = mainWindow;

   _w = XtVaCreateWidget(name, xmFileSelectionBoxWidgetClass, parent,
	NULL);
   installDestroyHandler();

   XtUnmanageChild(XmFileSelectionBoxGetChild(_w,XmDIALOG_TEXT));
   XtUnmanageChild(XmFileSelectionBoxGetChild(_w,XmDIALOG_SELECTION_LABEL));

   _fileText = new FileTextInterface(_w, loadFileCmd);
   _fileText->manage();

   // Add an Apply button.  We only do this for Motif 1.2 or later since
   // 1.1 FileSelectionBox can only handle one child.  If we're running
   // 1.1, there's no Apply button, but that's not a major loss since the
   // user can always hit OK.

#if XmVERSION != 1 || XmREVISION > 1
   _applyWidget = XtVaCreateManagedWidget("Apply", xmPushButtonWidgetClass, _w,
		NULL);
   XtAddCallback(_applyWidget, XmNactivateCallback,
		&FileSelBox::applyButtonCallback, (XtPointer)this);
#else
   _applyWidget = NULL;
#endif

   XtAddCallback(XmFileSelectionBoxGetChild(_w,XmDIALOG_TEXT),
		XmNvalueChangedCallback,
		&FileSelBox::newTextCallback, (XtPointer) this);

   XtAddCallback(XmFileSelectionBoxGetChild(_w,XmDIALOG_OK_BUTTON),
		XmNactivateCallback,
		&FileSelBox::okButtonCallback, (XtPointer) this);

   XtAddCallback(_w, XmNcancelCallback,
		&FileSelBox::cancelButtonCallback, (XtPointer)this);

   XtAddCallback(_w, XmNhelpCallback,
		&FileSelBox::helpButtonCallback, (XtPointer)this);
}

////////////////////////////////////////////////////////////////////////
// Callback called when the "real" Selection text changes.  We use
// this to notify the FileTextInterface of the new value.
////////////////////////////////////////////////////////////////////////

void FileSelBox::newTextCallback(Widget w, XtPointer clientData, XtPointer)
{
   FileSelBox *obj = (FileSelBox *)clientData;

   obj->newText(w);
}

////////////////////////////////////////////////////////////////////////
// Callback called when the "real" Selection text changes.  We use
// this to notify the FileTextInterface of the new value.
////////////////////////////////////////////////////////////////////////

void FileSelBox::newText(Widget w)
{
   char *text = XmTextGetString(w);

   _fileText->enterName(text);

   XtFree(text);
}

////////////////////////////////////////////////////////////////////////
// Callback called when the OK button is hit.
////////////////////////////////////////////////////////////////////////

void FileSelBox::okButtonCallback(Widget w, XtPointer clientData,
					    XtPointer callData)
{
   FileSelBox *obj = (FileSelBox *)clientData;

   obj->okButton(w, callData);
}

////////////////////////////////////////////////////////////////////////
// Callback called when the OK button is hit.  We must determine if this
// is the REAL OK button (user clicked on it or hit return with focus
// set there), or if it is a FAKE OK button caused by double-clicking on
// a List entry (or hitting return on the selection text, but since that's
// unmanaged it's not an issue here).
////////////////////////////////////////////////////////////////////////

void FileSelBox::okButton(Widget w, XtPointer callData)
{
   XmPushButtonCallbackStruct *cb = (XmPushButtonCallbackStruct *)callData;
   Boolean mine = True;

   // If it's a keyboard event, the focus widget has to be the OK button.
   // We can't just check to see if the window matches because SelectionBox
   // uses gadget children and thus OK has no window.

   if (cb->event->type == KeyPress || cb->event->type == KeyRelease) {
      if (XmGetFocusWidget(w) != w)
         mine = False;
   }

   // Otherwise, check to make sure the event's window is the same as the
   // OK button's.  If not, it came from somewhere else, probably the
   // file selection list.

   else {
      if (XtWindowOfObject(w) != cb->event->xany.window)
         mine = False;
   }

   if (!mine) {

      // FAKE OK.  The action here is to advance the "current" selection so
      // that the next text item will appear there.

      _fileText->nextFile();
   }
   else {

      // REAL OK.  Take the appropriate action.

      _fileText->startCmd();
      if (_mainWindow)
         _mainWindow->unmanage();
   }
}

////////////////////////////////////////////////////////////////////////
// Callback called when the Apply button is hit.
////////////////////////////////////////////////////////////////////////

void FileSelBox::applyButtonCallback(Widget, XtPointer clientData, XtPointer)
{
   FileSelBox *obj = (FileSelBox *)clientData;

   obj->applyButton();
}

////////////////////////////////////////////////////////////////////////
// Callback called when the Apply button is hit.
////////////////////////////////////////////////////////////////////////

void FileSelBox::applyButton()
{
   _fileText->startCmd();
}

////////////////////////////////////////////////////////////////////////
// Callback called when the Cancel button is hit.
////////////////////////////////////////////////////////////////////////

void FileSelBox::cancelButtonCallback(Widget, XtPointer clientData, XtPointer)
{
   FileSelBox *obj = (FileSelBox *)clientData;

   obj->cancelButton();
}

////////////////////////////////////////////////////////////////////////
// Callback called when the Cancel button is hit.
////////////////////////////////////////////////////////////////////////

void FileSelBox::cancelButton()
{
   if (_mainWindow)
      _mainWindow->unmanage();
}

////////////////////////////////////////////////////////////////////////
// Callback called when the Help button is hit.
////////////////////////////////////////////////////////////////////////

void FileSelBox::helpButtonCallback(Widget w, XtPointer clientData,
					      XtPointer callData)
{
   FileSelBox *obj = (FileSelBox *)clientData;

   obj->helpButton(w, callData);
}

////////////////////////////////////////////////////////////////////////
// Callback called when the Help button is hit.
////////////////////////////////////////////////////////////////////////

void FileSelBox::helpButton(Widget w, XtPointer)
{
   theHelpBrowser->run(XmFileSelectionBoxGetChild(w, XmDIALOG_HELP_BUTTON));
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create FileSelWindow.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create FileTextInterface.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// Compenent that implements the command interface (text keyin areas)
// for the FileSelBox.
////////////////////////////////////////////////////////////////////////

#include "FileTextInterface.h"
#include <Xm/Form.h>
#include <Xm/Label.h>
#include <Xm/ToggleB.h>
#include <Xm/TextF.h>
#include <stdio.h>		// for sprintf()

FileTextInterface::FileTextInterface(Widget parent, Cmd *cmd,
			int numFiles, Boolean useBands, Boolean useType)
			// = 3         = True            = True
		: CmdInterface(cmd)
{
   _numFiles = numFiles;		// number of file entries to have
   _useBands = useBands;

   _w = XtVaCreateWidget(_name, xmFormWidgetClass, parent,
	NULL);
   installDestroyHandler();

   // Add the PDS/VICAR type toggle button above the file list.

   if (useType)
      _typeForm = XtVaCreateManagedWidget("typeForm", xmFormWidgetClass, _w,
		XmNtopAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		NULL);
   else
      _typeForm = NULL;

   // Create the form widgets to hold each column.  We must use forms so we
   // can get each entry within the row to line up (via ATTACH_POSITION and
   // fractionBase).

   _filesForm = XtVaCreateManagedWidget("filesForm", xmFormWidgetClass, _w,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNfractionBase, _numFiles+1,
		NULL);

   if (_typeForm != NULL)
      XtVaSetValues(_filesForm,
		XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, _typeForm,
		NULL);
   else
      XtVaSetValues(_filesForm,
		XmNtopAttachment, XmATTACH_FORM,
		NULL);

   if (_numFiles > 1) {
      _toggleForm = XtVaCreateManagedWidget("toggleForm", xmFormWidgetClass, _w,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNleftAttachment, XmATTACH_FORM,
		XmNfractionBase, _numFiles+1,
		NULL);

      if (_typeForm != NULL)
         XtVaSetValues(_toggleForm, XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, _typeForm, NULL);
      else
         XtVaSetValues(_toggleForm, XmNtopAttachment, XmATTACH_FORM, NULL);

      XtVaSetValues(_filesForm,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, _toggleForm,
		NULL);
   }
   else {
      _toggleForm = NULL;
      XtVaSetValues(_filesForm, XmNleftAttachment, XmATTACH_FORM, NULL);
   }

   if (_useBands) {
      _bandsForm = XtVaCreateManagedWidget("bandsForm", xmFormWidgetClass, _w,
		XmNbottomAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNfractionBase, _numFiles+1,
		NULL);

      if (_typeForm != NULL)
         XtVaSetValues(_bandsForm, XmNtopAttachment, XmATTACH_WIDGET,
		XmNtopWidget, _typeForm, NULL);
      else
         XtVaSetValues(_filesForm, XmNtopAttachment, XmATTACH_FORM, NULL);

      XtVaSetValues(_filesForm,
		XmNrightAttachment, XmATTACH_WIDGET,
		XmNrightWidget, _bandsForm,
		NULL);

   }
   else {
      _bandsForm = NULL;
      XtVaSetValues(_filesForm, XmNrightAttachment, XmATTACH_FORM, NULL);
   }

   // Create column labels

   // This label should normally be blank in the resource file
   if (_toggleForm)
      XtVaCreateManagedWidget("toggleLabel", xmLabelWidgetClass, _toggleForm,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_POSITION,
		XmNtopPosition, 0,
		XmNbottomAttachment, XmATTACH_POSITION,
		XmNbottomPosition, 1,
		NULL);

   XtVaCreateManagedWidget("filesLabel", xmLabelWidgetClass, _filesForm,
		XmNalignment, XmALIGNMENT_CENTER,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_POSITION,
		XmNtopPosition, 0,
		XmNbottomAttachment, XmATTACH_POSITION,
		XmNbottomPosition, 1,
		NULL);

   if (_useBands)
      XtVaCreateManagedWidget("bandsLabel", xmLabelWidgetClass, _bandsForm,
		XmNalignment, XmALIGNMENT_CENTER,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_POSITION,
		XmNtopPosition, 0,
		XmNbottomAttachment, XmATTACH_POSITION,
		XmNbottomPosition, 1,
		NULL);

   // Now create each of the widgets, one set per file.

   _fileWidgetList = new Widget[_numFiles];
   if (_toggleForm)
      _toggleWidgetList = new Widget[_numFiles];
   else
      _toggleWidgetList = NULL;
   if (_bandsForm)
      _bandWidgetList = new Widget[_numFiles];
   else
      _bandWidgetList = NULL;

   for (int i=0; i<_numFiles; i++)
      createWidgetRow(i);

   _activeIndex = 0;

   // Create the type toggle, if needed

   if (_typeForm != NULL) {
      _typeLabel = XtVaCreateManagedWidget("typeLabel", xmLabelWidgetClass,
								_typeForm,
		XmNleftAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
      _typeTogglePDS = XtVaCreateManagedWidget("typePDS",
		xmToggleButtonWidgetClass, _typeForm,
		XmNindicatorType, XmONE_OF_MANY,
		XmNset, True,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, _typeLabel,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
      _typeToggleVICAR = XtVaCreateManagedWidget("typeVICAR",
		xmToggleButtonWidgetClass, _typeForm,
		XmNindicatorType, XmONE_OF_MANY,
		XmNset, False,
		XmNleftAttachment, XmATTACH_WIDGET,
		XmNleftWidget, _typeTogglePDS,
		XmNtopAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM,
		NULL);
      XtAddCallback(_typeTogglePDS, XmNvalueChangedCallback,
		&FileTextInterface::typeToggleCallback, (XtPointer)this);
      XtAddCallback(_typeToggleVICAR, XmNvalueChangedCallback,
		&FileTextInterface::typeToggleCallback, (XtPointer)this);
   }

}

////////////////////////////////////////////////////////////////////////
// Create all the widgets (and set the callbacks) for one row (one file)
////////////////////////////////////////////////////////////////////////

void FileTextInterface::createWidgetRow(int which)
{
   char name[20];

   if (_toggleWidgetList) {
      sprintf(name, "Band %d", which+1);
      _toggleWidgetList[which] = XtVaCreateManagedWidget(name,
		xmToggleButtonWidgetClass, _toggleForm,
		XmNindicatorType, XmONE_OF_MANY,
		XmNset, (which==0) ? True : False,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_POSITION,
		XmNtopPosition, which+1,
		XmNbottomAttachment, XmATTACH_POSITION,
		XmNbottomPosition, which+2,
		NULL);
      XtAddCallback(_toggleWidgetList[which], XmNvalueChangedCallback,
		&FileTextInterface::toggleCallback, (XtPointer)this);
   }

   sprintf(name, "file_field%d", which+1);
   _fileWidgetList[which] = XtVaCreateManagedWidget(name,
		xmTextFieldWidgetClass, _filesForm,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_POSITION,
		XmNtopPosition, which+1,
		XmNbottomAttachment, XmATTACH_POSITION,
		XmNbottomPosition, which+2,
		NULL);
   XtAddCallback(_fileWidgetList[which], XmNfocusCallback,
		&FileTextInterface::fileFocusCallback, (XtPointer)this);

   if (_bandWidgetList) {
      sprintf(name, "band_field%d", which+1);
      _bandWidgetList[which] = XtVaCreateManagedWidget(name,
		xmTextFieldWidgetClass, _bandsForm,
		XmNcolumns, 4,
		XmNleftAttachment, XmATTACH_FORM,
		XmNrightAttachment, XmATTACH_FORM,
		XmNtopAttachment, XmATTACH_POSITION,
		XmNtopPosition, which+1,
		XmNbottomAttachment, XmATTACH_POSITION,
		XmNbottomPosition, which+2,
		NULL);
      XtAddCallback(_bandWidgetList[which], XmNfocusCallback,
		&FileTextInterface::bandFocusCallback, (XtPointer)this);
   }

}

////////////////////////////////////////////////////////////////////////
// Turn on one of the toggle buttons and makes it the active one
////////////////////////////////////////////////////////////////////////

void FileTextInterface::enableFileIndex(int which)
{
   int i;

   // Take care of radio behavior

   if (_toggleWidgetList) {
      for (i=0; i<_numFiles; i++) {
         if (i == which)
            XmToggleButtonSetState(_toggleWidgetList[i], True, False);
         else
            XmToggleButtonSetState(_toggleWidgetList[i], False, False);
      }
   }

   // If we moved to a new one, check to see if the focus needs to be
   // moved too.

   if (_activeIndex != which) {
      if (XmGetFocusWidget(_w) == _fileWidgetList[_activeIndex])
         XmProcessTraversal(_fileWidgetList[which], XmTRAVERSE_CURRENT);
      if (_bandWidgetList) {
         if (XmGetFocusWidget(_w) == _bandWidgetList[_activeIndex])
            XmProcessTraversal(_bandWidgetList[which], XmTRAVERSE_CURRENT);
      }
   }

   _activeIndex = which;
}

////////////////////////////////////////////////////////////////////////
// Public function that stuffs a name into the currently active file widget
////////////////////////////////////////////////////////////////////////

void FileTextInterface::enterName(char *filename)
{
   XmTextFieldSetString(_fileWidgetList[_activeIndex], filename);
   XmTextFieldSetCursorPosition(_fileWidgetList[_activeIndex],
		XmTextFieldGetLastPosition(_fileWidgetList[_activeIndex]));
}

////////////////////////////////////////////////////////////////////////
// Public function that advances to the next active file
////////////////////////////////////////////////////////////////////////

void FileTextInterface::nextFile()
{
   int which;

   if (_numFiles > 1) {
      which = _activeIndex + 1;
      if (which >= _numFiles)
         which = 0;
      enableFileIndex(which);
   }
}

////////////////////////////////////////////////////////////////////////
// Public function that actually executes the command.  The value
// string is of the form "file(band),file(band),file(band)" where the
// "(band)" is omitted if the band is blank.
////////////////////////////////////////////////////////////////////////

void FileTextInterface::startCmd()
{
   char **files, **bands;
   int i, len;
   char *value;

   files = new char *[_numFiles];
   bands = new char *[_numFiles];

   // First get all relevant strings and their lengths

   len = 0;
   for (i=0; i<_numFiles; i++) {
      files[i] = XmTextFieldGetString(_fileWidgetList[i]);
      len += strlen(files[i]) + 1;		// +1 for comma
      if (_bandWidgetList) {
         bands[i] = XmTextFieldGetString(_bandWidgetList[i]);
         if (strlen(bands[i]) > 0)
            len += strlen(bands[i]) + 2;	// +2 for surrounding ()
      }
      else
         bands[i] = NULL;
   }

   // Get the type toggle state

   Boolean forceVicar = False;
   if (_typeToggleVICAR != NULL && XmToggleButtonGetState(_typeToggleVICAR)) {
      forceVicar = True;
   }

   // Now allocate the string and fill it up

   value = new char[len+1];			// +1 for null terminator
   strcpy(value, "");

   if (forceVicar)			// PDS is the default so no need to set
      strcat(value, "{V}");

   for (i=0; i<_numFiles; i++) {
      if (i != 0)
         strcat(value, ",");
      strcat(value, files[i]);
      if (bands[i] && strlen(bands[i])) {
         strcat(value, "(");
         strcat(value, bands[i]);
         strcat(value, ")");
      }
   }

   delete []files;
   delete []bands;

   runCmd(value);			// Freed by Cmd object

}

////////////////////////////////////////////////////////////////////////
// Callback for toggle buttons
////////////////////////////////////////////////////////////////////////

void FileTextInterface::toggleCallback(Widget w,
				XtPointer clientData, XtPointer callData)
{
   FileTextInterface *obj = (FileTextInterface *)clientData;

   obj->toggle(w, callData);
}

////////////////////////////////////////////////////////////////////////
// Check which toggle was pressed and activate that one
////////////////////////////////////////////////////////////////////////

void FileTextInterface::toggle(Widget w, XtPointer callData)
{
   XmToggleButtonCallbackStruct *cb=(XmToggleButtonCallbackStruct *)callData;
   int i;

   if (!cb->set) {		// Don't let the user turn it off
      XmToggleButtonSetState(w, True, False);
      return;
   }
 
   // Find which button got turned on

   if (_toggleWidgetList == NULL)
      return;			// shouldn't happen

   for (i=0; i<_numFiles; i++) {
      if (w == _toggleWidgetList[i])
         break;
   }
   if (i == _numFiles)
      return;			// couldn't find the widget??

   enableFileIndex(i);		// takes care of turning the others off
}

////////////////////////////////////////////////////////////////////////
// Callback for type-toggle buttons
////////////////////////////////////////////////////////////////////////

void FileTextInterface::typeToggleCallback(Widget w,
				XtPointer clientData, XtPointer callData)
{
   FileTextInterface *obj = (FileTextInterface *)clientData;

   obj->typeToggle(w, callData);
}

////////////////////////////////////////////////////////////////////////
// Check which type-toggle was pressed and activate that one
////////////////////////////////////////////////////////////////////////

void FileTextInterface::typeToggle(Widget w, XtPointer callData)
{
   XmToggleButtonCallbackStruct *cb=(XmToggleButtonCallbackStruct *)callData;

   if (!cb->set) {		// Don't let the user turn it off
      XmToggleButtonSetState(w, True, False);
      return;
   }
 
   // Find which button got turned on and turn off the other

   if (w == _typeToggleVICAR)
      XmToggleButtonSetState(_typeTogglePDS, False, False);
   if (w == _typeTogglePDS)
      XmToggleButtonSetState(_typeToggleVICAR, False, False);
}

////////////////////////////////////////////////////////////////////////
// Callback for gaining focus on one of the filename TextField's.
////////////////////////////////////////////////////////////////////////

void FileTextInterface::fileFocusCallback(Widget w,
				XtPointer clientData, XtPointer callData)
{
   FileTextInterface *obj = (FileTextInterface *)clientData;

   obj->fileFocus(w, callData);
}

////////////////////////////////////////////////////////////////////////
// Callback for gaining focus on one of the filename TextField's.
////////////////////////////////////////////////////////////////////////

void FileTextInterface::fileFocus(Widget w, XtPointer)
{
   int i;

   // Find which text widget got focus

   for (i=0; i<_numFiles; i++) {
      if (w == _fileWidgetList[i])
         break;
   }
   if (i == _numFiles)
      return;			// couldn't find the widget??

   enableFileIndex(i);
}

////////////////////////////////////////////////////////////////////////
// Callback for gaining focus on one of the band TextField's.
////////////////////////////////////////////////////////////////////////

void FileTextInterface::bandFocusCallback(Widget w,
				XtPointer clientData, XtPointer callData)
{
   FileTextInterface *obj = (FileTextInterface *)clientData;

   obj->bandFocus(w, callData);
}

////////////////////////////////////////////////////////////////////////
// Callback for gaining focus on one of the band TextField's.
////////////////////////////////////////////////////////////////////////

void FileTextInterface::bandFocus(Widget w, XtPointer)
{
   int i;

   // Find which text widget got focus

   if (_bandWidgetList == NULL)
      return;			// shouldn't happen

   for (i=0; i<_numFiles; i++) {
      if (w == _bandWidgetList[i])
         break;
   }
   if (i == _numFiles)
      return;			// couldn't find the widget??

   enableFileIndex(i);
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LoadFileCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// LoadFileCmd: A Command class that loads a file.  The Command value
// is a dynamically allocated single string, suitable for passing in to
// an ImageData subclass.  However, surrounding parentheses, if present,
// are removed (note: this modifies the input string!)
////////////////////////////////////////////////////////////////////////
#include "LoadFileCmd.h"
#include "ImageData.h"
#include "ErrorDialogManager.h"
#include <assert.h>

LoadFileCmd::LoadFileCmd(const char *name, int active, ImageData *imageData)
		: NoUndoCmd(name, active)
{
   _imageData = imageData;
}

void LoadFileCmd::doit()
{
   StatusType status;

   assert(_imageData != NULL);

   if (_imageData->isDataSourceOpened())
      _imageData->close();

   char *p = (char *)_value;
   if (*p == '(' && *(p+strlen(p)-1) == ')') {		// remove parentheses
      *(p+strlen(p)-1) = '\0';
      status = _imageData->open((char *)_value+1);
   }
   else
      status = _imageData->open((char *)_value);

   if (status != imSUCCESS) {
      if (!_imageData->errorMsgIssued()) {
         theErrorDialogManager->post(_imageData->getErrorMsg());
      }
   }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LoadMenuCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// LoadMenuCmd.h: Handle posting the File dialog box.
////////////////////////////////////////////////////////////////////////
#include "LoadMenuCmd.h"
#include "FileSelWindow.h"

LoadMenuCmd::LoadMenuCmd(const char *name, int active, Cmd *loadFileCmd)
		: NoUndoCmd ( name, active )
{
   _loadFileCmd = loadFileCmd;

   // Dialog is created here rather than when needed so that the SAGE
   // communication (if any) will get set up regardless of whether the
   // dialog is displayed or not.

   _fileSelWindow = new FileSelWindow("FileSelWindow", _loadFileCmd);
   _fileSelWindow->initialize();
}

void LoadMenuCmd::doit()
{
   _fileSelWindow->manage();
}      

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ReloadFileCmd.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// ReloadFileCmd: A Command class, the purpose of which
// is simply to call execute(filename) on a LoadFileCmd
// object.
////////////////////////////////////////////////////////////////////////
#include "ReloadFileCmd.h"
#include "ImageWindow.h"
#include "ImageToReloadGlue.h"
#include "ImageData.h"

ReloadFileCmd::ReloadFileCmd(const char *name, int active, ImageData *image, 
			     Cmd *loadFileCmd)
  : NoUndoCmd(name, active)
{
  _image = image;
  _loadFileCmd = loadFileCmd;
   
  _glue = new ImageToReloadGlue(_image, this);
}

void ReloadFileCmd::doit()
{
  if(_loadFileCmd && _image->getReloadDataSourceName()) {
    char *temp = strdup(_image->getReloadDataSourceName());
    // we make a copy, since the Cmd will try to free the string
    _loadFileCmd->execute(temp);
  }

}

ReloadFileCmd::~ReloadFileCmd()
{
  delete _glue;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create ImageToReloadGlue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// ImageToReloadGlue: class that serves as a "glue" class between an
// ImageData object and a ReloadFileCmd object.
//
// This class, though a UIComponent, creates no widget, and therefore 
// should never be managed. 
////////////////////////////////////////////////////////////////////////
#include "ImageToReloadGlue.h"
#include "ImageData.h"
#include "ReloadFileCmd.h"

ImageToReloadGlue::ImageToReloadGlue (ImageData *model, Cmd *reloadFileCmd)
  : BasicImageView("glue", model)
{
  _reloadFileCmd = reloadFileCmd;
 
  _model->attachView(this);
}

ImageToReloadGlue::~ImageToReloadGlue ( )
{
    // Detach itself from the model so that the are no more updates sent
    _model->detachView(this);
}

////////////////////////////////////////////////////////////////////////
// Whenever the image changes, reset the ReloadCmd interface.
////////////////////////////////////////////////////////////////////////
void ImageToReloadGlue::update()
{
  if(_model->isDataSourceOpened())
    _reloadFileCmd->activate();
  else
    _reloadFileCmd->deactivate();
}

void ImageToReloadGlue::updatePart(int /* flags */) { }
$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create file.imake
#define SUBROUTINE file
#define MODULE_LIST FileSelBox.cc FileSelWindow.cc FileTextInterface.cc \
 LoadMenuCmd.cc LoadFileCmd.cc ReloadFileCmd.cc ImageToReloadGlue.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#ifdef ENABLE_SAGE
#define LIB_DALI
#define LIB_SAGE_BASE
#define LIB_SAGE_CLIENT
#define LIB_DD_PLUS_PLUS
#define CCC_TEMPLATES
#endif

#define LIB_GUI
#define LIB_RTL
#define LIB_TAE
#define LIB_MOTIF
#define LIB_MOTIFAPP
$ Return
$!#############################################################################
