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

