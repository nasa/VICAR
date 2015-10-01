////////////////////////////////////////////////////////////////////////
// PrefManager - class that manages a preferences file and resources for
// various parts of the application.
////////////////////////////////////////////////////////////////////////

#include "PrefManager.h"
#include "PrefView.h"

#include "UIComponent.h"

#include <X11/Intrinsic.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/ShellP.h>

#include <stdio.h>

PrefManager *thePrefManager = NULL;

static Cardinal GetNamesAndClasses(
    register Widget       w,
    register XrmNameList  names,
    register XrmClassList classes);

Boolean PrefManager::_firsttime = True;

////////////////////////////////////////////////////////////////////////
// Constructor.
// Display parameter is only needed if pathname is given.
// See below for important notes concerning pathname.
////////////////////////////////////////////////////////////////////////

PrefManager::PrefManager(Display *dpy, const char *pathname)
{
   if (_firsttime) {
      installReverseConverters();
      _firsttime = False;
   }

   // Set the global pointer
   thePrefManager = this;

   // Load the pathname into the database, for use by sets which are not
   // yet registered.
   // This is not done correctly; full implementation TBD !!!!

   // Problems with the current implementation:
   // 1) it only works on the default screen (not usually a problem since
   //    specifying host::0.1 sets the default screen to 1).
   // 2) command-line arguments do not override the file, as they should.

   // #2 is fixable by parsing the command line again and adding it to the
   // DB after adding this file.  But, since standard Xt resources are
   // inaccessible, the only way to do that and include standard Xt resources
   // is to call XtDisplayInitialize again (this also fixes #1).  This should
   // be okay, although it may leave extra databases allocated.  However, the
   // only way for the prefs file to then get included is to either put it in
   // someplace searched by the standard X search path (messy and platform-
   // dependent), or to get and enumerate the DB for the prefs file, and
   // prepend each line to argc/argv as -xrm options before calling
   // XtDisplayInitialize.  Still messy, but not platform dependent.
   // The original arg list is available from ApplicationShell resources,
   // and the application options list is available from theApplication.

   // As for problem #1, fortunately Xt does in fact use XrmGetDatabase()
   // when you're on the default screen, so we can use XrmSetDatabase() to
   // change it.  Don't use XtScreenDatabase() because we don't work for the
   // non-default screen anyway.

   // An alternative would be to not touch the real database (thus fixing
   // problem #1, but to manually load (into the real record, not a copy)
   // the preferences file resources after XtGetSubresources is called when the
   // resource set is registered.  But this still does not help with problem
   // #2 and might make memory management issues even murkier (if possible!?!).

   // In the meantime, if either restriction is a problem, the application
   // can make sure the prefs file is somewhere in the X search path itself
   // (by setting env vars before starting up, or at least before initializing
   // Xt) and then pass in NULL for the filename.

   if (pathname != NULL) {
      XrmDatabase db = XrmGetDatabase(dpy);
      XrmCombineFileDatabase((char *)pathname, &db, True);
      XrmSetDatabase(dpy, db);
   }

   _recs = NULL;
   _num_recs = 0;

   _XtRStringQuark = XrmPermStringToQuark(XtRString);
}

////////////////////////////////////////////////////////////////////////

PrefManager::~PrefManager()
{
   thePrefManager = NULL;

   for (int i=0; i<_num_recs; i++)
      delete _recs[i];
   delete _recs;

}

////////////////////////////////////////////////////////////////////////
// Load the given preferences file, notifying everyone of the changes.
////////////////////////////////////////////////////////////////////////

int PrefManager::loadPrefFile(const char *pathname)
{
   XrmDatabase db;

   db = NULL;		// start with empty DB

   db = XrmGetFileDatabase((char *)pathname);
   if (db == NULL) {
      return -1;
   }

   // We can't just use XtGetSubresources() because that will reset the
   // defaults for anything not found in the preferences file, which is *not*
   // what we want.

   for (int i=0; i<_num_recs; i++)		// read in each set
      readResourcesFromDB(&db, _recs[i]);

   XrmDestroyDatabase(db);

   return 0;
}

////////////////////////////////////////////////////////////////////////
// Save all registered prefs to the given file.
////////////////////////////////////////////////////////////////////////

void PrefManager::savePrefFile(const char *pathname)
{
   XrmDatabase db;

   db = NULL;		// start with empty DB

   for (int i=0; i<_num_recs; i++)		// write out each set
      writeResourcesToDB(&db, _recs[i]);

   XrmPutFileDatabase(db, (char *)pathname);

   XrmDestroyDatabase(db);
}

////////////////////////////////////////////////////////////////////////
// Write the resources from one registered set to the Xrm database.
////////////////////////////////////////////////////////////////////////

void PrefManager::writeResourcesToDB(XrmDatabase *db, PrefRec *rec)
{
   // Resource records can be "compiled" into quarks for efficiency by
   // the get resource routines.  So we must check for that here.  This
   // is largely cribbed from Xt/Resources.c.
   // NOTE: There is #ifdef CRAY2 code in Resoures.c that is not repeated
   // here.  If you want this to work on a CRAY2, you need to put that code
   // in as well.

   Boolean compiled = True;
   XrmResourceList *table, *res, rx;
   XrmResource rx_nocomp;
   Cardinal i;

   if ((int)rec->_resources->resource_offset >= 0)
      compiled = False;

   if (compiled) {			// Create table for access
      table = new XrmResourceList[rec->_num_resources];
      for (i=0; i<rec->_num_resources; i++)
         table[i] = (XrmResourceList)(&(rec->_resources[i]));
      res = table;
   }

   // Now loop through each element of the table and do something with it

   for (i=0; i<rec->_num_resources; i++) {
      if (compiled) {
         rx = *res;
         res++;
      }
      else {		// "compile" each entry on the fly
         rx = &rx_nocomp;
         rx->xrm_name = XrmStringToQuark(rec->_resources[i].resource_name);
         rx->xrm_class = XrmStringToClass(rec->_resources[i].resource_class);
         rx->xrm_type = XrmStringToRepresentation(rec->_resources[i].resource_type);
         rx->xrm_size = rec->_resources[i].resource_size;
         // this won't work on CRAY
         rx->xrm_offset = - rec->_resources[i].resource_offset - 1;
         // Defaults not used
      }

      char resname[300];
      strcpy(resname, rec->_res_path);
      strcat(resname, XrmQuarkToString(rx->xrm_name));
      if (rx->xrm_type == _XtRStringQuark) {
         // No resource conversion necessary, just write the puppy out!
         XrmPutStringResource(db, resname,
		*(char **)(((char *)rec->_base_ptr) - rx->xrm_offset - 1));
      }
      else {
         Boolean cvt_ok;
         XrmValue from, to;
         from.size = rx->xrm_size;
         from.addr = (XPointer) (((char *)rec->_base_ptr) - rx->xrm_offset - 1);
         to.size = 0;
         to.addr = NULL;
         cvt_ok = XtConvertAndStore(rec->_object,
		XrmQuarkToString(rx->xrm_type), &from, XtRString, &to);
         if (cvt_ok) {
            XrmPutStringResource(db, resname, *(char **)to.addr);
         }
         // Xt will issue error so don't check.
      }
   }
   if (compiled)
      delete[] table;
}

////////////////////////////////////////////////////////////////////////
// Read the resources for one registered set from the Xrm database.
////////////////////////////////////////////////////////////////////////

void PrefManager::readResourcesFromDB(XrmDatabase *db, PrefRec *rec)
{
   // Check to see if the View wants a copy or not, and whether we're supposed
   // to modify the copy or the original

   XtPointer base = rec->_base_ptr;
   XtPointer copy = NULL;

   if (rec->_view != NULL) {	// if no view, make no copy and modify original
      if (rec->_view->copySize() > 0) {		// make copy
         copy = (XtPointer)new unsigned char[rec->_view->copySize()];
         if (copy) {
            memcpy(copy, rec->_base_ptr, rec->_view->copySize());
            if (rec->_view->modifyCopy())	// modify copy instead of orig
               base = copy;
         }
      }
   }

   // Create the name/class quark list for the resource search.  Stolen
   // (as usual) from Xt/Resources.c (XtGetSubresources())

   XrmName names[100];		// Yes, Xt has this limit hardcoded...
   XrmClass classes[100];
   Cardinal length;

   length = GetNamesAndClasses(rec->_object, names, classes);
   names[length] = XrmStringToName(rec->_subpart_name);
   classes[length] = XrmStringToClass(rec->_subpart_class);
   length++;
   names[length] = NULLQUARK;
   classes[length] = NULLQUARK;

   names[length+1] = NULLQUARK;		// leave room for item name/class
   classes[length+1] = NULLQUARK;

   // Resource records can be "compiled" into quarks for efficiency by
   // the get resource routines.  So we must check for that here.  This
   // is largely cribbed from Xt/Resources.c.
   // NOTE: There is #ifdef CRAY2 code in Resoures.c that is not repeated
   // here.  If you want this to work on a CRAY2, you need to put that code
   // in as well.

   Boolean compiled = True;
   XrmResourceList *table, *res, rx;
   XrmResource rx_nocomp;
   Cardinal i;

   if ((int)rec->_resources->resource_offset >= 0)
      compiled = False;

   if (compiled) {			// Create table for access
      table = new XrmResourceList[rec->_num_resources];
      for (i=0; i<rec->_num_resources; i++)
         table[i] = (XrmResourceList)(&(rec->_resources[i]));
      res=table;
   }

   // Now loop through each element of the table and do something with it

   for (i=0; i<rec->_num_resources; i++) {
      if (compiled) {
         rx = *res;
         res++;
      }
      else {		// "compile" each entry on the fly
         rx = &rx_nocomp;
         rx->xrm_name = XrmStringToQuark(rec->_resources[i].resource_name);
         rx->xrm_class = XrmStringToClass(rec->_resources[i].resource_class);
         rx->xrm_type = XrmStringToRepresentation(rec->_resources[i].resource_type);
         rx->xrm_size = rec->_resources[i].resource_size;
         // this won't work on CRAY
         rx->xrm_offset = - rec->_resources[i].resource_offset - 1;
         // Defaults not used
      }

      names[length] = rx->xrm_name;
      classes[length] = rx->xrm_class;

      XrmRepresentation type_rtn;
      XrmValue value;
      value.size = 0;
      value.addr = NULL;
      Boolean found = XrmQGetResource(*db, names, classes, &type_rtn,&value);
      if (found) {
         XrmValue to;
         to.size = rx->xrm_size;
         to.addr = (XPointer)(((char *)base) - rx->xrm_offset - 1);
         // type_rtn is almost always String
         if (rx->xrm_type == _XtRStringQuark) // Special-case string copy
            *(char **)(to.addr) = value.addr; // (NOTE: NOT dynamic alloc)
         else
            XtConvertAndStore(rec->_object, XrmQuarkToString(type_rtn),
			&value, XrmQuarkToString(rx->xrm_type), &to);
      }
   }
   if (compiled)
      delete[] table;

   // Tell the app that things changed...

   if (rec->_view)
      rec->_view->prefsLoaded(rec->_base_ptr, copy);

   // Delete copy

   if (copy)
      delete[] (unsigned char *)copy;
}

////////////////////////////////////////////////////////////////////////
// Register a set of resources.
////////////////////////////////////////////////////////////////////////

void PrefManager::registerResources(UIComponent *comp,
		XtResourceList resources, Cardinal num_resources,
		const char *res_path, PrefView *view)
{
   registerResources(XtParent(comp->baseWidget()), (XtPointer)comp,
		comp->name(), comp->className(),
		resources, num_resources, res_path, view);
}

void PrefManager::registerResources(Widget object, XtPointer base,
		const char *subpart_name, const char *subpart_class,
		XtResourceList resources, Cardinal num_resources,
		const char *res_path, PrefView *view)
{
   PrefRec *rec = new PrefRec;

   rec->_object = object;
   rec->_base_ptr = base;
   rec->_subpart_name = strdup(subpart_name);
   rec->_subpart_class = strdup(subpart_class);
   rec->_resources = resources;
   rec->_num_resources = num_resources;
   rec->_res_path = strdup(res_path);
   rec->_view = view;

   PrefPtr *newlist = new PrefPtr[_num_recs+1];
   for (int i=0; i<_num_recs; i++)
      newlist[i] = _recs[i];
   newlist[_num_recs] = rec;
   _num_recs++;
   delete[] _recs;
   _recs = newlist;

   getResources(rec);
}

////////////////////////////////////////////////////////////////////////
// Unregister a set of resources.
////////////////////////////////////////////////////////////////////////

void PrefManager::unregisterResources(Widget object, XtPointer base,
		XtResourceList resources)
{
   PrefPtr *newlist = new PrefPtr[_num_recs-1];
   for (int i=0; i<_num_recs; i++) {
      if (_recs[i]->_object == object && _recs[i]->_base_ptr == base &&
	  _recs[i]->_resources == resources) {		// Found the one to del
         delete _recs[i];
         for (int j=i; j<_num_recs-1; j++)
            newlist[j] = _recs[j+1];	// move everything else down
         _num_recs--;
         delete[] _recs;
         _recs = newlist;
         return;
      }
      if (i != _num_recs-1)	// copy over unless last
         newlist[i] = _recs[i];
   }
   delete[] newlist;		// Not found, ignore new list
}

////////////////////////////////////////////////////////////////////////
// Read standard X resources into the given resource set.  Called when
// the set is registered.
////////////////////////////////////////////////////////////////////////

void PrefManager::getResources(PrefRec *rec)
{
   XtGetSubresources(rec->_object, rec->_base_ptr,
	rec->_subpart_name, rec->_subpart_class,
	rec->_resources, rec->_num_resources, NULL, 0);
}

////////////////////////////////////////////////////////////////////////
// Convert a widget into a list of names and classes (represented as
// quarks) for use in querying the database.
//
// This is copied verbatim from Xt/Resources.c
////////////////////////////////////////////////////////////////////////

static Cardinal GetNamesAndClasses(
    register Widget       w,
    register XrmNameList  names,
    register XrmClassList classes)
{
    register Cardinal length, j;
    register XrmQuark t;
    WidgetClass clas;

    /* Return null-terminated quark arrays, with length the number of
       quarks (not including NULL) */

    for (length = 0; w != NULL; w = (Widget) w->core.parent) {
        names[length] = w->core.xrm_name;
        clas = XtClass(w);
        /* KLUDGE KLUDGE KLUDGE KLUDGE */
        if (w->core.parent == NULL && XtIsApplicationShell(w)) {
            classes[length] =
                ((ApplicationShellWidget) w)->application.xrm_class;
        } else classes[length] = clas->core_class.xrm_class;
        length++;
     }
    /* They're in backwards order, flop them around */
    for (j = 0; j < length/2; j++) {
        t = names[j];
        names[j] = names[length-j-1];
        names[length-j-1] = t;
        t = classes[j];
        classes[j] = classes[length-j-1];
        classes[length-j-1] = t;
    }
    names[length] = NULLQUARK;
    classes[length] = NULLQUARK;
    return length;
} /* GetNamesAndClasses */

////////////////////////////////////////////////////////////////////////
// Define "reverse" converters which go from various representation types
// to XtRString.  Xt and Xm provide none of these (with the exception of
// XmRepTypeAddReverse(), but that doesn't apply to basic types).
////////////////////////////////////////////////////////////////////////

static Boolean CvtBooleanToString(Display *dpy,
	XrmValue *, Cardinal *num_args,
	XrmValue *fromVal, XrmValue *toVal, XtPointer *)
{
   if (*num_args != 0)
      XtAppWarningMsg(
            XtDisplayToApplicationContext(dpy),
            "wrongParameters", "cvtBooleanToString", "XtToolkitError",
            "Boolean to String conversion needs no extra arguments",
            NULL, NULL);
 
   static char *out_val;

   if (*((Boolean *)(fromVal->addr)))
      out_val = (char *)"true";
   else
      out_val = (char *)"false";

   if (toVal->addr != NULL) {
      if (toVal->size < sizeof(char *)) {
         toVal->size = sizeof(char *);
         return FALSE;
      }
      *((char **)(toVal->addr)) = out_val;
   }
   else
      toVal->addr = (XPointer)&out_val;
   toVal->size = sizeof(char *);
   return TRUE;
}

////////////////////////////////////////////////////////////////////////

static Boolean CvtIntToString(Display *dpy,
	XrmValue *, Cardinal *num_args,
	XrmValue *fromVal, XrmValue *toVal, XtPointer *)
{
   if (*num_args != 0)
      XtAppWarningMsg(
            XtDisplayToApplicationContext(dpy),
            "wrongParameters", "cvtIntToString", "XtToolkitError",
            "Int to String conversion needs no extra arguments",
            NULL, NULL);
 
   static char out_val[20];

   sprintf(out_val, "%d", *((int *)(fromVal->addr)));

   if (toVal->addr != NULL) {
      if (toVal->size < sizeof(char *)) {
         toVal->size = sizeof(char *);
         return FALSE;
      }
      *((char **)(toVal->addr)) = out_val;
   }
   else
      toVal->addr = (XPointer)&out_val;
   toVal->size = sizeof(char *);
   return TRUE;
}

////////////////////////////////////////////////////////////////////////

static Boolean CvtDimensionToString(Display *dpy,
	XrmValue *, Cardinal *num_args,
	XrmValue *fromVal, XrmValue *toVal, XtPointer *)
{
   if (*num_args != 0)
      XtAppWarningMsg(
            XtDisplayToApplicationContext(dpy),
            "wrongParameters", "cvtDimensionToString", "XtToolkitError",
            "Dimension to String conversion needs no extra arguments",
            NULL, NULL);
 
   static char out_val[20];

   sprintf(out_val, "%d", *((Dimension *)(fromVal->addr)));

   if (toVal->addr != NULL) {
      if (toVal->size < sizeof(char *)) {
         toVal->size = sizeof(char *);
         return FALSE;
      }
      *((char **)(toVal->addr)) = out_val;
   }
   else
      toVal->addr = (XPointer)&out_val;
   toVal->size = sizeof(char *);
   return TRUE;
}


////////////////////////////////////////////////////////////////////////
// Install all of the X->String converters defined above.
////////////////////////////////////////////////////////////////////////

void PrefManager::installReverseConverters()
{
   XtSetTypeConverter(XtRBoolean, XtRString, CvtBooleanToString,
		NULL, 0, XtCacheNone, (XtDestructor)NULL);
   XtSetTypeConverter(XtRInt, XtRString, CvtIntToString,
		NULL, 0, XtCacheNone, (XtDestructor)NULL);
   XtSetTypeConverter(XtRDimension, XtRString, CvtDimensionToString,
		NULL, 0, XtCacheNone, (XtDestructor)NULL);
}

