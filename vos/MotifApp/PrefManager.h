////////////////////////////////////////////////////////////////////////
// PrefManager - class that manages a preferences file and resources for
// various parts of the application.
////////////////////////////////////////////////////////////////////////

#ifndef PREFMANAGER_H
#define PREFMANAGER_H

#include <X11/Intrinsic.h>

class PrefView;
class UIComponent;

class PrefManager {

 private:
   static Boolean _firsttime;		// flag for class initialization

 protected:

   typedef struct _PrefRec {
      Widget _object;
      XtPointer _base_ptr;
      String _subpart_name;
      String _subpart_class;
      XtResourceList _resources;
      Cardinal _num_resources;
      PrefView *_view;
      String _res_path;

      _PrefRec() { _subpart_name = _subpart_class = _res_path = NULL; }
      ~_PrefRec()
	{  if (_subpart_name) delete _subpart_name;
	   if (_subpart_class) delete _subpart_class;
	   if (_res_path) delete _res_path;
	}
   } PrefRec, *PrefPtr;

   PrefPtr *_recs;
   int _num_recs;

   XrmQuark _XtRStringQuark;

   void getResources(PrefRec *);
   void readResourcesFromDB(XrmDatabase *db, PrefRec *rec);
   void writeResourcesToDB(XrmDatabase *db, PrefRec *rec);

   virtual void installReverseConverters();

 public:
   PrefManager(Display *dpy = NULL, const char *pathname = NULL);
   virtual ~PrefManager();

   virtual int loadPrefFile(const char *pathname);	// returns 0 for success
   virtual void savePrefFile(const char *pathname);

   virtual void registerResources(Widget object, XtPointer base,
	const char *subpart_name, const char *subpart_class,
	XtResourceList resources, Cardinal num_resources,
	const char *res_path, PrefView *view);

   virtual void registerResources(UIComponent *comp,
	XtResourceList resources, Cardinal num_resources,
	const char *res_path, PrefView *view);

   virtual void unregisterResources(Widget object, XtPointer base,
	XtResourceList resources);

};

// Pointer to single global instance

extern PrefManager *thePrefManager;

#endif

