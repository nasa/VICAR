////////////////////////////////////////////////////////////////////////
// PrefView - A View class to PrefManager that allows the app to be
// notified when a preferences file is loaded (and thus the resources in a
// given object have changed).  This is *not* called during initial setup.
//
// There are two basic ways a subclass can deal with loading a prefs file:
// 1) No copy
//    This is the default.  The resources are modified directly in the
//    provided object.  If the prefsLoaded() function needs to know what
//    changed, it must figure it out via external means; no information is
//    provided to it regarding what changed.  This would be appropriate if
//    nothing needed to be done to react to the changes.  This mode is
//    selected by the copySize() function returning 0.  In this mode, the
//    copy argument to prefsLoaded() is NULL.
// 2) Copy object
//    In this mode, the PrefManager will create a bitwise copy of the
//    entire object, with both the original object and the bitwise copy
//    provided to the prefsLoaded() function.  The function can then compare
//    resource fields between the two in order to determine what has changed.
//    The copy is automatically destroyed when prefsLoaded() returns.  This
//    allows the prefsLoaded() function to determine precisely what has
//    changed, and react to it by calling whatever functions are necessary
//    to change the state of the object.  This closely mimics the behavior
//    of the X Toolkit Widgets' setValues() function.  This mode is selected
//    by copySize() returning a non-0 value (which is the size in bytes of
//    the object being copied).  Note that this is a *bitwise* copy; the
//    copy constructor (if any) is not called.  Therefore, the copy is not
//    a true "object" and no functions should be called on the copy that
//    could modify it, as pointers point to the same place as the original.
//    For this reason, the PrefManager subclass should either be a friend of
//    the object, or prefsLoaded() should call a function inside the object,
//    to allow direct access to the resource fields.
//
// An additional choice under #2 is whether the loaded resources should be
// stored in the original object, or in the copy.  Storing them in the
// original object mimics Xt most closely.  Storing them in the copy allows
// the application to maintain internal state while it selects which items
// it will accept, but it must copy over all resources that it wishes to keep.
// This choice is determined by the return value of modifyCopy().
// Note:  if modifyCopy is True, then the size of the copy (provided by
// copySize()) *must* be big enough to hold all resource changes, or the
// resources will go into random memory and probably cause a crash.
////////////////////////////////////////////////////////////////////////

#ifndef PREFVIEW_H
#define PREFVIEW_H

#include <X11/Intrinsic.h>	// for Boolean and XtPointer

class PrefView {

 private:

 protected:

 public:
   PrefView() { }
   virtual ~PrefView() { }

   virtual int copySize() { return 0; }		// =0 means no copy
   virtual Boolean modifyCopy() { return False; }	// def=modify real one
   virtual void prefsLoaded(XtPointer object, XtPointer copy) = 0;

};

#endif

