////////////////////////////////////////////////////////////////////////
// ImageToCursorDnViewGlue: class that serves as a "glue" class between
// an ImageData object and a CursorDnView, so that the CursorDnView
// can adjust its # of DN boxes when the mode changes from color to bw or
// back, or their width when the data type changes.  The class is a View to
// ImageData, so whenever it receives an update() from ImageData, it
// notifies the CursorDnView.  This class, even though it's a UIComponent,
// creates no widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#ifndef IMAGETOCURSORDNVIEWGLUE_H
#define IMAGETOCURSORDNVIEWGLUE_H
#include "BasicImageView.h"
#include "ImageData.h"
#include "CursorDnView.h"

class ImageToCursorDnViewGlue : public BasicImageView {

 protected:
   ModeType _oldMode;
   ImagePixelType _oldType;
   CursorDnView *_cursorDnView;

 public:

   ImageToCursorDnViewGlue (ImageData *model, CursorDnView *cursorDnView)
		: BasicImageView("glue", model)
	{  _cursorDnView = cursorDnView;
	   _oldMode = _model->getMode();
	   _model->attachView(this);
	}

   virtual void update()	// the whole reason for the class existing
	{  if (_oldMode != _model->getMode() ||
		_oldType != _model->getPixelType()) {
	      // We're really like to _cursorDnView->unmanage() here and
	      // then manage again after creating the displays in order to
	      // minimize flashing as each view is destroyed and re-created.
	      // Unfortunately, while this works on Solaris, it doesn't work
	      // on any other platform - the unmanaged RowColumn won't
	      // resize when it is managed again.  Sigh.
	      _oldMode = _model->getMode();
	      _oldType = _model->getPixelType();
	      _cursorDnView->removeCursorDisplays();
	      _cursorDnView->createCursorDisplays();
	   }
	}

   virtual Widget getWidget() { return NULL; }

   virtual const char *const className() { return  "ImageToCursorGlue"; }

};
#endif

