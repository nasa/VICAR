////////////////////////////////////////////////////////////////////////
// SiCursorStretchInterface.h: Cursor-controlled linear stretch.
// Vertical motion controls contrast (slope of line) while horizontal
// motion controls brightness (y-intercept of line)
// Although this is a CmdInterface, there is no specific screen
// control for it... merely turn it on and it uses cursor callbacks
// on the IW and turns itself off.
////////////////////////////////////////////////////////////////////////
#ifndef SICURSORSTRETCHINTERFACE
#define SICURSORSTRETCHINTERFACE
#include "CmdInterface.h"
#include "StretchValue.h"

class CallbackCompressor;

class SiCursorStretchInterface : public CmdInterface {

 private:

   static void cursorMovedCallback(Widget, XtPointer, XtPointer);

 protected: 

   static Pixmap _cursorPixmap, _cursorMask;

   StretchValue *_undoStretchValue;	// Saved for undo (must save original,
					// not each cursor increment)

   Widget _iw;				// image widget we're talking to

   Boolean _saveTrackFC;
   char *_saveCursor;
   unsigned char _saveCursorMode;
   double _saveCursX, _saveCursY;
   CallbackCompressor *_cb_compress;

   void cursorMoved(XtPointer);
   void setLocationFromStretch();

 public:

   SiCursorStretchInterface(Widget, Cmd *, Widget iw);
   virtual ~SiCursorStretchInterface();

   virtual void activate();		// overrides CmdInterface
   virtual void deactivate();		// ditto
   Boolean active() { return _active; }

};
#endif

