////////////////////////////////////////////////////////////////
// CollectHistBG.cc - collect a histogram from an ImageData model
// in the background, using a WorkProc.
////////////////////////////////////////////////////////////////
#include "CollectHistBG.h"
#include "ImageData.h"
#include "Histogram.h"
#include "ImageTile.h"
#include "Application.h"
#include "ErrorDialogManager.h"
#include "CollectHist.h"

#ifndef MIN
#define MIN(a,b) ((a)<(b) ? (a) : (b))
#endif

struct CollectHistBGState {
   int nl, ns;
   int tileWidth, tileHeight;
   int currentLine, currentSamp;
   ImageData *imageModel;
   Histogram *histR, *histG, *histB;
   ImageTile *tile;
   XtWorkProcId workProcId;
   void **activePtr;
};

static Boolean CollectHistBGWorkProc(XtPointer clientData);

////////////////////////////////////////////////////////////////
// Set up the state structure and register the work proc.
// The "active" is a pointer to a void * in the caller (it is
// really a pointer to the state structure).  This void * should
// be initialized to NULL before any hists are collected.  Then,
// the address of this pointer should be passed in to all calls
// to CollectHistBG that use the same imageModel and Histogram
// objects.  If there's a collection currently in progress, it
// is terminated and a new one is started.  The WorkProc will set
// active to NULL when it completes.
////////////////////////////////////////////////////////////////

void CollectHistBG(ImageData *imageModel, 
		Histogram *histR, Histogram *histG, Histogram *histB,
		void **active)
{
   CollectHistBGState *cb;

   if (*active) {		// An old collection is still active
      CollectHistBGState *oldcb = (CollectHistBGState *)(*active);
      XtRemoveWorkProc(oldcb->workProcId);
      delete oldcb;
      *active = NULL;
   }

   histR->clear();
   histG->clear();
   histB->clear();

   if (!imageModel->isDataSourceOpened())
      return;			// nothing to do if no data available

   cb = new CollectHistBGState;
   if (cb == NULL)
      return;
   *active = (void *)cb;
   cb->activePtr = active;

   cb->imageModel = imageModel;
   cb->histR = histR;
   cb->histG = histG;
   cb->histB = histB;

   cb->nl = imageModel->getNumbLines();
   cb->ns = imageModel->getNumbSamples();
   imageModel->getSuggestedUnzoomedTileSize(cb->tileHeight, cb->tileWidth);

   imageModel->setUnzoomedTileSize(cb->tileHeight, cb->tileWidth);

   ZoomFactor zoom(1,1,1,1);
   ImageTile &tileRef = imageModel->createTile(zoom);
   cb->tile = &tileRef;

   cb->currentLine = 0;
   cb->currentSamp = 0;

   cb->workProcId = XtAppAddWorkProc(theApplication->appContext(),
			CollectHistBGWorkProc, (XtPointer) cb);
}

////////////////////////////////////////////////////////////////
// Work proc to gather the histogram.  Read one tile's worth of
// data, and add it to the histogram.  If we're done, update the
// histogram views, delete the state structure, and unregister
// the callback.
////////////////////////////////////////////////////////////////

static Boolean CollectHistBGWorkProc(XtPointer clientData)
{
   CollectHistBGState *cb = (CollectHistBGState *)clientData;
   StatusType status;

   int endLine, endSamp;
   int readWidth, readHeight;

   if (cb->currentLine < cb->nl && cb->currentSamp < cb->ns) {

      // Read the next tile

      endLine = MIN(cb->currentLine + cb->tileHeight - 1, cb->nl - 1);
      readHeight = endLine - cb->currentLine + 1;
      endSamp = MIN(cb->currentSamp + cb->tileWidth - 1, cb->ns - 1);
      readWidth = endSamp - cb->currentSamp + 1;

      status = cb->imageModel->readTile(cb->currentSamp, cb->currentLine,
			readWidth, readHeight, *cb->tile);
      if (status != imSUCCESS) {
         if (!cb->imageModel->errorMsgIssued()) {
            theErrorDialogManager->post(cb->imageModel->getErrorMsg());
         }
      }

      // Use the tile data to fill in the histogram

      if (cb->tile->getMode() == BWmode) {
         CollectHistFromTile(cb->histR, 0, *cb->tile, readWidth, readHeight);
      }
      else {
         CollectHistFromTile(cb->histR, 0, *cb->tile, readWidth, readHeight);
         CollectHistFromTile(cb->histG, 1, *cb->tile, readWidth, readHeight);
         CollectHistFromTile(cb->histB, 2, *cb->tile, readWidth, readHeight);
      }

      // Index to next tile

      cb->currentSamp += cb->tileWidth;
      if (cb->currentSamp >= cb->ns) {
         cb->currentLine += cb->tileHeight;
         cb->currentSamp = 0;
      }
   }

   // Are we done?

   if (cb->currentLine < cb->nl && cb->currentSamp < cb->ns)
      return False;			// no, call us again

   // We're done, clean up

   cb->histR->updateViews();
   cb->histG->updateViews();
   cb->histB->updateViews();

   *cb->activePtr = NULL;	// clears "active" in caller

   delete cb;

   return True;			// don't call us again

}

////////////////////////////////////////////////////////////////
// Collect histogram info for one tile's worth of data.
////////////////////////////////////////////////////////////////

void CollectHistFromTile(Histogram *hist, int bufferIndex, ImageTile &tile,
		int width, int height)
{
   int i;
   unsigned char *ptr;

   ptr = tile.getBufferPtr(bufferIndex) + tile.getByteOffset();

   for (i=0; i<height; i++) {

      CollectHistLine(hist, ptr, width, tile.getPixelType());

      ptr += tile.getLineWidth();
   }
}

