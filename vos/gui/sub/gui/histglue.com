$!****************************************************************************
$!
$! Build proc for MIPL module histglue
$! VPACK Version 1.8, Wednesday, September 04, 1996, 11:09:36
$!
$! Execute by entering:		$ @histglue
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
$ write sys$output "*** module histglue ***"
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
$ write sys$output "Invalid argument given to histglue.com file -- ", primary
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
$   if F$SEARCH("histglue.imake") .nes. ""
$   then
$      vimake histglue
$      purge histglue.bld
$   else
$      if F$SEARCH("histglue.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake histglue
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @histglue.bld "STD"
$   else
$      @histglue.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create histglue.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack histglue.com -mixed -
	-s ImageToHistGlue.cc RawHistToStrHistGlue.cc LutToStrHistGlue.cc -
	   CollectStretchedHist.cc CollectHist.cc CollectHistBG.cc -
	-i histglue.imake
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create ImageToHistGlue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////////////
// ImageToHistGlue: class that serves as a "glue" class between an
// ImageData object and a (set of) Histogram objects.  The class is a
// View to ImageData, so whenever it receives an update() from ImageData,
// it recollects the histograms (which in turn cause them to update their
// own views).  This class, even though it's a UIComponent, creates no
// widget, therefore it should never be managed.
////////////////////////////////////////////////////////////////////////
#include "ImageToHistGlue.h"
#include "Histogram.h"
#include "CollectHistBG.h"
#include "ImageData.h"

ImageToHistGlue::ImageToHistGlue (ImageData *model,
			Histogram *histR, Histogram *histG, Histogram *histB)
		: BasicImageView("glue", model)
{
   _histR = histR;
   _histG = histG;
   _histB = histB;

   _collectionActive = NULL;

   _model->attachView(this);
}

ImageToHistGlue::~ImageToHistGlue ( )
{
    // Detach itself from the model so that the are no more updates sent
    _model->detachView(this);
}

////////////////////////////////////////////////////////////////////////
// This is the meat of the function.  Whenever the image changes,
// recompute the histogram.  Note that when ROI is implemented, this
// class should be a "view" of the ROI rather than the image model directly.
////////////////////////////////////////////////////////////////////////
void ImageToHistGlue::update()
{

   updatePart(IMAGE_DATA_UPDATE_RANGE);		// Set hist limits

   if (_model->isDataSourceOpened()) {
      CollectHistBG(_model, _histR, _histG, _histB, &_collectionActive);
   }
   else {		// No data, so clear out histograms
      if (_histR)
         _histR->clear();
      if (_histG)
         _histG->clear();
      if (_histB)
         _histB->clear();
   }
}

void ImageToHistGlue::updatePart(int flags)
{
   if (flags & IMAGE_DATA_UPDATE_RANGE) {
      if (_model->getPixelType().isIntegral()) {
         if (_histR)
            _histR->setLimits((int)_model->getMinDataRange(),
			      (int)_model->getMaxDataRange());
         if (_histG)
            _histG->setLimits((int)_model->getMinDataRange(),
			      (int)_model->getMaxDataRange());
         if (_histB)
            _histB->setLimits((int)_model->getMinDataRange(),
			      (int)_model->getMaxDataRange());
      }
      else {
         if (_histR)
            _histR->setLimits(_model->getMinDataRange(),
			      _model->getMaxDataRange());
         if (_histG)
            _histG->setLimits(_model->getMinDataRange(),
			      _model->getMaxDataRange());
         if (_histB)
            _histB->setLimits(_model->getMinDataRange(),
			      _model->getMaxDataRange());
      }
   }
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create RawHistToStrHistGlue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////
// RawHistToStrHistGlue.cc
//////////////////////////////////////////////////////////
#include "RawHistToStrHistGlue.h"
#include "Histogram.h"
#include "LutView.h"
#include "HistView.h"
#include "CollectStretchedHist.h"


RawHistToStrHistGlue::RawHistToStrHistGlue ( 
		Histogram *histR, Histogram *histG, Histogram *histB,
		Histogram *strhistR, Histogram *strhistG, Histogram *strhistB,
		Lut *lutR, Lut *lutG, Lut *lutB)
	: HistView ( "glue")
{
    _lutR = lutR;
    _lutG = lutG;
    _lutB = lutB;

    _hist  = histR;
    _hist1 = histG;
    _hist2 = histB;

    _strhistR = strhistR;
    _strhistG = strhistG;
    _strhistB = strhistB;

    _hist ->attachView ( this);
    _hist1->attachView ( this);
    _hist2->attachView ( this);

}

void RawHistToStrHistGlue::update()
{
   // Call the "glue" function to perform the operation
   CollectStretchedHist ( _hist, _hist1, _hist2,
                _strhistR, _strhistG, _strhistB,
                _lutR, _lutG, _lutB);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create LutToStrHistGlue.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////
// LutToStrHistGlue.cc
//////////////////////////////////////////////////////////
#include "LutToStrHistGlue.h"
#include "Histogram.h"
#include "LutView.h"
#include "CollectStretchedHist.h"

LutToStrHistGlue::LutToStrHistGlue ( 
		Histogram *histR, Histogram *histG, Histogram *histB,
		Histogram *strhistR, Histogram *strhistG, Histogram *strhistB,
		Lut *lutR, Lut *lutG, Lut *lutB)
	: LutView ( "glue", lutR, lutG, lutB )
{
    _histR = histR;
    _histG = histG;
    _histB = histB;

    _strhistR = strhistR;
    _strhistG = strhistG;
    _strhistB = strhistB;

    if (_lut) _lut ->attachView ( this);
    if (_lut1) _lut1->attachView ( this);
    if (_lut2) _lut2->attachView ( this);
}

void LutToStrHistGlue::update()
{
   // Call the "glue" function to perform the operation
   CollectStretchedHist ( _histR, _histG, _histB,
		_strhistR, _strhistG, _strhistB,
		_lut, _lut1, _lut2);
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create CollectStretchedHist.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
//////////////////////////////////////////////////////////////////////
// CollectStretchedHist.cc
//////////////////////////////////////////////////////////////////////

#include "CollectStretchedHist.h"
#include "Lut.h"
#include "Histogram.h"

void CollectStretchedHist ( 
        Histogram *histR, Histogram *histG, Histogram *histB,
        Histogram *strhistR, Histogram *strhistG, Histogram *strhistB,
        Lut *lutR, Lut *lutG, Lut* lutB)
{
    CollectStretchedHist ( histR, strhistR, lutR);
    CollectStretchedHist ( histG, strhistG, lutG);
    CollectStretchedHist ( histB, strhistB, lutB);
}


void CollectStretchedHist ( Histogram *hist, Histogram *strhist, Lut *lut)
{

   int i, origTempValue, strTempValue;
   int *lut_vector;
   int str_i;

// Copy primary histogram parameters to stretched hist

   if (hist->isIntRange())
      strhist->setLimits((int)hist->getLowerLimit(),(int)hist->getUpperLimit(),
		hist->numBins());
   else
      strhist->setLimits(hist->getLowerLimit(),hist->getUpperLimit(),
		hist->numBins());

   lut_vector = lut->getAsArray();

   strhist->clear_noupdate();

   for ( i=0; i<hist->numBins(); i++) {

	//  Do the translation  to get a stretched histogram model

	//!!!! FIX THIS when Lut is updated to more than 8 bits !!!!
	// Assume Lut value of 0..255 maps to full histogram range.
	// Pick corresponding value out of LUT.
      str_i = ((lut->getUpperLimit()-lut->getLowerLimit()+1) * i) /
							hist->numBins();

      origTempValue = hist->getBin ( i );
      strTempValue = strhist->getBin ( lut_vector[str_i] );
      strhist->setBin ( lut_vector[str_i], origTempValue + strTempValue);
   }

   strhist->updateViews();

}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create CollectHist.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
////////////////////////////////////////////////////////////////
// CollectHist.cc: These subroutines can be used to fill in histogram
// model.  The caller should pass a pointer to existing histogram
// model object.
////////////////////////////////////////////////////////////////
#include "ImageDefs.h"
#include "ImageData.h"
#include "Histogram.h"
#include "CollectHist.h"
#include "ErrorDialogManager.h"
#include <iostream>
using namespace std;

void CollectHist(ImageData *imageModel, 
		Histogram *histR, Histogram *histG, Histogram *histB)
{
   // GET INFO THAT WILL HELP YOU CREATE BUFFERS AND LOOP THRU PIXEL DATA
   int pixelSize = imageModel->getPixelSize(); 	   // number of bytes per pixel
   int numberSamples = imageModel->getNumbSamples();
   int numberLines = imageModel->getNumbLines();
   ModeType mode = imageModel->getMode();

   int lineWidth = pixelSize * numberSamples;  	   // calc size of each buffer

   if (mode == COLORmode) { 
      histR->clear();
      getHistPtr(histR, imageModel, numberLines, numberSamples, lineWidth, RED);
      histG->clear();
      getHistPtr(histG, imageModel, numberLines, numberSamples,lineWidth,GREEN);
      histB->clear();
      getHistPtr(histB, imageModel, numberLines, numberSamples, lineWidth,BLUE);
   }
   else if (mode == BWmode) { 
      histR->clear();
      getHistPtr(histR, imageModel,numberLines,numberSamples,lineWidth,BWcolor);
      if (histG)
         histG->clear();
      if (histB)
         histB->clear();
   }
}

void getHistPtr (Histogram *hist, ImageData *data, 
		int numberLines, int numberSamples, int lineWidth,
		ColorType color)
{
   StatusType status;
   unsigned char * buffer;

   buffer = new unsigned char[lineWidth];

   // GET *ALL* LINES OF PIXELS FROM FILES (1 buffer for each band)
   // (NOTICE THAT EACH TIME THRU THE LOOP I USE THE SAME BUFFERS
   // YOU DON'T HAVE TO DO IT THIS WAY THOUGH

   for (int line = 0; line < numberLines; line++) {
   status = data->readLine(color, line, buffer);
      if (status != imSUCCESS) {
         if (!data->errorMsgIssued())
            theErrorDialogManager->post(data->getErrorMsg());
      }

      // COLLECT HISTOGRAM
      CollectHistLine(hist, buffer, numberSamples, data->getPixelType());


   }
   hist->updateViews();
}

#define LOOP(type)					\
   {							\
   type *buf = (type *)buffer;				\
   for (i=0; i<size; i++)				\
      hist->incBin(hist->getBinNumber(buf[i]));		\
   }

void CollectHistLine(Histogram *hist, unsigned char *buffer, int size,
		ImagePixelType type)
{
   int i;

   switch (type.get()) {
      case imBYTE:		// Special case for efficiency
         if (hist->getLowerLimit()==0 && hist->getUpperLimit()==255 &&
			hist->numBins() == 256) {
            imByte *buf = (imByte *)buffer;
            for (i=0; i<size; i++)
               hist->incBin(*buf++);
         }
         else {
            LOOP(imByte);
         }
         break;

      case imHALF:
         LOOP(imHalf);
         break;

      case imUHALF:
         LOOP(imUHalf);
         break;

      case imFULL:
         LOOP(imFull);
         break;

      case imUFULL:
         LOOP(imUFull);
         break;

      case imREAL:
         LOOP(imReal);
         break;

      case imDOUBLE:
         LOOP(imDouble);
         break;
   }
}

#define HISTSIZE 256

void CollectHist(int arrayR[HISTSIZE],int arrayG[HISTSIZE],int arrayB[HISTSIZE],
                Histogram *histR, Histogram *histG, Histogram *histB)
{
   if (arrayR && arrayG && arrayB) {
      histR->clear();
      getHistPtr(histR, arrayR, HISTSIZE);
      histG->clear();
      getHistPtr(histG, arrayG, HISTSIZE);
      histB->clear();
      getHistPtr(histB, arrayB, HISTSIZE);
   }
   else if (arrayR) {
      histR->clear();
      getHistPtr(histR, arrayR, HISTSIZE);
      if (histG)
         histG->clear();
      if (histB)
         histB->clear();
   }
}

void CollectHist(int *arrayR, int *arrayG, int *arrayB, int size, 
                Histogram *histR, Histogram *histG, Histogram *histB)
{
   if (arrayR && arrayG && arrayB) {
      histR->clear();
      getHistPtr(histR, arrayR, size);
      histG->clear();
      getHistPtr(histG, arrayG, size);
      histB->clear();
      getHistPtr(histB, arrayB, size);
   }
   else if (arrayR ) {
      histR->clear();
      getHistPtr(histR, arrayR, size);
      if (histG)
         histG->clear();
      if (histB)
         histB->clear();
   }
}

void getHistPtr (Histogram *hist, int *array, int size)
{
   if (size != hist->numBins()) {
      cerr << "Invalid CollectHist call, size="<<size<<", hist is "<<
						hist->numBins()<<endl;
      return;
   }
   for (int i = 0; i < size; i++) {
      hist->setBin(i, array[i]);
   }

   hist->updateViews();
}

$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create CollectHistBG.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
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

$ VOKAGLEVE
$ Return
$!#############################################################################
$Imake_File:
$ create histglue.imake
#define SUBROUTINE histglue
#define MODULE_LIST ImageToHistGlue.cc RawHistToStrHistGlue.cc \
 LutToStrHistGlue.cc \
 CollectStretchedHist.cc CollectHist.cc CollectHistBG.cc

#define GUI_SUBLIB

#define USES_C_PLUS_PLUS

#define LIB_GUI
#define LIB_MOTIFAPP
#define LIB_MOTIF
$ Return
$!#############################################################################
