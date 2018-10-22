////////////////////////////////////////////////////////////////
// SiCollectHist.cc: These subroutines can be used to fill in histogram
// model.  The caller should pass a pointer to existing histogram
// model object.
////////////////////////////////////////////////////////////////
#include "ImageDefs.h"
#include "ImageData.h"
#include "SiHistogram.h"
#include "SiCollectHist.h"
#include "ErrorDialogManager.h"
#include <iostream>
using namespace std;

void SiCollectHist(ImageData *imageModel, 
		SiHistogram *histR, SiHistogram *histG, SiHistogram *histB)
{
   // GET INFO THAT WILL HELP YOU CREATE BUFFERS AND LOOP THRU PIXEL DATA
   int pixelSize = imageModel->getPixelSize(); 	   // number of bytes per pixel
   int numberSamples = imageModel->getNumbSamples();
   int numberLines = imageModel->getNumbLines();
   ModeType mode = imageModel->getMode();

   int lineWidth = pixelSize * numberSamples;  	   // calc size of each buffer

   if (mode == COLORmode) { 
      histR->clear();
      siGetHistPtr(histR, imageModel, numberLines, numberSamples, lineWidth, RED);
      histG->clear();
      siGetHistPtr(histG, imageModel, numberLines, numberSamples,lineWidth,GREEN);
      histB->clear();
      siGetHistPtr(histB, imageModel, numberLines, numberSamples, lineWidth,BLUE);
   }
   else if (mode == BWmode) { 
      histR->clear();
      siGetHistPtr(histR, imageModel,numberLines,numberSamples,lineWidth,BWcolor);
      if (histG)
         histG->clear();
      if (histB)
         histB->clear();
   }
}

void siGetHistPtr (SiHistogram *hist, ImageData *data, 
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
      SiCollectHistLine(hist, buffer, numberSamples, data->getPixelType());


   }
   hist->updateViews();
}

#define LOOP(type)					\
   {							\
   type *buf = (type *)buffer;				\
   for (i=0; i<size; i++)				\
      hist->incBin(hist->getBinNumber(buf[i]));		\
   }

void SiCollectHistLine(SiHistogram *hist, unsigned char *buffer, int size,
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

/////////////////////////////////////////////////////////////////////////////
// SiCollectHist: Fill in 8 bit image histogram given an array of 256 values.
/////////////////////////////////////////////////////////////////////////////

#define HISTSIZE 256

void SiCollectHist(int arrayR[HISTSIZE],int arrayG[HISTSIZE],int arrayB[HISTSIZE],
                SiHistogram *histR, SiHistogram *histG, SiHistogram *histB)
{
   if (arrayR && arrayG && arrayB) {
      histR->clear();
      siGetHistPtr(histR, arrayR, HISTSIZE);
      histG->clear();
      siGetHistPtr(histG, arrayG, HISTSIZE);
      histB->clear();
      siGetHistPtr(histB, arrayB, HISTSIZE);
   }
   else if (arrayR) {
      histR->clear();
      siGetHistPtr(histR, arrayR, HISTSIZE);
      if (histG)
         histG->clear();
      if (histB)
         histB->clear();
   }
}

void SiCollectHist(int *arrayR, int *arrayG, int *arrayB, int size, 
                SiHistogram *histR, SiHistogram *histG, SiHistogram *histB)
{
   if (arrayR && arrayG && arrayB) {
      histR->clear();
      siGetHistPtr(histR, arrayR, size);
      histG->clear();
      siGetHistPtr(histG, arrayG, size);
      histB->clear();
      siGetHistPtr(histB, arrayB, size);
   }
   else if (arrayR ) {
      histR->clear();
      siGetHistPtr(histR, arrayR, size);
      if (histG)
         histG->clear();
      if (histB)
         histB->clear();
   }
}

void siGetHistPtr (SiHistogram *hist, int *array, int size)
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
