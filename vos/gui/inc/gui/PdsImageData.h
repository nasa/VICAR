////////////////////////////////////////////////////////////////
// PdsImageData.h
//
//	This is a subclass of ImageData.  It retrieves data
//	from Pds formatted files.  
//
////////////////////////////////////////////////////////////////
#ifndef PDSIMAGEDATA_H
#define PDSIMAGEDATA_H

#include "ImageData.h"

extern "C" {
 #include "oal.h"
}

#include <Xm/Xm.h>

class ZoomFactor;

class PdsImageData : public ImageData {

 protected:

   // INFO ABOUT EACH FILE  --- Pds Specific

   int		_numbFiles;   // # of different physical files	
   int		_numbChannels;// #of logical channels, 3 for RGB or 1 for BW
   ODLTREE      _odlTree;

   OA_OBJECT _files[MAX_FILES];	            // hold the file handles (one/band)
   OA_OBJECT _fileForChannel[MAX_CHANS];    // ptr copies for each chan
   int	     _bandForChannel[MAX_CHANS];    // band for each channel

   // INIT VALUES IN FILES

   StatusType initNumbers();	   // inits bands,lines,samples, etc

   // FILE MANAGEMENT

   virtual StatusType addFile( char *, int);
   //Opens one band of PDS Image File
   virtual StatusType validateFile(OA_OBJECT);
   virtual void readDataRange();		    

 public:

   PdsImageData();
   virtual ~PdsImageData();

   // FILE ROUTINES:

   virtual StatusType open(char *);
   virtual StatusType close();	

   virtual StatusType getSuggestedUnzoomedTileSize(int &height, int &width);

   // READ DATA

   virtual StatusType readTile(int unzoomedStartSample, int unzoomedStartLine,
			       int unzoomedWidth, int unzoomedHeight,
			       ImageTile &tile);

   virtual StatusType readLine(ColorType color, int line,
			       unsigned char * bufferPtr);

   virtual StatusType readPixel(ColorType color, int sampleOffset,
				int lineOffset, unsigned char * pixelBuffer);
   
   virtual StatusType read1Tile1Band(int, int, int, int, int, ZoomFactor &,
				     unsigned char *, int);
   // Label display
   ImageLabel *buildLabelTree(ImageData *image, ODLTREE odltree, char *parentkey);
   virtual StatusType getLabelSetValue(char*& labels, char *key, int *maxsize);
   ODLTREE parseLabelKey(char *key);
   StatusType PdsPrintLabel(char*& labels, ODLTREE labelNode, int *maxsize);

   virtual const char *className() { return ("PdsImageData"); }
};

#endif

