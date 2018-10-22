////////////////////////////////////////////////////////////////
// VicarImageData.h
//
//	This is a subclass of ImageData.  It retrieves data
//	from VICAR formatted files.  
//
////////////////////////////////////////////////////////////////
#ifndef VICARIMAGEDATA_H
#define VICARIMAGEDATA_H
#include "ImageData.h"
#include "zvproto.h"

#ifndef NO_MP_ROUTINES
#include "mp_routines.h"
#else 
#define MP void*
#endif

#define MAXSTRINGSIZE   4000

class VicarImageFile;
class ZoomFactor;

enum VicarLabelType {V_LISTALL, V_DUMPALL, V_SYSTEM, V_PROPERTY, V_HISTORY};

class VicarImageData : public ImageData {

 protected:

   // INFO ABOUT EACH FILE  --- Vicar Specific

   int		_numbFiles;	// # of different physical files
   int		_numbChannels;	// # of logical channels, 3 for RGB or 1 for BW

   VicarImageFile *_files[MAX_FILES];	// ptrs to unique physical files
   VicarImageFile *_fileForChannel[MAX_CHANS];	// ptr copies for each chan
   int		_bandForChannel[MAX_CHANS];	// band for each channel

   // INIT VALUES IN FILES

   StatusType	initNumberOfLines();
   StatusType	initNumberOfSamples();
   StatusType	initPixelType();
   StatusType	initNumbers();		// inits bands,lines,samples, etc

   // FILE MANAGEMENT

   virtual StatusType addFile( char *, int);

   MP _mpObject;

 public:

   VicarImageData();
   virtual ~VicarImageData();

   // FILE ROUTINES:

   virtual StatusType open(char *);
   virtual StatusType close();	

   // INFORMATIONAL ONLY,	

   virtual StatusType getSuggestedUnzoomedTileSize(int &height, int &width);

   // OUTSIDE CONTROLS

   virtual ZoomFactor &calcTileZoom(ImageTile *tile,
				int Xin=1, int Xout=1, int Yin=1, int Yout=1,
				int Xsubpan=0, int Ysubpan=0);

   // READ DATA

   virtual StatusType readTile(int unzoomedStartSample, int unzoomedStartLine,
				int unzoomedWidth, int unzoomedHeight,
				ImageTile &tile);

   virtual StatusType readLine(ColorType color, int line,
				unsigned char * bufferPtr);

   virtual StatusType readPixel(ColorType color, int sampleOffset,
				int lineOffset, unsigned char * pixelBuffer);

   // outside access to the files
   virtual VicarImageFile *getVicarFile(int index);

   virtual int readMapProjInfo();
   virtual int lineSampToLatLon(double line, double samp, double *lat,
				double *lon, int type);
   virtual int latLonToLineSamp(double *line, double *samp, double lat,
				double lon, int type);

   virtual LongitudeDirection getLonDirection();

   // Label display
   int getFileNumb() { return _numbFiles; }
   virtual StatusType getLabelSetValue(char*& label, char *key, int *maxsize);
   StatusType parseLabelKey(char *key, int *fileIndex,
                    VicarLabelType *type, char *set, int *instance);
   virtual const char *const className () { return "VicarImageData"; }
};

#endif

