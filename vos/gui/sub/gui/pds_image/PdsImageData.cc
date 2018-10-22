/////////////////////////////////////////////////////////////////
// PdsImageData.cc -- NOTE: _files[] in this class will be an
//                    array of OA_OBJECTs (ie; each cell is an
//                    image *handle* object)
////////////////////////////////////////////////////////////////

#include "PdsImageData.h"
#include "BasicComponent.h"
#include "ImageLabel.h"
#include "toolbox.h"
#include <iostream>
using namespace std;
#include <stdlib.h>
#include "defines.h"

extern "C" {
 #include "binrep.h"
}

const int MSG_SIZE  = 512;

static void flushLabelValue(char*& labels, char *labelbuf, int *maxsize);

////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////
PdsImageData::PdsImageData()
{
   int i;

   _numbFiles = 0;
   _numbChannels = 0;
   _odlTree = NULL;

   for (i=0; i<MAX_FILES; i++)
      _files[i] = NULL;

   for (i=0; i<MAX_CHANS; i++) {
      _fileForChannel[i] = NULL;
      _bandForChannel[i] = 0;
   }
}

////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////
PdsImageData::~PdsImageData()
{
    for (int i=0; i < _numbFiles; i++) {
	OaCloseImage(_files[i]);
    }
}

////////////////////////////////////////////////////////////////
// open
//	Creates 1 to 3 file objects.
//	stringOfFilenames argument can have 1 to 3 filenames
//	in a single string separated by commas or whitespace.  An optional
//	band number can follow a filename in parentheses (with no space).
//	Names that start with a "." but don't have any "/"s (or ":" or "]"
//	for VMS) in them are abbreviations meaning to take the previous
//	filename, strip off the extension (actually, everything after the
//	last period), and add the given extension.  If the band number is
//	not given, one is assigned automatically (the usual cases are three
//	files with one band each or one file with three bands).
//
// examples:
// "/usr/local/images/io.red,/usr/local/images/io.grn,/usr/local/images/io.blu"
// "/usr/local/images/io.red .grn  .blu"
// "nims.cube(3),nims.cube(10),nims.cube(33)"
// "/usr/local/images/big.img"	(three bands in one file)
// "/usr/local/images/big.img(2)"	(one band from above as bw)
// "/tmp/F_65N002.L"		(b&w image)
//
// VMS:
// "images:io.red images:io.grn images:io.blu"
// "disk:[mydata.project]file.red,.grn,.blu"
// "disk:[project.gll.nims]nims.cube(2), .cube(3), .cube(4)"
////////////////////////////////////////////////////////////////
StatusType PdsImageData::open(char *stringOfFilenames)
{
   StatusType status, return_status = imSUCCESS;
   char filename[2048];
   char *p, *input_string;
   int i;

   // Close previous files
   close();

   // Separate string into individual filenames and process each.

   _numbChannels = 0;		// 0==RED/BW, 1==GREEN, 2==BLUE
   _numbFiles = 0;
   for (i=0; i<MAX_FILES; i++)
      _files[i] = NULL;
   for (i=0; i<MAX_CHANS; i++) {
      _fileForChannel[i] = NULL;
      _bandForChannel[i] = 0;
   }
   input_string = stringOfFilenames;
   strcpy(filename, "");

   // Set up label tree root
   _labelTree = new ImageLabel(this, "LABELS", "pdslabeltreeroot");

   while (input_string && (strlen(input_string) > 0)) {

      int band = 0;

      // Search for comma or whitespace
      int len = strcspn(input_string, ", \t\n");

      // Check for ".grn" style abbreviation.  Must start with . and no /
      p = filename;
      if (*input_string == '.' && (int)strcspn(input_string,"/:]") >= len) {
         // Find the extension in the previous filename and replace it
         p = strrchr(filename, '.');	// is there a '.'?
         if (p == NULL)
            p = filename;		// no '.', so copy whole string
      }
      strncpy(p, input_string, len);
      *(p+len) = '\0';

      // Check for band number
      if ((p = strchr(filename, '('))) {	// don't bother checking for ')'
         band = atoi(p+1);
         *p = '\0';		// strip off the band
      }

      // Add it to the list.  If no band is given, several may be used

      status = addFile(filename, band);

      if (status != imSUCCESS)
         return_status = status;

      input_string += len;		// Skip this filename
      // Now skip past the comma (if present) and any whitespace on
      // either side.  But, we want to make sure that there is only one
      // comma skipped. (for degenerate cases like "file.red,,file.blu"
      // which may or may not work with everything else).
      input_string += strspn(input_string, " \t\n"); // Skip whitespace
      if (*input_string == ',')
         input_string++;
      input_string += strspn(input_string, " \t\n"); // Skip whitespace
   }

   // Init internal info based on label

   // Make sure we don't have empty channels at the end
   for (i=_numbChannels-1; i>=0; i--) {
      if (_fileForChannel[i] == NULL)
         _numbChannels--;
      else
         break;		// found a non-empty one
   }

   if (_numbChannels == 0)
      _mode = UNDEFmode;
   else if (_numbChannels == 1)
      _mode = BWmode;
   else {
      _mode = COLORmode;
      _numbChannels = 3; // In case we *do* have empty channels at the end
   }

   status = initNumbers();
   if (status != imSUCCESS)
      return_status = status;

   strcpy(_inputDataSourceName, stringOfFilenames);

   // Load the map projection information, if any:
   readMapProjInfo();

   initDataRange();

   updateViews();

   return (return_status);
}

////////////////////////////////////////////////////////////////
// close
////////////////////////////////////////////////////////////////
StatusType PdsImageData::close()
{
    int i;
    
    // close all the bands (files) we've opened

    for (i=0; i < _numbFiles; i++) {
	OaCloseImage( _files[i] );
	_files[i] = NULL;
    }
   for (i=0; i<MAX_CHANS; i++ )
      _fileForChannel[i] = NULL;
    
    _numbFiles = 0;
    _numbChannels = 0;
    strcpy(_inputDataSourceName,"\0");
    _dataSourceOpened = FALSE;
    
    _errorMsgIssued = False;	// Clear the error so it can be reissued
    if (_errorMsg)
        delete []_errorMsg;
    _errorMsg = NULL;

    return imSUCCESS;
    return imSUCCESS; // OaCloseImage doesn't return a value
}

////////////////////////////////////////////////////////////////
// addFile (Private)
//	Called by open for each new filename.  If a band is given,
//	add that file/band combo to the list.  If no band is given
//	(i.e. it's 0), add as many bands as needed (and available)
//	from the file to fill up the color slots.  An empty filename
//	(not NULL but 0-length) will simply set a NULL for the
//	file pointer and a 0 for the band.
////////////////////////////////////////////////////////////////
StatusType PdsImageData::addFile(char *filename, int band)
{
   StatusType return_status = imSUCCESS;
   ODLTREE odlTree, image_node = NULL;
   
   char msg[MSG_SIZE];

   if (filename == NULL)
      return imFAILURE;

   if (_numbChannels >= MAX_CHANS) {
      if (strlen(filename) > 0) {
         sprintf( msg, "Too many filenames at:  %s  \n", filename );
         setErrorMsg(msg);
         cerr << msg;
         return imFAILURE;
      }
      return imSUCCESS;
   }

   if (strlen(filename) == 0) {
      _fileForChannel[_numbChannels] = NULL;
      _bandForChannel[_numbChannels] = 0;
      _numbChannels++;
      return imSUCCESS;
   }
  
   // See if the file matches one already used

   /*
   for (i = 0; i<_numbFiles; i++) {
     if (_files[i] && 
	 _files[i]->odltree->file_name &&
	 strcmp(_files[i]->odltree->file_name, filename) == 0) {
         _fileForChannel[_numbChannels] = _files[i];
	 // !!!! check how VicarImageData does this
	 _files[i+1] = _files[i];
         break;
      }
   }
  */ 
   if (_fileForChannel[_numbChannels] == NULL) {

    odlTree = OaParseLabelFile(filename, NULL, ODL_EXPAND_STRUCTURE, TRUE);
    odlTree = OaConvertLabel(odlTree);
    
    if (odlTree == NULL) {
	sprintf(msg, "Unable to read PDS label file: %s\n", filename);
	setErrorMsg(msg);
	cerr << msg;
	return imFAILURE;   // couldn't handle the label file
    }

    // Assign member variable to the first valid odlTree
    // So all label processing later on will be done
    // using first files odlTree.
    if (_odlTree == NULL)
	_odlTree = odlTree;

    // find the image node within the tree
    
    image_node = OdlFindObjDesc(odlTree, (char *)"IMAGE", NULL, NULL, 0, 
				 ODL_RECURSIVE_DOWN);
    if (image_node == NULL) {
	sprintf(msg, "Unable to find PDS image node in file: %s\n", 
		filename);
	setErrorMsg(msg);
	cerr << msg;
	return imFAILURE;   // couldn't find the image node
    }

    // Open and Validate PDS Image File
    // Note that unlike VICAR, PDS can open only
    // one band at a time.  Also bands are 1-based.
    if (band)
        _files[_numbFiles] = OaOpenImage(image_node, band);
    else  // band was not specified
       _files[_numbFiles] = OaOpenImage(image_node, 1);
 
    if (validateFile(_files[_numbFiles]) == imFAILURE) {
        sprintf(msg,"Unable to open PDS image: %s band: %d\n", filename, band);
	setErrorMsg(msg);
	cerr << msg;
    }
    else
        _dataSourceOpened = True;

    // Read in all labels and build the internal label tree
    ImageLabel *UNUSED(lTree) = buildLabelTree(this, _odlTree, (char *)"0");

      _fileForChannel[_numbChannels] = _files[_numbFiles];
      _numbFiles++;
   }

   if (band != 0) {		// We're given a band     
      if (_fileForChannel[_numbChannels]->appl1 &&
	  (band <= 
           ((oa_image_handle*)(_fileForChannel[_numbChannels]->appl1))->bands))
         _bandForChannel[_numbChannels] = band;
      else {
         sprintf(msg, "Invalid band (%d), 1 assumed, for file:  %s\n",
					band, filename);
         setErrorMsg(msg);
         cerr << msg;
         _bandForChannel[_numbChannels] = 1;
         return_status = imFAILURE;
      }
   }
   else {				// Not given a band, figure it out
      _bandForChannel[_numbChannels] = 1;
      int bandsLeft = 
	(((oa_image_handle*)(_fileForChannel[_numbChannels]->appl1))->bands)-1;

      // If we're not full of channels and there are bands left in this,
      // file, copy the file to the next channel and increment the band

      while (_numbChannels < MAX_CHANS-1 && bandsLeft > 0) {
         _numbChannels++;
         bandsLeft--;

	 _files[_numbChannels] = OaOpenImage(image_node, _numbChannels+1);
	// assign the correct number of lines
         _files[_numbFiles]->odltree->first_keyword->right_sibling->value =  
	                  image_node->first_keyword->right_sibling->value;
	 if (validateFile(_files[_numbChannels]) == imFAILURE) {
	     sprintf(msg,"Unable to Open or Read PDS image: %s band: %d\n", 
		     filename, band);
	     setErrorMsg(msg);
	     cerr << msg; 
	     return imFAILURE;
	 }

	 _fileForChannel[_numbChannels] = _files[_numbChannels];
         _bandForChannel[_numbChannels] = _bandForChannel[_numbChannels-1] + 1;
      }
   }

   _numbChannels++;

   return (return_status);
}
////////////////////////////////////////////////////////////////
//  Given OA_OBJECT *handle* check that it's not NULL and
//  perform a test to confirm that OAL really is able to
// read the file.
////////////////////////////////////////////////////////////////
StatusType PdsImageData::validateFile(OA_OBJECT file)
{

    if (file == NULL) {
	return imFAILURE;
    }

    // perform a test to be sure that OAL really be able to read the file
    OA_OBJECT dummy_image;
    dummy_image = OaReadPartialImage(file, 1, 1, 1, 1);
    if (dummy_image == NULL)
        return imFAILURE;
    else
        OaCloseImage(dummy_image);

    return imSUCCESS;
}
////////////////////////////////////////////////////////////////
// initNumbers (Private)
// Assigns the number of samples in image to the file with
// the largest number of samples per line.
// Assigns the number of lines in image to the file with
// the largest number of lines per image.
// Assigns the pixel type to the first
// file's pixel type. Reports error if type is different
// for any other file.
////////////////////////////////////////////////////////////////

StatusType PdsImageData::initNumbers()
{
    int enc, bandOrgType; 
    long pre, suf, numLines, numSamples, sample_bits, numChannels;
    char *samp_type;

    int numbSamples, numbLines;
    char msg[MSG_SIZE];
    DataType pixel_type;
    StatusType return_status = imSUCCESS;
	
   // initialize member vars.
   _numberOfLines = 0;
   _numberOfSamples = 0;


   for (int i=0; i< _numbFiles; i++ ) {
       // Read Image attributes from PDS File
       if (!_files[i])
	   return imFAILURE;
       ODLTREE image_node = OdlFindObjDesc((_files[i]->odltree),
					   (char *)"IMAGE", NULL,
					   NULL, (unsigned long)0, 
					   (unsigned short)ODL_RECURSIVE_DOWN);
       //image_node = OaConvertLabel(image_node);
       if (!image_node)
	   return imFAILURE;
       if (OaGetImageKeywords(image_node, &numLines, &numSamples,
			      &sample_bits, &samp_type, &numChannels,  
			      &bandOrgType,  &pre, &suf, &enc))
	 return imFAILURE;
       OaKwdValuetoLong((char *)"SOURCE_LINES", _files[i]->odltree, &numLines);
       OaKwdValuetoLong((char *)"SOURCE_LINE_SAMPLES",_files[i]->odltree,
								&numSamples);

       // Assigns the number of samples in image to the file with
       // the largest number of samples per line.   
       if (i == 0) _numberOfSamples = numSamples;
       numbSamples = numSamples;
       if (_numberOfSamples != numbSamples) {
	   _inconsistentImage = TRUE;
	   sprintf(msg,
	    "Number of Samples in file#: %d  doesn't match previous file(s)\n",
		   i);
	   setErrorMsg(msg);
	   cerr << msg;
	   return_status = imFAILURE;
       }
       if ( _numberOfSamples < numbSamples)  
	   _numberOfSamples = numbSamples;
       
       // Assigns the number of lines in image to the file with
       // the largest number of lines per image.
       if (i == 0) _numberOfLines = numLines;
       numbLines = numLines;
       if (_numberOfLines != numbLines) {
	   _inconsistentImage = TRUE;
	   sprintf(msg,
	      "Number of Lines in file#: %d  doesn't match previous file(s)\n",
		   i);
	   setErrorMsg(msg);
	   cerr << msg;
	   return_status = imFAILURE;
       }
       if ( _numberOfLines < numbLines)  
	   _numberOfLines = numbLines;
       
       // Get the data type from the PDS library.  This is amazingly ugly
       // for such a simple, necessary operation!
       // We don't have to worry about endian-ness because the library should
       // convert things to the native format when read.
       
       int pds_type = OaStrtoPDSDataType(samp_type, 
					 OA_BINARY_INTERCHANGE_FORMAT);
       struct oa_type_conversion_info *type_conv =
	   OalGetTypeConversionFromProfile(pds_type, sample_bits / 8);
       struct binrep_desc *binrep = OalFindBinrepDescrip(
					      type_conv->native_binrep_q_code);
       
       // Now we can simply look at binrep->type and binrep->bytes.
       
       pixel_type = imBYTE;		// default if we don't recognize things
       
       if (binrep->type == BINREP_FLOAT) {
	   if (binrep->bytes == 4)
	       pixel_type = imREAL;
	   else if (binrep->bytes == 8)
	       pixel_type = imDOUBLE;
       }
       else if (binrep->type == BINREP_INTEGER) {// shouldn't be anything else
	   if (binrep->bytes == 1)
	       pixel_type = imBYTE;
	   else if (binrep->bytes == 2)
	       pixel_type = imHALF;
	   else if (binrep->bytes == 4)
	       pixel_type = imFULL;
       }
       
       // Assigns the pixel type to the first
       // file's pixel type.
       if (i == 0) _pixelType.set(pixel_type);
       else {
	   if (pixel_type != _pixelType) {
	       _inconsistentImage = TRUE;
	       sprintf(msg,
	      "Pixel format in file#: %d  doesn't match previous file(s)\n",i);
	       setErrorMsg(msg);
	       cerr << msg;
	       return_status = imFAILURE;
	   }
       }
   }
   return return_status;
}
////////////////////////////////////////////////////////////////
// From widget docs
////////////////////////////////////////////////////////////////
static inline int IDIV(int x, int y)
{ 
    return ((x)>0 ? (x)/(y) : ((x)-(y)+1)/(y)); 
}

////////////////////////////////////////////////////////////////
// readTile -- required by ImageData
////////////////////////////////////////////////////////////////
StatusType PdsImageData::readTile(int startSampleOffset,
				  int startLineOffset,
				  int width, 
				  int height,
				  ImageTile &tile)
{
    StatusType status, return_status;
    return_status = imSUCCESS;
    
    ZoomFactor &tileZoom = tile.getTileZoom();
    
    if (_mode != COLORmode && _mode != BWmode)
	return imFAILURE;
    
    // loop through each band and get the ptr to the pixel data
    for (int i = 0; i < _numbChannels; i++) {
	
	unsigned char *bufferPtr = tile.getBufferPtr(i);
    
	if (_files[i] != NULL) {
	    
	    status = read1Tile1Band(i, startLineOffset, height,
				    startSampleOffset, width,
				    tileZoom, bufferPtr, tile.getLineWidth());
	    if (status == imFAILURE) {
	      setErrorMsg( "PdsImageData::readTile - error!");
	      

	      //		cerr << "PdsImageData::readTile(): ";
	      //	cerr << "Problem reading band number "<<i+1<<endl;
		return_status = imFAILURE;
	    }
	}
	else
	    memset(bufferPtr, 0, tile.getBufferSize());
	
    } // end for
    
    
    // Set the current start line & sample in tile object
    
    tile.setStartLineSample( startLineOffset, startSampleOffset);
    
    return return_status;
} 

////////////////////////////////////////////////////////////////
// read1Tile1Band() - read only from the specified band
////////////////////////////////////////////////////////////////
StatusType PdsImageData::read1Tile1Band(int band,
					int startLineOffset,
					int height,
					int startSampleOffset,
					int width,
					ZoomFactor &tileZoom,
					unsigned char *bufferPtr, 
					int buffer_width)
{
    unsigned char *tempBuf;
    StatusType return_status = imFAILURE;
    long prezoomLineStart;
    long prezoomLineEnd;
    long line;
    long startSample, endSample;
    long nsamps;          // amount to read, not size of buf
    int extra_pixels_on_line = 0;
    
    OA_OBJECT image_object = NULL;
    
    startSample = startSampleOffset + 1;
    endSample = (startSampleOffset + width);
    
    nsamps = (endSample - startSampleOffset);
    
    int ZoomYin = tileZoom.getYIn();
    int ZoomYout = tileZoom.getYOut();
    int subPixelPanY = tileZoom.getSubPixelPanY();
    if (ZoomYout < ZoomYin) {
	ZoomYout = 1;
	ZoomYin = 1;
    }
    
    // Calculate prezoom coordinates from formulas in widget docs
    
    prezoomLineStart=
	IDIV((startLineOffset * ZoomYin - subPixelPanY + ZoomYout-1),
	     ZoomYout);
    prezoomLineEnd=
	IDIV(((startLineOffset + height) * ZoomYin - subPixelPanY - 1),
	     ZoomYout);
    
    // Read one line at a time, applying the zoom factor as we go
    
    return_status = imSUCCESS;
    
    for (int i = prezoomLineStart; i <= prezoomLineEnd; i++ ) {
    
	// Convert from prezoom to unzoomed coordinates
	
	line = IDIV((i * ZoomYout + subPixelPanY), ZoomYin) + 1;
	
	if ( nsamps > 0 && line > 0 && line <= _numberOfLines) {
	    
	    image_object = OaReadPartialImage(_files[band],
					      line,
					      line,
					      startSample,
					      endSample );
	    
	    if (image_object != NULL) {
	      
	      tempBuf = (unsigned char *)image_object->data_ptr;
	      memcpy ( (void *) bufferPtr, (void *) tempBuf, 
		       nsamps*_pixelType.getPixelSize() );

	      OaDeleteObject( image_object );
	      
	      if (extra_pixels_on_line) {
		memset( (void*) (bufferPtr + 
				 nsamps*_pixelType.getPixelSize()), 0,
			extra_pixels_on_line * _pixelType.getPixelSize() );
	      }
	    }
	    else {

#ifdef DEBUG
	      cerr << "PdsImageData::read1Tile1Band() :";
	      cerr << "image_object is NULL!" << endl;
#endif
	      return_status = imFAILURE;
	    }
	}      
	
	if ( image_object == NULL ) {
#ifdef DEBUG
	  cerr << "PdsImageData::read1Tile1Band() :";
	  cerr << "Error reading line "<< line << ", band "<<band +1;
	  cerr << " ; image_object is NULL." << endl;
#endif
	  memset((void *)bufferPtr, 0, width * _pixelType.getPixelSize());
	  return_status = imFAILURE;
	}
	
	bufferPtr += buffer_width;
	
    }// end for
    
    return return_status; 
}


////////////////////////////////////////////////////////////////
// readLine -- required by ImageData
////////////////////////////////////////////////////////////////
// NOTE: ColorType is an enum:  UNDEFcolor, BWcolor, RED, GREEN, BLUE
StatusType PdsImageData::readLine(ColorType color, int line,
				  unsigned char *bufferPtr)
{
    StatusType status = imFAILURE;
    ZoomFactor zoom;
    int index;
    
    switch (color) {
    case RED:
	index = 0;
	break;
    case GREEN:
	index = 1;
	break;
    case BLUE:
	index = 2;
	break;
    case BWcolor:
	index = 0;
	break;
    default:
	return imFAILURE;              // oops!
    }
    
    if (_files[index] != NULL) {
	// read a line from the specified band ONLY
	status = read1Tile1Band( index, line, 1, 0, _numberOfSamples, zoom, 
				 bufferPtr, 0 );
	
	if (status == imFAILURE) {
#ifdef DEBUG
	    cerr << "PdsImageData::readLine: ";
	    cerr << "Error reading from band " << index+1 << endl;
#endif
	    return imFAILURE;
	}
    }  
    else {
	memset(bufferPtr, 0, _numberOfSamples * _pixelType.getPixelSize());
	status = imSUCCESS;
    }
    
    return status;
}

////////////////////////////////////////////////////////////////
// readPixel -- required by ImageData
////////////////////////////////////////////////////////////////
StatusType PdsImageData::readPixel(ColorType color,
				   int sampleOffset,
				   int lineOffset,
				   unsigned char *pixelBuffer)
{
    StatusType status = imFAILURE;
    ZoomFactor zoom;     // Zoom will default to no zoom
    int index;
    
    switch (color) {
    case RED:
	index = 0;
	break;
    case GREEN:
	index = 1;
	break;
    case BLUE:
	index = 2;
	break;
    case BWcolor:
	index = 0;
	break;
    default:
	return imFAILURE;              // oops!
    }
    
    if ( _files[index] != NULL ) {
	status = read1Tile1Band( index, lineOffset, 1, sampleOffset, 1, zoom,
				 pixelBuffer, 0 );
	if ( status == imFAILURE ) {
#ifdef DEBUG
	    cerr << "PdsImageData::readPixel: ";
	    cerr << "Error reading pixel from band "<< index+1 << endl;
#endif
	}
    }
    else {
	memset(pixelBuffer, 0, _pixelType.getPixelSize());
	status = imSUCCESS;
    }
    return status;
}

////////////////////////////////////////////////////////////////
// readDataRange
//	Determine min/max range of data values in the file.  Default
//	behavior is to read the entire file to determine this.
//	Should be overridden by any subclass that has data range info
//	easily available (e.g. from a label instead of reading all
//	the data).
////////////////////////////////////////////////////////////////
void PdsImageData::readDataRange()
{
    unsigned char *buffer;
    int line;
    StatusType status = imSUCCESS;
    
    if (_pixelType.get() == imBYTE) {
	_fileMinDataRange = _pixelType.getMinDataRange();
	_fileMaxDataRange = _pixelType.getMaxDataRange();
	_fileDataRangeValid = True;
	return;
    }
    
    _fileDataRangeValid = False;
    buffer = new unsigned char[_pixelType.getPixelSize() * _numberOfSamples];
    if (buffer == NULL) {
	return;			// valid is False
    }
    
    _fileMinDataRange = _pixelType.getMaxDataRange();
    _fileMaxDataRange = _pixelType.getMinDataRange();
    
    for (line=0; line<_numberOfLines; line++) {
	if (_mode == COLORmode) {
	    status = readLine(RED, line, buffer);
	    if (status != imSUCCESS)
		break;
	    findMaxMinInLine(buffer);
	    status = readLine(GREEN, line, buffer);
	    if (status != imSUCCESS)
		break;
	    findMaxMinInLine(buffer);
	    status = readLine(BLUE, line, buffer);
	    if (status != imSUCCESS)
		break;
	    findMaxMinInLine(buffer);
	}
	else if (_mode == BWmode) {
	    status = readLine(BWcolor, line, buffer);
	    if (status != imSUCCESS)
		break;
	    findMaxMinInLine(buffer);
	}
	else {
	    status = imFAILURE;
	    break;			// unknown mode
	}
    }
    if (status == imSUCCESS)
	_fileDataRangeValid = True;
    
}

StatusType PdsImageData::getSuggestedUnzoomedTileSize(int &height, int &width)
{
   StatusType result = imFAILURE;

   if (_numbChannels >= 0 && _numbChannels <= MAX_CHANS) result=imSUCCESS;
   height = 100;
   if ( height > _numberOfLines ) height = _numberOfLines;
   width = _numberOfSamples;
   return (result);
}

////////////////////////////////////////////////////////////////
// Construct the internal label tree
////////////////////////////////////////////////////////////////
ImageLabel *PdsImageData::buildLabelTree(ImageData *image, ODLTREE odlTree,
                                         char *parentkey)
{
   ODLTREE obj;
   char name[MAX_IMAGE_LABEL_STRING_SIZE+1]; // Identifier for a ImageLabel set
   char key[MAX_IMAGE_LABEL_STRING_SIZE+1]; // Identifier for a ImageLabel set
   ImageLabel *objLabel;
   ImageLabel *treeLabel, *allLabel;
   int subset = 0;
   int numbChild;

   name[0] = '\0';
   key[0] = '\0';

   strcpy(name, odlTree->class_name);
   strcpy(key, parentkey);

   if (odlTree->parent) 
      treeLabel = new ImageLabel(image, name, key);
   else
      treeLabel = _labelTree;
   
   // add label node for ALL
   allLabel = new ImageLabel(image, "All", key);
   treeLabel->addChild(allLabel);

   // subset = OdlGetObjDescChildCount(odlTree);
   // temp fix for (odl?) error where first/last child is not nil
   // when child_count is indeed zero
   if (odlTree)
      subset = odlTree->child_count;
   obj = odlTree->first_child;
   for (int i=0; i<subset; i++) {
      if (obj != NULL) {
         strcpy(name, OdlGetObjDescClassName(obj));
         sprintf(key, "%s/%d", parentkey, i);
         // numbChild = OdlGetObjDescChildCount(obj);
         if (obj) 
            numbChild = obj->child_count;
         if (numbChild > 0) 
            objLabel = buildLabelTree(image, obj, key); 
         else
            objLabel = new ImageLabel(image, name, key); 
         treeLabel->addChild(objLabel);
         obj = obj->right_sibling;
      }
   }

   return treeLabel;
}

////////////////////////////////////////////////////////////////
// Read in a label set
////////////////////////////////////////////////////////////////
StatusType PdsImageData::getLabelSetValue(char*& labels, char *key,
                                          int *maxsize) 
{
   ODLTREE labelNode;
   StatusType status;
   int lastKeyUsed = 0;
   char msg[132];

   if (key == NULL) {
      if (_lastKey) {
         key = strdup(_lastKey);
         lastKeyUsed = 1;
      }
      else
         key = strdup("0");
   }

   labelNode = parseLabelKey(key);
   status = PdsPrintLabel(labels, labelNode, maxsize);

   // Use the default key (0 -- all) if lastkey yields nil labels
   if (!strcmp(labels, "") && lastKeyUsed)
      status = PdsPrintLabel(labels, _odlTree, maxsize);

   // If unable to read label, print error
   if ( status == imFAILURE ) {
      sprintf( msg, "Unable to read label.\n");
      setErrorMsg(msg);
      cerr << msg;
   }

   strcpy(_lastKey, key);

   return status;
}

////////////////////////////////////////////////////////////////
// Get the odl tree node from a label key
// key has the format of odltreeroot/index of subtree at level #i/
// i = 1,2,3... max level numbers of theis odltree.
////////////////////////////////////////////////////////////////
ODLTREE PdsImageData::parseLabelKey(char *key)
{
   ODLTREE obj;
   char *ptr;
   char *thiskey, *element;

   if (!key) return NULL;
   thiskey = new char[strlen(key)+1];
   strcpy(thiskey, key);
   thiskey[strlen(thiskey)] = '\0';

   obj = _odlTree;
   ptr = strchr(thiskey, '/');
   if (ptr) {
      int str_len = strlen(thiskey) - strlen(ptr);
      element = new char [str_len + 1];
      strncpy(element, thiskey, str_len);
      element[str_len] = '\0';    // strncpy doesn't terminate string
      thiskey = ptr + 1;            // move past the separator
   } else {
      element = strdup(thiskey);
      thiskey = NULL;
   }

   while (thiskey != NULL) {
      if (obj == NULL) break;
      obj = obj->first_child;
      ptr = strchr(thiskey, '/');
      if (ptr) {
         int str_len = strlen(thiskey) - strlen(ptr);
         element = new char [str_len + 1];
         strncpy(element, thiskey, str_len);
         element[str_len] = '\0';    // strncpy doesn't terminate string
         thiskey = ptr + 1;            // move past the separator
      } else {
         element = strdup(thiskey);
         thiskey = NULL;
      }
      if (element[0] < '0' || element[0] > '9')  // not a number
         return NULL;
      int subset = atoi(element);
      if (subset==0) continue;
      for (int i=0; i<subset; i++) {
         obj = obj->right_sibling;
      } 
   }

   return obj;
}

////////////////////////////////////////////////////////////////
// Read in all labels under the labelNode
////////////////////////////////////////////////////////////////
StatusType PdsImageData::PdsPrintLabel(char*& labels, ODLTREE labelNode,
                                       int *maxsize)
{
   char name[MAX_IMAGE_LABEL_STRING_SIZE+1];  // key name
   char value[MAX_IMAGE_LABEL_ITEM_SIZE+1]; // key value
   char pre_comment[MAX_IMAGE_LABEL_ITEM_SIZE+1]; // pre-comment
   char thislabel[MAX_IMAGE_LABEL_ITEM_SIZE+1]; // one label item
   char buf[MAX_IMAGE_LABEL_ITEM_SIZE+1]; // one line of label
   KEYWORD *kwdptr;
   ODLTREE childNode;
   char *blanks = NULL;

   thislabel[0] = '\0';
   if (labelNode == NULL)
      return imFAILURE;

   blanks = new char[2*labelNode->level+1];
   if (blanks == NULL) {
      cerr << "Out of memory!!!" << endl;
   }

   blanks[0] = '\0';
   for (int i=0; i < (int)labelNode->level; i++) 
      strcat(blanks, "  ");
   blanks[2*labelNode->level] = '\0';
   
   if (labelNode->pre_comment != NULL) {
      sprintf(buf, " %s\n", labelNode->pre_comment);
      flushLabelValue(labels, buf, maxsize);
   }

   if (labelNode->parent != NULL) {
      if (labelNode->class_name == NULL) 
         sprintf(buf, "%sOBJECT", blanks);
      else 
         sprintf(buf, "%sOBJECT = %s", blanks, labelNode->class_name);

      strcpy(thislabel, buf);

      if (labelNode->line_comment != NULL) {
         sprintf(buf, " %s\n", labelNode->line_comment);
         strcat(thislabel, buf);
      }
      flushLabelValue(labels, thislabel, maxsize);
   }

   kwdptr = OdlGetFirstKwd(labelNode);

   while (kwdptr) {
      strcpy(name, OdlGetKwdName(kwdptr));
      strcpy(value, OdlGetKwdValue(kwdptr));
      strcpy(pre_comment, OdlGetKwdPreComment(kwdptr));
      if (strcmp(pre_comment, "")) {
	  sprintf(buf, "%s \n",pre_comment);
	  flushLabelValue(labels, buf, maxsize);
      }
      sprintf(buf, "  %s %s  =  %s \n", blanks, name, value);
      flushLabelValue(labels, buf, maxsize);
      kwdptr = OdlGetNextKwd(kwdptr);
   }


   // int numbChild = OdlGetObjDescChildCount(labelNode);
   int numbChild = labelNode->child_count;
   if (numbChild > 0) {
      childNode = labelNode->first_child;
      PdsPrintLabel(labels, childNode, maxsize);
      for (int i=1; i<numbChild; i++) {
         childNode = childNode->right_sibling;
         PdsPrintLabel(labels, childNode, maxsize);
      }
   }
   
   if (labelNode->post_comment != NULL) {
      sprintf(buf, " %s\n", labelNode->post_comment);
      flushLabelValue(labels, buf, maxsize);
   }

   if (labelNode->parent != NULL) {
      if (labelNode->class_name == NULL) 
         sprintf(buf, "%sEND_OBJECT", blanks);
      else 
         sprintf(buf, "%sEND_OBJECT = %s", blanks, labelNode->class_name);
      strcpy(thislabel, buf);

      if (labelNode->end_comment != NULL) {
         sprintf(buf, " %s\n", labelNode->end_comment);
         strcat(thislabel, buf);
      }
      flushLabelValue(labels, thislabel, maxsize);
   }

   if (labelNode->parent == NULL) {    // at the root lavel
      sprintf(buf, "END\n");
      flushLabelValue(labels, buf, maxsize);
   }


   delete [] blanks;

   return imSUCCESS;
}

////////////////////////////////////////////////////////////////
//  subroutine to flush label buffer
////////////////////////////////////////////////////////////////
static void flushLabelValue(char*& labels, char *labelbuf, int *maxsize)
{
   // if maxsize is NULL, do not check to allocate memory
   if (maxsize) {
      if (*maxsize != 0) {    // check to dynamically allocating memory
         while (((int)strlen(labels) + (int)strlen(labelbuf) + 1) >= *maxsize) {
            *maxsize = 2*(*maxsize);
            labels = (char *)realloc((char *)labels, *maxsize);
         }
      }
      else {
         *maxsize = MAX_IMAGE_LABEL_SIZE+1;
         if (((int)strlen(labelbuf)+1) > *maxsize)
            *maxsize = strlen(labelbuf) + 1;
         labels = new char[*maxsize];
         labels[0] = '\0';
      }
   }

   if (strlen(labelbuf) != 0) {
      strcat(labels, labelbuf);
      labelbuf[0]='\0';
   }
}

