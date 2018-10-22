////////////////////////////////////////////////////////////////
// ImageDefs.h
////////////////////////////////////////////////////////////////
#ifndef IMAGEDEFS_H
#define IMAGEDEFS_H

enum StatusType	{ imFAILURE, imSUCCESS };
enum ColorType { UNDEFcolor, BWcolor, RED, GREEN, BLUE };
enum ModeType { UNDEFmode, BWmode, COLORmode, ERRORmode };
enum ImageType { UNDEFimage, VICARimage, PDSimage, PDS_VICARimage };


//enum LongitudeDirection { UNDEFdirection, EAST, WEST }; // ImageDefs.h maybe?

//enum	ZoomType { ZOOMspecial, ZOOM2fit, ZOOMnone };

// Bit flags to indicate only a part of the model changed
// More should probably be added in the future
#define IMAGE_DATA_UPDATE_RANGE 0x01

#endif

