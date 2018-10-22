////////////////////////////////////////////////////////////////
// This is a *function* that is called by ImageDisplayView to fill
// the XvicImageData structure used by the image widget.
//
// CALL:
//	void getXvicImageDataStruct(ImageTile tile, XvicImageData &img);
//
// REFERENCE:
//	(1) Specifications for Imaging Widget
//
// COMMENTS:
//	(1) Notice that this code doesn't support 3 black and white bands.
//
// FUTURE:
//	(1) Needs more logic for memory management, below.
////////////////////////////////////////////////////////////////

#include "ImageDefs.h"
#include "ImageTile.h"
#include "XvicBasicImage.h"

void getXvicImageDataStruct(ImageTile &tile, XvicImageData &img)
{

   // Get pointers to each color's buffer

   ModeType mode = tile.getMode();
   if (mode == BWmode) {
      img.bw_pixels = tile.getBufferPtr(0);
      img.red_pixels = NULL;
      img.grn_pixels = NULL;
      img.blu_pixels = NULL;
   }
   else {
      img.bw_pixels = NULL;
      img.red_pixels = tile.getBufferPtr(0); 
      img.grn_pixels = tile.getBufferPtr(1);
      img.blu_pixels = tile.getBufferPtr(2);
   }

   // Get tile height & width
   img.height = tile.getHeight();
   img.width  = tile.getWidth();

   // Get the top left X/Y coordinate of the tile
   img.x = tile.getXcoordinate();
   img.y = tile.getYcoordinate();

   // Get flag describing buffer owner
   // ........ needs more logic for XvicMEMORY_WIDGET & MEMORY_SHARED.
   img.memory_control = XvicMEMORY_APPLIC;

   // Get line width and start offset
   img.line_width = tile.getLineWidth();
   img.start_offset = tile.getByteOffset();
}

