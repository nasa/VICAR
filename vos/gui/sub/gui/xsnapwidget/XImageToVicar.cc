////////////////////////////////////////////////////////////////////////
// XImageToVicar - function (not class) which uses XImageHandler to write
// to a VICAR file.  This function is in a separate module to isolate
// VICAR dependencies.
////////////////////////////////////////////////////////////////////////

#include "XImageToVicar.h"
#include "XImageHandler.h"
#include "zvproto.h"

////////////////////////////////////////////////////////////////////////
// Write the given part of the XImage structure into a set of three
// VICAR files (in color).  Width==0 or height==0 means the entire
// width or height of the XImage.  X and Y are 0-based (image starts at 0,0).
////////////////////////////////////////////////////////////////////////

void writeXImageToVicarFile(XImageHandler *img,
		char *red_file, char *green_file, char *blue_file,
		int x, int y, int w, int h)
{
   unsigned char *buf1, *buf2, *buf3;
   int unit1, unit2, unit3;

   static int instance = 1;

   if (w == 0)
      w = img->width();
   if (h == 0)
      h = img->height();

   buf1 = new unsigned char[w];
   buf2 = new unsigned char[w];
   buf3 = new unsigned char[w];

   zvunit(&unit1, (char *)"XImageHandler", instance++, "u_name", red_file,NULL);
   zvunit(&unit2, (char *)"XImageHandler", instance++,"u_name",green_file,NULL);
   zvunit(&unit3, (char *)"XImageHandler", instance++, "u_name",blue_file,NULL);

   zvopen(unit1, "op", "write", "u_nl", h, "u_ns", w, NULL);
   zvopen(unit2, "op", "write", "u_nl", h, "u_ns", w, NULL);
   zvopen(unit3, "op", "write", "u_nl", h, "u_ns", w, NULL);

   for (int i=0; i<h; i++) {
      img->getColorLine(buf1, buf2, buf3, y+i, x, w);
      zvwrit(unit1, buf1, NULL);
      zvwrit(unit2, buf2, NULL);
      zvwrit(unit3, buf3, NULL);
   }
   zvclose(unit1, "clos_act", "free", NULL);
   zvclose(unit2, "clos_act", "free", NULL);
   zvclose(unit3, "clos_act", "free", NULL);

   delete buf1;
   delete buf2;
   delete buf3;
}

