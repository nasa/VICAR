////////////////////////////////////////////////////////////////////////
// XImageToVicar - function (not class) which uses XImageHandler to write
// to a VICAR file.  This function is in a separate module to isolate
// VICAR dependencies.
////////////////////////////////////////////////////////////////////////

#ifndef _XIMAGETOVICAR_H
class XImageHandler;

void writeXImageToVicarFile(XImageHandler *img,
		char *red_file, char *green_file, char *blue_file,
		int x, int y, int w, int h);

#endif

