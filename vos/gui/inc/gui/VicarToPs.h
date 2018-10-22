//////////////////////////////////////////////////////////////////////////////
// VicarToPostScript - function which converts a VICAR-File
// into a PostScript-File in order to print the main widget
// either into a file or send it imidiatly to the printer.
// Author: Rainer Berlin, DLR
// Modifications: Vadim Parizher, JPL-MIPS
//////////////////////////////////////////////////////////////////////////////

#ifndef VICAR_TO_PS_H
#define VICAR_TO_PS_H

#include <stdio.h>

struct PaperSize {
    const int width;
    const int height;
    const int xpos;
    const int ypos;
    PaperSize(int w, int h, int x, int y)
        : width(w), height(h), xpos(x), ypos(y) { }
};
 
const PaperSize PaperA4(540, 780, 30, 30);
const PaperSize PaperA3(780, 1080, 30, 30);
const PaperSize PaperA2(1080, 1560, 30, 30);
const PaperSize PaperLetter(540, 720, 36, 36);
const PaperSize PaperLegal(720, 1008, 36, 36);

int vicImageToPostScript(char *vicFile, char *psFile, char *title, 
			 PaperSize psize = PaperLetter);

#endif
