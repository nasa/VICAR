///////////////////////////////////////////////////////////////////
// TpContrastCmd.cc: Contrast control.
///////////////////////////////////////////////////////////////////
#include "TpContrastCmd.h"
#include "TpContrastValue.h"
#include "ImageData.h"
#include "Lut.h"
#include "StretchFun.h"
#include "XvicImage.h"
#include <stdlib.h>
#include <stdio.h>

TpContrastCmd::TpContrastCmd(const char *name, int active, 
		Widget iw, Widget ziw, Widget pan, ImageData *)
	: Cmd(name, active)
{
    _iw = iw;
    _ziw = ziw;
    _pan = pan;
}

void TpContrastCmd::doit()
{
    TpContrastValue *value = (TpContrastValue *)_value;
    Lut *lut = new Lut;
    stretch_linear(lut, value->getMin(), value->getMax());
    XvicImageSetMonoLUT(_iw, lut->getAsArray());
    XvicImageSetMonoLUT(_ziw, lut->getAsArray());
    XvicImageSetMonoLUT(_pan, lut->getAsArray());
    delete lut;
}
