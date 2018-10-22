////////////////////////////////////////////////////////////
// TpLoadImageCmd.h: 
////////////////////////////////////////////////////////////
#include "TpLoadImageCmd.h"
#include "TpDisplayer.h"
#include <stdlib.h>

TpLoadImageCmd::TpLoadImageCmd(const char *name, int active, TpDisplayer *d) : 
                    Cmd(name, active)
{
    _displayer = d;
}

void TpLoadImageCmd::doit()
{
    _displayer->addImage((char *)_value);
}       

void TpLoadImageCmd::undoit()
{
    // Empty
}
