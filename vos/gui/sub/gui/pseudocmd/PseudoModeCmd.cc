/////////////////////////////////////////////////////////////////
// PseudoModeCmd.cc
//	
//	PseudoModeCmd switches between NONE and PSEUDO modes
//
////////////////////////////////////////////////////////////////
#include "PseudoModeCmd.h"
#include "XvicImage.h"

PseudoModeCmd::PseudoModeCmd(const char *name, int active, Widget iw)
		: Cmd(name, active)
{
   _iw = iw;
}

////////////////////////////////////////////////////////////////
// doit()
////////////////////////////////////////////////////////////////
void PseudoModeCmd::doit()
{
   if (_value)
       XtVaSetValues (_iw, XvicNlutType, XvicPSEUDO, NULL );
   else 
       XtVaSetValues (_iw, XvicNlutType, XvicSTRETCH, NULL );
}

////////////////////////////////////////////////////////////////
// undoit()
////////////////////////////////////////////////////////////////
void PseudoModeCmd::undoit()
{
   if (_value)
       XtVaSetValues (_iw, XvicNlutType, XvicSTRETCH, NULL );
   else
       XtVaSetValues (_iw, XvicNlutType, XvicPSEUDO, NULL );

   _value = (CmdValue)(!_value);
   newValue();
}
