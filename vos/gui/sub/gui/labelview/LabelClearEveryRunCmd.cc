///////////////////////////////////////////////////////////////
//
//   LabelClearEveryRunCmd.cc: 
//
//   This is a class derived from Cmd.
//   It sets the toggled value for clearing label output
//   everytime new output is generated.
//
///////////////////////////////////////////////////////////////
#include "LabelClearEveryRunCmd.h"
#include "TextDisplayView.h"
#include <Xm/Xm.h>   

LabelClearEveryRunCmd::LabelClearEveryRunCmd ( const char *name, int active, TextDisplayView *textW) 
	: Cmd ( name, active, (CmdValue)1 )
{
   _view = textW;
}

////////////////////////////////////////////////////////////////
// doit()
////////////////////////////////////////////////////////////////
void LabelClearEveryRunCmd::doit()
{
   if ( _value )
      _view->setClearEveryRun( True );
   else
      _view->setClearEveryRun( False );
}

////////////////////////////////////////////////////////////////
// undoit()
////////////////////////////////////////////////////////////////
void LabelClearEveryRunCmd::undoit()
{
   _value = (CmdValue)(!_value);
   newValue( );
   if ( _value )
      _view->setClearEveryRun( True );
   else
      _view->setClearEveryRun( False );

}

