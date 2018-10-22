///////////////////////////////////////////////////////////////
//
//   ClearLabelCmd.cc: 
//
//   This is a class derived from NoUndoCmd.
//   It clears image label display.
//
///////////////////////////////////////////////////////////////
#include "ClearLabelCmd.h"
#include "TextDisplayModel.h"

ClearLabelCmd::ClearLabelCmd ( const char *name, int active, TextDisplayModel *textM) 
	: NoUndoCmd ( name, active )
{
   _textM = textM;
}

void ClearLabelCmd::doit()
{
   _textM->clear();
}
