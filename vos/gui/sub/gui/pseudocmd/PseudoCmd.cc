///////////////////////////////////////////////////////////
// PseudoCmd.cc: Command class to execute the PSEUDOCOLOR
//                command.
//////////////////////////////////////////////////////////
#include "PseudoCmd.h"
#include "BasicImageView.h"
#include "Lut.h"
#include "PseudoValue.h"
#include "stdio.h"

PseudoCmd::PseudoCmd ( const char *name, int active, 
	Lut *lutR, Lut *lutG, Lut *lutB ) : NoUndoCmd ( name, active )
{
    _created = FALSE;

    _lutR = lutR;
    _lutG = lutG;
    _lutB = lutB;
}

void PseudoCmd::doit()
{
   // Allocate pseudocolor value dynamically
   PseudoValue *pseudoValue = (PseudoValue*)_value;

   int *r, *g, *b;

   r = pseudoValue->getRedAsArray();
   g = pseudoValue->getGrnAsArray();
   b = pseudoValue->getBluAsArray();

   _lutR->setAsArray(r);
   _lutG->setAsArray(g);
   _lutB->setAsArray(b);
}      

void PseudoCmd::freeValue(CmdValue value)
{
	if (value)
	   delete (PseudoValue *)value;
}
