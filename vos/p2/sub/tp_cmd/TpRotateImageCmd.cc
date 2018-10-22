////////////////////////////////////////////////////////////
// TpRotateImageCmd.cc: Rotate image,
////////////////////////////////////////////////////////////
#include "TpRotateImageCmd.h"
#include "TpSubDisplayer.h"
#include "ErrorManager.h"

TpRotateImageCmd::TpRotateImageCmd(const char *name, int active, CmdList *radioList,
		TpSubDisplayer *subDisplayer, RotationType rotation)
	: RadioCmd(name, active, radioList)
{
    _subDisplayer = subDisplayer;
    _rotation = rotation;
}

void TpRotateImageCmd::doit()
{
    if (_value) {
	if (_subDisplayer)
	    _subDisplayer->setRotationMode(_rotation);
	else 
	    theErrorManager->process(Error, "Change Rotation", 
			"You are trying to rotate an undefined image"); 
    }
}
