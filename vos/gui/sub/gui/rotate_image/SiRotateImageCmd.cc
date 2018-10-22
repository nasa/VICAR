////////////////////////////////////////////////////////////
// SIRotateImageCmd.cc: Rotate image,
////////////////////////////////////////////////////////////
#include "SiRotateImageCmd.h"
#include "RotatedImageData.h"
#include "ErrorManager.h"
#include <stdint.h>

SiRotateImageCmd::SiRotateImageCmd(const char *name, int active,
		CmdList *radioList,
		RotatedImageData *image, RotationType rotation)
	: RadioCmd(name, active, radioList)
{
    _image = image;
    _rotation = rotation;

    // Set the default value

    int value = (int) (_rotation == _image->getRotationMode());
    _value = (CmdValue) (uintptr_t) value;
    newValue();
}

void SiRotateImageCmd::doit()
{
    if (_value) {
	if (_image)
	    _image->setRotationMode(_rotation);
	else 
	    theErrorManager->process(Error, "Change Rotation", 
			"You are trying to rotate an undefined image"); 
    }
}

