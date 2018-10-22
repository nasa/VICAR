///////////////////////////////////////////////////////////
// SiHistSetVerGraphCmd.C:  
//////////////////////////////////////////////////////////
#include "SiHistSetVerGraphCmd.h"
#include "SiHistBox.h"

SiHistSetVerGraphCmd::SiHistSetVerGraphCmd ( const char *name, int active, 
		SiHistBox *box, CmdList *list=NULL ) 
	: RadioCmd ( name, active, list )
{
    _box = box;
    if (_box->getOrientType() == HORIZONTAL)
	_value = (CmdValue)TRUE;
    else 
	_value = (CmdValue)FALSE;

    newValue ();
}

void SiHistSetVerGraphCmd::doit()
{
    _oldValue = _box->getOrientType();

    if (_value)
	_box->setOrientType ( HORIZONTAL );
}

void SiHistSetVerGraphCmd::undoit()
{
    _box->setOrientType ( _oldValue );

    if ( _oldValue == HORIZONTAL)
	_value = (CmdValue)TRUE;
    else 
	_value = (CmdValue)FALSE;

    newValue ();
}
