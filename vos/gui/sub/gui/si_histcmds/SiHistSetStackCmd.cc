///////////////////////////////////////////////////////////
// SiHistSetStackCmd.C: 
//////////////////////////////////////////////////////////
#include "SiHistSetStackCmd.h"
#include "SiHistBox.h"

SiHistSetStackCmd::SiHistSetStackCmd ( const char *name, int active, 
		SiHistBox *box, CmdList *list=NULL ) 
	: RadioCmd ( name, active, list )
{
    _box = box;
    if (_box->getMethodType() == STACKED  || _box->getHistB() == NULL )
	_value = (CmdValue)TRUE;
    else
	_value = (CmdValue)FALSE;

    newValue();
}

void SiHistSetStackCmd::doit()
{
    _oldValue = _box->getMethodType();
    if (_value) {
       _box->setMethodType ( STACKED );
    }
}      

void SiHistSetStackCmd::undoit()
{
    _box->setMethodType ( _oldValue );
    if (_box->getMethodType() == STACKED  || _box->getHistB() == NULL )
	_value = (CmdValue)TRUE;
    else 
	_value = (CmdValue)FALSE;

    newValue();
}
