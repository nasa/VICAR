/////////////////////////////////////////////////////////////
// SiHistSetHorGraphCmd.h:  Set horizontal orientation on hstogram
// plot.
/////////////////////////////////////////////////////////////
#include "SiHistSetHorGraphCmd.h"
#include "SiHistBox.h"

SiHistSetHorGraphCmd::SiHistSetHorGraphCmd ( const char *name, int active, 
		SiHistBox *box, CmdList *list=NULL ) 
	: RadioCmd ( name, active, list )
{
    _box = box;

    if (_box->getOrientType() == VERTICAL)
	_value = (CmdValue)TRUE;
    else 
	_value = (CmdValue)FALSE;

    newValue();
}

void SiHistSetHorGraphCmd::doit()
{
    if (_value )
	_box->setOrientType ( VERTICAL );
}

void SiHistSetHorGraphCmd::undoit()
{
  Boolean value = (_value != 0);

    if ( value ) 
        _box->setOrientType ( HORIZONTAL );
    else
        _box->setOrientType ( VERTICAL );

    _value = (CmdValue)!value;
}
