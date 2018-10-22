///////////////////////////////////////////////////////////
// SiHistSetDescAxisCmd.C:  Set descending orientation on histogram 
// axis.
//////////////////////////////////////////////////////////
#include "SiHistSetDescAxisCmd.h"
#include "SiHistBox.h"

SiHistSetDescAxisCmd::SiHistSetDescAxisCmd ( const char *name, int active, 
		SiHistBox *box, CmdList *list=NULL ) 
	: RadioCmd ( name, active, list )
{
    _box = box;
    if ( _box->getVerAxisDirType() == DESC)
	_value = (CmdValue)TRUE;
    else 
	_value = (CmdValue)FALSE;

    newValue();
}

void SiHistSetDescAxisCmd::doit()
{
    if (_value)
	_box->setVerAxisDirType ( DESC );
    else 
	_box->setVerAxisDirType ( ASC );
}

void SiHistSetDescAxisCmd::undoit()
{
  Boolean value = (_value != 0);

    if ( value )
	_box->setVerAxisDirType ( ASC );
    else 
	_box->setVerAxisDirType ( DESC );

    _value = (CmdValue)!value;
    newValue();
}
