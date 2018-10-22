///////////////////////////////////////////////////////////
// SiHistSetAscAxisCmd.C:  Display histogram with ascending/discending
// axis.
//////////////////////////////////////////////////////////
#include "SiHistSetAscAxisCmd.h"
#include "SiHistBox.h"

SiHistSetAscAxisCmd::SiHistSetAscAxisCmd ( const char *name, int active, 
		SiHistBox *box, CmdList *list=NULL ) 
	: RadioCmd ( name, active, list )
{
    _box = box;
    if (_box->getVerAxisDirType() == ASC) {
	_value = (CmdValue)TRUE;
	newValue();
    }
}

void SiHistSetAscAxisCmd::doit()
{
    if (_value)
	_box->setVerAxisDirType ( ASC );
    else
	_box->setVerAxisDirType ( DESC );
}

void SiHistSetAscAxisCmd::undoit()
{
  Boolean value = (_value != 0);

    if ( value )
	_box->setVerAxisDirType ( DESC );
    else
	_box->setVerAxisDirType ( ASC );

   _value = (CmdValue)!value;
    newValue();
}
