//////////////////////////////////////////////////////////////
// SeparatorInterface.C: A separator interface to a (dummy) Cmd object
///////////////////////////////////////////////////////////////
#include "SeparatorInterface.h"
#include <Xm/Separator.h>

SeparatorInterface::SeparatorInterface ( Widget parent, 
				  Cmd *cmd ) : CmdInterface ( cmd )
{
    _w = XtCreateWidget ( _name, 
			 xmSeparatorWidgetClass,
			 parent,
			 NULL, 0 );
    installDestroyHandler();
    
    // The _active member is set when each instance is registered
    // with an associated Cmd object. Now that a widget exists,
    // set the widget's sensitivity according to its active state.
    
    if ( _active )
	activate();     
    else
	deactivate();   

}
