//////////////////////////////////////////////////////////////
// ArrowButtonInterface.C: An ArrowButton interface to a Cmd object
///////////////////////////////////////////////////////////////
#include "ArrowButtonInterface.h"
#include <Xm/ArrowB.h>

ArrowButtonInterface::ArrowButtonInterface ( Widget parent, 
				  Cmd *cmd ) : CmdInterface ( cmd )
{
    _w = XtCreateWidget ( _name, 
			 xmArrowButtonWidgetClass,
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

// No idea what the right ifdef is for automatically recognizing g++
#ifdef GNU_CC
    
    // G++ reportedly doesn't like the form expected by cfront. I'm
    // told this will work, but I haven't tested it myself.
    
    XtAddCallback ( _w,  
		   XmNactivateCallback, 
		   executeCmdCallback,
		   (XtPointer) this );  
#else

    XtAddCallback ( _w,  
		   XmNactivateCallback, 
		   &CmdInterface::executeCmdCallback,
		   (XtPointer) this );  
#endif
    
}


