//////////////////////////////////////////////////////////////
// RadioCmdBox.cc: A UIComponent that creates a radio bank given a
// list of RadioCmd's.  All items in the list must be RadioCmd's,
// but this is not checked for in the code.  The provided CmdList
// will typically be a MenuList or the actual radio list (the one
// that specifies which commands make up the mutual-exclude set).
// This is merely a convenience class; if you want separators or other
// weird things in the radio bank it can be created manually.
// The returned RowColumn can be manipulated, for example to set the
// number of columns.
///////////////////////////////////////////////////////////////
#include "RadioCmdBox.h"
#include "RadioButtonInterface.h"
#include "CmdList.h"
#include <Xm/RowColumn.h>

RadioCmdBox::RadioCmdBox ( Widget parent, const char *name,
				CmdList *cmdList, CmdList *deferList )
						// deferList = NULL
		: UIComponent ( name )
{

    _w = XtVaCreateManagedWidget(name, xmRowColumnWidgetClass, parent,
		XmNradioBehavior, True, XmNradioAlwaysOne, True, NULL);
    installDestroyHandler();

    _numElements = cmdList->size();
    _list = new RadioButtonInterface *[_numElements];

    for (int i = 0; i < _numElements; i++) {
        _list[i] = new RadioButtonInterface(_w, (*cmdList)[i]);
        _list[i]->manage();
        if (deferList)
            _list[i]->setDeferredExec(deferList);
    }
}

RadioCmdBox::~RadioCmdBox()
{
    for (int i=0; i<_numElements; i++)
        delete _list[i];
    delete _list;
}

