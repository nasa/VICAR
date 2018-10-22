//////////////////////////////////////////////////////////////
// ListControl.cc: Contains "Add" and "Delete" buttons for the list
// representation of table values
///////////////////////////////////////////////////////////////
#include "ListControl.h"
#include "CurListValue.h"
#include "StretchParmList.h"
#include "StretchListInterface.h"
#include "TableValue.h"
#include <Xm/PushB.h>
#include <Xm/Form.h>
#include <stdlib.h>

ListControl::ListControl ( Widget parent, const char *name, 
			   CurListValue *curListInValue,
			   CurListValue *curListOutValue,
			   StretchParmList *list,
			   StretchListInterface *interface ) 
    : UIComponent ( name )
{
    // Save the current values and the tables

    _curListInValue = curListInValue;
    _curListOutValue = curListOutValue;
    _list = list;
    _interface = interface;
    _tableValue = new TableValue;
    
    _w = XtVaCreateWidget ( _name, 
			    xmFormWidgetClass,
			    parent,
			    NULL);
    installDestroyHandler();
    
    Widget addButton = XtVaCreateManagedWidget ( "add",
			 xmPushButtonWidgetClass, _w,
			 XmNshowAsDefault, True,
			 XmNdefaultButtonShadowThickness, 1,
			 XmNalignment, XmALIGNMENT_CENTER,
			 XmNleftAttachment, XmATTACH_FORM,
                         NULL );
    Widget deleteButton = XtVaCreateManagedWidget ( "delete",
                         xmPushButtonWidgetClass, _w,
			 XmNshowAsDefault, False,
			 XmNdefaultButtonShadowThickness, 1,
			 XmNalignment, XmALIGNMENT_CENTER,
			 XmNrightAttachment, XmATTACH_FORM,
                         NULL );

    XtAddCallback ( addButton,  
		   XmNactivateCallback, 
		   &ListControl::addToListCallback,
		   (XtPointer) this );

    XtAddCallback ( deleteButton,
                   XmNactivateCallback,
                   &ListControl::deleteFromListCallback,
                   (XtPointer) this );
}

void ListControl::addToListCallback ( Widget, 
				      XtPointer clientData,
				      XtPointer callData)
{
    ListControl *obj = (ListControl *) clientData;
    obj->addToList(callData);
}

void ListControl::addToList ( XtPointer )
{
    // Make sure the values are correct
    if ((_curListInValue->getValue() < 0) || 
	(_curListInValue->getValue() > 255))
	_curListInValue->setValue(0);

    if ((_curListOutValue->getValue() < 0) || 
	(_curListOutValue->getValue() > 255))
	_curListOutValue->setValue(0);

    // Make room for one more element

    int count = _tableValue->count+1;

    int i;

    // Copy 'in' array
    
    int *in = new int[count];
    for (i = 0; i < count-1; i++)
	in[i] = _tableValue->inTable[i];
    
    // Copy 'out' array

    int *out = new int[count];
    for (i = 0; i < count-1; i++)
	out[i] = _tableValue->outTable[i];
    
    i = 0;
    while ( (i < count-1) && (in[i] != _curListInValue->getValue()) )
	i++;
    
    if ( i < count-1 ) {
	out [i] = _curListOutValue->getValue();
	count --;	// no need for another element
    }
    else {
	in [count-1] = _curListInValue->getValue();
	out [count-1] = _curListOutValue->getValue();
    }
    _list->update( in, out, count );
    
    // Sort the "in" values
    for (int m = 0; m < count-1; m++) {
	for (int n = 0; n < count-1; n++) {
	    if (in[n] > in[n+1]) {
		int tempIn = in[n];
		int tempOut = out[n];
		in[n] = in[n+1];
		out[n] = out[n+1];
		in[n+1] = tempIn;
		out[n+1] = tempOut;
	    }
	}
    }
    
    delete _tableValue->inTable;
    delete _tableValue->outTable;
    
    _tableValue->inTable = new int[count];
    _tableValue->outTable = new int[count];
    _tableValue->count = count;
    for ( i = 0; i < count; i ++ ) {
	_tableValue->inTable[i] = in[i];
	_tableValue->outTable[i] = out[i];
    }
    
    if (in) delete [] in;
    if (out) delete [] out;
    
    _interface->runIt();
}

void ListControl::deleteFromListCallback ( Widget,
					   XtPointer clientData,
					   XtPointer callData)
{
    ListControl *obj = (ListControl *) clientData;
    obj->deleteFromList(callData);
}

void ListControl::deleteFromList ( XtPointer )
{
    int count = _tableValue->count;
    
    int i;

        // Copy 'in' array

    int *in = new int[count];
    for (i = 0; i < count; i++)
	in[i] = _tableValue->inTable[i];
    
    // Copy 'out' array

    int *out = new int[count];
    for (i = 0; i < count; i++)
	out[i] = _tableValue->outTable[i];
    
    i = 0;
    int n;
    
    while ( ( in[i] != _curListInValue->getValue() ) && ( i < count) ) {
	i++;
    }
    if ( i < count ) {
	for ( n = i; n < count; n++ ) {
	    in[n] = in[n+1];
	    out[n] = out[n+1];
	}
	count--;
	
	_list->update( in, out, count );
    }

    delete _tableValue->inTable;
    delete _tableValue->outTable;
    
    _tableValue->inTable = new int[count];
    _tableValue->outTable = new int[count];
    _tableValue->count = count;
    for ( i = 0; i < count; i ++ ) {
	_tableValue->inTable[i] = in[i];
	_tableValue->outTable[i] = out[i];
    }
    _interface->runIt();
}

void ListControl::setTableValue ( TableValue *tv )
{
    delete _tableValue;
    _tableValue = tv;
    _list->update( tv->inTable, tv->outTable, tv->count );	
    
    TableValue *temp = new TableValue;
    *temp = *_tableValue;
}

int *ListControl::getInAsArray()
{
    return _tableValue->inTable;
}

int *ListControl::getOutAsArray()
{
    return _tableValue->outTable;
}

int ListControl::getCount()
{
    return _tableValue->count;
}

