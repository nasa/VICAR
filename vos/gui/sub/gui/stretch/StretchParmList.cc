///////////////////////////////////////////////////////
// StretchParmList.cc:
///////////////////////////////////////////////////////
#include "StretchParmList.h"
#include "CurListValue.h"
#include <Xm/ScrolledW.h>
#include <Xm/List.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream>
using namespace std;

StretchParmList::StretchParmList ( Widget parent, const char *name, 
		CurListValue *inSingleValue, CurListValue *outSingleValue ) 
    : UIComponent (name)
{
    _w = XmCreateScrolledList ( parent, _name, NULL, 0 );
    
    installDestroyHandler ();
    
    XtVaSetValues( _w, XmNselectionPolicy, XmSINGLE_SELECT, NULL );
    
    _count = 0;
    _in = NULL;
    _out = NULL;

    _inSingleValue = inSingleValue;
    _outSingleValue = outSingleValue;
    
    XtAddCallback ( _w, XmNsingleSelectionCallback,
		    &StretchParmList::selectCallback,
		    ( XtPointer ) this );
}

void StretchParmList::update ( int *in, int *out, int count )
{
    int i;

    // Save the "in" and "out" values

    if (_in) delete [] _in;
    if (_out) delete [] _out;

    _count = count;
    _in = new int [count];
    _out = new int [count];

    for ( i = 0; i < _count; i++ ) {
	_in[i] = in[i];
	_out[i] = out[i];
    }
    
    // Sort the "in" values

    for (int m = 0; m < _count-1; m++) {
	for (int n = 0; n < _count-1; n++) {
	    if (_in[n] > _in[n+1]) {
		int tempIn = _in[n];
		int tempOut = _out[n];
		_in[n] = _in[n+1];
		_out[n] = _out[n+1];
		_in[n+1] = tempIn;
		_out[n+1] = tempOut;
	    }
	}
    }
    
    XmStringTable strList;
    strList = (XmStringTable) XtMalloc (count * sizeof (XmString));
    
    for (i = 0; i < count; i++) {
	char *inOut = new char[256];
	sprintf ( inOut, "%6d%6d", _in[i], _out[i] );
	strList[i] = XmStringCreateLocalized ( inOut );
	delete [] inOut;
    }
    
    XtVaSetValues ( _w,
		    XmNitems, strList,
		    XmNitemCount, count,
		    NULL );
    
    for(i = 0; i < count; i++)
	XmStringFree(strList[i]);
    XtFree((char *)strList);
}

void StretchParmList::selectCallback ( Widget,
				       XtPointer clientData,
				       XtPointer callData)
{
    XmListCallbackStruct *cbs = (XmListCallbackStruct *) callData;
    char *choice;
    XmStringGetLtoR (cbs->item, XmFONTLIST_DEFAULT_TAG, &choice);
    XtFree (choice);
    
    StretchParmList *obj;
    obj = ( StretchParmList * ) clientData;
    if (obj != NULL)
	obj->select(cbs->item_position);
}

void StretchParmList::select ( int position )
{
    _inSingleValue->setValue(_in[position-1]);
    _outSingleValue->setValue(_out[position-1]);	
}
