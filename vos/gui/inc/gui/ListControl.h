//////////////////////////////////////////////////////////////
// ListControl.h
///////////////////////////////////////////////////////////////
#ifndef LISTCONTROL_H
#define LISTCONTROL_H
#include "UIComponent.h"

class CurListValue;
class StretchParmList;
class StretchListInterface;
class TableValue;

class ListControl : public UIComponent {
    
  protected:

    CurListValue *_curListInValue, *_curListOutValue;
    StretchParmList *_list;
    StretchListInterface *_interface;
    TableValue *_tableValue;
    
    static void addToListCallback ( Widget, 
				    XtPointer, 
				    XtPointer );
    static void deleteFromListCallback ( Widget,
					 XtPointer,
					 XtPointer );
    
  public:
    
    ListControl ( Widget, const char *, CurListValue *, CurListValue *, 
		  StretchParmList *, StretchListInterface * );
    
    void setTableValue ( TableValue * );
    
    void addToList ( XtPointer );
    void deleteFromList ( XtPointer );
    
    int *getInAsArray();
    int *getOutAsArray();
    int getCount();
};
#endif
