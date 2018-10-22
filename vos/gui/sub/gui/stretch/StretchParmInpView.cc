///////////////////////////////////////////////////////
// StretchParmInpView.cc:  Implements keyin view for one stretch parameter
///////////////////////////////////////////////////////
#include "StretchParmInpView.h"
#include "StretchCmdInterface.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

StretchValue StretchParmInpView::_defaultStretchValue;	// init static data

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////
StretchParmInpView::StretchParmInpView ( Widget parent, const char *name,
		StretchValue *stretchValue, int offset, 
		StretchParmType type, StretchCmdInterface *ci )
    : KeyinView (parent, name)
{
    _stretchValue = stretchValue;
    _offset = offset;
    _type = type;
    _stretchCmdInterface = ci;
    
    installCallback();
}

////////////////////////////////////////////////////////////////////////
// Fill the stretch value structure, then execute the stretch
////////////////////////////////////////////////////////////////////////
void StretchParmInpView::update ( XtPointer )
{
    Boolean useDefault = False;
    char *string = getFieldValue();
    
    if (string == NULL || strlen(string) == 0)
	useDefault = True;
    
    switch (_type) {
	
      case StretchInt:
	if (useDefault)
            *(int *)(((char *)_stretchValue) + _offset) =
		*(int *)(((char *)&_defaultStretchValue) + _offset);
	else
            *(int *)(((char *)_stretchValue) + _offset) = atoi(string);
	break;
	
      case StretchDouble:
	if (useDefault)
            *(double *)(((char *)_stretchValue) + _offset) =
		*(double *)(((char *)&_defaultStretchValue) + _offset);
	else
            *(double *)(((char *)_stretchValue) + _offset) = atof(string);
	break;
	
      case StretchString:
	if (useDefault)		// No non-NULL defaults for strings!
            *(char **)(((char *)_stretchValue) + _offset) = NULL;
	else {
            *(char **)(((char *)_stretchValue) + _offset) =
		new char[strlen(string)+1];
            strcpy(*(char **)(((char *)_stretchValue) + _offset), string);
	}
	break;
	
      default:
	assert(0);	// Shouldn't happen!
	break;
    }

    XtFree(string);
    
    _stretchCmdInterface->stretchIt(_stretchValue);
}

////////////////////////////////////////////////////////////////////////
// Update the text field with the value from the struct.  Don't execute 
// the cmd.
////////////////////////////////////////////////////////////////////////
void StretchParmInpView::setValue(StretchValue *value)
{
    _stretchValue = value;

    char string[1024];

    switch (_type) {
      case StretchInt:
	sprintf(string, "%d", *(int *)(((char *)_stretchValue) + _offset));
	break;

      case StretchDouble:
	sprintf(string, "%f", *(double *)(((char *)_stretchValue) + _offset));
	break;

      case StretchString:
	if (*(char **)(((char *)_stretchValue) + _offset)) {
            strncpy(string, *(char **)(((char *)_stretchValue) + _offset),
		    sizeof(string));
            string[sizeof(string)-1] = '\0';
	}
	else			// null string
            strcpy(string, "");
	break;

      default:
	assert(0);	// Shouldn't happen!
	break;
    }

    setFieldValue(string);
}

void StretchParmInpView::setForeground(char *color)
{
    XtVaSetValues(_field, 
		  XtVaTypedArg, 
		      XmNforeground, XmRString, color, (strlen(color) + 1), 
		  NULL);

    XtVaSetValues(_label, 
                  XtVaTypedArg,
                      XmNforeground, XmRString, color, (strlen(color) + 1),
                  NULL);
}
