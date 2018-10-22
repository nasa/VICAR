///////////////////////////////////////////////////////////////////
// SgSearchCmdValue.h: Contains parameters needed for a text search.
// Objects of this class are used by SgSearchText* objects
///////////////////////////////////////////////////////////////////

#include "SgSearchCmdValue.h"
#include <iostream>
using namespace std;
#include <stdio.h>
#include <string.h>

///////////////////////////////////////////////////////////////////
// Constructor
///////////////////////////////////////////////////////////////////
SgSearchCmdValue::SgSearchCmdValue()
{
  text = NULL;
  case_sens = 0;
}


///////////////////////////////////////////////////////////////////
// Destructor
///////////////////////////////////////////////////////////////////
SgSearchCmdValue::~SgSearchCmdValue()
{
  if (text)
    delete []text;
} 

///////////////////////////////////////////////////////////////////
// Copy constructor
///////////////////////////////////////////////////////////////////
SgSearchCmdValue::SgSearchCmdValue(SgSearchCmdValue &val)
{
  text = NULL;
  if (val.text) {
    text = new char[strlen(val.text)+1];
    strcpy(text, val.text);
  }
  case_sens = val.case_sens;
}

///////////////////////////////////////////////////////////////////
// Assignment operator
///////////////////////////////////////////////////////////////////
SgSearchCmdValue &SgSearchCmdValue::operator=(SgSearchCmdValue &val)
{
    if (this == &val)
	return *this;		// assignment to self
    
    if (text)
	delete []text;
    
    text = NULL;

    if (val.text) {
	text = new char[strlen(val.text)+1];
	strcpy(text, val.text);
    }

    case_sens = val.case_sens;
    
    return *this;
}

///////////////////////////////////////////////////////////////////
// Equality operator
///////////////////////////////////////////////////////////////////
int SgSearchCmdValue::operator==(SgSearchCmdValue &val)
{
    if (this == &val)
        return 1;
 
    if (case_sens != val.case_sens) 
      return 0;
    else {
      if (text && val.text) {
	if (strlen(text) != strlen(val.text))
	  return 0;
	else if ((text && !val.text) || (!text && val.text))
	  return 0;
	else if (!strcmp(text, val.text))
	  return 0;
      }
      return 1;
    }
}





