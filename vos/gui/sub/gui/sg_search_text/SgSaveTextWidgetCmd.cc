/////////////////////////////////////////////////////////////////
// SgSaveTextWidgetCmd.cc - writes the contents of a Motif
// text widget to a file
////////////////////////////////////////////////////////////////

#include "SgSaveTextWidgetCmd.h"
#include <Xm/Text.h>
#include <fstream>
#include <iostream>

SgSaveTextWidgetCmd::SgSaveTextWidgetCmd( const char *name, int active, 
		      Widget textWidget ) : NoUndoCmd(name, active)
{
  _textWidget = textWidget;   // the widget whose text we're saving
}

////////////////////////////////////////////////////////////////
// doit()
////////////////////////////////////////////////////////////////
void SgSaveTextWidgetCmd::doit()
{
  char *filename = (char *)_value;  
  char *logText = NULL;

  // Get the text from the output window
  
  if ((logText = XmTextGetString(_textWidget))) {
    
    // Save text to the specified file; ios::out will OVERWRITE existing file
    std::ofstream outputFile(filename, std::ios::out);
    outputFile << logText << std::endl;
    outputFile.close();
    
    // free the memory using XtFree()
    XtFree(logText);
  }
}
