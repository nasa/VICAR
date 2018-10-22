///////////////////////////////////////////////////////////////////
// SgSearchAgainCmd.h - a simple cmd to execute an SgSearchTextCmd
///////////////////////////////////////////////////////////////////

#include "SgSearchAgainCmd.h"

SgSearchAgainCmd::SgSearchAgainCmd( const char *name, int active, 
				    SgSearchTextCmd *search_cmd ) 
  : NoUndoCmd(name, active)
{
  _search_cmd = search_cmd;
  
  // notify the main search cmd of our existence
  _search_cmd->registerAgainCmd(this);
}

void SgSearchAgainCmd::doit()
{
  _search_cmd->execute();
}
