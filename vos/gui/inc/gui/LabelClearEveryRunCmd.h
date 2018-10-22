///////////////////////////////////////////////////////////////
//
//   LabelClearEveryRunCmd.h: 
//
//   This is a class derived from Cmd.
//   It sets the toggled value for clearing label output
//   everytime new output is generated.
//
///////////////////////////////////////////////////////////////
#ifndef LABELCLEAREVERYRUNCMD_H
#define LABELCLEAREVERYRUNCMD_H
#include "Cmd.h"
#include <Xm/Xm.h>

class TextDisplayView;

class LabelClearEveryRunCmd : public Cmd {

   protected:

      TextDisplayView *_view;

      virtual void doit();

      virtual void undoit( );

   public:

      LabelClearEveryRunCmd( const char *, int, TextDisplayView * );

      virtual const char *const className() { return ("LabelClearEveryRunCmd"); }

};
#endif
