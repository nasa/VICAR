#ifndef SEPARATORCOMMAND_H
#define SEPARATORCOMMAND_H

#include "Cmd.h"

class SeparatorCommand : public Cmd {

private:

protected:

    virtual void doit() { }
    virtual void undoit() { }

public:

    SeparatorCommand ( const char *name = "sep" ) : Cmd ( name, 1 ) { }
    virtual const char *const className() { return "Separator"; }

};

extern SeparatorCommand *theSeparatorCmd;

#endif

