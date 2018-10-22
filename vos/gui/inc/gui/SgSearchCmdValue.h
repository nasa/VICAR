///////////////////////////////////////////////////////////////////
// SgSearchCmdValue.h: Contains parameters needed for a text search.
// Objects of this class are used by SgSearchText* objects
///////////////////////////////////////////////////////////////////

#ifndef SGSEARCHCMDVALUE_H
#define SGSEARCHCMDVALUE_H

struct SgSearchCmdValue {

    int case_sens;       // 1 if it *is* case sensitive; 0 if not

    char *text;          // the text we seek
    
    SgSearchCmdValue();
    SgSearchCmdValue(SgSearchCmdValue &);		// copy ctor
    ~SgSearchCmdValue ();

    SgSearchCmdValue &operator=(SgSearchCmdValue &val);
    int operator==(SgSearchCmdValue &val);
};
#endif
