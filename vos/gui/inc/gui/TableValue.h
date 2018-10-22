///////////////////////////////////////////////////////////////////
// TableValue.h: Contains pairs of "in" and "out" values, also 
// keeps the value of the type of the table.  Everything is 
// public.
///////////////////////////////////////////////////////////////////
#ifndef TABLEVALUE_H
#define TABLEVALUE_H

struct TableValue {

    int *inTable, *outTable;
    int count;
    
    TableValue () 
    {
	inTable = outTable = NULL; 
	count = 0;
    };

    ~TableValue ()
    {
	if (inTable) delete [] inTable;
	if (outTable) delete [] outTable;
    };
};
#endif
