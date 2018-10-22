///////////////////////////////////////////////////////////////////
// TpQualFormatValue.h
///////////////////////////////////////////////////////////////////
#ifndef TPQUALFORMATVALUE_H
#define TPQUALFORMATVALUE_H

#include "TpQualifier.h"

typedef struct _TpQualInfo {
    char *_qualName;
    TpQualType _qualType;
    char *_qualUnit;
} TpQualInfo;

class TpQualFormatValue {

  public:

    int _numQuals;

    TpQualInfo *_info;

    TpQualFormatValue();
    TpQualFormatValue(int numQuals, TpQualInfo *info);
    TpQualFormatValue(const TpQualFormatValue &);
    virtual ~TpQualFormatValue() { }

    TpQualFormatValue &operator=(TpQualFormatValue &val);

    int getNumQuals() { return _numQuals; }
    void setNumQuals(int n);

    void addQualInfo(const char *name = "", TpQualType type = TpReal, 
			const char *unit = "");
    void deleteQualInfo();
};

#endif
