//////////////////////////////////////////////////////////////////////////////
// TpQualGroup.h: 
//////////////////////////////////////////////////////////////////////////////
#ifndef TPQUALGROUP_H
#define TPQUALGROUP_H
#include "TpQualifier.h"
#include <Xm/Xm.h>

class TpQualGroupMgr;

class TpQualGroup {
    
  protected:

    TpQualGroupMgr *_mgr;

    int _numQuals;
    TpQualifier **_qualifiers;
    
    TpQualType *_qualFormat;

  friend std::ostream &operator<<(std::ostream &, const TpQualGroup &);

  public:

    TpQualGroup(TpQualGroupMgr *);
    TpQualGroup(TpQualGroupMgr *, int numQuals, TpQualType *qualFormat);
    ~TpQualGroup();

    int getNumQuals() const { return _numQuals; }
    TpQualType getType(int i) const { return _qualFormat[i]; }

    void incNumQuals(TpQualType);
    void decNumQuals();
    void deleteAllQuals();

    void getValue(int n, int &value) const;
    void getValue(int n, float &value) const;
    void getValue(int n, char *&value) const;
 
    char *valueToString(int n) const;

    // IBIS routines require separate arrays for each type

    float *getRealQuals();
    int *getFullQuals();
    char *getTextQuals();

    void setValue(int n, char *value);
    void setValue(int n, int value);
    void setValue(int n, float value);

    Boolean isEqual(int n, char *value);

    static TpQualType getValueType(char *s);

};
#endif

