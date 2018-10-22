///////////////////////////////////////////////////////////////
// TpQualGroupMgr.h: 
///////////////////////////////////////////////////////////////
#ifndef TPQUALGROUPMGR_H
#define TPQUALGROUPMGR_H
#include "TpQualGroup.h"
#include <Xm/Xm.h> // for Boolean only!

class TpQualGroupMgr {
    
  protected:

    int _numGroups;
    TpQualGroup **_groups;

    int _numQuals;
    TpQualType *_qualFormat;

    char **_names;
    char **_units;

  friend std::ostream &operator<<(std::ostream &ostr, const TpQualGroupMgr &q);

  public:

    TpQualGroupMgr();
    TpQualGroupMgr(int numQuals, TpQualType *qualFormat);
    ~TpQualGroupMgr();

    void addGroup(TpQualGroup *);
    void deleteGroup(TpQualGroup *);

    void incNumQuals(TpQualType);
    void decNumQuals();
    void deleteAllQuals();
    void setFormat(int numQuals, char (*newFormat)[6]);

    int getNumQuals() const { return _numQuals; }
    int getNumFullQuals() const;
    int getNumRealQuals() const;
    int getNumTextQuals() const;
    TpQualType *getQualFormat() const { return _qualFormat; }
    TpQualType getType(int n) const { return _qualFormat[n]; }
    void getValue(int group, int n, int &value) const;
    void getValue(int group, int n, float &value) const;
    void getValue(int group, int n, char *&value) const;

    Boolean isUnique(TpQualGroup *excludeGroup, int n, char *);

    int getMinValue(int n, int &value);
    int getMaxValue(int n, int &value);

    void setValue(int group, int n, char *);
    void setValue(TpQualGroup *group, int n, char *);

    char *getQualName(int n) const;
    char *getQualUnit(int n) const;

    void setQualName(int n, char *name);
    void setQualUnit(int n, char *unit);
};
#endif
