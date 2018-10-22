// ads_DataBase.h
//
// Apr 25, 2002
// Michael Brady

#if !defined ADS_DATABASE_H_INCLUDED
#define ADS_DATABASE_H_INCLUDED

#include "jpl_mipl_spice_corba.C.h"

/**
 *  Encapsulates access to the geometry update data base.
 */
class ads_DataBase
{
private:
   /** Creates a new ads_DataBase object. */
   ads_DataBase();

   /** Destructor. */
   virtual ~ads_DataBase();

   /** Creates a new ads_DataBase object 
    *  which is a copy of the specified object. 
    */
   ads_DataBase(const ads_DataBase& src);

   /** Sets this to the value of the specified object. */
   ads_DataBase& operator=(const ads_DataBase& rhs);
   
public:

   /** Finds a geometry which matches the specified criteria.
    * @return true if one was found, else false.
    */
   static 
   bool findGeometry(const jpl::mipl::spice::corba::ImageData & image,
                     const jpl::mipl::spice::corba::GeometryMetaData & criteria,
                     jpl::mipl::spice::corba::Geometry_out theGeometry,
                     jpl::mipl::spice::corba::GeometryMetaData_out metaData );


   /** Stores the specified geometry with the specified meta data.
    */
   static
   void storeGeometry(
            const jpl::mipl::spice::corba::ImageData & image,
            const jpl::mipl::spice::corba::Geometry & theGeometry,
            const jpl::mipl::spice::corba::GeometryMetaData & theMetaData );


};

#endif // !defined ADS_DATABASE_H_INCLUDED
