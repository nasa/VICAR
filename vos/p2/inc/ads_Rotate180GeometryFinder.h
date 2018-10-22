// ads_Rotate180GeometryFinder.h
//
//  April 9, 2002
//  Michael Brady

#if !defined ADS_ROTATE_180_GEOMETRY_FINDER_H_INCLUDED
#define ADS_ROTATE_180_GEOMETRY_FINDER_H_INCLUDED


#include "ads_GeometryFinder.h"

using namespace jpl::mipl::spice::corba;

#include <memory>

/**
 *  Gets the geometry from another GeometryFinder, then
 *  rotates the instrument pointing 180 degrees.
 */
class ads_Rotate180GeometryFinder : public ads_GeometryFinder
{
public:

   /** Creates a Rotate180GeometryFinder which gets geometry info
    *  from  the specified
    * finder, then rotates the instrument pointing by 180 degrees.
    *
    * @param finder should be allocated using 'new'.  This object
    *               takes ownership and will 'delete' it.
    */  
   ads_Rotate180GeometryFinder(ads_GeometryFinder* finder);
   
   /** Destructor. */
   virtual ~ads_Rotate180GeometryFinder();
   
   /** Inherited from ads_DataFinder. */
   virtual void getGeometry (int instNaifId,
                             int targetNaifId,
                             const char* utcTime,
                             const char* referenceFrame,
                             double tolerance,
                             Geometry_out theGeometry,
                             GeometryMetaData_out metaData)
      throw ( SpiceLib::ToolkitException,
              MiplSpiceLib::PointingNotFound );

private:
   std::auto_ptr<ads_GeometryFinder> m_finder;
};


#endif // !defined ADS_ROTATE_180_GEOMETRY_FINDER_H_INCLUDED
