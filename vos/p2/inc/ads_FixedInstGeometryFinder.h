// ads_FixedInstGeometryFinder.h
//
//  April 9, 2002
//  Michael Brady

#if !defined ADS_FIXED_INST_GEOMETRY_FINDER_H_INCLUDED
#define ADS_FIXED_INST_GEOMETRY_FINDER_H_INCLUDED


#include "ads_GeometryFinder.h"

#include <memory>

using namespace jpl::mipl::spice::corba;

/**
 *  Finds the pointing for instruments which are fixed to the
 *  spacecraft and whose offset is defined in a SPICE frame kernel.
 */
class ads_FixedInstGeometryFinder : public ads_GeometryFinder
{
public:
   /** Creates a FixedInstGeometryFinder which uses the specified
    * finder to find the geometry of the spacecraft to which the
    * instrument is attached.
    *
    * @param finder should be allocated using 'new'.  This object
    *               takes ownership and will 'delete' it.
    */
   ads_FixedInstGeometryFinder(ads_GeometryFinder* finder);
  
  /** Destructor. */
  virtual ~ads_FixedInstGeometryFinder();
     
  /** Inherited from ads_GeometryFinder. */
  virtual void getGeometry (int instNaifId,
                            int targetNaifId,
                            const char* utcTime,
                            const char* referenceFrame,
                            double tolerance,
                            Geometry_out theGeometry,
                            GeometryMetaData_out metaData )
     throw ( SpiceLib::ToolkitException,
             MiplSpiceLib::PointingNotFound );

private:
   std::auto_ptr<ads_GeometryFinder> m_finder;

};


#endif // !defined ADS_FIXED_INST_GEOMETRY_FINDER_H_INCLUDED
