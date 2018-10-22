// ads_CassiniDataFinder.h
//
//  April 9, 2002
//  Michael Brady

#if !defined ADS_CASSINI_DATA_FINDER_H_INCLUDED
#define ADS_CASSINI_DATA_FINDER_H_INCLUDED


#include "ads_DefaultDataFinder.h"
#include "ads_GeometryFinder.h"

using namespace jpl::mipl::spice::corba;

/**
 *  Finds Cassini ISS image information.
 */
class ads_CassiniDataFinder : public ads_DefaultDataFinder
{
public:

   /** Creates a new CassiniDataFinder */
   ads_CassiniDataFinder();
  
  /** Destructor. */
  virtual ~ads_CassiniDataFinder();
     
  /** Inherited from ads_MissionDataFinder. */
   virtual void getGeometryNoUpdates (int instNaifId,
                                      int targetNaifId,
                                      const char* utcTime,
                                      const char* referenceFrame,
                                      double tolerance,
                                      Geometry_out theGeometry,
                                      GeometryMetaData_out metaData)
      throw ( SpiceLib::ToolkitException,
              MiplSpiceLib::PointingNotFound );

private:
   std::auto_ptr<ads_GeometryFinder> m_geometryFinder;
};


#endif // !defined ADS_CASSINI_DATA_FINDER_H_INCLUDED
