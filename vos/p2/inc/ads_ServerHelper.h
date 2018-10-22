// ads_ServerHelper.h
//
// August 27, 2001
// Michael Brady

#if !defined ADS_SERVER_HELPER_H_INCLUDED
#define ADS_SERVER_HELPER_H_INCLUDED

#include "tao/PortableServer/PortableServer.h"
#include "tao/PortableServer/Servant_Base.h"

/**
 *  <P>A utility class for CORBA servers.</P>
 *
 *  <P>After creating all objects of this class which will be used in his
 *  program, the user should run the orb event loop by calling:</P>
 *
 *  <P><CODE>TLM_ORB::instance()->run();</CODE></P>
 */
class ads_ServerHelper
{
public:
  
  /** <P>Creates a new ServerHelper for the specified servant.</P>
   *
   *  <P>It is the caller's responsibility to initialize 
   *  the ORB before calling
   *  this method.  If you do not understand why you would want to 
   *  initialize the ORB yourself, you should probably use the other
   *  constructor.</P>
   *
   *  <P>The servant will not be deleted by this object, so if it was
   *  created via 'new', it is the caller's responsibility 
   *  to delete it.</P>
   *
   *  @param servant If null, behavior is undefined.
   */
  ads_ServerHelper(PortableServer::Servant servant);
  
  /** <P>Creates a new ServerHelper for the specified servant 
   *  and initializes 
   *  the ORB with the specified parameters.</P>
   *
   *  <P>The servant will not be deleted by this object, so if it was
   *  created via 'new', 
   *  it is the caller's responsibility to delete it.</P>
   *
   *  @param servant If null, behavior is undefined.
   */
  ads_ServerHelper(int& argc, 
		   char* argv[], 
		   PortableServer::Servant servant);
  
  /** Destructor */
  ~ads_ServerHelper();
  
  
  /**
   *  Writes this object's servant's reference (IOR) to the specified file.
   */
  void writeToFile(const ACE_CString& fileName);
  
  /**
   *  Writes this object's servant's reference (IOR) to the specified file
   *  if there is a "-o fileName" on the command line.
   */
  void writeToFile(int& argc, char* argv[]);
  
  /** Returns the POA in which the servant is registered. */
  PortableServer::POA_var thePoa();
  
  /** Starts the ORB event loop. */
  void run();
  
  /** Halts the ORB event loop. */
  void halt();
  
private:

  void init(PortableServer::Servant servant);

  void deactivate();
  
  void fini();
  
  
private:
  
  PortableServer::POA_var poa_;
  PortableServer::Servant servant_;
  CORBA::Object_var servantObject_;
      
  int i_own_the_orb_;
  int has_been_deactivated_;
  
};

#endif // !defined ADS_SERVER_HELPER_H_INCLUDED
