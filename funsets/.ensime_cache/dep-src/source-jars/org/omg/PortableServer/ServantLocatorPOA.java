package org.omg.PortableServer;


/**
* org/omg/PortableServer/ServantLocatorPOA.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from /HUDSON/workspace/8-2-build-linux-amd64/jdk8u92/6642/corba/src/share/classes/org/omg/PortableServer/poa.idl
* Thursday, March 31, 2016 9:09:07 PM PDT
*/


/**
	 * When the POA has the NON_RETAIN policy it uses servant 
	 * managers that are ServantLocators. Because the POA 
	 * knows that the servant returned by this servant 
	 * manager will be used only for a single request, 
	 * it can supply extra information to the servant 
	 * manager's operations and the servant manager's pair 
	 * of operations may be able to cooperate to do 
	 * something different than a ServantActivator. 
	 * When the POA uses the ServantLocator interface, 
	 * immediately after performing the operation invocation 
	 * on the servant returned by preinvoke, the POA will 
	 * invoke postinvoke on the servant manager, passing the 
	 * ObjectId value and the Servant value as parameters 
	 * (among others). This feature may be used to force 
	 * every request for objects associated with a POA to 
	 * be mediated by the servant manager.
	 */
public abstract class ServantLocatorPOA extends org.omg.PortableServer.Servant
 implements org.omg.PortableServer.ServantLocatorOperations, org.omg.CORBA.portable.InvokeHandler
{

  // Constructors

  private static java.util.Hashtable _methods = new java.util.Hashtable ();
  static
  {
    _methods.put ("preinvoke", new java.lang.Integer (0));
    _methods.put ("postinvoke", new java.lang.Integer (1));
  }

  public org.omg.CORBA.portable.OutputStream _invoke (String $method,
                                org.omg.CORBA.portable.InputStream in,
                                org.omg.CORBA.portable.ResponseHandler $rh)
  {
    throw new org.omg.CORBA.BAD_OPERATION();
  } // _invoke

  // Type-specific CORBA::Object operations
  private static String[] __ids = {
    "IDL:omg.org/PortableServer/ServantLocator:1.0", 
    "IDL:omg.org/PortableServer/ServantManager:1.0"};

  public String[] _all_interfaces (org.omg.PortableServer.POA poa, byte[] objectId)
  {
    return (String[])__ids.clone ();
  }

  public ServantLocator _this() 
  {
    return ServantLocatorHelper.narrow(
    super._this_object());
  }

  public ServantLocator _this(org.omg.CORBA.ORB orb) 
  {
    return ServantLocatorHelper.narrow(
    super._this_object(orb));
  }


} // class ServantLocatorPOA
