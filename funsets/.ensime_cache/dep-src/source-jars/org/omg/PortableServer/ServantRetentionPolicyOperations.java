package org.omg.PortableServer;


/**
* org/omg/PortableServer/ServantRetentionPolicyOperations.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from /HUDSON/workspace/8-2-build-linux-amd64/jdk8u92/6642/corba/src/share/classes/org/omg/PortableServer/poa.idl
* Thursday, March 31, 2016 9:09:07 PM PDT
*/


/**
	 * This policy specifies whether the created POA retains 
	 * active servants in an Active Object Map. 
	 */
public interface ServantRetentionPolicyOperations  extends org.omg.CORBA.PolicyOperations
{

  /**
  	 * specifies the policy value
  	 */
  org.omg.PortableServer.ServantRetentionPolicyValue value ();
} // interface ServantRetentionPolicyOperations
