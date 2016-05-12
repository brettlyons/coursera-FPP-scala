package org.omg.PortableInterceptor;


/**
* org/omg/PortableInterceptor/ServerRequestInterceptorOperations.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from /HUDSON/workspace/8-2-build-linux-amd64/jdk8u92/6642/corba/src/share/classes/org/omg/PortableInterceptor/Interceptors.idl
* Thursday, March 31, 2016 9:09:07 PM PDT
*/


/**
   * Server-side request interceptor.
   * <p>
   * A request Interceptor is designed to intercept the flow of a 
   * request/reply sequence through the ORB at specific points so that 
   * services can query the request information and manipulate the service 
   * contexts which are propagated between clients and servers. The primary 
   * use of request Interceptors is to enable ORB services to transfer 
   * context information between clients and servers. There are two types 
   * of request Interceptors: client-side and server-side. 
   * <p>
   * To write a server-side Interceptor, implement the 
   * ServerRequestInterceptor interface.
   *
   * @see ServerRequestInfo
   */
public interface ServerRequestInterceptorOperations  extends org.omg.PortableInterceptor.InterceptorOperations
{

  /**
       * Allows the interceptor to process service context information.
       * <p>
       * At this interception point, Interceptors must get their service 
       * context information from the incoming request transfer it to 
       * <code>PortableInterceptor.Current</code>'s slots.  
       * <p>
       * This interception point is called before the servant manager is called. 
       * Operation parameters are not yet available at this point. This 
       * interception point may or may not execute in the same thread as 
       * the target invocation. 
       * <p>
       * This interception point may throw a system exception. If it does, 
       * no other Interceptors' <code>receive_request_service_contexts</code> 
       * operations are called. Those Interceptors on the Flow Stack are 
       * popped and their <code>send_exception</code> interception points are 
       * called. 
       * <p>
       * This interception point may also throw a <code>ForwardRequest</code> 
       * exception.  If an Interceptor throws this exception, no other 
       * Interceptors' <code>receive_request_service_contexts</code> operations 
       * are called. Those Interceptors on the Flow Stack are popped and 
       * their <code>send_other</code> interception points are called. 
       * <p>
       * Compliant Interceptors shall properly follow 
       * <code>completion_status</code> semantics if they throw a system 
       * exception from this interception point. The 
       * <code>completion_status</code> shall be COMPLETED_NO.
       *
       * @param ri Information about the current request being intercepted.
       * @exception ForwardRequest If thrown, indicates to the ORB that a
       *     retry of the request should occur with the new object given in
       *     the exception.
       */
  void receive_request_service_contexts (org.omg.PortableInterceptor.ServerRequestInfo ri) throws org.omg.PortableInterceptor.ForwardRequest;

  /**
       * Allows an Interceptor to query request information after all the 
       * information, including operation parameters, are available. This 
       * interception point shall execute in the same thread as the target 
       * invocation.
       * <p>
       * In the DSI model, since the parameters are first available when 
       * the user code calls <code>arguments</code>, <code>receive_request</code>
       * is called from within <code>arguments</code>. It is possible that 
       * <code>arguments</code> is not called in the DSI model. The target 
       * may call <code>set_exception</code> before calling 
       * <code>arguments</code>. The ORB shall guarantee that 
       * <code>receive_request</code> is called once, either through 
       * <code>arguments</code> or through <code>set_exception</code>. If it 
       * is called through <code>set_exception</code>, requesting the 
       * arguments will result in <code>NO_RESOURCES</code> being thrown with 
       * a standard minor code of 1. 
       * <p>
       * This interception point may throw a system exception. If it does, no 
       * other Interceptors' <code>receive_request</code> operations are 
       * called. Those Interceptors on the Flow Stack are popped and their 
       * <code>send_exception</code> interception points are called. 
       * <p>
       * This interception point may also throw a <code>ForwardRequest</code> 
       * exception.  If an Interceptor throws this exception, no other 
       * Interceptors' <code>receive_request</code> operations are called. 
       * Those Interceptors on the Flow Stack are popped and their 
       * <code>send_other</code> interception points are called.
       * <p>
       * Compliant Interceptors shall properly follow 
       * <code>completion_status</code> semantics if they throw a system 
       * exception from this interception point. The 
       * <code>completion_status</code> shall be <code>COMPLETED_NO</code>.
       *
       * @param ri Information about the current request being intercepted.
       * @exception ForwardRequest If thrown, indicates to the ORB that a
       *     retry of the request should occur with the new object given in
       *     the exception.
       */
  void receive_request (org.omg.PortableInterceptor.ServerRequestInfo ri) throws org.omg.PortableInterceptor.ForwardRequest;

  /**
       * Allows an Interceptor to query reply information and modify the 
       * reply service context after the target operation has been invoked 
       * and before the reply is returned to the client. This interception 
       * point shall execute in the same thread as the target invocation. 
       * <p>
       * This interception point may throw a system exception. If it does, 
       * no other Interceptors' <code>send_reply</code> operations are called. 
       * The remaining Interceptors in the Flow Stack shall have their 
       * <code>send_exception</code> interception point called. 
       * <p>
       * Compliant Interceptors shall properly follow 
       * <code>completion_status</code> semantics if they throw a 
       * system exception from this interception point. The 
       * <code>completion_status</code> shall be <code>COMPLETED_YES</code>.
       *
       * @param ri Information about the current request being intercepted.
       */
  void send_reply (org.omg.PortableInterceptor.ServerRequestInfo ri);

  /**
       * Allows an Interceptor to query the exception information and modify 
       * the reply service context before the exception is thrown to the client. 
       * When an exception occurs, this interception point is called. This 
       * interception point shall execute in the same thread as the target 
       * invocation. 
       * <p>
       * This interception point may throw a system exception. This has the 
       * effect of changing the exception which successive Interceptors 
       * popped from the Flow Stack receive on their calls to 
       * <code>send_exception</code>. The exception thrown to the client will 
       * be the last exception thrown by an Interceptor, or the original 
       * exception if no Interceptor changes the exception. 
       * <p>
       * This interception point may also throw a <code>ForwardRequest</code> 
       * exception.  If an Interceptor throws this exception, no other 
       * Interceptors' <code>send_exception</code> operations are called. The 
       * remaining Interceptors in the Flow Stack shall have their 
       * <code>send_other</code> interception points called. 
       * <p>
       * If the <code>completion_status</code> of the exception is not 
       * <code>COMPLETED_NO</code>, then it is inappropriate for this 
       * interception point to throw a <code>ForwardRequest</code> exception. 
       * The request's at-most-once semantics would be lost. 
       * <p>
       * Compliant Interceptors shall properly follow 
       * <code>completion_status</code> semantics if they throw a system 
       * exception from this interception point. If the original exception 
       * is a system exception, the <code>completion_status</code> of the new 
       * exception shall be the same as on the original. If the original 
       * exception is a user exception, then the <code>completion_status</code> 
       * of the new exception shall be <code>COMPLETED_YES</code>.
       *
       * @param ri Information about the current request being intercepted.
       * @exception ForwardRequest If thrown, indicates to the ORB that a
       *     retry of the request should occur with the new object given in
       *     the exception.
       */
  void send_exception (org.omg.PortableInterceptor.ServerRequestInfo ri) throws org.omg.PortableInterceptor.ForwardRequest;

  /**
       * Allows an Interceptor to query the information available when a 
       * request results in something other than a normal reply or an 
       * exception. For example, a request could result in a retry 
       * (e.g., a GIOP Reply with a <code>LOCATION_FORWARD</code> status was 
       * received). This interception point shall execute in the same thread 
       * as the target invocation. 
       * <p>
       * This interception point may throw a system exception. If it does, 
       * no other Interceptors' <code>send_other</code> operations are called. 
       * The remaining Interceptors in the Flow Stack shall have their 
       * <code>send_exception</code> interception points called. 
       * <p>
       * This interception point may also throw a <code>ForwardRequest</code> 
       * exception.  If an Interceptor throws this exception, successive 
       * Interceptors' <code>send_other</code> operations are called with 
       * the new information provided by the <code>ForwardRequest</code> 
       * exception. 
       * <p>
       * Compliant Interceptors shall properly follow 
       * <code>completion_status</code> semantics if they throw a system 
       * exception from this interception point. The 
       * <code>completion_status</code> shall be <code>COMPLETED_NO</code>.
       *
       * @param ri Information about the current request being intercepted.
       * @exception ForwardRequest If thrown, indicates to the ORB that a
       *     retry of the request should occur with the new object given in
       *     the exception.
       */
  void send_other (org.omg.PortableInterceptor.ServerRequestInfo ri) throws org.omg.PortableInterceptor.ForwardRequest;
} // interface ServerRequestInterceptorOperations
