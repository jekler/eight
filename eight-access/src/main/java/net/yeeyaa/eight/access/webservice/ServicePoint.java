package net.yeeyaa.eight.access.webservice;

import javax.activation.DataHandler;
import javax.annotation.Resource;
import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebService;
import javax.jws.soap.SOAPBinding;
import javax.jws.soap.SOAPBinding.ParameterStyle;
import javax.jws.soap.SOAPBinding.Style;
import javax.jws.soap.SOAPBinding.Use;
import javax.xml.bind.annotation.XmlMimeType;
import javax.xml.ws.WebServiceContext;
import javax.xml.ws.handler.MessageContext;
import javax.xml.ws.soap.MTOM;

import net.yeeyaa.eight.IProcessor;


@MTOM(enabled=true)
@WebService
@SOAPBinding(style = Style.DOCUMENT, use = Use.LITERAL, parameterStyle = ParameterStyle.WRAPPED)
public class ServicePoint{
	@Resource 
	protected WebServiceContext context;
	protected IProcessor<String, String> next;
	protected IProcessor<MessageContext, Void> preProcessor;
	
	@WebMethod
	public String service(@WebParam(name = "msg")String msg){
		if(preProcessor != null) preProcessor.process(context.getMessageContext());
		return next.process(msg);
	}

	@WebMethod
	public DataHandler stream(@WebParam(name = "target")String target, @WebParam(name = "token")String token, @XmlMimeType("application/octet-stream") @WebParam(name = "data") DataHandler data) {

		return null;
	}
	
	@WebMethod(exclude=true)
	public void setNext(IProcessor<String, String> next) {
		this.next = next;
	}

	@WebMethod(exclude=true)
	public void setPreProcessor(IProcessor<MessageContext, Void> preProcessor) {
		this.preProcessor = preProcessor;
	}
}
