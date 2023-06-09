package net.yeeyaa.eight.service.filter;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.service.ServiceError;
import net.yeeyaa.eight.service.ServiceMsg;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class MarshalFilter implements IProcessor<String, String>{
	protected final Logger log;
	protected IProcessor<ServiceMsg, ServiceMsg> next;
	protected IProcessor<ServiceMsg, String> marshaller;
	protected IProcessor<String, ServiceMsg> unmarshaller;	
	
	public MarshalFilter() {
		log = LoggerFactory.getLogger(MarshalFilter.class);
	}

	public MarshalFilter(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(MarshalFilter.class) : log;
	}
	
	public void setNext(IProcessor<ServiceMsg, ServiceMsg> next) {
		this.next = next;
	}
	
	public void setMarshaller(IProcessor<ServiceMsg, String> marshaller) {
		this.marshaller = marshaller;
	}

	public void setUnmarshaller(IProcessor<String, ServiceMsg> unmarshaller) {
		this.unmarshaller = unmarshaller;
	}

	@Override
	public String process(String msg) {
		ServiceMsg newMsg = null;
		String ret = null;
		try{
			newMsg = unmarshaller.process(msg);
			newMsg = next.process(newMsg);
			if (newMsg != null) ret = marshaller.process(newMsg);
		}catch(PlatformException e){
			log.error("MarshalFilter: processor failed.", e);
			if(newMsg == null) newMsg = new ServiceMsg();
			newMsg.error = e.getType();
			newMsg.content = null;
			if (newMsg != null) ret = marshaller.process(newMsg);
		}catch(Exception e){
			log.error("MarshalFilter: processor failed.", e);
			if(newMsg == null) newMsg = new ServiceMsg();
			newMsg.error = ServiceError.ERROR_QUERY_MSG;
			newMsg.content = null;
			ret = marshaller.process(newMsg);
		}
		return ret;
	}
}
