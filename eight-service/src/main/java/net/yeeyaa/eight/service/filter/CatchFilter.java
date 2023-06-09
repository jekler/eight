package net.yeeyaa.eight.service.filter;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.service.ServiceError;
import net.yeeyaa.eight.service.ServiceMsg;


public class CatchFilter<K> implements IProcessor<ServiceMsg, ServiceMsg>{
	protected IProcessor<ServiceMsg, ServiceMsg> next;
	
	public void setNext(IProcessor<ServiceMsg, ServiceMsg> next) {
		this.next = next;
	}

	@Override
	public ServiceMsg process(ServiceMsg msg) {
		try{
			msg = next.process(msg);
		}catch(PlatformException e){
			msg.error = e.getType();
			msg.content = null;
		}catch(Exception e){
			msg.error = ServiceError.FILTER_FAIL;
			msg.content = null;
		}
		return msg;
	}
}
