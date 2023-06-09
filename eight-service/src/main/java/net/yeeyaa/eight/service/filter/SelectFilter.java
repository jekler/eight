package net.yeeyaa.eight.service.filter;

import java.util.Collection;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.service.ServiceError;
import net.yeeyaa.eight.service.ServiceMsg;


public class SelectFilter implements IProcessor<ServiceMsg, ServiceMsg>{
	protected Collection<IProcessor<ServiceMsg, ServiceMsg>> filters;
	protected Boolean nullable = false;
	
	public void setNullable(Boolean nullable) {
		this.nullable = nullable;
	}

	public void setProcessors(Collection<IProcessor<ServiceMsg, ServiceMsg>> processors) {
		this.filters = processors;
	}
	
	@Override
	public ServiceMsg process(ServiceMsg msg) {
		ServiceMsg ret = new ServiceMsg(msg.token, msg.name, msg.id, null, ServiceError.NO_SUCH_SERVICE);
		for(IProcessor<ServiceMsg, ServiceMsg> filter : filters) try{
			ServiceMsg r = filter.process(msg);
			if(r != null && r.error == null || (r == null && nullable)) return r;
		}catch(Exception e){}
		return ret;
	}
}
