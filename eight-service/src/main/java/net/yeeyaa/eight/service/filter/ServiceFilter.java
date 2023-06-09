package net.yeeyaa.eight.service.filter;

import java.util.Map;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.service.ServiceMsg;


public class ServiceFilter implements IProcessor<ServiceMsg, ServiceMsg>{
	protected IProcessor<ServiceMsg, ServiceMsg> next;
	protected Map<String, IProcessor<ServiceMsg, ServiceMsg>> filters;
	protected Boolean alwaysNext = false;
	protected IProcessor<String, String> nameProcessor;
	
	public void setNameProcessor(IProcessor<String, String> nameProcessor) {
		this.nameProcessor = nameProcessor;
	}

	public void setAlwaysNext(Boolean alwaysNext) {
		if(alwaysNext != null) this.alwaysNext = alwaysNext;
	}

	public void setNext(IProcessor<ServiceMsg, ServiceMsg> next) {
		this.next = next;
	}
	
	public void setFilters(Map<String, IProcessor<ServiceMsg, ServiceMsg>> filters) {
		this.filters = filters;
	}

	@Override
	public ServiceMsg process(ServiceMsg msg) {
		String key = msg.name;
		if(nameProcessor != null) key = nameProcessor.process(key);
		if(filters.containsKey(key)) {
			msg = filters.get(key).process(msg);
			if(alwaysNext) msg = next.process(msg);
			return msg;
		} else return next.process(msg);
	}
}
