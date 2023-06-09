package net.yeeyaa.eight.service.processor;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.service.ServiceMsg;


public class TokenOutProcessor<K> implements IProcessor<ServiceMsg, ServiceMsg>{
	protected IInputResource<K, String> resource;
	protected K[] paras;

	public void setResource(IInputResource<K, String> resource) {
		this.resource = resource;
	}

	public void setParas(K[] paras) {
		this.paras = paras;
	}

	@Override
	public ServiceMsg process(ServiceMsg msg) {
		msg.token = resource.find(paras);
		return msg;
	}
}
