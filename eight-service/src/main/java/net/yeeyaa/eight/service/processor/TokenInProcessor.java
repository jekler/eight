package net.yeeyaa.eight.service.processor;

import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.service.ServiceMsg;


public class TokenInProcessor<K> implements IProcessor<ServiceMsg, ServiceMsg>{
	protected IOutputResource<K, String> resource;
	protected K[] paras;

	public void setResource(IOutputResource<K, String> resource) {
		this.resource = resource;
	}

	public void setParas(K[] paras) {
		this.paras = paras;
	}

	@Override
	public ServiceMsg process(ServiceMsg msg) {
		if(msg != null && msg.token != null) resource.store(msg.token, paras);
		return msg;
	}
}
