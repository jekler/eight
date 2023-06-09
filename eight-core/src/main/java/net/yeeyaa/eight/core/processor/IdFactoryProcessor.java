package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;

public class IdFactoryProcessor implements IBiProcessor<Object, Object, Object>{
	protected IResource<Object, Object> resource;
	protected IProcessor<Object, Object> processor;
	
	public void setResource(IResource<Object, Object> resource) {
		this.resource = resource;
	}

	public void setProcessor(IProcessor<Object, Object> processor) {
		this.processor = processor;
	}

	@Override
	public Object perform(Object id, Object paras) {
		Object ret = resource.find(id);
		if (ret == null) synchronized (this) {
			ret = resource.find(id);
			if (ret == null) {
				ret = processor.process(paras);
				if (ret != null) resource.store(ret, id);
			}
		}
		return ret;
	}
}
