package net.yeeyaa.eight.core.processor;

import java.util.UUID;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;

//level 1: idProxy + idProxy; level 2: Void + beanHolder + idProxy = idproxy; //level m: beanHolder + beanHolder + ... + idProxy (or) idProxy + idProxy + .. + idProxy
public class IdProxyProcessor implements IProcessor<Object, Object>, IBiProcessor<Object, Object, Object>{
	protected IBiProcessor<Object, Object, Object> processor;//outer processor proxy
	protected IProcessor<Object, Object> proxy; //callback + platformUniveral = platformUniveral proxy
	
	public void setProcessor(IBiProcessor<Object, Object, Object> processor) {
		this.processor = processor;
	}

	public void setProxy(IProcessor<Object, Object> proxy) {
		this.proxy = proxy;
	}

	protected class Holder implements IProcessor<Object, Object> {
		protected final Object id;
		protected final Object paras;
		
		public Holder(Object id, Object paras) {
			this.id = id;
			this.paras = paras;
		}

		@Override
		public Object process(Object object) {
			return processor.perform(id, paras);
		}
	}
	
	@Override
	public Object process(final Object paras) {
		return proxy == null ? new Holder(UUID.randomUUID(), paras) : proxy.process(new Holder(UUID.randomUUID(), paras));
	}

	@Override
	public Object perform(Object id, Object paras) {
		return proxy == null ? new Holder(id, paras) : proxy.process(new Holder(id, paras));
	}
}
