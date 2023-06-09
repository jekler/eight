package net.yeeyaa.eight.client;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;

public class ProxyClient<K, V, R> implements IBiProcessor<K, V, R>{
	protected IProcessor<K, IProcessor<Object, R>> beanHolder;
	
	public void setBeanHolder(IProcessor<K, IProcessor<Object, R>> beanHolder) {
		this.beanHolder = beanHolder;
	}

	@Override
	public R perform(K name, V parameters) {
		IProcessor<Object, R> processor = beanHolder.process(name);
		if(processor != null) return processor.process(parameters);
		else return null;
	}
}
