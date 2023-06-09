package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;


public class ProxyProcessor<T, R> implements IProcessor<T, R>, IBiProcessor<Object, Object, Object> {
	protected IProcessor<T, T> preProcessor;
	protected IProcessor<T, R> processor;
	protected IProcessor<R, R> postProcessor;
	protected IBiProcessor<IProcessor<T, R>, T, R> aroundProcessor;
	protected Object id;
	
	public void setId(Object id) {
		this.id = id;
	}

	public void setAroundProcessor(IBiProcessor<IProcessor<T, R>, T, R> aroundProcessor) {
		this.aroundProcessor = aroundProcessor;
	}

	public void setPreProcessor(IProcessor<T, T> preProcessor) {
		this.preProcessor = preProcessor;
	}

	public void setProcessor(IProcessor<T, R> processor) {
		this.processor = processor;
	}

	public void setPostProcessor(IProcessor<R, R> postProcessor) {
		this.postProcessor = postProcessor;
	}

	@Override
	public R process(T instance) {
		R ret = null;
		if(preProcessor != null) instance = preProcessor.process(instance);
		if(aroundProcessor != null) ret = aroundProcessor.perform(processor, instance);
		else if(processor != null) ret = processor.process(instance);
		if(postProcessor != null) ret = postProcessor.process(ret);
		return ret;
	}

	@Override
	public Object perform(Object name, Object content) {
		return id;
	}
}
