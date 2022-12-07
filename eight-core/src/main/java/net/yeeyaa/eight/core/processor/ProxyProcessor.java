package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ProxyProcessor<T, R> implements IProcessor<T, R>, IBiProcessor<Object, Object, Object> {
	protected final Logger log;
	protected IProcessor<T, T> preProcessor;
	protected IProcessor<T, R> processor;
	protected IProcessor<R, R> postProcessor;
	protected IProcessor<Object[], R> aroundProcessor;
	protected Object id;
	
	public ProxyProcessor() {
		this.log = LoggerFactory.getLogger(ProxyProcessor.class);
	}
	
	public ProxyProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ProxyProcessor.class) : log;
	}
	
	public void setId(Object id) {
		this.id = id;
	}

	public void setAroundProcessor(IProcessor<Object[], R> aroundProcessor) {
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
		try{
			R ret = null;
			if(preProcessor != null) instance = preProcessor.process(instance);
			if(aroundProcessor != null) ret = aroundProcessor.process(new Object[]{processor, instance});
			else if(processor != null) ret = processor.process(instance);
			if(postProcessor != null) ret = postProcessor.process(ret);
			return ret;
		}catch(Exception e){
			log.error("ProxyProcessor: processor failed.", e);
		}
		return null;
	}

	@Override
	public Object perform(Object name, Object content) {
		return id;
	}
}