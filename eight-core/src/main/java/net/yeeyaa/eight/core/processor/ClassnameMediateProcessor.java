package net.yeeyaa.eight.core.processor;

import java.util.Map;

import net.yeeyaa.eight.IProcessor;


public class ClassnameMediateProcessor<T> implements IProcessor<Object[], T>{
	protected Map<String, IProcessor<Object[], T>> processors;
	protected IProcessor<Object[], T> defaultProcessor;
	
	public void setProcessors(Map<String, IProcessor<Object[], T>> processors) {
		this.processors = processors;
	}

	public void setDefaultProcessor(IProcessor<Object[], T> defaultProcessor) {
		this.defaultProcessor = defaultProcessor;
	}

	@Override
	public T process(Object[] in) {
		if(in != null && in.length > 0 && in[0] != null){
			IProcessor<Object[], T> processor= processors.get(in[0].getClass().getName());
			if(processor == null && defaultProcessor != null) processor = defaultProcessor;
			if(processor != null){
				Object[] paras = new Object[in.length - 1];
				for(int i = 1; i < in.length; i++) paras[i - 1] = in[i];
				return processor.process(paras);
			}
		}
		return null;
	}
}
