package net.yeeyaa.eight.core.processor;

import java.util.List;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class SelectXProcessor<T> implements IProcessor<T, T>{
	protected final Logger log;
	protected List<IProcessor<T, T>> processors;
	protected Boolean nullable; 
	
	public SelectXProcessor() {
		this.log = LoggerFactory.getLogger(SelectXProcessor.class);
	}
	
	public SelectXProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(SelectXProcessor.class) : log;
	}
	
	public void setNullable(Boolean nullable) {
		this.nullable = nullable;
	}
	
	public void setProcessors(List<IProcessor<T, T>> processors) {
		this.processors = processors;
	}

	@Override
	public T process(T in) {	
		T ret = null;
		if(processors != null) for(IProcessor<T, T> processor : processors)  try{
			ret = processor.process(in);
			if(Boolean.TRUE.equals(nullable) || (nullable == null && ret != null)) return ret;
		}catch(Exception e){
			log.error("SelectXProcessor: processor failed.", e);
		} 
		return ret;
	}
}
