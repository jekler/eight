package net.yeeyaa.eight.core.processor;

import java.util.Collection;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ForkXProcessor<T, R> implements IProcessor<T, T>{
	protected final Logger log;
	protected Collection<IProcessor<T, R>> processors;
	protected Boolean sync; 
	
	public ForkXProcessor() {
		this.log = LoggerFactory.getLogger(ForkXProcessor.class);
	}
	
	public ForkXProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ForkXProcessor.class) : log;
	}
	
	public void setSync(Boolean sync) {
		this.sync = sync;
	}
	
	public void setProcessors(Collection<IProcessor<T, R>> processors) {
		this.processors = processors;
	}
	
	@Override
	public T process(T in) {	
		if(processors != null && processors.size() > 0) if (sync == null) for(IProcessor<T, R> processor : processors)  synchronized(this) {
			try{
				processor.process(in);
			}catch(Exception e){
				log.error("ForkXProcessor: processor failed.", e);
			}
		} else if (sync) synchronized(this) {
			for(IProcessor<T, R> processor : processors)  try{
				processor.process(in);
			}catch(Exception e){
				log.error("ForkXProcessor: processor failed.", e);
			}
		} else for(IProcessor<T, R> processor : processors)  try{
			processor.process(in);
		}catch(Exception e){
			log.error("ForkXProcessor: processor failed.", e);
		}
		return in;
	}
}
