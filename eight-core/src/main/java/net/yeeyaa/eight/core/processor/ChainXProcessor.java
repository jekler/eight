package net.yeeyaa.eight.core.processor;

import java.util.Collection;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ChainXProcessor<T> implements IProcessor<T, T>{
	protected final Logger log;
	protected Collection<IProcessor<T, T>> processors;
	protected Boolean ignoreError = false;;	
	protected Boolean nullable = false;
	protected Boolean sync; 

	public ChainXProcessor() {
		this.log = LoggerFactory.getLogger(ChainXProcessor.class);
	}
	
	public ChainXProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ChainXProcessor.class) : log;
	}

	public void setSync(Boolean sync) {
		this.sync = sync;
	}

	public void setNullable(Boolean nullable) {
		if(nullable != null) this.nullable = nullable;
	}

	public void setIgnoreError(Boolean ignoreError) {
		if(ignoreError != null) this.ignoreError = ignoreError;
	}
	
	public void setProcessors(Collection<IProcessor<T, T>> processors) {
		this.processors = processors;
	}
	
	@Override
	public T process(T in) {
		if(processors != null && processors.size() > 0) if (sync == null) for(IProcessor<T, T> processor : processors)  synchronized(this) {
			try{
				in = processor.process(in);
				if(in == null && !nullable) return null;
			}catch(Exception e){
				log.error("ChainXProcessor: processor failed.", e);
				if(!ignoreError) if (e instanceof PlatformException) throw (PlatformException) e;
				else throw new PlatformException(PlatformError.ERROR_OTHER_FAIL, e);
				else if(!nullable) return null;
			}
		} else if(sync) synchronized(this) {
			for(IProcessor<T, T> processor : processors)  try{
				in = processor.process(in);
				if(in == null && !nullable) return null;
			}catch(Exception e){
				log.error("ChainXProcessor: processor failed.", e);
				if(!ignoreError) if (e instanceof PlatformException) throw (PlatformException) e;
				else throw new PlatformException(PlatformError.ERROR_OTHER_FAIL, e);
				else if(!nullable) return null;
			}
		} else for(IProcessor<T, T> processor : processors)  try{
			in = processor.process(in);
			if(in == null && !nullable) return null;
		}catch(Exception e){
			log.error("ChainXProcessor: processor failed.", e);
			if(!ignoreError) if (e instanceof PlatformException) throw (PlatformException) e;
			else throw new PlatformException(PlatformError.ERROR_OTHER_FAIL, e);
			else if(!nullable) return null;
		}
		return in;
	}
}
