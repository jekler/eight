package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class OutputProcessor<K, V> implements IProcessor<V, V> {
	protected final Logger log;
	protected IOutputResource<K, V> resource;
	protected K[] paras;
	protected Boolean ignoreError = false;;	
	protected Boolean method; 
	
	public OutputProcessor() {
		this.log = LoggerFactory.getLogger(OutputProcessor.class);
	}
	
	public OutputProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(OutputProcessor.class) : log;
	}
	
	public void setMethod(Boolean method) {
		this.method = method;
	}

	public void setIgnoreError(Boolean ignoreError) {
		if(ignoreError != null) this.ignoreError = ignoreError;
	}

	public void setResource(IOutputResource<K, V> resource) {
		this.resource = resource;
	}

	public void setParas(K[] paras) {
		this.paras = paras;
	}

	@Override
	public V process(V in) {	
		try{
			if (method == null) resource.store(in, paras);
			else if (method) return resource.discard(paras);
			else resource.empty(paras);
		}catch(Exception e){
			log.error("OutputProcessor: processor failed.", e);
			if(!ignoreError) if (e instanceof PlatformException) throw (PlatformException) e;
			else throw new PlatformException(PlatformError.ERROR_OTHER_FAIL, e);
		}
		return in;
	}
}
