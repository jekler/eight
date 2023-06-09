package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class CloneProcessor<T> implements IProcessor<T, T> {
	protected final Logger log;
	protected Boolean strict = true;
	
	public CloneProcessor() {
		this.log = LoggerFactory.getLogger(CloneProcessor.class);
	}
	
	public CloneProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(CloneProcessor.class) : log;
	}
	
	public void setStrict(Boolean strict) {
		if (strict != null) this.strict = strict;
	}
	
	@Override
	public T process(T in) {
		if(Cloneable.class.isInstance(in)) try{
			return (T)in.getClass().getMethod("clone").invoke(in);
		}catch(Exception e){
			log.error("CloneProcessor: processor failed.", e);
		}
		if(!strict) return in;
		else return null;
	}
}
