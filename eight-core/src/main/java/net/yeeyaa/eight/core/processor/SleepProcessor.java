package net.yeeyaa.eight.core.processor;

import javax.annotation.PostConstruct;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class SleepProcessor implements IProcessor<Object, Object>{
	protected final Logger log;
	protected long span;
	protected Boolean change; 
	
	public SleepProcessor() {
		this.log = LoggerFactory.getLogger(SleepProcessor.class);
	}
	
	public SleepProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(SleepProcessor.class) : log;
	}
	
	public void setSpan(Long span) {
		if (span != null && span > 0) this.span = span;
	}

	public void setChange(Boolean change) {
		this.change = change;
	}
	
	@PostConstruct
	public void initialize(){
		if (change == null && span > 0) span = Math.round(span * Math.random());
	}
	
	@Override
	public Object process(Object instance) {
		long delay = span;
		if (Boolean.TRUE.equals(change)) delay = Math.round(span * Math.random());
		if (delay > 0) try {
			Thread.sleep(span);
		} catch (Exception e) {
			log.error("SleepProcessor : process error.", e);
		}
		return instance;
	}
}
