package net.yeeyaa.eight.core.pipeline.common;

import java.util.Map;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class TaskAdaptor implements IProcessor<Map<String, String>, Void> {
	protected final Logger log;
	protected IProcessor<Object, Object> beanHolder;
	
	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}
	
	public TaskAdaptor() {
		this.log = LoggerFactory.getLogger(TaskAdaptor.class);
	}

	public TaskAdaptor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(TaskAdaptor.class) : log;
	}
	
	
	@Override
	public Void process(Map<String, String> paras) {
		String bean = paras.get("bean");
		String newparas = paras.get("paras");
		if(bean != null && bean.trim().length() > 0)try{
			Object o = beanHolder.process(bean);
			if(IProcessor.class.isInstance(o)) ((IProcessor<String, Void>)o).process(newparas);
		}catch(Exception e){
			log.error("TaskAdaptor: performing error.", e);
		}
		return null;
	}
}
