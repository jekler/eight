package net.yeeyaa.eight.core.pipeline.base;

import java.util.Map;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class For implements IProcessor<Map<String, String>, Void> {
	protected final Logger log;
	protected String regex="\\|";
	protected IProcessor<Object, Object> beanHolder;
	
	public For() {
		this.log = LoggerFactory.getLogger(For.class);
	}

	public For(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(For.class) : log;
	}
	
	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}
	
	public void setRegex(String regex) {
		if(regex != null) this.regex = regex;
	}
	@Override
	public Void process(Map<String, String> paras) {
		try {
			Object o = beanHolder.process(paras.get("bean"));
			if (IProcessor.class.isInstance(o)){
				String[] parameters = paras.get("paras").split(regex);
				if(parameters.length > 2){
					Double i = new Double(parameters[0]);
					Double j = new Double(parameters[1]);
					Double k = new Double(parameters[2]);
					String breakpoint = paras.get("break");
					IInputResource ctx = null;
					String key = null;
					String stopflag = null;
					if(breakpoint != null) {
						String[] terminalset = breakpoint.split(regex);
						if(terminalset.length > 2){
							Object obj = beanHolder.process(terminalset[0]);
							if(IInputResource.class.isInstance(obj)) ctx =(IInputResource) obj;
							key = terminalset[1];
							stopflag = terminalset[2];
						}				
					}
					if ("-".equals(paras.get("change"))){
						for (; i > k ; i -= j){
							if(key != null && stopflag != null && ctx != null && stopflag.equals(ctx.find(key))) break;
							paras.put("i", i.toString());
							((IProcessor<Map<String, String>, Void>) o).process(paras);
						}
					}
					else {
						for (; i < k ; i += j){
							if(key != null && stopflag != null && ctx != null && stopflag.equals(ctx.find(key))) break;
							paras.put("i", i.toString());
							((IProcessor<Map<String, String>, Void>) o).process(paras);					
						}
					}
				}
			}
		} catch (Exception e) {
			log.error("For: performing error.", e);
		} 
		return null;
	}
}
