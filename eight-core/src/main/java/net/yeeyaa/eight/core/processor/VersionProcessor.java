package net.yeeyaa.eight.core.processor;

import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;


public class VersionProcessor implements IProcessor<Object, Object> {
	protected String ver = System.getProperty("java.specification.version");
	protected Object matcher;
	protected Boolean proxy; 
	protected volatile Object cache;
	
	public VersionProcessor() {}

	public VersionProcessor(Object matcher, String ver) {
		this.matcher = matcher; 
		this.ver = ver; 
	}

	public VersionProcessor(String ver) {
		this.ver = ver;
	}

	public VersionProcessor(Object matcher) {
		this.matcher = matcher;
	}

	public void setMatcher(Map<String, Object> matcher) {
		if (matcher != null)  {
			Object o = matcher.get(ver);
			if (o != null) this.matcher = o;
			if (this.matcher == null) for (Entry<String, Object> entry : matcher.entrySet()) if (entry.getKey() != null && entry.getKey().compareTo(ver) <= 0) {
				this.matcher = entry.getValue();  
				break;
			}
		}
	}

	@Override
	public Object process(Object instance) {
		if (cache != null) return cache;
		else if (proxy == null) {
			if (matcher instanceof IProcessor) cache = ((IProcessor<Object, Object>) matcher).process(instance);
			return cache;
		} else if (proxy) {
			if (matcher instanceof IProcessor) return ((IProcessor<Object, Object>) matcher).process(instance);
			else return null;
		} else return matcher;
	}
}
