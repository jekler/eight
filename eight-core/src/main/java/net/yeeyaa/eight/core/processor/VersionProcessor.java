package net.yeeyaa.eight.core.processor;

import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;


public class VersionProcessor implements IProcessor<Object, Object> {//Real entity + (platformBean + plaformRef/VoidProcessor + other factory) + this + PlatformUniversal = proxy every thing by version
	protected String ver = System.getProperty("java.specification.version");
	protected Object matcher;
	protected Boolean proxy; //null: proxy and cache; true: always proxy; false; return directly
	protected volatile Object cache;
	
	public VersionProcessor() {}

	public VersionProcessor(Object matcher, String ver) {
		this.matcher = matcher; //default matcher
		this.ver = ver; //via other string bean factory
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
			if (o != null) this.matcher = o;//matcher exist --> equals (default matcher). matcher not exist --> compare (default null-->value or ""-->value)
			if (this.matcher == null) for (Entry<String, Object> entry : matcher.entrySet()) if (entry.getKey() != null && entry.getKey().compareTo(ver) <= 0) {
				this.matcher = entry.getValue();  //map matcher: big --> small order
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