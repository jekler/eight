package net.yeeyaa.eight.core.processor;

import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;


public class MapConvertProcessor implements IProcessor<Map<Object, Object>, Map<Object, Object>>{
	protected Map<Object, IProcessor<Object, Object>> matchers;
	protected IProcessor<Object, Object> defaultMatcher;
	
	public void setMatchers(Map<Object, IProcessor<Object, Object>> matchers) {
		this.matchers = matchers;
	}

	public void setDefaultMatcher(IProcessor<Object, Object> defaultMatcher) {
		this.defaultMatcher = defaultMatcher;
	}

	@Override
	public Map<Object, Object> process(Map<Object, Object> instance) {
		if(instance != null && instance.size() > 0) for (Entry<Object, Object> entry : instance.entrySet()) if (matchers != null && matchers.containsKey(entry.getKey())) entry.setValue(matchers.get(entry.getKey()).process(entry.getValue()));
			else if (defaultMatcher != null) entry.setValue(defaultMatcher.process(entry.getValue()));
		return instance;
	}
}
