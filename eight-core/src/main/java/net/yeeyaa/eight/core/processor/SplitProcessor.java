package net.yeeyaa.eight.core.processor;

import java.util.HashMap;

import net.yeeyaa.eight.IProcessor;


public class SplitProcessor implements IProcessor<Object, Object> {
	protected String regex=":";
	protected Boolean map;
	
	public void setMap(Boolean map) {
		this.map = map;
	}

	public void setRegex(String regex) {
		if(regex != null) this.regex = regex;
	}
	
	@Override
	public Object process(Object in) {
		if(in == null) return null;
		else if (Boolean.TRUE.equals(map)) {
			String[] pairs = in.toString().split(regex + regex);
			HashMap<String, String> ret = new HashMap<String, String>(pairs.length * 2);
			for (String pair : pairs) {
				String[] kv = pair.split(regex);
				if (kv.length > 1) ret.put(kv[0], kv[1]);
			}
			return ret;
		} else return in.toString().split(regex);
	}
}
