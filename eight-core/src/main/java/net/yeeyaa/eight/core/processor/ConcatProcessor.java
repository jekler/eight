package net.yeeyaa.eight.core.processor;

import java.lang.reflect.Array;
import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;


public class ConcatProcessor implements IProcessor<Object, String> {
	protected String regex=":";

	public void setRegex(String regex) {
		if(regex != null) this.regex = regex;
	}
	
	@Override
	public String process(Object in) {
		if(in == null) return null;
		else if (in instanceof Map) {
			StringBuilder sb = new StringBuilder();
			for (Entry<Object, Object> entry : ((Map<Object, Object>)in).entrySet()) 
				sb.append(regex).append(regex).append(entry.getKey()).append(regex).append(entry.getValue());
			return sb.length() > 1 ? sb.substring(2).toString() : "";
		} else if (in instanceof Collection) {
			StringBuilder sb = new StringBuilder();
			for (Object o : ((Collection<Object>)in)) sb.append(regex).append(o);
			return sb.length() > 0 ? sb.substring(1).toString() : "";
		} else if(in.getClass().isArray()){
			StringBuilder sb = new StringBuilder();
    		for (int i = 0; i < Array.getLength(in); i ++) sb.append(regex).append(Array.get(in, i));
    		return sb.length() > 0 ? sb.substring(1).toString() : "";
    	} else return in.toString();
	}
}
