package net.yeeyaa.eight.core.processor;

import java.util.LinkedList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.yeeyaa.eight.IProcessor;


public class RegexProcessor implements IProcessor<String, Object> {
	protected Pattern pattern; 
	protected Integer index = 0;
	protected Boolean fetchAll = false;
	protected Boolean keepOrigin = false; 
	protected ThreadLocal<String> cache;
	
	public void setKeepOrigin(Boolean keepOrigin) {
		if(Boolean.TRUE.equals(keepOrigin)) {
			cache = new ThreadLocal<String>();
			this.keepOrigin = keepOrigin;
		}
	}

	public void setPattern(Pattern pattern) {
		this.pattern = pattern;
	}

	public void setIndex(Integer index) {
		if(index != null && index > 0) this.index = index;
	}

	public void setFetchAll(Boolean fetchAll) {
		if(fetchAll != null) this.fetchAll = fetchAll;
	}

	public void setRegex(String regex) {
		if(regex != null) pattern = Pattern.compile(regex);
	}
	
	@Override
	public Object process(String in) {
		if(keepOrigin) cache.set(in);
		if(in != null && pattern != null) {
			Matcher matcher = pattern.matcher(in);
			if(fetchAll) {
				LinkedList<String> matches = new LinkedList<String>();
				while(matcher.find()) matches.add(matcher.group(index));
				return matches.toArray(new String[matches.size()]);
			}else if(matcher.find()) return matcher.group(index);
		}
		return null;
	}
	
	public String getOrigin(){
		if(keepOrigin) return cache.get();
		else return null;
	}
	
	public class GetOrigin implements IProcessor<Object, String>{
		@Override
		public String process(Object instance) {
			return getOrigin();
		}
	}
}
