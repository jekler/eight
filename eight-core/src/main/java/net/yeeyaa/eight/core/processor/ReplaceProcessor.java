package net.yeeyaa.eight.core.processor;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.yeeyaa.eight.IProcessor;


public class ReplaceProcessor implements IProcessor<Object[], String> {
	protected String template;
	protected Pattern pattern = Pattern.compile("\\?");
	protected IProcessor<Object, String> merger;
	
	public void setMerger(IProcessor<Object, String> merger) {
		this.merger = merger;
	}

	public void setTemplate(String template) {
		this.template = template;
	}

	public void setRegex(String regex) {
		if(regex != null) pattern = Pattern.compile(regex);
	}
	
	@Override
	public String process(Object[] in) {
		if(template != null) {
		    StringBuffer s = new StringBuffer();
		    Matcher matcher = pattern.matcher(template);
		    int count = 0;
		    int len = (in == null) ? 0 : in.length;
		    while (matcher.find()) {
		    	String replace = "";
		    	if (count < len) {
		    		 Object o = in[count];
		    		 replace = merger != null ? merger.process(o) : o.toString();
		    	}
		    	matcher.appendReplacement(s, replace);
		    	count ++;
		    }
		    matcher.appendTail(s);
			return s.toString();
		} else return null;
	}
}
