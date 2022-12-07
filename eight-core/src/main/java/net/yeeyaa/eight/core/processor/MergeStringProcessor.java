package net.yeeyaa.eight.core.processor;

import java.util.Collection;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.TypeConvertor;


public class MergeStringProcessor implements IProcessor<Object, String> {
	protected Boolean nullable = true;
	protected String prefix;
	protected String append;
	
	public void setNullable(Boolean nullable) {
		if (nullable != null) this.nullable = nullable;
	}
	
	public void setPrefix(String prefix) {
		this.prefix = prefix;
	}

	public void setAppend(String append) {
		this.append = append;
	}

	@Override
	public String process(Object in) {
		StringBuilder sb = new StringBuilder();
		if(prefix != null) sb.append(prefix);
		if(in != null) {
			if(in.getClass().isArray()) in = TypeConvertor.asCollection(in);
			if(in instanceof Collection) { for(Object o : (Collection) in) if(nullable || o != null) sb.append(o); }
			else sb.append(in);
		} else if (nullable) sb.append(in);
		if(append != null) sb.append(append);
		return sb.toString();
	}
}