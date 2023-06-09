package net.yeeyaa.eight.core.processor;

import java.util.Collection;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.TypeConvertor;


public class MergeStringProcessor implements IProcessor<Object, String> {
	protected Boolean nullable = true;
	protected String prefix;
	protected String append;
	protected String p;
	protected String a;	
	protected String splitter;
	
	public void setNullable(Boolean nullable) {
		if (nullable != null) this.nullable = nullable;
	}
	
	public void setPrefix(String prefix) {
		this.prefix = prefix;
	}

	public void setAppend(String append) {
		this.append = append;
	}

	public void setP(String p) {
		this.p = p;
	}

	public void setA(String a) {
		this.a = a;
	}

	public void setSplitter(String splitter) {
		this.splitter = splitter;
	}

	@Override
	public String process(Object in) {
		StringBuilder sb = new StringBuilder();
		if(in != null) {
			if(prefix != null) sb.append(prefix);
			if(in.getClass().isArray()) in = TypeConvertor.asCollection(in);
			if(in instanceof Collection) { 
				for(Object o : (Collection) in) if(nullable || o != null) {
					if (p != null) sb.append(p); 
					sb.append(o); 
					if (a != null) sb.append(a);
					if (splitter != null) sb.append(splitter); 
				}
				if (splitter != null) sb.delete(sb.length() - splitter.length(), sb.length());
			} else {
				if (p != null) sb.append(p); 
				sb.append(in);
				if (a != null) sb.append(a);
			}
			if(append != null) sb.append(append);
		} else if (nullable) {
			if(prefix != null) sb.append(prefix);
			if (p != null) sb.append(p); 
			sb.append(in);
			if (a != null) sb.append(a);
			if(append != null) sb.append(append);
		}
		return sb.toString();
	}
}
