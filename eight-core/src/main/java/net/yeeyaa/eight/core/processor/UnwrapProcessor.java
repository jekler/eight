package net.yeeyaa.eight.core.processor;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.Content;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class UnwrapProcessor<T> implements IProcessor<Object, Object> {	
	protected final Logger log;
	protected Integer deep = 0;
	protected Boolean reflect = false;
	
	public UnwrapProcessor() {
		this.log = LoggerFactory.getLogger(UnwrapProcessor.class);
	}
	
	public UnwrapProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(UnwrapProcessor.class) : log;
	}
	
	public void setDeep(Integer deep) {
		if(deep != null && deep > 0) this.deep = deep;
	}

	public void setReflect(Boolean reflect) {
		if(reflect != null) this.reflect = reflect;
	}

	@Override
	public Object process(Object in) {
		if(deep == 0) return Content.<Object>strip(in);
		else if(deep == 1) {
			if(in != null) {
				if(in instanceof Content) in = ((Content)in).get();
				if(in != null) {
					if(in instanceof Map) for(Entry<Object, Object> entry : ((Map<Object, Object>) in).entrySet()){
						if(entry.getValue() instanceof Content) entry.setValue(((Content)entry.getValue()).get());
					}else if(in instanceof Collection){
						Object[] os = ((Collection)in).toArray();
						((Collection)in).clear();
						for(Object o : os) {
							if(o instanceof Content) o = ((Content)o).get();
							((Collection)in).add(o);
						}
					}else if(Object[].class.equals(in.getClass())) {
						ArrayList<Object> ret = new ArrayList<Object>(Array.getLength(in));
			    		for (int i = 0; i < Array.getLength(in); i ++) {
			    			Object o = Array.get(in, i);
			    			if(o instanceof Content) o = ((Content)o).get();
			    			ret.add(o);
			    		}
			    		in = ret.toArray((Object[])in);
					}else if(reflect && !in.getClass().isArray()) for(Field field : getAllField(in.getClass())) 
						if(Object.class.equals(field.getType()) && !Modifier.isStatic(field.getModifiers())) try{
							field.setAccessible(true);
							Object o = field.get(in);
							if(o instanceof Content) field.set(in, ((Content)o).get());
					}catch(Exception e){
						log.error("UnwrapProcessor: processor failed: " + in, e);
					}
				}
			}
			return in;
		} else return unwrap(in, new HashSet<Object>());
	}
	
	protected Object unwrap(Object in, Collection<Object> set){
		if(in != null) {
			if(in instanceof Content) in = ((Content)in).get();
			if(in != null && !set.contains(in)) {
				set.add(in);
				if(in instanceof Map) for(Entry<Object, Object> entry : ((Map<Object, Object>) in).entrySet()){
					entry.setValue(unwrap(entry.getValue(), set));
				}else if(in instanceof Collection){
					Object[] os = ((Collection)in).toArray();
					((Collection)in).clear();
					for(Object o : os) ((Collection)in).add(unwrap(o, set));
				}else if(Object[].class.equals(in.getClass())) {
					ArrayList<Object> ret = new ArrayList<Object>(Array.getLength(in));
		    		for (int i = 0; i < Array.getLength(in); i ++) ret.add(unwrap(Array.get(in, i), set));
		    		in = ret.toArray((Object[])in);
				}else if(in.getClass().isArray()) {
					ArrayList<Object> ret = new ArrayList<Object>(Array.getLength(in));
		    		for (int i = 0; i < Array.getLength(in); i ++) unwrap(Array.get(in, i), set);
				}else if(reflect) for(Field field : getAllField(in.getClass())) 
					if(!Modifier.isStatic(field.getModifiers()) && (Object.class.equals(field.getType()) || deep == 4 || (deep == 3 &&
							(Map.class.isAssignableFrom(field.getType()) || Collection.class.isAssignableFrom(field.getType()) || field.getType().isArray())))) try{
					field.setAccessible(true);
					field.set(in, unwrap(field.get(in), set));
				}catch(Exception e){
					log.error("UnwrapProcessor: processor failed: " + in, e);
				}
			}
		}
		return in;
	}
	
	protected Field[] getAllField(Class<?> clazz) {
		List<Field> ls = new LinkedList<Field>();
	    while(clazz != null){
	    	ls.addAll(Arrays.asList(clazz.getDeclaredFields()));
	    	clazz = clazz.getSuperclass();
	    }
	    return ls.toArray(new Field[ls.size()]);
	}
}
