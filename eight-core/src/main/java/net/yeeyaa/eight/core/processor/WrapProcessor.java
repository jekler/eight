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


public class WrapProcessor<T> implements IProcessor<Object, Object> {	
	protected final Logger log;
	protected Integer deep = 0;
	protected Boolean outWrap = false;
	protected Boolean allWrap = false;
	protected Boolean reflect = false;

	public WrapProcessor() {
		this.log = LoggerFactory.getLogger(WrapProcessor.class);
	}
	
	public WrapProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(WrapProcessor.class) : log;
	}
	
	public void setDeep(Integer deep) {
		if(deep != null && deep > 0) this.deep = deep;
	}

	public void setOutWrap(Boolean outWrap) {
		if(outWrap != null) this.outWrap = outWrap;
	}

	public void setAllWrap(Boolean allWrap) {
		if(allWrap != null) this.allWrap = allWrap;
	}

	public void setReflect(Boolean reflect) {
		if(reflect != null) this.reflect = reflect;
	}

	@Override
	public Object process(Object in) {
		if(deep == 1){ 
			if(in != null) {
				if(in instanceof Map) for(Entry<Object, Object> entry : ((Map<Object, Object>) in).entrySet()){
					Object value = entry.getValue();
					if(value != null && (allWrap || value instanceof Map || value instanceof Collection || value.getClass().isArray())) 
						entry.setValue(Content.create(value));
				}else if(in instanceof Collection){
					Object[] os = ((Collection)in).toArray();
					((Collection)in).clear();
					for(Object o : os) if(o != null && (allWrap || o instanceof Map || o instanceof Collection || o.getClass().isArray())) 
						((Collection)in).add(Content.create(o));
					else ((Collection)in).add(o);
				}else if(in.getClass().isArray()) {
					ArrayList<Object> ret = new ArrayList<Object>(Array.getLength(in));
		    		for (int i = 0; i < Array.getLength(in); i ++) {
		    			Object o = Array.get(in, i);
		    			if(o != null && (allWrap || o instanceof Map || o instanceof Collection || o.getClass().isArray())) 
		    				ret.add(Content.create(o));
		    			else ret.add(o);
		    		}
		    		in = ret.toArray();
				}else if(reflect) for(Field field : getAllField(in.getClass())) 
				if(Object.class.equals(field.getType()) && !Modifier.isStatic(field.getModifiers())) try{
					field.setAccessible(true);
					Object o = field.get(in);
					if(o != null && (allWrap || o instanceof Map || o instanceof Collection || o.getClass().isArray()))
						field.set(in, Content.create(o));
				}catch(Exception e){
					log.error("WrapProcessor: processor failed: " + in, e);
				}
			}
		}else if(deep != 0) in = wrap(in, new HashSet<Object>());
		if(outWrap && in != null && (allWrap || in instanceof Map || in instanceof Collection || in.getClass().isArray())) return Content.create(in);
		else return in;
	}
	
	protected Object wrap(Object in, Collection<Object> set){
		if(in != null && !set.contains(in)) {
			set.add(in);
			if(in instanceof Map) for(Entry<Object, Object> entry : ((Map<Object, Object>) in).entrySet()){
				Object value = entry.getValue();
				if(value != null) if(allWrap || value instanceof Map || value instanceof Collection || value.getClass().isArray()) 
					entry.setValue(Content.create(wrap(value, set)));
				else wrap(value, set);
			}else if(in instanceof Collection){
				Object[] os = ((Collection)in).toArray();
				((Collection)in).clear();
				for(Object o : os) if(o != null && (allWrap || o instanceof Map || o instanceof Collection || o.getClass().isArray())) 
					((Collection)in).add(Content.create(wrap(o, set)));
				else ((Collection)in).add(wrap(o, set));
			}else if(in.getClass().isArray()) {
				ArrayList<Object> ret = new ArrayList<Object>(Array.getLength(in));
	    		for (int i = 0; i < Array.getLength(in); i ++) {
	    			Object o = Array.get(in, i);
	    			if(o != null && (allWrap || o instanceof Map || o instanceof Collection || o.getClass().isArray())) ret.add(Content.create(wrap(o, set)));
	    			else ret.add(wrap(o, set));
	    		}
	    		in = ret.toArray();
			} else if(reflect) for(Field field : getAllField(in.getClass())) if(!Modifier.isStatic(field.getModifiers())) try{
				if(Object.class.equals(field.getType())){
					field.setAccessible(true);
					Object o = field.get(in);
					if(o != null) if(allWrap || o instanceof Map || o instanceof Collection || o.getClass().isArray())
						field.set(in, Content.create(wrap(o, set)));
					else field.set(in, wrap(o, set));
				}else if(deep == 3 && (Map.class.isAssignableFrom(field.getType()) || Collection.class.isAssignableFrom(field.getType()) || field.getType().isArray()) || deep == 4){
					field.setAccessible(true);
					Object o = wrap(field.get(in), set);
					if(field.getType().isInstance(o)) field.set(in, o);
				}
			}catch(Exception e){
				log.error("WrapProcessor: processor failed: " + in, e);
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
