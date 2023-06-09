package net.yeeyaa.eight.core.processor;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.Content;


public class ParseProcessor implements IProcessor<Object, Object>{	
	protected static final Set<Class<?>> WRAPPER_TYPES = new HashSet<Class<?>>(Arrays.asList(
		    Boolean.class, Character.class, Byte.class, Short.class, Integer.class, Long.class, Float.class, Double.class, Void.class));
	
	public Object process(Object o){
		if(o != null){
			if(o instanceof Collection) {
				Object[] os = ((Collection<Object>)o).toArray();
				((Collection<Object>)o).clear();
				for(Object so : os) {
					if(so != null) if(so instanceof String || isWrapperType(so.getClass())) so = new Content(so);
					((Collection<Object>)o).add(process(so));
				}
			}else if(o.getClass().isArray()){
				ArrayList<Object> ret = new ArrayList<Object>(Array.getLength(o));
	    		for (int i = 0; i < Array.getLength(o); i ++) {
	    			Object so = Array.get(o, i);
	    			if(so != null) if(so instanceof String || isWrapperType(so.getClass())) so = new Content(so);
	    			ret.add(so);
	    		}
	    		o = ret.toArray();
			}
		}
		return o;
	}
	
	protected boolean isWrapperType(Class<?> clazz) {
	    return WRAPPER_TYPES.contains(clazz);
	}
}
