package net.yeeyaa.eight.core.processor;

import java.util.Collection;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IProcessor;


public class SingletonProcessor implements IProcessor<Object, Object>{
	protected IProcessor<Object, Object> factory; 
	protected ConcurrentHashMap<Object, Object> map = new ConcurrentHashMap<Object, Object>();
	protected Collection<Object> constraint;
	
	public void setConstraint(Collection<Object> constraint) {
		this.constraint = constraint;
	}

	public void setFactory(IProcessor<Object, Object> factory) {
		this.factory = factory;
	}

	@Override
	public Object process(Object name) {
		if(name != null && (constraint == null || constraint.contains(name))) {
			Object ret = map.get(name);
			if(ret == null) synchronized(this){
				ret = map.get(name);
				if(ret == null) {
					ret = factory.process(name);
					if(ret != null) map.put(name, ret);
				}
			}
			return ret;
		}else return null;
	}	
}
