package net.yeeyaa.eight.annotation;

import java.lang.reflect.Method;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.annotation.tag.ServiceClass;
import net.yeeyaa.eight.annotation.tag.ServiceMethod;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class AnnotationBeanHolder implements IProcessor<Object, Object> {
	protected volatile ConcurrentHashMap<String, Method> cache = new ConcurrentHashMap<String, Method>();
	protected String regex = "@";
	protected IProcessor<String, Object> beanHolder;
	
	public void setRegex(String regex) {
		if(regex != null) this.regex = regex;
	}

	public void setBeanHolder(IProcessor<String, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}
	
	public void refresh(){
		cache = new ConcurrentHashMap<String, Method>();
	}
	
	@Override
	public Object process(Object instance) {
		if(instance != null){
			String[] keys = instance.toString().split(regex);
			if(keys.length < 2) return beanHolder.process(keys[0].trim());
			else {
				String clazz = keys[0].trim();
				String method = keys[1].trim();
				if(clazz.length() > 0 && method.length() > 0){
					Object o = beanHolder.process(clazz);
					String name = clazz + regex + method;
					Method m = cache.get(name);
					if(m == null && o != null){
						Class<?> c = o.getClass();
						if(c.isAnnotationPresent(ServiceClass.class)) for (Method me : c.getMethods()) if (me.isAnnotationPresent(ServiceMethod.class)){
							ServiceMethod sm = me.getAnnotation(ServiceMethod.class);
							String methodName = sm.name();
							if(methodName == null || methodName.trim().length() == 0) methodName = me.getName();
							if(method.equals(methodName)) m = me;
							name = clazz + regex + methodName;
							cache.put(name, me);
						}
					}
					if(m != null && o != null) return new MethodProcessor(m, o, m.getParameterTypes().length, name);
				}
			}
		}
		return null;
	}
	
	public class Refresh implements IProcessor<Object, Object>{
		@Override
		public Object process(Object instance) {
			refresh();
			return instance;
		}
	}
	
	protected static class MethodProcessor implements IProcessor<Object, Object>, IBiProcessor<String, Object, Object>{
		protected final Logger log;
		protected Method method;
		protected Object instance;
		protected Integer paracount;
		protected Object id;
		
		public MethodProcessor(Method method, Object instance, Integer paracount, Object id) {
			this.log = LoggerFactory.getLogger(MethodProcessor.class);
			this.method = method;
			this.instance = instance;
			this.paracount = paracount;
			this.id = id;
		}

		public MethodProcessor(Method method, Object instance, Integer paracount, Object id, Logger log) {
			this.log = log == null ? LoggerFactory.getLogger(MethodProcessor.class) : log;
			this.method = method;
			this.instance = instance;
			this.paracount = paracount;
			this.id = id;
		}
		
		@Override
		public Object process(Object paras) {
			try {
				switch(paracount){
					case 0: return method.invoke(instance);
					case 1: return method.invoke(instance, paras);
					default : if(paras instanceof Object[]) return method.invoke(instance, (Object[])paras);
					else {
						Object[] newparas = new Object[paracount];
						newparas[0] = paras;
						return method.invoke(instance, newparas);
					}
				}
			}catch(Exception e){
				log.error("MethodProcessor: processor failed.", e);
			}
			return null;
		}
		
		@Override
		public boolean equals(Object obj) {
			if(this == obj) return true;
			else if(obj instanceof MethodProcessor){
				MethodProcessor other = (MethodProcessor) obj;
				return ((method == other.method ||(method != null && method.equals(other.method))) && 
						(instance == other.instance || (instance != null && instance.equals(other.instance))));
			}
			return false;
		}
		
		@Override
		public int hashCode() {
			int hash = 0;
			if(method != null) hash = method.hashCode();
			if(instance != null) hash = hash * 19 + instance.hashCode();
			return hash;
		}

		@Override
		public Object perform(String name, Object content) {
			return id;
		}
	}
}
