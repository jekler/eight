package net.yeeyaa.eight.common.meta;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.aopalliance.intercept.MethodInvocation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.aop.support.DelegatingIntroductionInterceptor;


public class MetaBean extends DelegatingIntroductionInterceptor implements IMetaBean {
	private static final long serialVersionUID = -434251731913936873L;
	protected final Logger log;
	protected final ThreadLocal<Object> target = new ThreadLocal<Object>();
	protected ConcurrentHashMap<String, Object> data; 
	protected Interceptor interceptor;
	protected Object factory;
	
	public MetaBean() {
		this.log = LoggerFactory.getLogger(MetaBean.class);
	}

	public MetaBean(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(MetaBean.class) : log;
	}
	
	public void setInterceptor(IProcessor<Object[], Object> interceptor) {
		if (interceptor != null) this.interceptor = new Interceptor(interceptor);
	}

	public void setData(Map<String, Object> data) {
		if(data != null && data.size() > 0) this.data = new ConcurrentHashMap<String, Object>(data);
	}

	public Object getFactory() {
		return factory;
	}

	public void setFactory(Object factory) {
		this.factory = factory;
	}

	public Object getInitValue() {
		Object target = this.target.get();
		if(data == null || target ==null) return new HashMap<String, Object>(0);
		else {
			HashMap<String, Object> ret = new HashMap<String, Object>(data.size() * 2);
			for (Entry<String, Object> entry : data.entrySet()) try {
				Object value = entry.getValue();
				String key = entry.getKey();
				if (value instanceof Field) ret.put(key, ((Field) value).get(target));
				else if (Boolean.TRUE.equals(value)) {
					Field f = target.getClass().getField(key);
					f.setAccessible(true);
					entry.setValue(f);
					ret.put(key, f.get(target));
				} else {
					if(!(value instanceof Object[])) synchronized(entry) {
						value = entry.getValue();
						if(!(value instanceof Object[])) {
							value = new Object[2];
							entry.setValue(value);
						}
					}
					Object[] v = (Object[]) value;
					Method m = (Method) v[1];
					if (m == null) {
						m = target.getClass().getMethod("get" + key.substring(0, 1).toUpperCase() + key.substring(1));
						v[1] = m;
					}
					ret.put(key, m.invoke(target));
				}
			} catch (Exception e) {
        		log.error("MetaBean: get init value.", e);
			}
			return ret;
		}
	}

	public void setInitValue(Object initValue) {
		Object target = this.target.get();
		if(data != null && initValue != null && initValue instanceof Map && ((Map)initValue).size() > 0 && target != null) for (Entry<String, Object> entry : ((Map<String, Object>)initValue).entrySet()) try {
			String key = entry.getKey();
			if (data.contains(key)) {
				Object type = data.get(key);
				Object value = entry.getValue();
				if (type instanceof Field) ((Field)type).set(target, value);
				else if(Boolean.TRUE.equals(type)){
					Field f = target.getClass().getField(key);
					f.setAccessible(true);
					entry.setValue(f);
					f.set(target, value);
				} else {
					if(!(value instanceof Object[])) synchronized(entry) {
						value = entry.getValue();
						if(!(value instanceof Object[])) {
							value = new Object[2];
							entry.setValue(value);
						}
					}
					Object[] v = (Object[]) value;
					Method m = (Method) v[0];
					if (m == null) {
						String name = "set" + key.substring(0, 1).toUpperCase() + key.substring(1);
						Method[] ms = target.getClass().getMethods();
						for (Method sm : ms) if (name.equals(sm.getName()) && sm.getParameterTypes().length == 1) {
							m = sm;
							break;
						}
						v[0] = m;
					}
					if (m != null) m.invoke(target, value);
				}
			}
		} catch (Exception e) {
    		log.error("MetaBean: set init value.", e);
		}
	}
	
	public Object invoke(MethodInvocation invocation) throws Throwable {
		target.set(invocation.getThis());
		Object ret;
		if(interceptor == null) ret = super.invoke(invocation);
		ret = interceptor.invoke(invocation);
		target.set(null);
		return ret;
    }
	
	protected Object invokeSuper(MethodInvocation invocation) throws Throwable {
		return super.invoke(invocation);
	}
	
	protected class Interceptor implements IProcessor<MethodInvocation, Object> {
		protected IProcessor<Object[], Object> interceptor;
		
		protected Interceptor(IProcessor<Object[], Object> interceptor) {
			this.interceptor = interceptor;
		}

		protected Object invoke(MethodInvocation invocation) {
			return interceptor.process(new Object[]{invocation, this});
		}
		
		@Override
		public Object process(MethodInvocation instance) {
			try {
				return invokeSuper(instance);
			} catch (Throwable e) {
				throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
			}
		}
	}
}
