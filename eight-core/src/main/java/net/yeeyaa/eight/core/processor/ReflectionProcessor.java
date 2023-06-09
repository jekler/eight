package net.yeeyaa.eight.core.processor;

import java.lang.reflect.Method;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ReflectionProcessor implements IProcessor<Object, Object>{
	protected final Logger log;
	protected IProcessor<Object, Object> beanHolder;
	protected Object bean;
	protected String methodName;
	protected Class<?>[] paraTypes;
	protected Boolean strict = true;
	protected volatile Method method;
	protected Integer paracount = 0;
	
	public ReflectionProcessor() {
		this.log = LoggerFactory.getLogger(ReflectionProcessor.class);
	}
	
	public ReflectionProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ReflectionProcessor.class) : log;
	}
	
	public void setStrict(Boolean strict) {
		if(strict != null) this.strict = strict;
	}

	public void setMethodName(String methodName) {
		this.methodName = methodName;
	}

	public void setParaTypes(Class<?>[] paraTypes) {
		this.paraTypes = paraTypes;
	}

	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}

	public void setBean(Object bean) {
		this.bean = bean;
	}
	
	public void setParacount(Integer paracount) {
		if (paracount != null && paracount > 0) this.paracount = paracount;
	}

	@Override
	public Object process(Object paras) {
		Object instance = beanHolder.process(bean);
		if (instance != null) try {
			if(method == null) if (paraTypes == null && paracount > 0) for (Method m : instance.getClass().getMethods()) if (m.getName().equals(methodName) && m.getParameterTypes().length == paracount) {
				method = m;
				break;
			} else {
				method = instance.getClass().getMethod(methodName, paraTypes);
				paracount = method.getParameterTypes().length;
			}
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
			log.error("ReflectionProcessor: processor failed.", e);
			if(strict) throw new PlatformException(PlatformError.ERROR_PARAMETERS);
		}
		return null;
	} 
}
