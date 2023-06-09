package net.yeeyaa.eight.core.processor;

import java.lang.reflect.Method;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class SetterProcessor implements IProcessor<Object, Object>{
	protected final Logger log;
	protected ThreadLocal<Object> instance = new ThreadLocal<Object>();
	protected String methodName;
	protected Boolean strict = true;
	protected Class<?>[] paraTypes;
	protected volatile Method method;
	
	public SetterProcessor() {
		this.log = LoggerFactory.getLogger(SetterProcessor.class);
	}
	
	public SetterProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(SetterProcessor.class) : log;
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
	
	public void setInstance(Object instance) {
		this.instance.set(instance);
	}

	@Override
	public Object process(Object paras) {
		Object instance = this.instance.get();
		this.instance.remove();
		try {
			if(method == null) method = instance.getClass().getMethod(methodName, paraTypes);
			if(paras instanceof Object[]) method.invoke(instance, (Object[])paras);
			else method.invoke(instance, paras);
		}catch(Exception e){
			log.error("SetterProcessor: processor failed.", e);
			if(strict) throw new PlatformException(PlatformError.ERROR_PARAMETERS);
		}
		return instance;
	} 
	
	protected SetterProcessor getThis(){
		return this;
	}
	
	public class SetInstance implements IProcessor<Object, SetterProcessor>{
		@Override 
		public SetterProcessor process(Object instance) {
			setInstance(instance); 
			return getThis();
		}
	}
}
