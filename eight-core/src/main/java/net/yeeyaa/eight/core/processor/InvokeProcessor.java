package net.yeeyaa.eight.core.processor;

import java.lang.reflect.Method;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class InvokeProcessor<K, V> implements IProcessor<Object, Object> {
	protected final Logger log;
	protected String method;
	protected Class<?>[] paras;
	protected Map<K[], IInputResource<K, V>> inputs;
	
	public InvokeProcessor() {
		this.log = LoggerFactory.getLogger(InvokeProcessor.class);
	}
	
	public InvokeProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(InvokeProcessor.class) : log;
	}
	
	public void setMethod(String method) {
		this.method = method;
	}

	public void setInputs(Map<K[], IInputResource<K, V>> inputs) {
		this.inputs = inputs;
	}

	public void setParas(Class<?>[] paras) {
		this.paras = paras;
	}

	@Override
	public Object process(Object in) {
		if(method != null && in != null) try{
			Method m = in.getClass().getMethod(method, paras);
			Object[] os = new Object[paras == null ? 0 : paras.length];
			if(inputs != null && inputs.size() > 0) {
				int i = 0;
				for(Entry<K[], IInputResource<K, V>> entry : inputs.entrySet()){
					if(i >= os.length) break;
					os[i++] = entry.getValue().find(entry.getKey());
				}
			}
			return m.invoke(in, os);
		}catch(Exception e){
			log.error("InvokeProcessor: processor failed.", e);	
		}
		return null;
	}
}
