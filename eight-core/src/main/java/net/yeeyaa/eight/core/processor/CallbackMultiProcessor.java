package net.yeeyaa.eight.core.processor;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.yeeyaa.eight.IProcessor;


public class CallbackMultiProcessor implements IProcessor<Map<String, Object>, Object> {
	protected ThreadLocal<Map<String, Object>> local = new ThreadLocal<Map<String, Object>>();
	protected IProcessor<Object, Object> processor; //usually beanholder
	protected Object paras;
	protected IProcessor<Object[], Object> valuePrcoessor;
	
	public void setProcessor(IProcessor<Object, Object> processor) {
		this.processor = processor;
	}

	public void setParas(Object paras) {
		this.paras = paras;
	}

	public void setValuePrcoessor(IProcessor<Object[], Object> valuePrcoessor) {
		this.valuePrcoessor = valuePrcoessor;
	}

	public Object getValue(String key) { //beanHolder (usually platformBean) gets parameters from local thread 
		Map<String, Object> map = local.get();
		if(map == null) return null;
		else if (valuePrcoessor == null) return map.get(key);
		else return valuePrcoessor.process(new Object[]{key, map.get(key)});
	}

	public void setValue(String key, Object value){
		Map<String, Object> map = local.get();
		if(map == null){
			map = new HashMap<String, Object>();
			local.set(map);
		}
		map.put(key, value);
	}
	
	@Override
	public Object process(Map<String, Object> paras) {//when paras is null, this processor can do with multi paras processor to inject several paras
		if(paras != null) this.local.set(paras); //set paras
		Object ret = processor.process(this.paras); //gets paras and generators bean
		this.local.remove(); //clear temp paras, prevent memory leak
		return ret;
	}
	
	public class SetValue implements IProcessor<Object, Void>{
		protected String key;
		
		public void setKey(String key) {
			this.key = key;
		}

		@Override
		public Void process(Object value) {
			setValue(key, value);
			return null;
		}
	}
	
	public static class ParasMap implements IProcessor<Object[], Map<String, Object>>{//map a collection to a parameter map;
		protected List<String> keys;
		
		public void setKeys(List<String> keys) {
			this.keys = keys;
		}

		@Override
		public Map<String, Object> process(Object[] instance) {
			Map<String, Object> map = new HashMap<String, Object>();
			if(instance == null) instance = new Object[0];
			for(int i = 0; i < keys.size(); i++) if(i < instance.length) map.put(keys.get(i), instance[i]);
			else map.put(keys.get(i), null);
			return map;
		} 
	}
}