package net.yeeyaa.eight.common.meta;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.PlatformUtil;


public class UnserializeProcessor implements IProcessor<Object, Object> {
	protected Boolean overlap = false; 
	protected Integer mode = 0; 
	protected Boolean copy = false;
	protected Boolean retry = false; 
	protected IProcessor<Object, Object> beanHolder;
	protected Object factory;
	protected Object initValue;
	
	public void setMode(Integer mode) {
		if (mode != null) this.mode = mode;
	}

	public void setFactory(Object factory) {
		this.factory = factory;
	}

	public void setInitValue(Object initValue) {
		this.initValue = initValue;
	}

	public void setOverlap(Boolean overlap) {
		this.overlap = overlap;
	}

	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}

	public void setCopy(Boolean copy) {
		if(copy != null) this.copy = copy;
	}
	
	public void setRetry(Boolean retry) {
		if(retry != null) this.retry = retry;
	}

	protected Object unserialize(Object instance, Map<String, Object> map){
		if(instance instanceof BeanMeta) {
			BeanMeta meta = (BeanMeta)instance;
			if(meta.getRef()) {
				Object ret = map.get(meta.getId());
				if(ret != null) return ret;
			} else {
				Object factory = meta.getFactory();
				if (!Boolean.FALSE.equals(overlap)) factory = this.factory;
				Object ret = beanHolder.process(factory);
				if(ret instanceof IMetaBean) map.put(meta.getId(), ret);
				Object initValue = meta.getInitValue();
				if (Boolean.TRUE.equals(overlap)) initValue = this.initValue; 
				if (initValue instanceof IMetaBean || initValue instanceof Object[] || initValue instanceof Collection || initValue instanceof Map) initValue = unserialize(initValue, map);
				if (mode != 2) if(ret instanceof IMetaBean) ((IMetaBean) ret).setInitValue(initValue); 
				else if(ret instanceof IProcessor) {
					Object result = ((IProcessor<Object, Object>) ret).process(initValue);
					if (mode == 0 || (mode != 1 && result != null)) ret = result;
				}
				map.put(meta.getId(), ret);
				return ret;
			}	
		} else if (instance instanceof Object[]) {
			Object[] arr = (Object[]) instance;
			for (int i = 0; i < arr.length; i++) if (arr[i] instanceof IMetaBean || arr[i] instanceof Object[] || arr[i] instanceof Collection || arr[i] instanceof Map) arr[i] = unserialize(arr[i], map);
		} else if (instance instanceof Collection) {
			LinkedList<Object> list = new LinkedList<Object>();
			Iterator<Object> itr = ((Collection<Object>) instance).iterator();
			while (itr.hasNext()) {
				Object obj = itr.next();
				if (obj instanceof IMetaBean) {
					list.add(unserialize(obj, map));
					itr.remove();
				} else if (obj instanceof Object[] || obj instanceof Collection || obj instanceof Map) unserialize(obj, map);
			}
			if(list.size() > 0) ((Collection<Object>) instance).addAll(list);
		} else if (instance instanceof Map) for(Entry<Object, Object> entry : ((Map<Object, Object>)instance).entrySet()) {
			Object obj = entry.getValue();
			if (obj instanceof IMetaBean || obj instanceof Object[] || obj instanceof Collection || obj instanceof Map) entry.setValue(unserialize(obj, map));
		}
		return instance;
	}
	
	public Object process(Object instance) {
		if (copy) instance = PlatformUtil.copy(instance);
		Map<String, Object> map = new HashMap<String, Object>();
		Object ret = unserialize(instance, map);
		if (retry) ret = unserialize(ret, map);
		return ret;
	}
}
