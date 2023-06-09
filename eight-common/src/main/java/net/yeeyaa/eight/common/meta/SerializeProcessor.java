package net.yeeyaa.eight.common.meta;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.PlatformUtil;


public class SerializeProcessor implements IProcessor<Object, Object> {
	protected Boolean overlap = false; 
	protected Boolean copy = true;
	protected Object factory;
	protected Object initValue;

	public void setOverlap(Boolean overlap) {
		this.overlap = overlap;
	}

	public void setFactory(Object factory) {
		this.factory = factory;
	}

	public void setInitValue(Object initValue) {
		this.initValue = initValue;
	}

	public void setCopy(Boolean copy) {
		if(copy != null) this.copy = copy;
	}

	protected Object serialize(Object instance, Map<IMetaBean, BeanMeta> map){
		if(instance instanceof IMetaBean) {
			BeanMeta meta = map.get(instance);
			if(meta == null) {
				meta = new BeanMeta((IMetaBean)instance);
				map.put(((IMetaBean)instance), meta);
				if (!Boolean.FALSE.equals(overlap)) meta.setFactory(factory);
				if (Boolean.TRUE.equals(overlap)) meta.setInitValue(initValue);
				Object initValue = meta.getInitValue();
				if (initValue instanceof IMetaBean || initValue instanceof Object[] || initValue instanceof Collection || initValue instanceof Map)  meta.setInitValue(serialize(initValue, map));
			} else meta = new BeanMeta(meta);
			return meta;		
		} else if (instance instanceof Object[]) {
			Object[] arr = (Object[]) instance;
			for (int i = 0; i < arr.length; i++) if (arr[i] instanceof IMetaBean || arr[i] instanceof Object[] || arr[i] instanceof Collection || arr[i] instanceof Map) arr[i] = serialize(arr[i], map);
		} else if (instance instanceof Collection) {
			LinkedList<Object> list = new LinkedList<Object>();
			Iterator<Object> itr = ((Collection<Object>) instance).iterator();
			while (itr.hasNext()) {
				Object obj = itr.next();
				if (obj instanceof IMetaBean) {
					list.add(serialize(obj, map));
					itr.remove();
				} else if (obj instanceof Object[] || obj instanceof Collection || obj instanceof Map) serialize(obj, map);
			}
			if(list.size() > 0) ((Collection<Object>) instance).addAll(list);
		} else if (instance instanceof Map) for(Entry<Object, Object> entry : ((Map<Object, Object>)instance).entrySet()) {
			Object obj = entry.getValue();
			if (obj instanceof IMetaBean || obj instanceof Object[] || obj instanceof Collection || obj instanceof Map) entry.setValue(serialize(obj, map));
		}
		return instance;
	}
	
	public Object process(Object instance) {
		if (copy) instance = PlatformUtil.copy(instance);
		return serialize(instance, new HashMap<IMetaBean, BeanMeta>());
	}
}
