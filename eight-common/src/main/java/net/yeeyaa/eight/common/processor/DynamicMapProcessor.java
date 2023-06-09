package net.yeeyaa.eight.common.processor;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;

import net.yeeyaa.eight.IProcessor;

import org.eclipse.persistence.dynamic.DynamicEntity;


public class DynamicMapProcessor implements IProcessor<DynamicEntity, Map<Object, Object>>{
	protected IProcessor<Object, Object> key;
	protected IProcessor<Object, Object> value;
	
	public void setKey(IProcessor<Object, Object> key) {
		this.key = key;
	}

	public void setValue(IProcessor<Object, Object> value) {
		this.value = value;
	}

	@Override
	public Map<Object, Object> process(DynamicEntity instance) {
		Map<Object, Object> ret = new LinkedHashMap<Object, Object>();
		if (instance != null && instance.get("entry") instanceof Collection) for (DynamicEntity de : (Collection<DynamicEntity>)instance.get("entry")) {
			Object key = this.key == null ? de.get("key") : this.key.process(de.get("key"));
			if (key != null) ret.put(key, this.value == null ? de.get("value") : this.value.process(de.get("value")));
		}
		return ret;
	}
}
