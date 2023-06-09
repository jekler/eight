package net.yeeyaa.eight.common.processor;

import java.util.ArrayList;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;

import org.eclipse.persistence.dynamic.DynamicEntity;


public class MapDynamicProcessor implements IProcessor<Map<Object, Object>, DynamicEntity>{
	protected IProcessor<Object, Object> key;
	protected IProcessor<Object, Object> value;
	protected IProcessor<String, DynamicEntity> factory;
	protected String name;
	protected String entry;
	
	public void setFactory(IProcessor<String, DynamicEntity> factory) {
		this.factory = factory;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setEntry(String entry) {
		this.entry = entry;
	}

	public void setKey(IProcessor<Object, Object> key) {
		this.key = key;
	}

	public void setValue(IProcessor<Object, Object> value) {
		this.value = value;
	}

	@Override
	public DynamicEntity process(Map<Object, Object> instance) {
		DynamicEntity ret = factory.process(name);
		if (instance != null && instance.size() > 0) {
			ArrayList<DynamicEntity> ls = new ArrayList<DynamicEntity>(instance.size());
			for (Entry<Object, Object> entry : instance.entrySet()) {
				DynamicEntity e = factory.process(this.entry);
				Object key = this.key == null ? entry.getKey() : this.key.process(entry.getKey());
				if (key != null) {
					e.set("key", key);
					e.set("value", this.value == null ? entry.getValue() : this.value.process(entry.getValue()));
					ls.add(e);
				}
			}
			ret.set("entry", ls);
		}
		return ret;
	}
}
