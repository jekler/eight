package net.yeeyaa.eight.core.resource;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IResource;


public class MapXResource<K> implements IResource<K, Object> {
	protected List<IInputResource<K, Object>> inputs;
	protected List<IOutputResource<K, Object>> outputs;
	
	public void setInputs(List<IInputResource<K, Object>> inputs) {
		this.inputs = inputs;
	}

	public void setOutputs(List<IOutputResource<K, Object>> outputs) {
		this.outputs = outputs;
	}
	
	@Override
	public Object find(K ... paras) {
		if(inputs == null || inputs.size() == 0) return null;
		else {
			HashMap<Object, Object> ret = new HashMap<Object, Object>();
			for(IInputResource<K, Object> input : inputs){
				Object result = input.find(paras);
				if(Map.class.isInstance(result)) ret.putAll((Map) result);
			}
			return ret;
		}
	}

	@Override
	public <P> P store(Object value, K ... paras) {
		if(outputs != null && outputs.size() > 0) for(IOutputResource<K, Object> output : outputs) output.store(value, paras);
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		if(outputs != null && outputs.size() > 0) for(IOutputResource<K, Object> output : outputs) output.discard(paras);
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		if(outputs != null && outputs.size() > 0) for(IOutputResource<K, Object> output : outputs) output.empty(paras);
		return null;
	}
}
