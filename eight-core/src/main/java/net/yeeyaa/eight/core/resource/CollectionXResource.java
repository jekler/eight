package net.yeeyaa.eight.core.resource;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.core.util.TypeConvertor;


public class CollectionXResource<K> implements IResource<K, Object> {
	protected Boolean identity = false;
	protected Boolean merge = false;
	protected Boolean array = false;
	protected List<IInputResource<K, Object>> inputs;
	protected List<IOutputResource<K, Object>> outputs;
	
	public void setArray(Boolean array) {
		this.array = array;
	}

	public void setIdentity(Boolean identity) {
		if(identity != null) this.identity = identity;
	}

	public void setMerge(Boolean merge){
		if(merge != null) this.merge = merge;
	}
	
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
			Collection<Object> ret;
			if(identity) ret = new HashSet<Object>();
			else ret = new LinkedList<Object>();
			for(IInputResource<K, Object> input : inputs){
				Object result = input.find(paras);
				if(result != null) if(merge && Collection.class.isInstance(result)) ret.addAll((Collection) result);
				else if(merge && result.getClass().isArray()) ret.addAll(TypeConvertor.asCollection(result));
				else ret.add(result);
			}
			if(array) return ret.toArray();
			else return ret;
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
