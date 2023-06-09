package net.yeeyaa.eight.core.resource;

import java.util.LinkedList;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IResource;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class MultipleXResource<K, V> implements IResource<K, V> {
	protected final Logger log;
	protected IInputResource<K, V> input;
	protected LinkedList<IOutputResource<K, V>> outputs;
	protected Boolean clone = false;
	protected Boolean strict = true;

	public MultipleXResource() {
		this.log = LoggerFactory.getLogger(MultipleXResource.class);
	}
	
	public MultipleXResource(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(MultipleXResource.class) : log;
	}
	
	public void setClone(Boolean clone) {
		if (clone != null) this.clone = clone;
	}

	public void setStrict(Boolean strict) {
		if (strict != null) this.strict = strict;
	}

	@Override
	public V find(K ... paras) {
		return input.find(paras);
	}

	@Override
	public <P> P store(V value, K ... paras) {
		if(outputs != null && outputs.size() > 0) for(IOutputResource<K, V> output : outputs) {
			if(clone && Cloneable.class.isInstance(value)) try{
				output.store((V)value.getClass().getMethod("clone").invoke(value), paras);
				return null;
			}catch(Exception e){
				log.error("MultipleXResource: set resource error.", e);
			}
			if(!clone || !strict) output.store(value, paras);
		}
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		if(outputs != null && outputs.size() > 0) for(IOutputResource<K, V> output : outputs) output.discard(paras);
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		if(outputs != null && outputs.size() > 0) for(IOutputResource<K, V> output : outputs) output.empty(paras);
		return null;
	}
}
