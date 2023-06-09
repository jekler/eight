package net.yeeyaa.eight.core.processor;

import java.util.Map;

import net.yeeyaa.eight.IProcessor;


public class MapProcessor<K, V> implements IProcessor<Map<K, V>, V> {
	protected K key;
	
	public void setKey(K key) {
		this.key = key;
	}

	@Override
	public V process(Map<K, V> in) {	
		if(in != null) return in.get(key);
		return null;
	}
}
