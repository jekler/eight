package net.yeeyaa.eight.core.resource;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.yeeyaa.eight.IListableResource;


public class ResourceMap<K, V> implements Map<K, V> {
	protected final IListableResource<K, V> resource;
	
	public ResourceMap(IListableResource<K, V> resource) {
		this.resource = resource;
	}

	@Override
	public int size() {
		return resource.keys().size();
	}

	@Override
	public boolean isEmpty() {
		return resource.keys().isEmpty();
	}

	@Override
	public boolean containsKey(Object o) {
		for (K[] key : resource.keys()) if (key[0] == o || (key[0] != null && key[0].equals(o))) return true;
		return false;
	}

	@Override
	public boolean containsValue(Object value) {
		return values().contains(value);
	}

	@Override
	public V get(Object key) {
		return resource.find((K)key);
	}

	@Override
	public V put(K key, V value) {
		V ret = resource.find(key);
		resource.store(value, key);
		return ret;
	}

	@Override
	public V remove(Object key) {
		return resource.discard((K)key);
	}

	@Override
	public void putAll(Map<? extends K, ? extends V> m) {
		if (m != null) for (Entry<? extends K, ? extends V> entry : m.entrySet()) resource.store(entry.getValue(), entry.getKey());
	}

	@Override
	public void clear() {
		resource.empty();
	}

	@Override
	public Set<K> keySet() {
		Collection<K[]> keys = resource.keys();
		HashSet<K> set = new HashSet<K>(keys.size() * 2);
		for (K[] key : keys) set.add(key[0]);
		return set;
	}

	@Override
	public Collection<V> values() {
		return resource.all().values();
	}

	@Override
	public Set<Entry<K, V>> entrySet() {
		Map<K[], V> map = resource.all();
		Map<K, V> m = new HashMap<K, V>(map.size());
		for (Entry<K[], V> entry : map.entrySet()) m.put(entry.getKey()[0], entry.getValue());
		return m.entrySet();
	}
}
