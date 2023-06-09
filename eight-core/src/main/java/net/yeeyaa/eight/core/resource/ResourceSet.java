package net.yeeyaa.eight.core.resource;

import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.core.util.PlatformUtil;


public class ResourceSet<K> implements Set<K> {
	protected final IListableResource<K, Boolean> resource;

	public ResourceSet(IListableResource<K, Boolean> resource) {
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
	public void clear() {
		resource.empty();
	}

	@Override
	public boolean contains(Object o) {
		for (K[] key : resource.keys()) if (key[0] == o || (key[0] != null && key[0].equals(o))) return true;
		return false;
	}

	@Override
	public Iterator<K> iterator() {
		Collection<K[]> keys = resource.keys();
		HashSet<K> set = new HashSet<K>(keys.size() * 2);
		for (K[] key : keys) set.add(key[0]);
		return set.iterator();
	}

	@Override
	public Object[] toArray() {
		Collection<K[]> keys = resource.keys();
		Object[] ret = new Object[keys.size()];
		int i = 0;
		for (K[] key : keys) ret[i++] = key;
		return ret;
	}

	@Override
	public <T> T[] toArray(T[] a) {
		Collection<K[]> keys = resource.keys();
		T[] ret = PlatformUtil.newArrayOf(a, keys.size());
		int i = 0;
		for (K[] key : keys) ret[i++] = (T)key;
		return ret;
	}

	@Override
	public boolean add(K e) {
		Boolean ret = resource.find(e);
		if (ret == null) {
			resource.store(Boolean.TRUE, e);
			return true;
		} else return false;
	}

	@Override
	public boolean remove(Object o) {
		Boolean ret = resource.discard((K)o);
		return ret != null;
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		if (c != null && c.size() > 0) {
			Collection<K[]> keys = resource.keys();
			HashSet<K> set = new HashSet<K>(keys.size() * 2);
			for (K[] key : keys) set.add(key[0]);
			return set.containsAll(c);
		} else return true;
	}

	@Override
	public boolean addAll(Collection<? extends K> c) {
		boolean ret = false;
		if (c != null && c.size() > 0) {
			Collection<K[]> keys = resource.keys();
			HashSet<K> set = new HashSet<K>(keys.size() * 2);
			for (K[] key : keys) set.add(key[0]);
			for (K key : c) if (!set.contains(key)) {
				resource.store(Boolean.TRUE, key);
				ret = true;
			}
		}
		return ret;
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		boolean ret = false;
		if (c != null && c.size() > 0) {
			Collection<K[]> keys = resource.keys();
			HashSet<K> set = new HashSet<K>(keys.size() * 2);
			for (K[] key : keys) set.add(key[0]);
			for (K key : set) if (!c.contains(key)) {
				resource.discard(key);
				ret = true;
			}
		} else if (resource.keys().size() > 0) {
			resource.empty();
			ret = true;
		}
		return ret;
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		boolean ret = false;
		if (c != null && c.size() > 0) {
			Collection<K[]> keys = resource.keys();
			HashSet<K> set = new HashSet<K>(keys.size() * 2);
			for (K[] key : keys) set.add(key[0]);
			for (K key : set) if (c.contains(key)) {
				resource.discard(key);
				ret = true;
			}
		}
		return ret;
	}
}
