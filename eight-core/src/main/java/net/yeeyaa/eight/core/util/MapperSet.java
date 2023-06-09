package net.yeeyaa.eight.core.util;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IProcessor;

public class MapperSet<E> implements Set<E>, IProcessor<Object, E> {
	protected final Map<String, E> map;
	
	public MapperSet(Boolean concurrent, E[] init) {
		map = Boolean.TRUE.equals(concurrent) ? new ConcurrentHashMap<String, E>() : new HashMap<String, E>();
		if (init != null && init.length > 0) for (E e : init) if (e != null) map.put(e.toString(), e);
 	}
	
	public MapperSet() {
		this(false, null);
	}

	public MapperSet(Boolean concurrent) {
		this(concurrent, null);
	}
	
	public MapperSet(E[] init) {
		this(false, init);
	}
	
	@Override
	public int size() {
		return map.size();
	}

	@Override
	public boolean isEmpty() {
		return map.isEmpty();
	}

	@Override
	public boolean contains(Object o) {
		if (o != null) return map.containsKey(o.toString());
		return false;
	}

	@Override
	public Iterator<E> iterator() {
		return map.values().iterator();
	}

	@Override
	public Object[] toArray() {
		return map.values().toArray();
	}

	@Override
	public <T> T[] toArray(T[] a) {
		return map.values().toArray(a);
	}

	@Override
	public boolean add(E e) {
		if (e != null) return map.put(e.toString(), e) == null;
		return false;
	}

	@Override
	public boolean remove(Object o) {
		if (o != null) return map.remove(o.toString()) != null;
		return false;
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		boolean ret = true;
		Set<String> set = map.keySet();
		for (Object s : c) if (!set.contains(s.toString())){
			ret = false;
			break;
		}
		return ret;
	}

	@Override
	public boolean addAll(Collection<? extends E> c) {
		boolean ret = false;
		for (E s : c) {
			E r = map.put(s.toString(), s);
			if (!ret && !!s.equals(r)) ret = true;
		}
		return ret;
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		boolean ret = false;
		Collection<String> keys = new HashSet<String>();
		for (Object o : c) if (o != null) keys.add(o.toString());
		Iterator<Entry<String, E>> itr = map.entrySet().iterator();
		while(itr.hasNext()) if (!keys.contains(itr.next())) {
			itr.remove();
			ret = true;
		}
		return ret;
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		boolean ret = false;
		Collection<String> keys = new HashSet<String>();
		for (Object o : c) if (o != null) keys.add(o.toString());
		Iterator<Entry<String, E>> itr = map.entrySet().iterator();
		while(itr.hasNext()) if (keys.contains(itr.next())) {
			itr.remove();
			ret = true;
		}
		return ret;
	}

	@Override
	public void clear() {
		map.clear();
	}

	@Override
	public E process(Object object) {
		if (object != null) return map.get(object.toString());
		return null;
	}

}
