package net.yeeyaa.eight.core.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.core.util.PlatformUtil;


public class ResourceList<V> implements List<V> {
	protected final IListableResource<Integer, V> resource;

	public ResourceList(IListableResource<Integer, V> resource) {
		this.resource = resource;
	}

	public List<V> list() {
		Map<Integer[], V> all = resource.all();
		Object[] arr = new Object[all.size()];
		for (Entry<Integer[], V> entry : all.entrySet()) arr[entry.getKey()[0]] = entry.getValue();
		ArrayList<V> ret = new ArrayList<V>(arr.length);
		for (Object value : arr) ret.add((V)value);
		return ret;
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
		return resource.all().values().contains(o);
	}

	@Override
	public Iterator<V> iterator() {
		return list().iterator();
	}

	@Override
	public Object[] toArray() {
		Map<Integer[], V> all = resource.all();
		Object[] arr = new Object[all.size()];
		for (Entry<Integer[], V> entry : all.entrySet()) arr[entry.getKey()[0]] = entry.getValue();
		return arr;
	}

	@Override
	public <T> T[] toArray(T[] a) {
		Map<Integer[], V> all = resource.all();
		T[] arr = PlatformUtil.newArrayOf(a, all.size());
		for (Entry<Integer[], V> entry : all.entrySet()) arr[entry.getKey()[0]] = (T)entry.getValue();
		return arr;
	}

	@Override
	public boolean add(V e) {
		resource.store(e, resource.keys().size());
		return true;
	}

	@Override
	public boolean remove(Object o) {
		Map<Integer[], V> all = resource.all();
		Object[] arr = new Object[all.size()];
		for (Entry<Integer[], V> entry : all.entrySet()) arr[entry.getKey()[0]] = entry.getValue();
		Boolean ret = false;
		for (int i = 0; i < arr.length; i++) if (!ret) {
			if (arr[i] == o || (arr[i] != null && arr[i].equals(o))) {
				resource.discard(arr.length - 1);
				ret = true;
			} 
		} else resource.store((V)arr[i], i - 1);
		return ret;
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		if (c == null || c.size() == 0) return true;
		else return resource.all().values().containsAll(c);
	}

	@Override
	public boolean addAll(Collection<? extends V> c) {
		if (c == null || c.size() == 0) return false;
		else {
			int i = resource.keys().size();
			for (V value : c) resource.store(value, i++);
			return true;
		}
	}

	@Override
	public boolean addAll(int index, Collection<? extends V> c) {
		if (c == null || c.size() == 0) return false;
		else {
			int size = resource.keys().size();
			if (index < 0 || index > size) throw new IndexOutOfBoundsException();
			else if (index == size) {
				int i = size;
				for (V value : c) resource.store(value, i++);
				return true;
			} else {
				Map<Integer[], V> all = resource.all();
				Object[] arr = new Object[size + c.size()];
				for (Entry<Integer[], V> entry : all.entrySet()) if (entry.getKey()[0] < index) arr[entry.getKey()[0]] = entry.getValue();
				else arr[entry.getKey()[0] + index] = entry.getValue();
				return true;
			}
		}
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		if (c == null || c.size() == 0) return false;
		else {
			Map<Integer[], V> all = resource.all();
			Object[] arr = new Object[all.size()];
			Map<V, HashSet<Integer>> reverse = new HashMap<V, HashSet<Integer>>(all.size() * 2);
			for (Entry<Integer[], V> entry : all.entrySet()) {
				HashSet<Integer> set = reverse.get(entry.getValue());
				if (set == null) {
					set = new HashSet<Integer>();
					reverse.put(entry.getValue(), set);
				}
				set.add(entry.getKey()[0]);
				arr[entry.getKey()[0]] = entry.getValue();
			}
			HashSet<Integer> remove = new HashSet<Integer>();
			for (Object o : c) if (reverse.containsKey(o)) remove.addAll(reverse.get(o));
			if (remove.size() > 0) {
				int j = 0;
				for (int i = 0; i < all.size(); i++) if (!remove.contains(i)) {
					if (i != j) resource.store((V)arr[i], j);
					j++;
				}
				for (; j < all.size(); j ++) resource.discard(j);
				return true;
			} else return false;
		}
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		if (c == null || c.size() == 0) return false;
		else {
			Map<Integer[], V> all = resource.all();
			Object[] arr = new Object[all.size()];
			Map<V, HashSet<Integer>> reverse = new HashMap<V, HashSet<Integer>>(all.size() * 2);
			for (Entry<Integer[], V> entry : all.entrySet()) {
				HashSet<Integer> set = reverse.get(entry.getValue());
				if (set == null) {
					set = new HashSet<Integer>();
					reverse.put(entry.getValue(), set);
				}
				set.add(entry.getKey()[0]);
				arr[entry.getKey()[0]] = entry.getValue();
			}
			LinkedList<Integer> retain = new LinkedList<Integer>();
			for (Object o : c) if (reverse.containsKey(o)) retain.addAll(reverse.get(o));
			if (retain.size() < arr.length) {
				Collections.sort(retain);
				int i = 0;
				for (; i < retain.size(); i++) if (i != retain.get(i)) resource.store((V)arr[retain.get(i)], i);
				for (; i < all.size(); i++) resource.discard(i);
				return true;
			} else return false;
		}
	}

	@Override
	public V get(int index) {
		if (index < 0 || index >= resource.keys().size()) throw new IndexOutOfBoundsException();
		else return resource.find(index);
	}

	@Override
	public V set(int index, V element) {
		if (index < 0 || index >= resource.keys().size()) throw new IndexOutOfBoundsException();
		else {
			V ret = resource.find(index);
			resource.store(element, index);
			return ret;
		}
	}

	@Override
	public void add(int index, V element) {
		int size = resource.keys().size();
		if (index < 0 || index > size) throw new IndexOutOfBoundsException();
		else if (index == size) resource.store(element, index);
		else {
			Map<Integer[], V> all = resource.all();
			Object[] arr = new Object[all.size()];
			for (Entry<Integer[], V> entry : all.entrySet()) arr[entry.getKey()[0]] = entry.getValue();
			resource.store(element, index); 
			for (int i = index; i < arr.length; i++) resource.store((V)arr[i], i + 1);
		}
	}

	@Override
	public V remove(int index) {
		int size = resource.keys().size();
		if (index < 0 || index >= size) throw new IndexOutOfBoundsException();
		else if (index == size - 1) return resource.discard(index);
		else {
			Map<Integer[], V> all = resource.all();
			Object[] arr = new Object[all.size()];
			for (Entry<Integer[], V> entry : all.entrySet()) arr[entry.getKey()[0]] = entry.getValue();
			for (int i = index; i < arr.length - 1; i++) resource.store((V)arr[i + 1], i);
			resource.discard(arr.length - 1); 
			return (V)arr[index];
		}
	}

	@Override
	public int indexOf(Object o) {
		Map<Integer[], V> all = resource.all();
		Object[] arr = new Object[all.size()];
		for (Entry<Integer[], V> entry : all.entrySet()) arr[entry.getKey()[0]] = entry.getValue();
		for (int i = 0; i < arr.length; i++) if (arr[i] == o || (arr[i] != null && arr[i].equals(o))) return i;
		return -1;
	}

	@Override
	public int lastIndexOf(Object o) {
		Map<Integer[], V> all = resource.all();
		Object[] arr = new Object[all.size()];
		for (Entry<Integer[], V> entry : all.entrySet()) arr[entry.getKey()[0]] = entry.getValue();
		for (int i = arr.length - 1; i >= 0; i--) if (arr[i] == o || (arr[i] != null && arr[i].equals(o))) return i;
		return -1;
	}

	@Override
	public ListIterator<V> listIterator() {
		return list().listIterator();
	}

	@Override
	public ListIterator<V> listIterator(int index) {
		return list().listIterator(index);
	}

	@Override
	public List<V> subList(int fromIndex, int toIndex) {
		return list().subList(fromIndex, toIndex);
	}
}
