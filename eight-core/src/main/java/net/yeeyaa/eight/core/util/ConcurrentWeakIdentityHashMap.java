package net.yeeyaa.eight.core.util;

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;


public class ConcurrentWeakIdentityHashMap<K, V> extends AbstractMap<K, V> implements ConcurrentMap<K, V>, IListableResource<K, V>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
    protected final ConcurrentMap<Key<K>, V> map;
    protected final Boolean identity;
    protected final IBiProcessor<String, V, Object> destroy;
    protected final ReferenceQueue<K> queue = new ReferenceQueue<K>();
    protected transient Set<Map.Entry<K, V>> es;
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(K[] paras) {
			return new Long(size());
		}
	};
	
    public ConcurrentWeakIdentityHashMap(int initialCapacity, Boolean identity, IBiProcessor<String, V, Object> destroy) {
        this.map = initialCapacity > 0 ? new ConcurrentHashMap<Key<K>, V>(initialCapacity) : new ConcurrentHashMap<Key<K>, V>();
        this.identity = identity == null ? true : identity;
        this.destroy = destroy;
    }

    public ConcurrentWeakIdentityHashMap(int initialCapacity, Boolean identity) {
        this.map = initialCapacity > 0 ? new ConcurrentHashMap<Key<K>, V>(initialCapacity) : new ConcurrentHashMap<Key<K>, V>();
        this.identity = identity == null ? true : identity;
        this.destroy = null;
    }
    
    public ConcurrentWeakIdentityHashMap(Boolean identity) {
        this.map = new ConcurrentHashMap<Key<K>, V>();
        this.identity = identity == null ? true : identity;
        this.destroy = null;
    }
    
    public ConcurrentWeakIdentityHashMap(Boolean identity, IBiProcessor<String, V, Object> destroy) {
        this.map = new ConcurrentHashMap<Key<K>, V>();
        this.identity = identity == null ? true : identity;
        this.destroy = destroy;
    }

    public ConcurrentWeakIdentityHashMap(IBiProcessor<String, V, Object> destroy) {
        this.map = new ConcurrentHashMap<Key<K>, V>();
        this.identity = true;
        this.destroy = destroy;
    }
    
    public ConcurrentWeakIdentityHashMap() {
        this.map = new ConcurrentHashMap<Key<K>, V>();
        this.identity = true;
        this.destroy = null;
    }

    @Override
    public V get(Object key) {
        purgeKeys();
        return map.get(new Key<K>((K)key, null));
    }

    @Override
    public V put(K key, V value) {
        purgeKeys();
        V ret = map.put(new Key<K>(key, queue), value);
        if (destroy != null && ret != value) destroy.perform("put", value);
        return ret;
    }

    @Override
    public int size() {
        purgeKeys();
        return map.size();
    }

    protected void purgeKeys() {
        Reference<? extends K> reference;
        while ((reference = queue.poll()) != null) {
        	V v = map.remove(reference);
        	if (destroy != null) destroy.perform("purge", v);
        }
    }

    @Override
    public Set<Map.Entry<K, V>> entrySet() {
        Set<Map.Entry<K, V>> entrySet;
        return ((entrySet = this.es) == null) ? es = new EntrySet() : entrySet;
    }

    @Override
    public V putIfAbsent(K key, V value) {
        purgeKeys();
        return map.putIfAbsent(new Key<K>(key, queue), value);
    }

    @Override
    public V remove(Object key) {
        V v = map.remove(new Key<K>((K)key, null));
    	if (destroy != null) destroy.perform("remove", v);
    	return v;
    }

    @Override
    public boolean remove(Object key, Object value) {
        purgeKeys();
        boolean ret = map.remove(new Key<K>((K)key, null), value);
        if (destroy != null && ret) destroy.perform("remove", (V)value);
        return ret;
    }

    @Override
    public boolean replace(K key, V oldValue, V newValue) {
        purgeKeys();
        return map.replace(new Key<K>(key, null), oldValue, newValue);
    }

    @Override
    public V replace(K key, V value) {
        purgeKeys();
        V ret = map.replace(new Key<K>(key, null), value);
        if (destroy != null && ret != value) destroy.perform("replace", ret);
        return ret;
    }

    @Override
    public boolean containsKey(Object key) {
        purgeKeys();
        return map.containsKey(new Key<K>((K)key, null));
    }

    @Override
    public void clear() {
    	for (V v : map.values()) destroy.perform("clear", v);
        while (queue.poll() != null);
        map.clear();
    }

    @Override
    public boolean containsValue(Object value) {
        purgeKeys();
        return map.containsValue(value);
    }

    protected class Key<T> extends WeakReference<T> {
        protected final int hash;

        protected Key(T t, ReferenceQueue<T> queue) {
            super(t, queue);
            if (t == null) throw new NullPointerException();
            else if (identity) hash = System.identityHashCode(t);
            else hash = t.hashCode();
        }

        @Override
        public boolean equals(Object obj) {
            return this == obj || obj instanceof Key && (identity ? ((Key<?>) obj).get() == get() : get().equals(((Key<?>) obj).get()));
        }

        @Override
        public int hashCode() {
            return hash;
        }
    }

    protected class ConcurrentWeakIdentityIterator implements Iterator<Map.Entry<K, V>> {
        protected final Iterator<Map.Entry<Key<K>, V>> it;
        protected Map.Entry<K, V> nextValue;

        protected ConcurrentWeakIdentityIterator(Iterator<Map.Entry<Key<K>, V>> it) {
            this.it = it;
        }

        @Override
        public boolean hasNext() {
            if (nextValue != null) return true;
            while (it.hasNext()) {
                Map.Entry<Key<K>, V> entry = it.next();
                K key = entry.getKey().get();
                if (key != null) {
                    nextValue = new Entry(key, entry.getValue());
                    return true;
                } else {
                    if (destroy != null) destroy.perform("next", entry.getValue());
                	it.remove();
                }
            }
            return false;
        }

        @Override
        public Map.Entry<K, V> next() {
            if (!hasNext()) throw new NoSuchElementException();
            Map.Entry<K, V> entry = nextValue;
            nextValue = null;
            return entry;
        }

        @Override
        public void remove() {
            if (destroy != null && nextValue != null) destroy.perform("itr", nextValue.getValue());
            it.remove();
            nextValue = null;
        }
    }

    protected class EntrySet extends AbstractSet<Map.Entry<K, V>> {
        @Override
        public Iterator<Map.Entry<K, V>> iterator() {
            return new ConcurrentWeakIdentityIterator(map.entrySet().iterator());
        }

        @Override
        public int size() {
            return ConcurrentWeakIdentityHashMap.this.size();
        }

        @Override
        public void clear() {
            ConcurrentWeakIdentityHashMap.this.clear();
        }

        @Override
        public boolean contains(Object o) {
            if (!(o instanceof Map.Entry)) return false;
            Map.Entry<?, ?> e = (Map.Entry<?, ?>) o;
            return ConcurrentWeakIdentityHashMap.this.get(e.getKey()) == e.getValue();
        }

        @Override
        public boolean remove(Object o) {
            if (!(o instanceof Map.Entry)) return false;
            Map.Entry<?, ?> e = (Map.Entry<?, ?>) o;
            return ConcurrentWeakIdentityHashMap.this.remove(e.getKey(), e.getValue());
        }
    }

    protected class Entry extends AbstractMap.SimpleEntry<K, V> {
        protected Entry(K key, V value) {
            super(key, value);
        }

        @Override
        public V setValue(V value) {
            ConcurrentWeakIdentityHashMap.this.put(getKey(), value);
            return super.setValue(value);
        }

        @Override
        public boolean equals(Object obj) {
            if (obj instanceof Map.Entry) {
                Map.Entry<?, ?> e = (Map.Entry<?, ?>) obj;
                if (identity) return getKey() == e.getKey() && getValue() == e.getValue();
                else return getKey().equals(e.getKey()) && getValue().equals(e.getValue());
            }
            return false;
        }

        @Override
        public int hashCode() {
            if (identity) return System.identityHashCode(getKey()) ^ System.identityHashCode(getValue());
            else return getKey().hashCode() ^ getValue().hashCode();
        }
    }

	@Override
	public V find(K... paras) {
		if(paras != null && paras.length > 0) return get(paras[0]);
		else return null;
	}

	@Override
	public <P> P store(V value, K... paras) {
		if(paras != null && paras.length > 0) put(paras[0], value);
		return null;
	}

	@Override
	public <P> P discard(K... paras) {
		if(paras != null && paras.length > 0) remove(paras[0]);
		return  null;
	}

	@Override
	public <P> P empty(K... paras) {
		clear();
		return null;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		Set<K> set = keySet();
		Collection<K[]> ret = new ArrayList<K[]>(set.size());
		for (K o : set) {
			K[] key = PlatformUtil.newArrayOf(1, o);
			key[0] = o;
			ret.add(key);
		}
		return ret;
	}

	@Override
	public Map<K[], V> all(K... paras) {
		Set<Map.Entry<K,V>> set = entrySet();
		Map<K[], V> ret = new HashMap<K[], V>(set.size() * 2);
		for (Map.Entry<K,V> entry : set) {
			K[] key = PlatformUtil.newArrayOf(1, entry.getKey());
			key[0] = entry.getKey();
			ret.put(key, entry.getValue());
		}
		return ret;
	}

	@Override
	public <N> N extend(Object object) {
		if (object != null) {
			Object method = object instanceof ResourceMethod ? object : methods.process(object);
			if (method!= null) switch((ResourceMethod) method) {
				case count : return (N) count;
			}
		}
		return null;
	}
	
	@Override
	public Collection<Object> methods() {
		return methods;
	}
}
