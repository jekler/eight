package net.yeeyaa.eight.core.util;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;

public class CopyOnWriteMap<K, V> implements Map<K, V>, Cloneable {
    protected volatile Map<K, V> internalMap;
    protected final boolean weak;
    
    public CopyOnWriteMap() {
    	this.weak = false;
        internalMap = new HashMap<K, V>();
    }

    public CopyOnWriteMap(int initialCapacity) {
    	this.weak = false;
        internalMap = new HashMap<K, V>(initialCapacity);
    }

    public CopyOnWriteMap(Map<K, V> data) {
    	this.weak = false;
        internalMap = new HashMap<K, V>(data);
    }

    public CopyOnWriteMap(boolean weak) {
    	this.weak = weak;
        if(weak) internalMap = new WeakHashMap<K, V>();
        else internalMap = new HashMap<K, V>();
    }

    public CopyOnWriteMap(boolean weak, int initialCapacity) {
    	this.weak = weak;
        if(weak) internalMap = new WeakHashMap<K, V>(initialCapacity);
        else internalMap = new HashMap<K, V>(initialCapacity);
    }

    public CopyOnWriteMap(boolean weak, Map<K, V> data) {
    	this.weak = weak;
        if(weak) internalMap = new WeakHashMap<K, V>(data);
        else internalMap = new HashMap<K, V>(data);
    }
    
    public V put(K key, V value) {
        synchronized (this) {
            Map<K, V> newMap = weak ? new WeakHashMap<K, V>(internalMap) : new HashMap<K, V>(internalMap);
            V val = newMap.put(key, value);
            internalMap = newMap;
            return val;
        }
    }

    public V remove(Object key) {
        synchronized (this) {
            Map<K, V> newMap = weak ? new WeakHashMap<K, V>(internalMap) : new HashMap<K, V>(internalMap);
            V val = newMap.remove(key);
            internalMap = newMap;
            return val;
        }
    }

    public void putAll(Map<? extends K, ? extends V> newData) {
        synchronized (this) {
            Map<K, V> newMap = weak ? new WeakHashMap<K, V>(internalMap) : new HashMap<K, V>(internalMap);
            newMap.putAll(newData);
            internalMap = newMap;
        }
    }

    public void clear() {
        synchronized (this) {
            internalMap = weak ? new WeakHashMap<K, V>() : new HashMap<K, V>();
        }
    }

    public int size() {
        return internalMap.size();
    }

    public boolean isEmpty() {
        return internalMap.isEmpty();
    }

    public boolean containsKey(Object key) {
        return internalMap.containsKey(key);
    }

    public boolean containsValue(Object value) {
        return internalMap.containsValue(value);
    }

    public V get(Object key) {
        return internalMap.get(key);
    }

    public Set<K> keySet() {
        return internalMap.keySet();
    }

    public Collection<V> values() {
        return internalMap.values();
    }

    public Set<Entry<K, V>> entrySet() {
        return internalMap.entrySet();
    }

    @Override
    public Object clone() {
        try {
            return super.clone();
        } catch (CloneNotSupportedException e) {
            throw new InternalError();
        }
    }
}
