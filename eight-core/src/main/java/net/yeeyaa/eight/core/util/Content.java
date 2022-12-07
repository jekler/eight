package net.yeeyaa.eight.core.util;

import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;


public class Content implements Cloneable, Serializable{
	private static final long serialVersionUID = 5194183658853832286L;
	public Object instance;
	public Map<?, ?> map;
	public Set<?> set;
	public Collection<?> list;
	public Object[] array;
	
	public Content(){}
	
	public Content(Object instance) {
		if(instance != null) if(instance instanceof Map) map = (Map) instance;
		else if(instance instanceof Set) set = (Set) instance;
		else if(instance instanceof Collection) list = (Collection) instance;
		else if(instance instanceof Object[]) array = (Object[]) instance;
		else if(instance.getClass().isArray()){
    		List<Object> ls = new ArrayList<Object>(Array.getLength(instance));
    		for (int i = 0; i < Array.getLength(instance); i ++) ls.add(Array.get(instance, i));
    		array = ls.toArray();
		} else this.instance = instance;
	}
	
	public Object get(){
		if(instance != null) return instance;
		else if(map != null) return map;
		else if(set != null) return set;
		else if(list != null) return list;
		else return array;
	}
	
	public void set(Object object){
		instance = null;
		map =null;
		set = null;
		list = null;
		array = null;
		if(object != null) if(object instanceof Map) map = (Map) object;
		else if(object instanceof Set) set = (Set) object;
		else if(object instanceof Collection) list = (Collection) object;
		else if(object instanceof Object[]) array = (Object[]) object;
		else if(object.getClass().isArray()){
    		List<Object> ls = new ArrayList<Object>(Array.getLength(object));
    		for (int i = 0; i < Array.getLength(object); i ++) ls.add(Array.get(object, i));
    		array = ls.toArray();
		} else this.instance = object;
	}
	
	public static <T> T strip(Object object){
		if(object instanceof Content) return (T) ((Content)object).get();
		else return (T) object;
	}
	
	public static Content create(Object object){
		if(object == null) return null;
		else if(object instanceof Content) return (Content) object;
		else return new Content(object);
	}
	
	public static Content createWithNull(Object object){
		if(object instanceof Content) return (Content) object;
		else return new Content(object);
	}
	
	@Override
	public boolean equals(Object object) {
		if(this == object) return true;
		else if(object instanceof Content){
			Content other = (Content) object;
			return ((instance == other.instance ||(instance != null && instance.equals(other.instance))) && 
					(map == other.map || (map != null && map.equals(other.map))) &&
					(set == other.set || (set != null && set.equals(other.set))) &&
					(list == other.list || (list != null && list.equals(other.list))) &&
					(array == other.array || (array != null && array.equals(other.array))));
		}
		return false;
	}

	@Override
	public Object clone() throws CloneNotSupportedException {
		return super.clone();
	}

	@Override
	public int hashCode() {
		int code = 0;
		if(instance != null) code += 4199 * instance.hashCode();
		if(map != null) code += 221 * map.hashCode();
		if(set != null) code += 13 * set.hashCode();
		if(list != null) code += 7 * list.hashCode();
		if(array != null) code += array.hashCode();
		return code;
	}

	@Override
	public String toString() {
		if(instance != null) return instance.toString();
		else if(map != null) return map.toString();
		else if(set != null) return set.toString();
		else if(list != null) return list.toString();
		else if(array != null) return array.toString();
		else return null;
	}
	
	public static class Couple<K, V> implements Entry<K, V>, Cloneable, Serializable{
		protected K key;
		protected V value;
	
		public Couple() {}

		public Couple(K key, V value) {
			this.key = key != null && (key instanceof Map || key instanceof Collection || key.getClass().isArray()) ? (K) new Content(key) : key;
			this.value = value != null && (value instanceof Map || value instanceof Collection || value.getClass().isArray()) ? (V) new Content(value) : value;
		}
		
		public K getK() {
			return key;
		}

		public void setK(K key) {
			this.key = key != null && (key instanceof Map || key instanceof Collection || key.getClass().isArray()) ? (K) new Content(key) : key;;
		}

		public V getV() {
			return value;
		}

		public void setV(V value) {
			this.value = value != null && (value instanceof Map || value instanceof Collection || value.getClass().isArray()) ? (V) new Content(value) : value;
		}

		public K getKey() {
			return key instanceof Content ? (K) ((Content) key).get() : key;
		}
		
		public V getValue() {
			return value instanceof Content ? (V) ((Content) value).get() : value;
		}
		
		public V setValue(V value) {
			V tmp = this.value;
			this.value = value != null && (value instanceof Map || value instanceof Collection || value.getClass().isArray()) ? (V) new Content(value) : value;
			return tmp instanceof Content ? (V) ((Content) tmp).get() : tmp;
		}
		
		public boolean equals(Object o) {
		    if (!(o instanceof Map.Entry)) return false;
		    Entry e = (Entry)o;
			K key = getKey();
			V value = getValue();
		    return key == null ? e.getKey() == null : key.equals(e.getKey()) && value == null ? e.getValue() == null : value.equals(e.getValue());
		}
		
		public int hashCode() {
			K key = getKey();
			V value = getValue();
		    return (key   == null ? 0 : key.hashCode()) ^ (value == null ? 0 : value.hashCode());
		}

		public String toString() {
		    return getKey() + "=" + getValue();
		}	
		
		@Override
		public Object clone() throws CloneNotSupportedException {
			return super.clone();
		}
	}
}
