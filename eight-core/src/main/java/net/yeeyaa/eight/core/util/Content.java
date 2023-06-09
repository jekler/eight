package net.yeeyaa.eight.core.util;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Collection;
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
	public byte[] bytes;
	public short[] shorts;
	public int[] ints;
	public long[] longs;
	public float[] floats;
	public double[] doubles;
	public boolean[] booleans;
	public char[] chars;
	
	
	public Content(){}
	
	public Content(Object instance) {
		if(instance != null) if(instance instanceof Map) map = (Map) instance;
		else if(instance instanceof Set) set = (Set) instance;
		else if(instance instanceof Collection) list = (Collection) instance;
		else if(instance instanceof Object[]) array = (Object[]) instance;
		else if (instance instanceof byte[]) bytes = (byte[]) instance;
		else if (instance instanceof int[]) ints = (int[]) instance;
		else if (instance instanceof long[]) longs = (long[]) instance;
		else if (instance instanceof short[]) shorts = (short[]) instance;
		else if (instance instanceof double[]) doubles = (double[]) instance;
		else if (instance instanceof float[]) floats = (float[]) instance;
		else if (instance instanceof boolean[]) booleans = (boolean[]) instance;
		else if (instance instanceof char[]) chars = (char[]) instance;
		else this.instance = instance;
	}
	
	public Object get(){
		if(instance != null) return instance;
		else if(map != null) return map;
		else if(set != null) return set;
		else if(list != null) return list;
		else if(array != null) return array;
		else if(bytes != null) return bytes;
		else if(ints != null) return ints;
		else if(longs != null) return longs;
		else if(shorts != null) return shorts;
		else if(doubles != null) return doubles;
		else if(floats != null) return floats;
		else if(booleans != null) return booleans;
		else return chars;
	}
	
	public void set(Object object){
		instance = null;
		map =null;
		set = null;
		list = null;
		array = null;
		bytes =null;
		ints = null;
		longs = null;
		shorts = null;
		doubles =null;
		floats = null;
		booleans = null;
		chars = null;
		if(object != null) if(object instanceof Map) map = (Map) object;
		else if(object instanceof Set) set = (Set) object;
		else if(object instanceof Collection) list = (Collection) object;
		else if(object instanceof Object[]) array = (Object[]) object;
		else if (object instanceof byte[]) bytes = (byte[]) object;
		else if (object instanceof int[]) ints = (int[]) object;
		else if (object instanceof long[]) longs = (long[]) object;
		else if (object instanceof short[]) shorts = (short[]) object;
		else if (object instanceof double[]) doubles = (double[]) object;
		else if (object instanceof float[]) floats = (float[]) object;
		else if (object instanceof boolean[]) booleans = (boolean[]) object;
		else if (object instanceof char[]) chars = (char[]) object;
		else this.instance = object;
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
					(PlatformUtil.compare(bytes, other.bytes)) && (PlatformUtil.compare(ints, other.ints)) &&
					(PlatformUtil.compare(longs, other.longs)) && (PlatformUtil.compare(shorts, other.shorts)) &&
					(PlatformUtil.compare(doubles, other.doubles)) && (PlatformUtil.compare(floats, other.floats)) &&
					(PlatformUtil.compare(booleans, other.booleans)) && (PlatformUtil.compare(chars, other.chars)) &&
					(PlatformUtil.compare(array, other.array)));
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
		if(array != null) code = code * 29 + PlatformUtil.hash(array);
		if(bytes != null) code = code * 29 + PlatformUtil.hash(bytes);
		if(ints != null) code = code * 29 + PlatformUtil.hash(ints);
		if(longs != null) code = code * 29 + PlatformUtil.hash(longs);
		if(shorts != null) code = code * 29 + PlatformUtil.hash(shorts);
		if(doubles != null) code = code * 29 + PlatformUtil.hash(doubles);
		if(floats != null) code = code * 29 + PlatformUtil.hash(floats);
		if(booleans != null) code = code * 29 + PlatformUtil.hash(booleans);
		if(chars != null) code = code * 29 + PlatformUtil.hash(chars);
		return code;
	}

	@Override
	public String toString() {
		if(instance != null) return instance.toString();
		else if(map != null) return map.toString();
		else if(set != null) return set.toString();
		else if(list != null) return list.toString();
		else if(array != null) return Arrays.asList(array).toString();
		else if (bytes != null) return PlatformUtil.toString(bytes);
		else if (ints != null) return PlatformUtil.toString(ints);
		else if (longs != null) return PlatformUtil.toString(longs);
		else if (shorts != null) return PlatformUtil.toString(shorts);
		else if (doubles != null) return PlatformUtil.toString(doubles);
		else if (floats != null) return PlatformUtil.toString(floats);
		else if (booleans != null) return PlatformUtil.toString(booleans);
		else if (chars != null) return PlatformUtil.toString(chars);
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
