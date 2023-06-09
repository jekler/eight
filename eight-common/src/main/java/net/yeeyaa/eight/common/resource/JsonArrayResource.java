package net.yeeyaa.eight.common.resource;

import java.io.StringWriter;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonString;
import javax.json.JsonValue;
import javax.json.JsonWriter;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;


public class JsonArrayResource<V> implements IListableResource<Integer, V>, IProcessor<List<V>, JsonArray>, List<V>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected List<V> json;
	protected IProcessor<Object, Object> postProcessor;
	protected IProcessor<Object, Object> preProcessor;
	protected Boolean readonly; 
	protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object paras) {
			return new Long(json.size());
		}
	};
	
	public JsonArrayResource(JsonArray json, IProcessor<Object, Object> preProcessor, IProcessor<Object, Object> postProcessor, Boolean readonly) {
		this.postProcessor = postProcessor;
		this.preProcessor = preProcessor;
		this.readonly = readonly;
		if (json != null) {
			Object raw = raw(json);
			if (raw instanceof List) this.json = (List<V>) raw;
		}
		if (this.json == null) if (readonly == null) this.json = new ArrayList<V>();
		else if (readonly) this.json = Collections.unmodifiableList(new ArrayList<V>(0));
		else this.json = new CopyOnWriteArrayList<V>();
	}
	
	protected Object raw(Object value) {
		if (value instanceof JsonValue) switch (((JsonValue)value).getValueType()) {
			case ARRAY: ArrayList<Object> list = new ArrayList<Object>(((JsonArray)value).size());
				for (JsonValue v : (JsonArray)value) list.add(raw(v));
				if (postProcessor != null) list = (ArrayList<Object>)postProcessor.process(list);
				if (readonly == null) return list;
				else if (readonly) return Collections.unmodifiableList(list);
				else return new CopyOnWriteArrayList<Object>(list);
			case OBJECT: Map<String, Object> map = Boolean.FALSE.equals(readonly) ? new ConcurrentHashMap<String, Object>(((JsonObject)value).size()) : new HashMap<String, Object>(((JsonObject)value).size());
				for (Entry<String, JsonValue> entry : ((JsonObject)value).entrySet()) map.put(entry.getKey(), raw(entry.getValue()));
				if (postProcessor != null) map = (Map<String, Object>)postProcessor.process(map);
				if (Boolean.TRUE.equals(readonly)) return Collections.unmodifiableMap(map);
				else return map;
			case STRING: value = ((JsonString)value).getString();
				break;
			case NUMBER: value = ((JsonNumber)value).bigDecimalValue();
				break;
			case TRUE: value = true;
				break; 
			case FALSE: value = false;
				break;
			case NULL: value = null;
		}
		if (postProcessor == null) return value;
		else return postProcessor.process(value);
	}

	protected Object json(Object value) {
		if (preProcessor != null) value = preProcessor.process(value);
		if (value instanceof Map) {
			JsonObjectBuilder builder = Json.createObjectBuilder();
			for (Entry<Object, Object> entry : ((Map<Object, Object>)value).entrySet()) {
				Object v = preProcessor == null ? entry.getValue() : preProcessor.process(entry.getValue());
				if (v == null) builder.addNull(entry.getKey().toString());
				else if (v instanceof String) builder.add(entry.getKey().toString(), (String) v);
				else if (v instanceof Boolean) builder.add(entry.getKey().toString(), (Boolean) v);
				else if (v instanceof Integer) builder.add(entry.getKey().toString(), (Integer) v);
				else if (v instanceof Long) builder.add(entry.getKey().toString(), (Long) v);
				else if (v instanceof Double) builder.add(entry.getKey().toString(), (Double) v);
				else if (v instanceof BigDecimal) builder.add(entry.getKey().toString(), (BigDecimal) v);
				else if (v instanceof BigInteger) builder.add(entry.getKey().toString(), (BigInteger) v);
				else if (v instanceof Map) builder.add(entry.getKey().toString(), (JsonObjectBuilder) json(v));
				else if (v instanceof Collection) builder.add(entry.getKey().toString(), (JsonArrayBuilder) json(v));
				else if (v instanceof JsonObjectBuilder) builder.add(entry.getKey().toString(), (JsonObjectBuilder) v);
				else if (v instanceof JsonArrayBuilder) builder.add(entry.getKey().toString(), (JsonArrayBuilder) v);
				else if (v instanceof JsonValue) builder.add(entry.getKey().toString(), (JsonValue) v);
			}
			return builder;
		} else if (value instanceof Collection) {
			JsonArrayBuilder builder = Json.createArrayBuilder();
			for (Object v : (Collection<Object>)value) {
				if(preProcessor != null) v = preProcessor.process(v);
				if (v == null) builder.addNull();
				else if (v instanceof String) builder.add((String) v);
				else if (v instanceof Boolean) builder.add((Boolean) v);
				else if (v instanceof Integer) builder.add((Integer) v);
				else if (v instanceof Long) builder.add((Long) v);
				else if (v instanceof Double) builder.add((Double) v);
				else if (v instanceof BigDecimal) builder.add((BigDecimal) v);
				else if (v instanceof BigInteger) builder.add((BigInteger) v);
				else if (v instanceof Map) builder.add((JsonObjectBuilder) json(v));
				else if (v instanceof Collection) builder.add((JsonArrayBuilder) json(v));
				else if (v instanceof JsonObjectBuilder) builder.add((JsonObjectBuilder)v);
				else if (v instanceof JsonArrayBuilder) builder.add((JsonArrayBuilder) v);
				else if (v instanceof JsonValue) builder.add((JsonValue) v);
			}
			return builder;
		} else return null;
	}
	
	@Override
	public JsonArray process(List<V> instance) {
		if (instance == null) instance = this.json;
		if (instance instanceof JsonArray) return (JsonArray) instance;
		else return (JsonArray) json(instance);
	}

	@Override
	public boolean contains(Object o) {
		return json.contains(o);
	}

	@Override
	public Iterator<V> iterator() {
		return json.iterator();
	}

	@Override
	public Object[] toArray() {
		return json.toArray();
	}

	@Override
	public <T> T[] toArray(T[] a) {
		return json.toArray(a);
	}

	@Override
	public boolean add(V e) {
		return json.add(e);
	}

	@Override
	public boolean containsAll(Collection<?> c) {
		return json.containsAll(c);
	}

	@Override
	public boolean addAll(Collection<? extends V> c) {
		return json.addAll(c);
	}

	@Override
	public boolean removeAll(Collection<?> c) {
		return json.removeAll(c);
	}

	@Override
	public boolean retainAll(Collection<?> c) {
		return json.retainAll(c);
	}

	@Override
	public boolean addAll(int index, Collection<? extends V> c) {
		return json.addAll(index, c);
	}

	@Override
	public V get(int index) {
		return json.get(index);
	}

	@Override
	public V set(int index, V element) {
		return json.set(index, element);
	}

	@Override
	public void add(int index, V element) {
		json.add(index, element);
	}

	@Override
	public V remove(int index) {
		return json.remove(index);
	}

	@Override
	public boolean remove(Object key) {
		return json.remove(key);
	}
	
	@Override
	public int indexOf(Object o) {
		return json.indexOf(o);
	}

	@Override
	public int lastIndexOf(Object o) {
		return json.lastIndexOf(o);
	}
	
	@Override
	public ListIterator<V> listIterator() {
		return json.listIterator();
	}
	
	@Override
	public ListIterator<V> listIterator(int index) {
		return json.listIterator(index);
	}
	
	@Override
	public List<V> subList(int fromIndex, int toIndex) {
		return json.subList(fromIndex, toIndex);
	}
	
	@Override
	public int size() {
		return json.size();
	}

	@Override
	public boolean isEmpty() {
		return json.isEmpty();
	}

	@Override
	public void clear() {
		json.clear();
	}

	@Override
	public V find(Integer... paras) {
		int size = json.size();
		if (paras == null || paras.length == 0 || paras[0] == null) return json.get(size - 1);
		else if (paras[0] >= 0 && paras[0] < size) return json.get(paras[0]);
		else return null;
	}
	
	@Override
	public <P> P store(V value, Integer... paras) {
		if (paras == null || paras.length == 0 || paras[0] == null) json.add(value);
		else if (paras[0] >= 0 && paras[0] <= json.size()) json.add(paras[0], value);
		return null;
	}

	@Override
	public <P> P discard(Integer... paras) {
		int size = json.size();
		if (paras == null || paras.length == 0 || paras[0] == null) json.remove(size - 1);
		else if (paras[0] >= 0 && paras[0] < json.size()) json.remove(paras[0].intValue());
		return null;
	}

	@Override
	public <P> P empty(Integer... paras) {
		json.clear();
		return null;
	}

	@Override
	public Collection<Integer[]> keys(Integer... paras) {
		int size = json.size();
		Collection<Integer[]> ret = new ArrayList<Integer[]>(size);
		for(int i = 0; i < size; i++) ret.add(new Integer[]{i});
		return ret;
	}

	@Override
	public Map<Integer[], V> all(Integer... paras) {
		Map<Integer[], V> map = new HashMap<Integer[], V>(json.size() * 2);
		int i = 0;
		for(V value : json) map.put(new Integer[]{i++}, value);
		return map;
	}
	
	@Override
	public String toString() {
		StringWriter stWriter = new StringWriter();
		JsonWriter jsonWriter = Json.createWriter(stWriter);
		try {
			jsonWriter.write(process(null));
		} finally {
			jsonWriter.close();
		}
		return stWriter.toString();
	}

	@Override
	public int hashCode() {
		int result = ((json == null) ? 0 : json.hashCode());
		result = 31 * result + ((postProcessor == null) ? 0 : postProcessor.hashCode());
		result = 31 * result + ((preProcessor == null) ? 0 : preProcessor.hashCode());
		result = 31 * result + ((readonly == null) ? 0 : readonly.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) return true;
		if (obj == null) return false;
		if (getClass() != obj.getClass()) return false;
		JsonArrayResource other = (JsonArrayResource) obj;
		if (json == null) {
			if (other.json != null) return false;
		} else if (!json.equals(other.json)) return false;
		if (postProcessor == null) {
			if (other.postProcessor != null) return false;
		} else if (!postProcessor.equals(other.postProcessor)) return false;
		if (preProcessor == null) {
			if (other.preProcessor != null) return false;
		} else if (!preProcessor.equals(other.preProcessor)) return false;
		if (readonly == null) {
			if (other.readonly != null) return false;
		} else if (!readonly.equals(other.readonly)) return false;
		return true;
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
