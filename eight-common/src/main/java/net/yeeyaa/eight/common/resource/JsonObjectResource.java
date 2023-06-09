package net.yeeyaa.eight.common.resource;

import java.io.StringWriter;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
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


public class JsonObjectResource<V> implements IListableResource<String, V>, IProcessor<Map<String, V>, JsonObject>, Map<String, V>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected Map<String, V> json;
	protected IProcessor<Object, Object> postProcessor;
	protected IProcessor<Object, Object> preProcessor;
	protected Boolean readonly; 
	protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object paras) {
			return new Long(((Map<String, V>)json).size());
		}
	};
	
	public JsonObjectResource(JsonObject json, IProcessor<Object, Object> preProcessor, IProcessor<Object, Object> postProcessor, Boolean readonly) {
		this.postProcessor = postProcessor;
		this.preProcessor = preProcessor;
		this.readonly = readonly;
		if (json != null) {
			Object raw = raw(json);
			if (raw instanceof Map) this.json = (Map<String, V>) raw;
		}
		if (this.json == null) if (readonly == null) this.json = new HashMap<String, V>();
		else if (readonly) this.json = Collections.unmodifiableMap(new HashMap<String, V>(0));
		else this.json = new ConcurrentHashMap<String, V>();
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
	public JsonObject process(Map<String, V> instance) {
		if (instance == null) instance = this.json;
		if (instance instanceof JsonObject) return (JsonObject) instance;
		else return (JsonObject) json(instance);
	}
	
	@Override
	public V remove(Object key) {
		return json.remove(key);
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
	public boolean containsKey(Object key) {
		return json.containsKey(key);
	}

	@Override
	public boolean containsValue(Object value) {
		return json.containsValue(value);
	}

	@Override
	public V get(Object key) {
		return json.get(key);
	}

	@Override
	public V put(String key, V value) {
		return json.put(key, value);
	}

	@Override
	public void putAll(Map<? extends String, ? extends V> m) {
		json.putAll(m);
	}

	@Override
	public void clear() {
		json.clear();
	}

	@Override
	public Set<String> keySet() {
		return json.keySet();
	}

	@Override
	public Collection<V> values() {
		return json.values();
	}

	@Override
	public Set<Entry<String, V>> entrySet() {
		return json.entrySet();
	}

	@Override
	public V find(String... paras) {
		return ((Map<String, V>)json).get(paras[0]);
	}
	
	@Override
	public <P> P store(V value, String... paras) {
		json.put(paras[0], value);
		return null;
	}

	@Override
	public <P> P discard(String... paras) {
		json.remove(paras[0]);
		return null;
	}

	@Override
	public <P> P empty(String... paras) {
		json.clear();
		return null;
	}

	@Override
	public Collection<String[]> keys(String... paras) {
		ArrayList<String[]> ret = new ArrayList<String[]>(json.size());
		for (String key : json.keySet()) ret.add(new String[]{key});
		return ret;
	}

	@Override
	public Map<String[], V> all(String... paras) {
		HashMap<String[], V> ret = new HashMap<String[], V>(json.size() * 2);
		for (Entry<String, V> entry : json.entrySet()) ret.put(new String[]{entry.getKey()}, entry.getValue());
		return ret;
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
		JsonObjectResource other = (JsonObjectResource) obj;
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
