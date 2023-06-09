package net.yeeyaa.eight.common.processor;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;

import net.yeeyaa.eight.IProcessor;


public class JsonRawProcessor implements IProcessor<JsonValue, Object>{
	protected IProcessor<Object, Object> postProcessor;
	protected Boolean readonly; 
	
	public void setPostProcessor(IProcessor<Object, Object> postProcessor) {
		this.postProcessor = postProcessor;
	}

	public void setReadonly(Boolean readonly) {
		this.readonly = readonly;
	}
	
	@Override
	public Object process(JsonValue instance) {
		Object ret = instance;
		if (instance != null) switch (((JsonValue)instance).getValueType()) {
			case ARRAY: ArrayList<Object> list = new ArrayList<Object>(((JsonArray)instance).size());
				for (JsonValue v : (JsonArray)instance) list.add(process(v));
				if (postProcessor != null) list = (ArrayList<Object>)postProcessor.process(list);
				if (readonly == null) return list;
				else if (readonly) return Collections.unmodifiableList(list);
				else return new CopyOnWriteArrayList<Object>(list);
			case OBJECT: Map<String, Object> map = Boolean.FALSE.equals(readonly) ? new ConcurrentHashMap<String, Object>(((JsonObject)instance).size()) : new HashMap<String, Object>(((JsonObject)instance).size());
				for (Entry<String, JsonValue> entry : ((JsonObject)instance).entrySet()) map.put(entry.getKey(), process(entry.getValue()));
				if (postProcessor != null) map = (Map<String, Object>)postProcessor.process(map);
				if (Boolean.TRUE.equals(readonly)) return Collections.unmodifiableMap(map);
				else return map;
			case STRING: ret = ((JsonString)instance).getString();
				break;
			case NUMBER: ret = ((JsonNumber)instance).bigDecimalValue();
				break;
			case TRUE: ret = true;
				break; 
			case FALSE: ret = false;
				break;
			case NULL: ret = null;
		}
		if (postProcessor == null) return ret;
		else return postProcessor.process(ret);
	}
}
