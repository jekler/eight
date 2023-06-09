package net.yeeyaa.eight.common.processor;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;

import javax.json.Json;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;

import net.yeeyaa.eight.IProcessor;


public class JsonValueProcessor implements IProcessor<Object, JsonValue>{
	protected IProcessor<Object, Object> preProcessor;
	
	public void setPreProcessor(IProcessor<Object, Object> preProcessor) {
		this.preProcessor = preProcessor;
	}

	@Override
	public JsonValue process(Object instance) {
		if (preProcessor != null) instance = preProcessor.process(instance);
		if (instance == null) return JsonValue.NULL;
		else if (instance instanceof JsonValue) return (JsonValue) instance;
		else if (instance instanceof Map) {
			JsonObjectBuilder builder = Json.createObjectBuilder();
			for (Entry<String, Object> o : ((Map<String, Object>) instance).entrySet()) if (o.getKey() != null) builder.add(o.getKey(), process(o.getValue()));
			return builder.build();
		} else if (instance instanceof Collection) {
			JsonArrayBuilder builder = Json.createArrayBuilder();
			for (Object o : (Collection<Object>) instance) builder.add(process(o));
			return builder.build();
		} else if (instance instanceof Boolean) if ((Boolean) instance) return JsonValue.TRUE;
		else return JsonValue.FALSE;
		else {
			JsonArrayBuilder builder = Json.createArrayBuilder();
			if (instance instanceof String) builder.add((String)instance);
			else if (instance instanceof Integer) builder.add((Integer) instance);
			else if (instance instanceof Double) builder.add((Double) instance);
			else if (instance instanceof Long) builder.add((Long) instance);
			else if (instance instanceof BigDecimal) builder.add((BigDecimal) instance);
			else if (instance instanceof BigInteger) builder.add((BigInteger) instance);
			else builder.add(instance.toString());
			return builder.build().get(0);
		}
	}
}
