package net.yeeyaa.eight.common.processor;

import java.io.StringWriter;

import javax.json.Json;
import javax.json.JsonStructure;
import javax.json.JsonWriter;

import net.yeeyaa.eight.IProcessor;


public class JsonMarshalProcessor implements IProcessor<JsonStructure, String>{
	@Override
	public String process(JsonStructure instance) {
		if (instance == null) return null;
		else {
			StringWriter stWriter = new StringWriter();
			JsonWriter jsonWriter = Json.createWriter(stWriter);
			try {
				jsonWriter.write(instance);
			} finally {
				jsonWriter.close();
			}
			return stWriter.toString();
		}
	}
}