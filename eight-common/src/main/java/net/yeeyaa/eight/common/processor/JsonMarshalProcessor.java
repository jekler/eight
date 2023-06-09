package net.yeeyaa.eight.common.processor;

import java.io.StringWriter;

import javax.json.Json;
import javax.json.JsonStructure;
import javax.json.JsonWriter;

import net.yeeyaa.eight.IProcessor;


public class JsonMarshalProcessor implements IProcessor<Object, String>{
	protected IProcessor<Object, JsonStructure> preProcessor;
	
	public void setPreProcessor(IProcessor<Object, JsonStructure> preProcessor) {
		this.preProcessor = preProcessor;
	}
	
	@Override
	public String process(Object instance) {
		JsonStructure json = preProcessor == null ? (JsonStructure) instance : preProcessor.process(instance);
		if (instance == null) return null;
		else {
			StringWriter stWriter = new StringWriter();
			JsonWriter jsonWriter = Json.createWriter(stWriter);
			try {
				jsonWriter.write(json);
			} finally {
				jsonWriter.close();
			}
			return stWriter.toString();
		}
	}
}
