package net.yeeyaa.eight.common.processor;

import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

import javax.json.Json;
import javax.json.JsonReader;

import net.yeeyaa.eight.IProcessor;


public class JsonUnmarshalProcessor implements IProcessor<Object, Object>{
	protected IProcessor<Object, Object> postProcessor;
	
	public void setPostProcessor(IProcessor<Object, Object> postProcessor) {
		this.postProcessor = postProcessor;
	}
	
	@Override
	public Object process(Object instance) {
		if (instance == null) return null;
		else {
			JsonReader reader;
			if (instance instanceof Reader) reader = Json.createReader((Reader) instance);
			else if (instance instanceof InputStream) reader = Json.createReader((InputStream) instance);
			else reader = Json.createReader(new StringReader(instance.toString()));
			try {
				return postProcessor == null ? reader.read() : postProcessor.process(reader.read());
			} finally {
				reader.close();
			}
		}
	}
}
