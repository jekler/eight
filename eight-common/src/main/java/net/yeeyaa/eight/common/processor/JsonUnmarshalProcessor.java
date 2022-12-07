package net.yeeyaa.eight.common.processor;

import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;

import javax.json.Json;
import javax.json.JsonReader;
import javax.json.JsonStructure;

import net.yeeyaa.eight.IProcessor;


public class JsonUnmarshalProcessor implements IProcessor<Object, JsonStructure>{
	@Override
	public JsonStructure process(Object instance) {
		if (instance == null) return null;
		else {
			JsonReader reader;
			if (instance instanceof Reader) reader = Json.createReader((Reader) instance);
			else if (instance instanceof InputStream) reader = Json.createReader((InputStream) instance);
			else reader = Json.createReader(new StringReader(instance.toString()));
			try {
				return reader.read();
			} finally {
				reader.close();
			}
		}
	}
}