package net.yeeyaa.eight.core.processor;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Properties;

import net.yeeyaa.eight.IProcessor;


public class PropertiesStringProcessor implements IProcessor<Properties, String> {
	@Override
	public String process(Properties properties) {
		StringWriter writer = new StringWriter();
		properties.list(new PrintWriter(writer));
		return writer.getBuffer().toString();
	}
}
