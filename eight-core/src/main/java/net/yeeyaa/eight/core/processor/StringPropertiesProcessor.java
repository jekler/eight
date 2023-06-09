package net.yeeyaa.eight.core.processor;

import java.io.IOException;
import java.io.StringReader;
import java.util.Properties;

import net.yeeyaa.eight.IProcessor;


public class StringPropertiesProcessor implements IProcessor<String, Properties> {
	@Override
	public Properties process(String input) {
		final Properties p = new Properties();
	    try {
			p.load(new StringReader(input));
		} catch (IOException e) {}
	    return p;
	}
}
