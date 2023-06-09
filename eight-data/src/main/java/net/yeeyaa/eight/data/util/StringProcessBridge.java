package net.yeeyaa.eight.data.util;

import java.util.Map;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;

import org.hibernate.search.bridge.ParameterizedBridge;
import org.hibernate.search.bridge.StringBridge;


public class StringProcessBridge implements StringBridge, ParameterizedBridge {	
	protected IProcessor<Object, String> processor;
	protected IBiProcessor<String, Object, Object> bi;
	protected String classname;
	
	protected class Processor implements IProcessor<Object, String> {
		protected volatile int resume; 
		protected volatile IProcessor<Object, String> processor;
		protected Object key;
		
		public Processor(Object key, int resume) {
			this.key = key;
			this.resume = resume;
		}

		@Override
		public String process(Object instance) {
			if (resume >= 0) {
				Object processor = bi.perform(classname, key);
				if (processor instanceof IProcessor) this.processor = (IProcessor<Object, String>) processor;
				switch (resume) {
					case 0: resume = -1;
					break;
					case 1: if (processor != null) resume = -1;
					break;
					case 2: if (this.processor != null) resume = -1;
				}
			}
			if (processor == null) return instance == null ? null : instance.toString();
			else return processor.process(instance);
		}
	}
	
	@Override
	public String objectToString(Object str) {
		if (processor == null) return str == null ? null : str.toString();
		else return processor.process(str);
	}

	@Override
	public void setParameterValues(Map<String, String> parameters) {
		ClassLoader classloader = getClass().getClassLoader();
		if (classloader instanceof IBiProcessor) {
			bi = (IBiProcessor<String, Object, Object>) classloader;
			classname = getClass().getName();
			String processor = parameters.get("processor_0");
			if (processor != null && processor.trim().length() > 0) this.processor = new Processor(processor, 0);
			else {
				processor = parameters.get("processor_1");
				if (processor != null && processor.trim().length() > 0) this.processor = new Processor(processor, 1);
				else {
					processor = parameters.get("processor_2");
					if (processor != null && processor.trim().length() > 0) this.processor = new Processor(processor, 2);
					else {
						processor = parameters.get("processor_3");
						if (processor != null && processor.trim().length() > 0) this.processor = new Processor(processor, 3);
					}
				}
			}
		}
	}
}
