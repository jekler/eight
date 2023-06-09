package net.yeeyaa.eight.core.processor;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.util.Content.Couple;


public class TypeForkProcessor implements IProcessor<Object, Object>, IBiProcessor<Object, Object, Object>{
	protected final Logger log;
	protected Map<String, Entry<Pattern, IProcessor<Object, Object>>> processors;
	protected IProcessor<Object, Object> defaultProcessor;
	protected IProcessor<Object, Object> typeProcessor;
	protected Boolean mode; 
	
	public TypeForkProcessor() {
		this.log = LoggerFactory.getLogger(TypeForkProcessor.class);
	}

	public TypeForkProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(TypeForkProcessor.class) : log;
	}
	
	public TypeForkProcessor(Boolean mode) {
		this.log = LoggerFactory.getLogger(TypeForkProcessor.class);
		this.mode = mode;
	}
	
	public TypeForkProcessor(Boolean mode, Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(TypeForkProcessor.class) : log;
		this.mode = mode;
	}

	public void setProcessors(Map<String, IProcessor<Object, Object>> processors) {
		if (processors != null) {
			this.processors = new LinkedHashMap<String, Entry<Pattern, IProcessor<Object, Object>>> (processors.size() * 2);
			for (Entry<String, IProcessor<Object, Object>> entry : processors.entrySet()) try {
				this.processors.put(entry.getKey(), new Couple<Pattern, IProcessor<Object, Object>>(mode == null ? null : Pattern.compile(entry.getKey()), entry.getValue()));
			} catch (Exception e) {
				log.error("StorageProcessor: processor failed.", e);
			}
		}
	}

	public void setDefaultProcessor(IProcessor<Object, Object> defaultProcessor) {
		this.defaultProcessor = defaultProcessor;
	}

	public void setTypeProcessor(IProcessor<Object, Object> typeProcessor) {
		this.typeProcessor = typeProcessor;
	}
	
	@Override
	public Object process(Object instance) {
		if (instance != null) {
			Object type = typeProcessor == null ? instance.getClass().getName() : typeProcessor.process(instance);
			IProcessor<Object, Object> processor = null;
			if (type != null) if (mode == null) processor = processors.containsKey(type) ? processors.get(type).getValue() : null;
			else for (Entry<Pattern, IProcessor<Object, Object>> entry : processors.values()) if (entry.getKey().matcher(type.toString()).matches()) if (mode) {
				processor = entry.getValue();
				break;
			} else {
				if (processor != null) try {
					processor.process(instance);
				} catch (Exception e) {
					log.error("process error.", e);
				}
				processor = entry.getValue();
			}
			if (processor == null) processor = defaultProcessor;
			if (processor == null) throw new PlatformException(PlatformError.ERROR_PARAMETERS);
			else return processor.process(instance);
		}
		return null;
	}

	@Override
	public Object perform(Object key, Object instance) {
		if (instance != null) {
			Object type = typeProcessor == null ? key : typeProcessor.process(key);
			IProcessor<Object, Object> processor = null;
			if (type != null) if (mode == null) processor = processors.containsKey(type) ? processors.get(type).getValue() : null;
			else for (Entry<Pattern, IProcessor<Object, Object>> entry : processors.values()) if (entry.getKey().matcher(type.toString()).matches()) if (mode) {
				processor = entry.getValue();
				break;
			} else {
				if (processor != null) try {
					processor.process(instance);
				} catch (Exception e) {
					log.error("process error.", e);
				}
				processor = entry.getValue();
			}
			if (processor == null) processor = defaultProcessor;
			if (processor == null) throw new PlatformException(PlatformError.ERROR_PARAMETERS);
			else return processor.process(instance);
		}
		return null;
	}
}
