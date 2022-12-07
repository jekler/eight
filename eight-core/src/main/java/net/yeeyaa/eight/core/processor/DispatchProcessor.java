package net.yeeyaa.eight.core.processor;

import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;


public class DispatchProcessor implements IProcessor<Object, Object>, IBiProcessor<Object, Object, Object>{//mainly use for dispatching msgs to their corresponding processor by their inner information (typical service dispatch)
	protected IProcessor<String, IProcessor<Object, Object>> dispatcher;
	protected Boolean cache = false;
	protected Pattern pattern; //Pattern.compile("([\\w.$]+)@");
	protected ConcurrentHashMap<String, IProcessor<Object, Object>> processors;
	protected String error;
	protected IProcessor<Object, String> nameProcessor;

	public void setNameProcessor(IProcessor<Object, String> nameProcessor) {
		this.nameProcessor = nameProcessor;
	}

	public void setError(String error) {
		this.error = error;
	}

	public void setDispatcher(IProcessor<String, IProcessor<Object, Object>> dispatcher) {
		this.dispatcher = dispatcher;
	}

	public void setCache(Boolean cache) {
		if(Boolean.TRUE.equals(cache)) {
			processors = new ConcurrentHashMap<String, IProcessor<Object, Object>>();
			this.cache = cache;
		}
	}

	public void setPattern(String pattern) {
		if(pattern != null) this.pattern = Pattern.compile(pattern);
	}

	@Override
	public Object process(Object msg) {
		if(msg != null){
			String name;
			if (nameProcessor == null) name = msg.toString();
			else name = nameProcessor.process(msg);
			if (pattern != null) {
				Matcher matcher = pattern.matcher(name);			
				if(matcher.find()) name = matcher.group(1);
				else name = null;
			}
			if(name != null){
				IProcessor<Object, Object> processor = null;
				if(cache) processor = processors.get(name);
				if(processor == null) processor = dispatcher.process(name);
				if(processor != null){
					msg = processor.process(msg);
					if(cache) processors.put(name, processor);
				}else if(error == null) throw new PlatformException(PlatformError.ERROR_PARAMETERS);
				else return error;
			}else if(error == null) throw new PlatformException(PlatformError.ERROR_PARAMETERS);
			else return error;
		}
		return msg;
	}
	
	@Override
	public Object perform(Object msg, Object value) {
		if(msg != null){
			String name;
			if (nameProcessor == null) name = msg.toString();
			else name = nameProcessor.process(msg);
			if (pattern != null) {
				Matcher matcher = pattern.matcher(name);			
				if(matcher.find()) name = matcher.group(1);
				else name = null;
			}
			if(name != null){
				IProcessor<Object, Object> processor = null;
				if(cache) processor = processors.get(name);
				if(processor == null) processor = dispatcher.process(name);
				if(processor != null){
					value = processor.process(value);
					if(cache) processors.put(name, processor);
				}else if(error == null) throw new PlatformException(PlatformError.ERROR_PARAMETERS);
				else return error;
			}else if(error == null) throw new PlatformException(PlatformError.ERROR_PARAMETERS);
			else return error;
		}
		return value;
	}
}