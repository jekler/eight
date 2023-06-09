package net.yeeyaa.eight.core.processor;

import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;


public class DispatchProcessor implements IProcessor<Object, Object>, IBiProcessor<Object, Object, Object>{
	protected IProcessor<String, IProcessor<Object, Object>> dispatcher;
	protected Boolean cache = false;
	protected Pattern pattern; 
	protected ConcurrentHashMap<String, IProcessor<Object, Object>> processors;
	protected Object error;
	protected IProcessor<Object, String> nameProcessor;
	protected Set<String> allow;
	protected Set<String> forbid;
	protected Set<String> allowFull;
	protected Set<String> forbidFull;

	public void setAllowFull(String allowFull) {
		if (allowFull != null) {
			String[] as = allowFull.split("\\|");
			if (as.length > 0) {
				HashSet<String> set = new HashSet<String>();
				for (String a : as) set.add(a);
				this.allowFull = set;
			}
		}
	}

	public void setForbidFull(String forbidFull) {
		if (forbidFull != null) {
			String[] fs = forbidFull.split("\\|");
			if (fs.length > 0) {
				HashSet<String> set = new HashSet<String>();
				for (String f : fs) set.add(f);
				this.forbidFull = set;
			}
		}
	}
	
	public void setAllow(String allow) {
		if (allow != null) {
			String[] as = allow.split("\\|");
			if (as.length > 0) {
				HashSet<String> set = new HashSet<String>();
				for (String a : as) set.add(a);
				this.allow = set;
			}
		}
	}

	public void setForbid(String forbid) {
		if (forbid != null) {
			String[] fs = forbid.split("\\|");
			if (fs.length > 0) {
				HashSet<String> set = new HashSet<String>();
				for (String f : fs) set.add(f);
				this.forbid = set;
			}
		}
	}
	
	public void setNameProcessor(IProcessor<Object, String> nameProcessor) {
		this.nameProcessor = nameProcessor;
	}

	public void setError(Object error) {
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
			String fullname = name;
			if (pattern != null) {
				Matcher matcher = pattern.matcher(name);			
				if(matcher.find()) {
					name = matcher.group(1);
					fullname = matcher.group();
				} else name = null;
			}
			if(name != null){
				IProcessor<Object, Object> processor = null;
				if(cache) processor = processors.get(name);
				if(processor == null) processor = dispatcher.process(name);
				if(processor != null){
					if(allow != null && !allow.contains(name) || forbid != null && forbid.contains(name) || 
							allowFull != null && !allowFull.contains(fullname) || forbidFull != null && forbidFull.contains(fullname))
						if(error == null) throw new PlatformException(PlatformError.ERROR_DATA_ACCESS);
						else return error;
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
			String fullname = name;
			if (pattern != null) {
				Matcher matcher = pattern.matcher(name);			
				if(matcher.find()) {
					name = matcher.group(1);
					fullname = matcher.group();
				} else name = null;
			}
			if(name != null){
				if(allow != null && !allow.contains(name) || forbid != null && forbid.contains(name) || 
						allowFull != null && !allowFull.contains(fullname) || forbidFull != null && forbidFull.contains(fullname))
					if(error == null) throw new PlatformException(PlatformError.ERROR_DATA_ACCESS);
					else return error;
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
