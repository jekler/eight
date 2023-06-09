package net.yeeyaa.eight.core.processor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.storage.Storage.Method;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class StorageTypeProcessor implements IProcessor<Object, Map<String, Object>> {
	protected final Logger log;
	protected Map<Pattern, IProcessor<Collection<IExtendable<Object>>, Object>> processors;
	protected IProcessor<Collection<IExtendable<Object>>, Object> defaultProcessor;
	protected IProcessor<IExtendable<Object>, String> typeProcessor;
	protected long wait = 0;
	
	public StorageTypeProcessor() {
		this.log = LoggerFactory.getLogger(StorageTypeProcessor.class);
	}
	
	public StorageTypeProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(StorageTypeProcessor.class) : log;
	}
	
	public void setWait(long wait) {
		if (wait > 0) this.wait = wait;
	}

	public void setProcessors(Map<String, IProcessor<Collection<IExtendable<Object>>, Object>> processors) {
		if (processors != null) {
			this.processors = new LinkedHashMap<Pattern, IProcessor<Collection<IExtendable<Object>>, Object>> (processors.size() * 2);
			for (Entry<String, IProcessor<Collection<IExtendable<Object>>, Object>> entry : processors.entrySet()) try {
				this.processors.put(Pattern.compile(entry.getKey()), entry.getValue());
			} catch (Exception e) {
				log.error("StorageProcessor: processor failed.", e);
			}
		}
	}

	public void setDefaultProcessor(IProcessor<Collection<IExtendable<Object>>, Object> defaultProcessor) {
		this.defaultProcessor = defaultProcessor;
	}

	public void setTypeProcessor(IProcessor<IExtendable<Object>, String> typeProcessor) {
		this.typeProcessor = typeProcessor;
	}

	@Override
	public Map<String, Object> process(Object instance) {
		if(instance != null) {
			Collection<IExtendable<Object>> storages = new ArrayList<IExtendable<Object>>();
			if(instance instanceof Collection) storages = (Collection<IExtendable<Object>>) instance;
			else if(instance instanceof IExtendable) storages.add((IExtendable<Object>)instance);
			LinkedHashMap<Pattern, HashSet<IExtendable<Object>>> map = new LinkedHashMap<Pattern, HashSet<IExtendable<Object>>>();
			if (processors != null) for (Pattern pattern : processors.keySet()) map.put(pattern, new HashSet<IExtendable<Object>>());
			HashSet<IExtendable<Object>> defaultSet = new HashSet<IExtendable<Object>>();
			for(IExtendable<Object> storage : storages) {
				String k = null;
				if(typeProcessor == null) {
					Object key = storage.extend(Method.key);
					k = key == null ? null : key.toString();
				} else k = typeProcessor.process(storage);
				boolean flag = true;
				if (processors != null) for (Pattern pattern : processors.keySet()) if (pattern.matcher(k).matches()) {
					HashSet<IExtendable<Object>> set = map.get(pattern);
					set.add(storage);
					flag = false;
				}
				if (flag) defaultSet.add(storage);
			}
			HashMap<String, Object> ret = new HashMap<String, Object>(map.size() * 2);
			for(Entry<Pattern, HashSet<IExtendable<Object>>> entry : map.entrySet()) if(entry.getValue().size() > 0) try{
				IProcessor<Collection<IExtendable<Object>>, Object> processor = processors.get(entry.getKey());
				if (processor == null) defaultSet.addAll(entry.getValue());
				else {
					ret.put(entry.getKey().toString(), processor.process(entry.getValue()));
					if (wait > 0) Thread.sleep(wait);
				}
			} catch(Exception e) {
				log.error("StorageProcessor: processor failed.", e);
			}
			if (defaultProcessor != null && defaultSet.size() > 0) try {
				ret.put(null, defaultProcessor.process(defaultSet));
			} catch(Exception e) {
				log.error("StorageProcessor: processor failed.", e);
			}
			return ret;
		}else return null;
	}
}
