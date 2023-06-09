package net.yeeyaa.eight.core.storage;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Map;
import java.util.Map.Entry;
import java.util.regex.Pattern;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IExtendable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ProxyGroup<K, V extends IExtendable<Object>> extends Storage<V> implements IProcessor<Object, Collection<V>> {
	protected final Logger log;
	protected IExtendable<Object> group;
	protected Map<Pattern, IProcessor<Collection<V>, Collection<V>>> processors;
	protected IProcessor<Collection<V>, Collection<V>> defaultProcessor;
	protected IProcessor<V, String> typeProcessor;
	protected Boolean set = true;
	
	public ProxyGroup() {
		this.log = LoggerFactory.getLogger(ProxyGroup.class);
	}
	
	public ProxyGroup(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ProxyGroup.class) : log;
	}
	
	public void setGroup(IExtendable<Object> group) {
		this.group = group;
	}

	public void setSet(Boolean set) {
		if (set != null) this.set = set;
	}
	
	public void setProcessors(Map<String, IProcessor<Collection<V>, Collection<V>>> processors) {
		if (processors != null) {
			this.processors = new HashMap<Pattern, IProcessor<Collection<V>, Collection<V>>> (processors.size() * 2);
			for (Entry<String, IProcessor<Collection<V>, Collection<V>>> entry : processors.entrySet()) try {
				this.processors.put(Pattern.compile(entry.getKey()), entry.getValue());
			} catch (Exception e) {
				log.error("ProxySGroup: processor failed.", e);
			}
		}
	}

	public void setDefaultProcessor(IProcessor<Collection<V>, Collection<V>> defaultProcessor) {
		this.defaultProcessor = defaultProcessor;
	}

	public void setTypeProcessor(IProcessor<V, String> typeProcessor) {
		this.typeProcessor = typeProcessor;
	}

	public synchronized Collection<V> list() {
		Collection<V> ret = set ? new HashSet<V>() : new LinkedList<V>();
		HashMap<Pattern, HashSet<V>> map = new HashMap<Pattern, HashSet<V>>();
		HashSet<V> defaultSet = new HashSet<V>();
		Collection<V> storages = group.extend(Method.list);
		for(V storage : storages) {
			String k = null;
			if(typeProcessor == null) {
				Object key = storage.extend(Method.key);
				k = key == null ? null : key.toString();
			} else k = typeProcessor.process(storage);
			boolean flag = true;
			if (processors != null) for (Pattern pattern : processors.keySet()) if (pattern.matcher(k).matches()) {
				HashSet<V> set = map.get(pattern);
				if(set == null) {
					set = new HashSet<V>();
					map.put(pattern, set);
				}
				set.add(storage);
				flag = false;
			}
			if (flag) defaultSet.add(storage);
		}
		for(Entry<Pattern, HashSet<V>> entry : map.entrySet()) try{
			IProcessor<Collection<V>, Collection<V>> processor = processors.get(entry.getKey());
			if (processor == null) defaultSet.addAll(entry.getValue());
			else ret.addAll(processor.process(entry.getValue()));
		} catch(Exception e) {
			log.error("ProxySGroup: processor failed.", e);
		}
		if (defaultProcessor != null && defaultSet.size() > 0) try {
			ret.addAll(defaultProcessor.process(defaultSet));
		} catch(Exception e) {
			log.error("ProxySGroup: processor failed.", e);
		} else ret.addAll(defaultSet);
		return ret;
	}

	public synchronized Long modified() {
		return group.extend(Method.modified);
	}

	@Override
	public Collection<V> process(Object instance) {
		return list();
	}
}
