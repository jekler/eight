package net.yeeyaa.eight.core.storage;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;
import net.yeeyaa.eight.IExtendable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ParentGroup<K, V extends IExtendable<Object>> extends Storage<V> implements IProcessor<Object, Collection<V>> {
	protected final Logger log;
	protected volatile Long lastModified = -1L;
	protected volatile HashMap<List<K>, IExtendable<Object>> groups = new HashMap<List<K>, IExtendable<Object>>();
	protected IReadonlyListable<K, IExtendable<Object>> resource;
	protected K[] key;
	protected IProcessor<Map<String, Object>, V> convertor;
	protected Boolean delete = true;
	protected Boolean set = true; 
	protected Boolean prefetch = false;
	protected IProcessor<K[], K[]> keyProcessor;
	
	public ParentGroup() {
		this.log = LoggerFactory.getLogger(ParentGroup.class);
	}
	
	public ParentGroup(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ParentGroup.class) : log;
	}
	
	public void setPrefetch(Boolean prefetch) {
		if (prefetch != null) this.prefetch = prefetch;
	}
	
	public void setDelete(Boolean delete) {
		if(delete != null) this.delete = delete;
	}

	public void setConvertor(IProcessor<Map<String, Object>, V> convertor) {
		this.convertor = convertor;
	}

	public void setResource(IReadonlyListable<K, IExtendable<Object>> resource) {
		this.resource = resource;
	}

	public void setSet(Boolean set) {
		if (set != null) this.set = set;
	}

	public void setKey(K[] key) {
		this.key = key;
	}
	
	public void setKeyProcessor(IProcessor<K[], K[]> keyProcessor) {
		this.keyProcessor = keyProcessor;
	}
	
	public Collection<V> list() {
		return list(key);
	}

	public synchronized Collection<V> list(K[] ks) {
		ks = keyProcessor == null ? ks : keyProcessor.process(ks);
		Map<K[], IExtendable<Object>> all = null;
		Collection<K[]> paras = null;
		if (prefetch) {
			all = ks == null ? resource.all() : resource.all(ks);
			paras = all.keySet();
		} else paras = ks == null ? resource.keys() : resource.keys(ks);
		Collection<V> ret = set ? new HashSet<V>() : new LinkedList<V>();
		HashMap<List<K>, IExtendable<Object>> tmp = new HashMap<List<K>, IExtendable<Object>>();
		for (K[] k : paras) try {
			List<K> key = Arrays.asList(k);
			IExtendable<Object> group = groups.remove(key);
			if (group == null) group = all == null ? resource.find(k) : all.get(k);
			tmp.put(key, group);
			if (convertor == null) ret.addAll(group.<Collection<V>>extend(Method.list));
			else for (IExtendable<Object> storage : group.<Collection<IExtendable<Object>>>extend(Method.list)) {
				Map<String, Object> map = new HashMap<String, Object>();
				map.put("key", key);
				map.put("storage", storage);
				V s = convertor.process(map);
				if (s != null) ret.add(s);
			}
		} catch (Exception e) {
			log.error("ParentGroup: list failed: ", e);
		}
		if (delete) for (Entry<List<K>, IExtendable<Object>> entry : groups.entrySet()) try {
			if (convertor == null) ret.addAll(entry.getValue().<Collection<V>>extend(Method.list));
			else for (IExtendable<Object> storage : entry.getValue().<Collection<IExtendable<Object>>>extend(Method.list)) {
				Map<String, Object> map = new HashMap<String, Object>();
				map.put("key", entry.getKey());
				map.put("storage", storage);
				V s = convertor.process(map);
				if (s != null) ret.add(s);
			}
		} catch (Exception e) {
			log.error("ParentGroup: list failed: ", e);
		}
		groups = tmp;
		return ret;
	}

	public Long modified() {
		return modified(key);
	}
	
	public synchronized Long modified(K[] ks) {
		ks = keyProcessor == null ? ks : keyProcessor.process(ks);
		Long modified = lastModified;
		Map<K[], IExtendable<Object>> all = null;
		Collection<K[]> paras = null;
		if (prefetch) {
			all = ks == null ? resource.all() : resource.all(ks);
			paras = all.keySet();
		} else paras = ks == null ? resource.keys() : resource.keys(ks);
		HashMap<List<K>, IExtendable<Object>> tmp = new HashMap<List<K>, IExtendable<Object>>(groups);
		for (K[] k : paras) try {
			IExtendable<Object> group = tmp.remove(Arrays.asList(k));
			if (group == null) {
				group = all == null ? resource.find(k) : all.get(k);
				groups.put(Arrays.asList(k), group);
			}
			Long thisModified = group.extend(Method.modified);
			if(thisModified > modified) modified = thisModified;
		} catch (Exception e) {
			log.error("ParentGroup: groupModified failed: ", e);
		}
		if (delete) for (IExtendable<Object> group : tmp.values()) try {
			Long thisModified = group.extend(Method.modified);
			if(thisModified > modified) modified = thisModified;
		} catch (Exception e) {
			log.error("ParentGroup: groupModified failed: ", e);
		}
		lastModified = modified;
		return modified;
	}

	@Override
	public Collection<V> process(Object instance) {
		return instance != null && instance instanceof Object[] ? list((K[]) instance) : list();
	}
	
	public static class NameProcessor implements IProcessor<IExtendable<Object>, Object[]> {
		protected Object prefix;
		protected Object suffix;
		
		public void setPrefix(Object prefix) {
			this.prefix = prefix;
		}

		public void setSuffix(Object suffix) {
			this.suffix = suffix;
		}

		@Override
		public Object[] process(IExtendable<Object> instance) {
			LinkedList<Object> list = new LinkedList<Object>();
			if (prefix != null) list.add(prefix);
			if (instance != null) list.add(instance.extend(Method.key));
			if (suffix != null) list.add(suffix);
			return list.toArray();
		}
	}
}
