package net.yeeyaa.eight.core.storage;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class WatchGroup<K, V extends IExtendable<Object>> extends Storage<V> implements IProcessor<Object, Collection<V>>, IListableResource<K, V> {
	protected final Logger log;
	protected IReadonlyListable<K, V> resource;
	protected K[] key;
	protected Integer mode = 0;
	protected volatile Long lastModified = -1L;
	protected volatile ConcurrentHashMap<Object, V> storages = new ConcurrentHashMap<Object, V>();
	protected Boolean delete = false;
	protected Boolean prefetch = false;
	protected IProcessor<K[], K[]> keyProcessor;
	
	public WatchGroup() {
		this.log = LoggerFactory.getLogger(WatchGroup.class);
	}
	
	public WatchGroup(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(WatchGroup.class) : log;
	}
	
	public void setKeyProcessor(IProcessor<K[], K[]> keyProcessor) {
		this.keyProcessor = keyProcessor;
	}

	public void setPrefetch(Boolean prefetch) {
		if (prefetch != null) this.prefetch = prefetch;
	}

	public void setMode(Integer mode) {
		if(mode != null) this.mode = mode;
	}

	public void setDelete(Boolean delete) {
		if(delete != null) this.delete = delete;
	}

	public void setResource(IReadonlyListable<K, V> resource) {
		this.resource = resource;
	}

	public void setKey(K[] key) {
		this.key = key;
	}

	public Collection<V> list() {
		return list(key);
	}
	
	public Collection<V> list(K[] key) {
		Map<K[], V> map = null;
		Collection<K[]> paras = null;
		if (prefetch) {
			map = key == null ? resource.all() : resource.all(key);
			paras = map.keySet();
		} else paras = key == null ? resource.keys() : resource.keys(key);
		ConcurrentHashMap<Object, V> ss = new ConcurrentHashMap<Object, V>();
		Set<V> news = new HashSet<V>();
		Set<V> delset = new HashSet<V>();
		if(delete) delset.addAll(storages.values());
		if(paras != null) synchronized(this){	
			for(K[] k : paras) {
				Object ls = keyProcessor == null ? Arrays.asList(k) : keyProcessor.process(k);
				if(storages.containsKey(ls)) {
					V oldone = storages.get(ls);
					ss.put(ls, map == null ? oldone : map.get(k));
					if(delete) delset.remove(oldone);
				}else {
					V newone = map == null ? resource.find(k) : map.get(k);
					news.add(newone);
					ss.put(ls, newone);
					storages.put(ls, newone);
				}
			}
		}
		if(mode != 3) storages = ss; 
		switch(mode) {
			case 1: synchronized(this){ 
				Long modified = lastModified;		
				for (V r : storages.values()) try{
					Long thisModified = r.extend(Method.modified);
					if(thisModified > lastModified){
						news.add(r);
						if(thisModified > modified) modified = thisModified;
					}
				}catch(Exception e){
					log.error("WatchGroup: processor failed.", e);
				}
				lastModified = modified;
				break;
			} default : { 
				Long modified = lastModified;
				if(mode == 0) news.addAll(storages.values());
				for (IExtendable<Object> r : news) try{
					Long thisModified = r.extend(Method.modified);
					if(thisModified > modified) modified = thisModified;
				}catch(Exception e){
					log.error("WatchGroup: processor failed.", e);
				}
				lastModified = modified;
			}
		}
		if(delset.size() > 0) for(V d : delset){
			ByteWStorage storage = new ByteWStorage();
			storage.setName(d.extend(Method.key));
			storage.setExist(false);
			news.add((V)storage);
		}
		return news;
	}

	public Long modified() {
		return modified(key);
	}
	
	public Long modified(K[] key) {
		Long modified = lastModified;
		Map<K[], V> map = null;
		Collection<K[]> paras = null;
		if (prefetch) {
			map = key == null ? resource.all() : resource.all(key);
			paras = map.keySet();
		} else paras = key == null ? resource.keys() : resource.keys(key);
		List<V> news = new ArrayList<V>(paras.size());
		if(paras != null){	
			for(K[] k : paras) {
				Object ls = keyProcessor == null ? Arrays.asList(k) : keyProcessor.process(k);
				if(!storages.containsKey(ls)) news.add(map == null ? resource.find(k) : map.get(k));
				else if(mode != 2 && mode != 3) news.add(storages.get(ls));
			}
			for (IExtendable<Object> r : news) try{
				Long thisModified = r.extend(Method.modified);
				if(thisModified > modified) modified = thisModified;
			}catch(Exception e){
				log.error("WatchGroup: processor failed.", e);
			}
		}
		return modified;
	}

	@Override
	public Collection<V> process(Object instance) {
		return instance != null && instance instanceof Object[] ? list((K[]) instance) : list();
	}

	@Override
	public <P> P store(V value, K... paras) {
		return (P) storages.put(keyProcessor == null ? Arrays.asList(paras) : keyProcessor.process(paras), value);
	}

	@Override
	public <P> P discard(K... paras) {
		return (P) storages.remove(keyProcessor == null ? Arrays.asList(paras) : keyProcessor.process(paras));
	}

	@Override
	public <P> P empty(K... paras) {
		storages.clear();
		return null;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		ArrayList<K[]> ret = new ArrayList<K[]>(storages.size());
		Enumeration<Object> keys = storages.keys();
		while (keys.hasMoreElements()) {
			Object key = keys.nextElement();
			if (key instanceof Object[]) ret.add((K[])key);
			else if (key instanceof Collection) ret.add((K[])((Collection<Object>)key).toArray());
			else ret.add((K[])new Object[]{key});
		}
		return ret;
	}

	@Override
	public Map<K[], V> all(K... paras) {
		HashMap<K[], V> ret = new HashMap<K[], V>(storages.size() * 2);
		for (Entry<Object, V> entry : storages.entrySet()) {
			Object key = entry.getKey();
			if (key instanceof Object[]) ret.put((K[])key, entry.getValue());
			else if (key instanceof Collection) ret.put((K[])((Collection<Object>)key).toArray(), entry.getValue());
			else ret.put((K[])new Object[]{key}, entry.getValue());
		}
		return ret;
	}

	@Override
	public V find(K... paras) {
		return storages.get(Arrays.asList(paras));
	}
}
