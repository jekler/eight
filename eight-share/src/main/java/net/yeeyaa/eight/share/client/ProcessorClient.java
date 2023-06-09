package net.yeeyaa.eight.share.client;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.share.ShareError;


public class ProcessorClient<K, V, R> implements IBiProcessor<K, V, R>, IProcessor<K, IProcessor<V, R>> {
	protected IInputResource<K, IProcessor<V, R>> service;
	protected volatile ConcurrentHashMap<K, IProcessor<V, R>> resource = new ConcurrentHashMap<K, IProcessor<V, R>>();	
	protected Boolean order;
	
	public void setOrder(Boolean order) {
		this.order = order;
	}
	
	public void setService(IInputResource<K, IProcessor<V, R>> service) {
		this.service = service;
	}

	protected ConcurrentHashMap<K, IProcessor<V, R>> getResource() {
		return resource;
	}

	public class ProcessorResource implements IListableResource<K, IProcessor<V, R>>{
		protected Boolean overlap = false;
		
		public void setOverlap(Boolean overlap) {
			if(overlap != null) this.overlap = overlap;
		}

		@Override
		public IProcessor<V, R> find(K ... paras) {
			if(paras != null && paras.length > 0) return getResource().get(paras[0]);
			else return null;
		}
		
		@Override
		public <P> P store(IProcessor<V, R> value, K... paras) {
			if(paras != null && value != null && paras.length > 0 && (overlap || !getResource().containsKey(paras[0]))) getResource().put(paras[0], value);
			return null;
		}

		@Override
		public <P> P discard(K... paras) {
			if(paras != null && paras.length > 0) getResource().remove(paras[0]);
			return null;
		}

		@Override
		public <P> P empty(K... paras) {
			getResource().clear();
			return null;
		}
		
		@Override
		public Collection<K[]> keys(K... paras) {
			Collection<K> c = getResource().keySet();
			Collection<K[]> ls = new ArrayList<K[]>(c.size());
			for(K o : c) {
				K[] key = o == null ? (K[])new Object[1] : (K[]) Array.newInstance(o.getClass(), 1);
				key[0] = o;
				ls.add(key);
			}
			return ls;
		}

		@Override
		public Map<K[], IProcessor<V, R>> all(K... paras) {
			Map<K[], IProcessor<V, R>> copy = new HashMap<K[], IProcessor<V, R>>(getResource().size() * 2);
			for(Entry<K, IProcessor<V, R>> entry : getResource().entrySet()) {
				K o = entry.getKey();
				K[] key = o == null ? (K[])new Object[1] : (K[]) Array.newInstance(o.getClass(), 1);
				key[0] = o;
				copy.put(key, entry.getValue());
			}
			return copy;
		}
	}
	
	public class ServiceProcessor implements IProcessor<IInputResource<K, IProcessor<V, R>>, IInputResource<K, IProcessor<V, R>>>{
		@Override
		public IInputResource<K, IProcessor<V, R>> process(IInputResource<K, IProcessor<V, R>> instance) {
			setService(instance);
			return instance;
		}		
	}
	
	@Override
	public R perform(K name, V content) {
		IProcessor<V, R> sw = null;
		if (order == null) {
			if (service != null) sw = service.find(name);
		} else if (order) {
			if (service != null) sw = service.find(name);
			if (sw == null) sw = resource.get(name);
		} else {
			sw = resource.get(name);
			if(sw == null && service != null) sw = service.find(name);
		}
		if(sw == null) throw new PlatformException(ShareError.NO_SUCH_SERVICE);
		else return sw.process(content);
	}

	@Override
	public IProcessor<V, R> process(K instance) {
		if (instance == null) return null;
		else {
			IProcessor<V, R> sw = null;
			if (order == null) {
				if (service != null) sw = service.find(instance);
			} else if (order) {
				if (service != null) sw = service.find(instance);
				if (sw == null) sw = resource.get(instance.toString());
			} else {
				sw = resource.get(instance.toString());
				if(sw == null && service != null) sw = service.find(instance);
			}
			return sw;
		}
	}
}
