package net.yeeyaa.eight.core.resource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IListable;
import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.core.util.PlatformUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class BatchFResource<K> implements IResource<K, Object> {
	protected static final MapperSet<Object> resourceMethods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final Logger log;
	protected IProcessor<Object, Object> beanHolder;
	
	public BatchFResource() {
		this.log = LoggerFactory.getLogger(BatchFResource.class);
	}
	
	public BatchFResource(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(BatchFResource.class) : log;
	}
	
	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}
	
	@Override
	public Object find(K ... paras) {
		if(paras != null && paras.length > 2) try{
			Integer prefixLength = (Integer)paras[1];
			Object resource = beanHolder.process(paras[0]);
			if(prefixLength >= 0 && paras.length > prefixLength + 2 && IInputResource.class.isInstance(resource)) {
				K[] reparas = PlatformUtil.newArrayOf(paras, prefixLength + 1);
				Object[] results = new Object[paras.length - prefixLength - 2];
				for(int i = 2; i < prefixLength + 2; i++) reparas[i] = paras[i + 2];
				for(int i = prefixLength + 2; i < paras.length; i++) try{
					reparas[prefixLength] = paras[i];
					results[i - prefixLength - 2] = ((IInputResource)resource).find(reparas);
				}catch(Exception e){
					log.error("BatchFResource: get resource error.", e);
				}
				return new ArrayList<Object>(Arrays.asList(results));
			}
		} catch(Exception e){
			log.error("BatchFResource: get resource error.", e);
		}
		return null;
	}

	@Override
	public <P> P store(Object value, K ... paras) {
		if(paras != null && paras.length > 1) {
			Integer prefixLength = (Integer)paras[1];
			Object resource = beanHolder.process(paras[0]);
			if(prefixLength >= 0 && IOutputResource.class.isInstance(resource)) if(paras.length == prefixLength + 2 && Map.class.isInstance(value)) {
				K[] reparas = PlatformUtil.newArrayOf(paras, prefixLength + 1);
				for(int i = 2; i < prefixLength + 2; i++) reparas[i] = paras[i + 2];
				for(Entry<K, Object> entry: ((Map<K, Object>)value).entrySet()) try{
					reparas[prefixLength] = entry.getKey();
					((IOutputResource) resource).store(entry.getValue(), reparas);
				}catch(Exception e){
					log.error("BatchFResource: set resource error.", e);
				}
			}else if(paras.length > prefixLength + 2) try{
				K[] reparas = PlatformUtil.newArrayOf(paras, prefixLength + 1);
				for(int i = 0; i < reparas.length; i++) reparas[i] = paras[i + 2];
				((IOutputResource) resource).store(value, reparas);
			}catch(Exception e){
				log.error("BatchFResource: set resource error.", e);
			}
		}
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		if(paras != null && paras.length > 2) try{
			Integer prefixLength = (Integer)paras[1];
			Object resource = beanHolder.process(paras[0]);
			if(prefixLength >= 0 && paras.length > prefixLength + 2 && IOutputResource.class.isInstance(resource)) {
				K[] reparas = PlatformUtil.newArrayOf(paras, prefixLength + 1);
				for(int i = 2; i < prefixLength + 2; i++) reparas[i] = paras[i];
				for(int i = prefixLength + 2; i < paras.length; i++) try{
					reparas[prefixLength] = paras[i];
					((IOutputResource)resource).discard(reparas);
				}catch(Exception e){
					log.error("BatchFResource: remove resource error.", e);
				}
			}
		} catch(Exception e){
			log.error("BatchFResource: remove resource error.", e);
		}
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		if(paras != null && paras.length > 1) {
			Integer prefixLength = (Integer)paras[1];
			Object output = beanHolder.process(paras[0]);
			if(paras.length >= prefixLength + 2 && IOutputResource.class.isInstance(output)) try{	
				K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 2);
				for(int i = 0; i < reparas.length; i++) reparas[i] = paras[i + 2];
				((IOutputResource)output).empty(reparas);
			}catch(Exception e){
				log.error("BatchFResource: clear resource error.", e);
			}
		}
		return null;
	}

	public static class Listable<K, V> implements IListable<K, V>, IExtendable<Object>{
		protected final Logger log;
		protected IProcessor<Object, Object> beanHolder;
		
		public Listable() {
			this.log = LoggerFactory.getLogger(Listable.class);
		}
		
		public Listable(Logger log) {
			this.log = log == null ? LoggerFactory.getLogger(Listable.class) : log;
		}
		
		protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
			@Override
			public Object process(K[] paras) {
				if(paras != null && paras.length > 1) {
					Integer prefixLength = (Integer)paras[1];
					Object listable = beanHolder.process(paras[0]);
					if(paras.length >= prefixLength + 2 && listable instanceof IExtendable) try{
						IProcessor process = ((IExtendable<Object>)listable).extend(ResourceMethod.count);
						if (process != null) {
							K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 2);
							for(int i = 0; i < reparas.length; i++) reparas[i] = paras[i + 2];
							return process.process(reparas);
						}
					}catch(Exception e){
						log.error("BatchFResource: count resource error.", e);
					}
				}
				return null;
			}
		};
		
		public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
			this.beanHolder = beanHolder;
		}
		
		@Override
		public Collection<K[]> keys(K... paras) {
			if(paras != null && paras.length > 1) {
				Integer prefixLength = (Integer)paras[1];
				Object listable = beanHolder.process(paras[0]);
				if(paras.length >= prefixLength + 2 && IListable.class.isInstance(listable)) try{	
					K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 2);
					for(int i = 0; i < reparas.length; i++) reparas[i] = paras[i + 2];
					Collection<K[]> ls = ((IListable)listable).keys(reparas);
					Collection<K[]> ret = new ArrayList<K[]>(ls.size());
					for(K[] s : ls) {
						ArrayList<K> a = new ArrayList<K>(s.length + 2);
						for(int i = 0 ; i < 2; i++) a.add(paras[i]);
						for(K key : s) a.add(key);
						ret.add(a.toArray(PlatformUtil.newArrayOf(reparas, a.size())));
					}
					return ret;
				}catch(Exception e){
					log.error("BatchFResource: list resource error.", e);
				}
			}
			return null;
		}

		@Override
		public Map<K[], V> all(K... paras) {
			if(paras != null && paras.length > 1) {
				Integer prefixLength = (Integer)paras[1];
				Object listable = beanHolder.process(paras[0]);
				if(paras.length >= prefixLength + 2 && IListable.class.isInstance(listable)) try{	
					K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 2);
					for(int i = 0; i < reparas.length; i++) reparas[i] = paras[i + 2];
					Map<K[], V> map = ((IListable)listable).all(reparas);
					Map<K[], V> ret = new HashMap<K[], V>(map.size());
					for(Entry<K[], V> s : map.entrySet()) {
						ArrayList<K> a = new ArrayList<K>(s.getKey().length + 2);
						for(int i = 0 ; i < 2; i++) a.add(paras[i]);
						for(K key : s.getKey()) a.add(key);
						ret.put(a.toArray(PlatformUtil.newArrayOf(reparas, a.size())), s.getValue());
					}
					return ret;
				}catch(Exception e){
					log.error("BatchFResource: list all error.", e);
				}
			}
			return null;
		}
		
		@Override
		public <N> N extend(Object object) {
			if (object != null) {
				Object method = object instanceof ResourceMethod ? object : resourceMethods.process(object);
				if (method!= null) switch((ResourceMethod) method) {
					case count : return (N) count;
				}
			}
			return null;
		}
		
		@Override
		public Collection<Object> methods() {
			return resourceMethods;
		}
	}
}
