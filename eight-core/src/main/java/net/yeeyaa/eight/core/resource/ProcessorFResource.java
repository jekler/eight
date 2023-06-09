package net.yeeyaa.eight.core.resource;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IListable;
import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.core.util.PlatformUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ProcessorFResource<K, V> implements IResource<K, V> {
	protected final Logger log;
	protected IProcessor<Object, Object> beanHolder;
	
	public ProcessorFResource() {
		this.log = LoggerFactory.getLogger(ProcessorFResource.class);
	}
	
	public ProcessorFResource(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ProcessorFResource.class) : log;
	}
	
	public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
		this.beanHolder = beanHolder;
	}
	
	@Override
	public V find(K ... paras) {
		if(paras != null && paras.length > 3) {
			Object resource = beanHolder.process(paras[0]);
			Object in = beanHolder.process(paras[1]);
			Object paraConvertor = beanHolder.process(paras[3]);
			if(IInputResource.class.isInstance(resource)) try{
				K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 4); 
				for(int i =0; i < reparas.length; i++) reparas[i] = paras[i+4];
				if(IProcessor.class.isInstance(paraConvertor)) reparas = (K[]) ((IProcessor)paraConvertor).process(new Object[]{"find", reparas});
				V ret = ((IInputResource<K, V>)resource).find(reparas);
				if(IProcessor.class.isInstance(in)) ret = ((IProcessor<V, V>)in).process(ret);
				return ret;
			}catch(Exception e){
				log.error("ProcessorFResource: get resource error.", e);
			}
		}
		return null;
	}

	@Override
	public <P> P store(V value, K ... paras) {
		if(paras != null && paras.length > 3) {
			Object resource = beanHolder.process(paras[0]);
			Object out = beanHolder.process(paras[2]);
			Object paraConvertor = beanHolder.process(paras[3]);
			if(IOutputResource.class.isInstance(resource)) try{
				K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 4); 
				for(int i =0; i < reparas.length; i++) reparas[i] = paras[i+4];
				if(IProcessor.class.isInstance(paraConvertor)) reparas = (K[]) ((IProcessor)paraConvertor).process(new Object[]{"store", reparas});
				if(IProcessor.class.isInstance(out)) value = ((IProcessor<V, V>)out).process(value);
				((IOutputResource) resource).store(value, reparas);
			}catch(Exception e){
				log.error("ProcessorFResource: set resource error.", e);
			}
		}
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		if(paras != null && paras.length > 3) {
			Object resource = beanHolder.process(paras[0]);
			Object paraConvertor = beanHolder.process(paras[3]);
			if(IOutputResource.class.isInstance(resource)) try{
				K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 4); 
				for(int i =0; i < reparas.length; i++) reparas[i] = paras[i+4];
				if(IProcessor.class.isInstance(paraConvertor)) reparas = (K[]) ((IProcessor)paraConvertor).process(new Object[]{"delete", reparas});
				((IOutputResource<K, V>)resource).discard(reparas);
			}catch(Exception e){
				log.error("ProcessorFResource: remove resource error.", e);
			}
		}
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		if(paras != null && paras.length > 3) {
			Object output = beanHolder.process(paras[0]);
			Object paraConvertor = beanHolder.process(paras[3]);
			if(IOutputResource.class.isInstance(output)) try{		
				K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 4); 
				for(int i = 0; i < reparas.length; i++) reparas[i] = paras[i + 4];
				if(IProcessor.class.isInstance(paraConvertor)) reparas = (K[]) ((IProcessor)paraConvertor).process(new Object[]{"empty", reparas});
				((IOutputResource)output).empty(reparas);
			}catch(Exception e){
				log.error("ProcessorFResource: clear resource error.", e);
			}
		}
		return null;
	}

	public static class Listable<K, V> implements IListable<K, V>{
		protected final Logger log;
		protected IProcessor<Object, Object> beanHolder;
		
		public Listable() {
			this.log = LoggerFactory.getLogger(Listable.class);
		}
		
		public Listable(Logger log) {
			this.log = log == null ? LoggerFactory.getLogger(Listable.class) : log;
		}
		
		public void setBeanHolder(IProcessor<Object, Object> beanHolder) {
			this.beanHolder = beanHolder;
		}
		
		@Override
		public Collection<K[]> keys(K... paras) {
			if(paras != null && paras.length > 3) {
				Object listable = beanHolder.process(paras[0]);
				Object paraConvertor = beanHolder.process(paras[3]);
				if(IListable.class.isInstance(listable)) try{	
					K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 4); 
					for(int i = 0; i < reparas.length; i++) reparas[i] = paras[i + 4];
					if(IProcessor.class.isInstance(paraConvertor)) reparas = (K[]) ((IProcessor)paraConvertor).process(new Object[]{"keys", reparas});
					Collection<K[]> ls = ((IListable<K, V>)listable).keys(reparas);
					Collection<K[]> ret = new ArrayList<K[]>(ls.size());
					for(K[] s : ls) {
						ArrayList<K> a = new ArrayList<K>(s.length + 4);
						for(int i = 0 ; i < 4; i++) a.add(paras[i]);
						for(K key : s) a.add(key);
						ret.add(a.toArray(PlatformUtil.newArrayOf(paras, a.size())));
					}
					return ret;
				}catch(Exception e){
					log.error("ProcessorFResource: list resource error.", e);
				}
			}
			return null;
		}

		@Override
		public Map<K[], V> all(K... paras) {
			if(paras != null && paras.length > 3) {
				Object listable = beanHolder.process(paras[0]);
				Object paraConvertor = beanHolder.process(paras[3]);
				if(IListable.class.isInstance(listable)) try{	
					K[] reparas = PlatformUtil.newArrayOf(paras, paras.length - 4); 
					for(int i = 0; i < reparas.length; i++) reparas[i] = paras[i + 4];
					if(IProcessor.class.isInstance(paraConvertor)) reparas = (K[]) ((IProcessor)paraConvertor).process(new Object[]{"all", reparas});
					Map<K[], V> map = ((IListable<K, V>)listable).all(reparas);
					Map<K[], V> ret = new HashMap<K[], V>(map.size());
					for(Entry<K[], V> s : map.entrySet()) {
						ArrayList<K> a = new ArrayList<K>(s.getKey().length + 4);
						for(int i = 0 ; i < 4; i++) a.add(paras[i]);
						for(K key : s.getKey()) a.add(key);
						ret.put(a.toArray(PlatformUtil.newArrayOf(paras, a.size())), s.getValue());
					}
					return ret;
				}catch(Exception e){
					log.error("ProcessorFResource: list all error.", e);
				}
			}
			return null;
		}
	}
}
