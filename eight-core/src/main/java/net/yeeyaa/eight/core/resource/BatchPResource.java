package net.yeeyaa.eight.core.resource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IListable;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.core.util.PlatformUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class BatchPResource<K, U extends IResource<K, Object>> extends ProxyResource<U> implements IResource<K, Object> {
	protected final Logger log;
	protected Integer prefixLength = 0;

	public BatchPResource() {
		this.log = LoggerFactory.getLogger(BatchPResource.class);
	}
	
	public BatchPResource(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(BatchPResource.class) : log;
	}
	
	public void setPrefixLength(Integer prefixLength) {
		if(prefixLength != null && prefixLength > 0) this.prefixLength = prefixLength;
	}

	@Override
	public Object find(K ... paras) {
		if(paras != null && paras.length > prefixLength && resource != null) {
			K[] reparas = PlatformUtil.newArrayOf(paras, prefixLength + 1);
			Object[] results = new Object[paras.length - prefixLength];
			for(int i = 0; i < prefixLength; i++) reparas[i] = paras[i];
			for(int i = prefixLength; i < paras.length; i++) try{
				reparas[prefixLength] = paras[i];
				results[i - prefixLength] = resource.find(reparas);
			}catch(Exception e){
				log.error("BatchPResource: get resource error.", e);
			}
			return new ArrayList<Object>(Arrays.asList(results));
		}
		return null;
	}

	@Override
	public <P> P store(Object value, K ... paras) {
		if(paras != null && paras.length >= prefixLength && resource != null) if(paras.length == prefixLength && Map.class.isInstance(value)) {
			K[] reparas = PlatformUtil.newArrayOf(paras, prefixLength + 1);
			for(int i = 0; i < prefixLength; i++) reparas[i] = paras[i];
			for(Entry<K, Object> entry: ((Map<K, Object>)value).entrySet()) try{
				reparas[prefixLength] = entry.getKey();
				resource.store(entry.getValue(), reparas);
			}catch(Exception e){
				log.error("BatchPResource: set resource error.", e);
			}
		}else if(paras.length > prefixLength) try{
			if(paras.length == prefixLength + 1) resource.store(value, paras);
			else {
				K[] reparas = PlatformUtil.newArrayOf(paras, prefixLength + 1);
				for(int i = 0; i < reparas.length; i++) reparas[i] = paras[i];
				resource.store(value, reparas);
			}
		}catch(Exception e){
			log.error("BatchPResource: set resource error.", e);
		}
		return null;
	}

	@Override
	public <P> P discard(K ... paras) {
		if(paras != null && paras.length > prefixLength && resource != null) {
			K[] reparas = PlatformUtil.newArrayOf(paras, prefixLength + 1);
			for(int i = 0; i < prefixLength; i++) reparas[i] = paras[i];
			for(int i = prefixLength; i < paras.length; i++) try{
				reparas[prefixLength] = paras[i];
				resource.discard(reparas);
			}catch(Exception e){
				log.error("BatchPResource: remove resource error.", e);
			}
		}
		return null;
	}

	@Override
	public <P> P empty(K... paras) {
		if(paras != null && paras.length >= prefixLength && resource != null) try{
			resource.empty(paras);
		}catch(Exception e){
			log.error("BatchPResource: clear resource error.", e);
		}
		return null;
	}
	
	public static class Listable<K, V, U extends IListable<K, V>> extends ProxyResource<U> implements IListable<K, V>{
		protected final Logger log;
		protected Integer prefixLength = 0;

		public Listable() {
			this.log = LoggerFactory.getLogger(Listable.class);
		}
		
		public Listable(Logger log) {
			this.log = log == null ? LoggerFactory.getLogger(Listable.class) : log;
		}
		
		public void setPrefixLength(Integer prefixLength) {
			if(prefixLength != null && prefixLength > 0) this.prefixLength = prefixLength;
		}
		
		@Override
		public Collection<K[]> keys(K... paras) {
			if(paras != null && paras.length >= prefixLength && resource != null) try{
				return resource.keys(paras);
			}catch(Exception e){
				log.error("BatchPResource: list resource error.", e);
			}
			return null;
		}

		@Override
		public Map<K[], V> all(K... paras) {
			if(paras != null && paras.length >= prefixLength && resource != null) try{
				return resource.all(paras);
			}catch(Exception e){
				log.error("BatchPResource: list all error.", e);
			}
			return null;
		}
	}
}
