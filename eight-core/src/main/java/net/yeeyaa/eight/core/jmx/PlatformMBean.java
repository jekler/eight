package net.yeeyaa.eight.core.jmx;

import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.ITransactionResource;
import net.yeeyaa.eight.core.util.TypeConvertor;


public class PlatformMBean{
	protected ITransactionResource<Object, Object, IResource<Object, Object>, Object> proxyResource;
	protected Object[] proxyParas;
	protected ConcurrentHashMap<String, Object> configMap = new ConcurrentHashMap<String, Object>();

	public class Get implements IProcessor<String, Object>{
		protected Integer type;
		
		public void setType(Integer type) {
			this.type = type;
		}
		
		@Override
		public Object process(String key) {
			if(type == null) return get(key);
			else return get(key, type);
		}
	}
	
	public class MBeanResource implements IResource<Object, Object>{
		protected Integer type;
		
		public void setType(Integer type) {
			this.type = type;
		}

		@Override
		public Object find(Object... paras) {
			if(paras!= null && paras.length > 0) if(type == null) return get(TypeConvertor.toString(paras[0]));
			else return get(TypeConvertor.toString(paras[0]), type);
			else return null;
		}

		@Override
		public <P> P store(Object value, Object... paras) {
			if(paras!= null && paras.length > 0) set(paras[0], value);
			return null;
		}

		@Override
		public <P> P discard(Object... paras) {
			if(paras!= null && paras.length > 0) PlatformMBean.this.remove(paras[0]);
			return null;
		}

		@Override
		public <P> P empty(Object... paras) {
			PlatformMBean.this.clear();
			return null;
		}
	}

	public class ProxyResource implements IResource<Object, Object>{
		protected Integer type;
		
		public void setType(Integer type) {
			this.type = type;
		}

		@Override
		public Object find(Object... paras) {
			if(paras!= null && paras.length > 0) if(type == null) return get(TypeConvertor.toString(paras[0]));
			else return get(TypeConvertor.toString(paras[0]), type);
			else return null;
		}

		@Override
		public <P> P store(Object value, Object... paras) {
			if(paras!= null && paras.length > 0) proxySet(paras[0], value);
			return null;
		}

		@Override
		public <P> P discard(Object... paras) {
			if(paras!= null && paras.length > 0) proxyRemove(paras[0]);
			return null;
		}

		@Override
		public <P> P empty(Object... paras) {
			proxyClear();
			return null;
		}
	}
	
	public void setProxyResource(ITransactionResource<Object, Object, IResource<Object, Object>, Object> proxyResource) {
		this.proxyResource = proxyResource;
	}

	public void setProxyParas(Object[] proxyParas) {
		this.proxyParas = proxyParas;
	}

	public void setConfigMap(ConcurrentHashMap<String, Object> config) {
		configMap = config;
	}
	
	public Object get(String key){
		Object result = null;
		if(key != null){
			result = configMap.get(key);
			if(result == null && proxyResource != null) {
				if(proxyParas != null && proxyParas.length > 0){
					Object[] newparas = Arrays.copyOf(proxyParas, proxyParas.length + 1);
					newparas[proxyParas.length] = key;
					result = proxyResource.find(newparas);
				}else result = proxyResource.find(key);
			}
		}
		return result;
	}

	public Object get(String key, Integer type){
		if(key != null){
			if(new Integer(2).equals(type)){
				if(proxyResource != null) {
					if(proxyParas != null && proxyParas.length > 0){
						Object[] newparas = Arrays.copyOf(proxyParas, proxyParas.length + 1);
						newparas[proxyParas.length] = key;
						return proxyResource.find(newparas);
					}else return proxyResource.find(key);
				} 
			} else return configMap.get(key);			
		}
		return null;
	}	
	
	public void set(Object key, Object value){
		if(key != null && value != null){
			String keyword = TypeConvertor.toString(key);
			configMap.put(keyword, value);
		}
	}
	
	public Object remove(Object key){
		return configMap.remove(TypeConvertor.toString(key));
	}
	
	public void clear(){
		configMap.clear();
	}
	
	public void proxySet(Object key, Object value){
		if(key != null && value != null && proxyResource != null){
			String keyword = TypeConvertor.toString(key);
			if(proxyParas != null && proxyParas.length > 0){
				Object[] newparas = Arrays.copyOf(proxyParas, proxyParas.length + 1);
				newparas[proxyParas.length] = keyword;
				proxyResource.store(value, newparas);
			}else proxyResource.store(value, keyword);
		}
	}
	
	public Object proxyRemove(Object key){
		if(key != null && proxyResource != null){
			String keyword = TypeConvertor.toString(key);
			if(proxyParas != null && proxyParas.length > 0){
				Object[] newparas = Arrays.copyOf(proxyParas, proxyParas.length + 1);
				newparas[proxyParas.length] = keyword;
				return proxyResource.discard(newparas);
			}else return proxyResource.discard(keyword);
		}
		return null;
	}
	
	public void proxyClear(){
		if(proxyResource != null){
			if(proxyParas != null) proxyResource.empty(proxyParas);
			else proxyResource.empty();
		}
	}
	
	public Object trans(IProcessor<IResource<Object, Object>, Object> processor){
		if(proxyResource != null) return proxyResource.execute(processor);
		else return null;
	}
}
