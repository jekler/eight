package net.yeeyaa.eight.core;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.PostConstruct;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.ITransactionResource;
import net.yeeyaa.eight.core.util.TypeConvertor;


public class PlatformSetting {
	protected IReadonlyListable<Object, Object> constResource;
	protected Object[] constParas;
	protected ITransactionResource<Object, Object, IResource<Object, Object>, Object> proxyResource;
	protected Object[] proxyParas;
	protected volatile ConcurrentHashMap<String, Object> configMap;
	protected volatile Map<String, Object> constMap;
	protected Boolean reverse = false;

	public void setReverse(Boolean rev) {
		reverse = rev;
	}

	public void setConstParas(Object[] constParas) {
		this.constParas = constParas;
	}

	public void setProxyParas(Object[] proxyParas) {
		this.proxyParas = proxyParas;
	}

	public void setConstResource(IReadonlyListable<Object, Object> constResource) {
		this.constResource = constResource;
	}

	public void setProxyResource(ITransactionResource<Object, Object, IResource<Object, Object>, Object> proxyResource) {
		this.proxyResource = proxyResource;
	}

	public void setConfigMap(ConcurrentHashMap<String, Object> config) {
		configMap = config;
	}

	@PostConstruct
	public void initialize(){
		if(constResource != null) {
			Map<Object[], Object> o = constParas == null ? constResource.all() : constResource.all(constParas);
			if(o != null) {
				Map<String, Object> map = new HashMap<String, Object>(o.size() * 2);
				for(Entry<Object[], Object> entry : o.entrySet()) if(entry.getKey() != null && entry.getValue() != null && entry.getKey().length > 0 && entry.getKey()[0] != null)
					map.put(entry.getKey()[0].toString(), entry.getValue());
				constMap = map;
			}
		}
	}

	public class Task implements IProcessor<Object, Object>{
		@Override
		public Object process(Object parameters) {
			initialize();
			return parameters;
		}
	}

	public class Setting implements IProcessor<ConcurrentHashMap<String, Object>, Object>{
		@Override
		public Object process(ConcurrentHashMap<String, Object> parameters) {
			setConfigMap(parameters);
			return parameters;
		}
	}

	public class Get implements IProcessor<Object, Object>{
		protected Integer type;
		
		public void setType(Integer type) {
			this.type = type;
		}
		
		@Override
		public Object process(Object key) {
			if(type == null) return get(key);
			else return get(key, type);
		}
	}

	public class SettingResource implements IResource<Object, Object>{
		protected Integer type;
		
		public void setType(Integer type) {
			this.type = type;
		}

		@Override
		public Object find(Object... paras) {
			if(paras!= null && paras.length > 0) if(type == null) return get(paras[0]);
			else return get(paras[0], type);
			else return null;
		}

		@Override
		public <P> P store(Object value, Object... paras) {
			if(paras!= null && paras.length > 0) set(paras[0], value);
			return null;
		}

		@Override
		public <P> P discard(Object... paras) {
			if(paras!= null && paras.length > 0) PlatformSetting.this.remove(paras[0]);
			return null;
		}

		@Override
		public <P> P empty(Object... paras) {
			PlatformSetting.this.clear();
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
			if(paras!= null && paras.length > 0) if(type == null) return get(paras[0]);
			else return get(paras[0], type);
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
	
	public Object get(Object key){
		Object result = null;
		if(key != null){
			String keyword = TypeConvertor.toString(key);
			if (configMap != null) result = configMap.get(keyword);
			if(result == null && constMap != null && !reverse) result = constMap.get(keyword);
			if(result == null && proxyResource != null) {
				if(proxyParas != null && proxyParas.length > 0){
					Object[] newparas = Arrays.copyOf(proxyParas, proxyParas.length + 1);
					newparas[proxyParas.length] = keyword;
					result = proxyResource.find(newparas);
				}else result = proxyResource.find(keyword);
			}
			if(result == null && constMap != null && reverse) result = constMap.get(keyword);
		}
		return result;
	}
	
	public Object get(Object key, Integer type){
		if(key != null){
			String keyword = TypeConvertor.toString(key);
			if(new Integer(2).equals(type)){
				if(proxyResource != null) {
					if(proxyParas != null && proxyParas.length > 0){
						Object[] newparas = Arrays.copyOf(proxyParas, proxyParas.length + 1);
						newparas[proxyParas.length] = keyword;
						return proxyResource.find(newparas);
					}else return proxyResource.find(keyword);
				} 
			} else if(new Integer(1).equals(type)) {
				if(constMap != null) return constMap.get(keyword);
			}else if (configMap != null) return configMap.get(keyword);			
		}
		return null;
	}	
	
	public void set(Object key, Object value){
		if(key != null && value != null){
			String keyword = TypeConvertor.toString(key);
			if(configMap == null) configMap = new ConcurrentHashMap<String, Object>();
			configMap.put(keyword, value);
		}
	}
	
	public Object remove(Object key){
		if(key != null && configMap != null){
			return configMap.remove(TypeConvertor.toString(key));
		}else return null;
	}
	
	public void clear(){
		if(configMap != null) configMap = new ConcurrentHashMap<String, Object>();
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
