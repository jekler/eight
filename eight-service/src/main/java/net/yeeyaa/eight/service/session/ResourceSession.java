package net.yeeyaa.eight.service.session;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.PostConstruct;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IListableTransaction;
import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.util.TypeConvertor;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.enumerate.SessionMethod;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.core.util.PlatformUtil;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ResourceSession<K, K1, R> implements IListableTransaction<K, Object, IListableResource<K, Object>, R>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{SessionMethod.isAlive, SessionMethod.create, SessionMethod.destroy, ResourceMethod.count, ResourceMethod.status});
	protected static final MapperSet<Object> resourceMethods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final Logger log;
	protected IListableResource<K, SessionStore<K>> resource;
	protected K token;
	protected IInputResource<K1, K> tokenInput;
	protected K1[] inputParas;
	protected IOutputResource<K1, K> tokenOutput;
	protected K1[] outputParas;
	protected Integer timeout = 300;
	protected Integer retry = 10;
	protected Integer sleep = 2;
	protected Boolean fluctuate = true;
	protected Boolean exitOnTimeout = false;
	protected IProcessor<Void, K> keygen;
	protected IProcessor<Object[], Boolean> comparator;
	protected final IProcessor<Object, Object> create = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			token = null;
			if(tokenOutput != null) tokenOutput.discard(outputParas);
			return null;
		}
	};
	protected final IProcessor<Object, Object> isAlive = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			if(token != null) {
				SessionStore<K> ss = resource.find(token);
				Boolean alive = (ss != null && ss.timestamp + timeout * 1000 > Calendar.getInstance().getTimeInMillis());
				if(!alive) {
					if(ss != null) resource.discard(token);
					token = null;
					if(tokenOutput != null) tokenOutput.discard(outputParas);
				}
				return alive; 
			}else return false;
		}
	};
	protected final IProcessor<Object, Object> destroy = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			if(token != null) resource.discard(token);
			token = null;
			if(tokenOutput != null) tokenOutput.discard(outputParas);
			return null;
		}
	};
	protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			if(token != null){
				Long now = Calendar.getInstance().getTimeInMillis();
				SessionStore<K> ss = resource.find(token);
				if(ss != null && ss.timestamp + timeout * 1000 > now){
					ss.timestamp = now;
					resource.store(ss, token);
					return new Long(ss.store.size());
				} else if(ss != null) resource.discard(token);
			} 
			return new Long(0);
		}
	};
	protected final IProcessor<Object, Object> status = new IProcessor<Object, Object> () {
		@Override
		public Object process(Object object) {
			if(token != null && object != null) {
				Long now = Calendar.getInstance().getTimeInMillis();
				SessionStore<K> ss = resource.find(token);
				if(ss != null && ss.timestamp + timeout * 1000 > now){
					if (object instanceof Object[] && ((Object[])object).length > 0 && ((Object[])object)[0] != null) object = ((Object[])object)[0];
					Object ret = ss.store.get(object);
					ss.timestamp = now;
					resource.store(ss, token);
					return ret == null ? -1 : 0;
				} else if(ss != null) resource.discard(token);
			}
			return -1;
		}
	};
	
	public ResourceSession() {
		log = LoggerFactory.getLogger(ResourceSession.class);
	}

	public ResourceSession(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ResourceSession.class) : log;
	}
	
	public static class SessionStore<K> implements Serializable, Cloneable{
		protected static final long serialVersionUID = 5521378229554901804L;
		protected Map<K, Object> store;
		protected Long timestamp;
		
		public Map<K, Object> getStore() {
			return store;
		}
		
		public void setStore(Map<K, Object> store) {
			this.store = store;
		}
		
		public Long getTimestamp() {
			return timestamp;
		}
		
		public void setTimestamp(Long timestamp) {
			this.timestamp = timestamp;
		}

		public SessionStore(Map<K, Object> store, Long timestamp) {
			this.store = store;
			this.timestamp = timestamp;
		}
		
		public SessionStore(){}

		@Override
		public int hashCode() {
			return store.hashCode() * 17 + timestamp.intValue();
		}

		@Override
		public boolean equals(Object obj) {
			if(this == obj) return true;
			else if(obj instanceof SessionStore) {
				SessionStore<K> other = (SessionStore<K>) obj;
				return ((timestamp == other.timestamp ||(timestamp != null && timestamp.equals(other.timestamp))) && 
						(store == other.store || (store != null && store.equals(other.store))));
			}
			return false;
		}

		@Override
		public SessionStore<K> clone() {
			Map<K, Object> map = new ConcurrentHashMap<K, Object>();
			map.putAll(store);
			return new SessionStore<K>(map, timestamp);
		}

		@Override
		public String toString() {
			return timestamp + " : " + store.toString();
		}
	}
	
	public static class Gc<K> implements IProcessor<Object, Object>{
		protected IListableResource<K, SessionStore<K>> resource;
		
		public void setResource(IListableResource<K, SessionStore<K>> resource) {
			this.resource = resource;
		}

		@Override
		public Object process(Object instance) {
			Long now = Calendar.getInstance().getTimeInMillis();
			for(Entry<K[], SessionStore<K>> entry : resource.all().entrySet()){
				SessionStore<K> ss = entry.getValue();
				if(ss != null && ss.timestamp <= now) resource.discard(entry.getKey());
			}
			return instance;
		}		
	}
	
	public void setComparator(IProcessor<Object[], Boolean> comparator) {
		this.comparator = comparator;
	}

	public void setExitOnTimeout(Boolean exitOnTimeout) {
		if(exitOnTimeout != null) this.exitOnTimeout = exitOnTimeout;
	}

	public void setInputParas(K1[] inputParas) {
		this.inputParas = inputParas;
	}

	public void setOutputParas(K1[] outputParas) {
		this.outputParas = outputParas;
	}

	public void setFluctuate(Boolean fluctuate) {
		if(fluctuate != null) this.fluctuate = fluctuate;
	}

	public void setRetry(Integer retry) {
		if (retry != null && retry >= 0) this.retry = retry;
	}

	public void setKeygen(IProcessor<Void, K> keygen) {
		this.keygen = keygen;
	}

	public void setSleep(Integer sleep) {
		if (sleep != null && sleep >= 0) this.sleep = sleep;
	}

	public void setTokenOutput(IOutputResource<K1, K> tokenOutput) {
		this.tokenOutput = tokenOutput;
	}

	public void setTokenInput(IInputResource<K1, K> tokenInput) {
		this.tokenInput = tokenInput;
	}
	
	public void setResource(IListableResource<K, SessionStore<K>> resource) {
		this.resource = resource;
	}
	
	public void setTimeout(Integer timeout) {
		if(timeout != null && timeout > 0) this.timeout = timeout;
	}
	
	@PostConstruct
	public void initialize() {
		if(tokenInput != null){
			K token = tokenInput.find(inputParas);
			if(token != null){
				SessionStore<K> ss = resource.find(token);
				if(ss != null && ss.timestamp + timeout * 1000 > Calendar.getInstance().getTimeInMillis()){
					this.token = token;
					if(tokenOutput != null) tokenOutput.store(token, outputParas);
				} else if(ss != null) resource.discard(token);
			}
		}
		if(this.token == null && tokenOutput != null) tokenOutput.discard(outputParas);
	}
	
	@Override
	public Object find(K... keys) {
		if(token != null && keys !=null && keys.length > 0){
			Long now = Calendar.getInstance().getTimeInMillis();
			SessionStore<K> ss = resource.find(token);
			if(ss != null && ss.timestamp + timeout * 1000 > now){
				Object ret = null;
				if(keys.length == 1) ret = ss.store.get(keys[0]);
				else {
					ret = new ArrayList<Object>(keys.length);
					for(K key : keys) ((List<Object>) ret).add(ss.store.get(key));
				}
				ss.timestamp = now;
				resource.store(ss, token);
				return ret;
			} else if(ss != null) resource.discard(token);
		} 
		return null;
	}

	@Override
	public <P> P discard(K... keys) {
		if(token != null && keys !=null && keys.length > 0){
			Long now = Calendar.getInstance().getTimeInMillis();
			SessionStore<K> ss = resource.find(token);
			if(ss != null && ss.timestamp + timeout * 1000 > now){
				if(keys.length == 1) ss.store.remove(keys[0]);
				else for(K key : keys) ss.store.remove(key);
				ss.timestamp = now;
				resource.store(ss, token);
			} else if(ss != null) resource.discard(token);
		}
		return null;
	}

	@Override
	public <P> P store(Object value, K... keys) {
		K newToken = null;
		if((keys != null && keys.length > 0 && keys[0] != null) || value instanceof Map) {
			SessionStore<K> ss = null;
			Long now = Calendar.getInstance().getTimeInMillis();
			if(token != null){
				ss = resource.find(token);
				if(ss != null && ss.timestamp + timeout * 1000 <= now){
					resource.discard(token);
					ss = null;
				}
			}
			if(ss == null) {
				if(keygen == null) token = (K)TypeConvertor.randomUuid();
				else token = keygen.process(null);
				newToken = token;
				if(tokenOutput != null) tokenOutput.store(token, outputParas);
				ss = new SessionStore<K>(new ConcurrentHashMap<K, Object>(), now);
			}
			if(keys == null || keys.length == 0 || keys[0] == null) ss.store.putAll((Map<K, Object>) value);
			else ss.store.put(keys[0], value);
			ss.timestamp = now;
			resource.store(ss, token);
		}
		return (P) newToken;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		if(token != null) {
			Long now = Calendar.getInstance().getTimeInMillis();
			SessionStore<K> ss = resource.find(token);
			if(ss != null && ss.timestamp + timeout * 1000 > now) {
				ss.timestamp = now;
				resource.store(ss, token);
				Collection<K> keys = ss.store.keySet();
				Collection<K[]> ret = new HashSet<K[]>();
				if (keys != null && keys.size() > 0) for (K key : keys) {
					K[] k = PlatformUtil.newArrayOf(1, key);
					k[0] = key;
					ret.add(k);
				}
				return ret;
			}else if(ss != null) resource.discard(token);
		} 
		return null;
	}
	
	@Override
	public <P> P empty(K... paras) {
		if(token != null) {
			Long now = Calendar.getInstance().getTimeInMillis();
			SessionStore<K> ss = resource.find(token);
			if(ss != null && ss.timestamp + timeout * 1000 > now) {
				ss.timestamp = now;
				resource.store(ss, token);
				ss.store.clear();
			}else if(ss != null) resource.discard(token);
		} 
		return null;
	}
	
	@Override
	public Map<K[], Object> all(K... paras) {
		if(token != null) {
			Long now = Calendar.getInstance().getTimeInMillis();
			SessionStore<K> ss = resource.find(token);
			if(ss != null && ss.timestamp + timeout * 1000 > now) {
				ss.timestamp = now;
				resource.store(ss, token);
				Map<K[], Object> ret = new HashMap<K[], Object>();
				for (Entry<K, Object> entry : ss.store.entrySet()) {
					K[] k = PlatformUtil.newArrayOf(1, entry.getKey());
					k[0] = entry.getKey();
					ret.put(k, entry.getValue());
				}
				return ret;
			}else if(ss != null) resource.discard(token);
		} 
		return null;
	}

	@Override
	public <N> N extend(Object object) {
		if (object != null) {
			Object method = methods.process(object);
			if (method instanceof ResourceMethod) switch((ResourceMethod) method) {
				case count : return (N) count;
				case status : return (N) status;
			} else if (method instanceof SessionMethod) switch((SessionMethod) method) {
				case create : return (N) create;
				case destroy: return (N) destroy;
				case isAlive : return (N) isAlive;
			} 
		}
		return null;
	}

	@Override
	public Collection<Object> methods() {
		return methods;
	}
	
	protected static class Value{
		protected int type;
		protected Object value;
		
		protected Value(int type, Object value) {
			this.type = type;
			this.value = value;
		}
	}
	
	protected class Resource implements IListableResource<K, Object>, IExtendable<Object> {
		protected Boolean exist = true;
		protected Map<K, Object> cache;
		protected ConcurrentHashMap<K, Object> compare = new ConcurrentHashMap<K, Object>();
		protected Map<K, Value> exec = Collections.synchronizedMap(new LinkedHashMap<K, Value>());
		protected IProcessor<IListableResource<K, Object>, R> performer;
		
		protected Resource(IProcessor<IListableResource<K, Object>, R> performer) {
			this.performer = performer;
		}
		
		@Override
		public Object find(K... paras) {
			if(paras != null && paras.length > 0 && token != null) {
				if(compare.containsKey(paras[0])) return compare.get(paras[0]);
				else if(cache != null){
					Object ret = cache.get(paras[0]);
					compare.put(paras[0], ret);
					return ret;
				} else if(!exist) compare.put(paras[0], null);
				else {
					Long now = Calendar.getInstance().getTimeInMillis();
					SessionStore<K> ss = resource.find(token);
					if(ss != null && ss.timestamp + timeout * 1000 > now){
						cache = ss.getStore();
						ss.timestamp = now;
						resource.store(ss, token);
						Object ret = ss.store.get(paras[0]);
						compare.put(paras[0], ret);
						return ret;
					} else {
						if(ss != null) resource.discard(token);
						compare.put(paras[0], null);
						exist = false;
					}
				}
			} 
			return null;
		}

		@Override
		public <P> P store(Object value, K... paras) {
			if(paras != null && paras.length > 0) {
				exec.remove(paras[0]);
				exec.put(paras[0], new Value(0, value));
			}
			return null;
		}

		@Override
		public <P> P discard(K... paras) {
			if(paras != null && paras.length > 0){
				exec.remove(paras[0]);
				exec.put(paras[0], new Value(1, null));
			}
			return null;
		}

		@Override
		public <P> P empty(K... paras) {
			exec.clear();
			exec.put(null, null);
			return null;
		}	

		@Override
		public Collection<K[]> keys(K... paras) {
			return ResourceSession.this.keys(paras);
		}
		
		@Override
		public Map<K[], Object> all(K... paras) {
			return ResourceSession.this.all(paras);
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
		
		protected boolean perform(){
			if(exec.size() > 0) {
				Boolean clear = false;
				HashMap<K, Object> put = new HashMap<K, Object>();
				HashSet<K> remove = new HashSet<K>();
				for(Entry<K, Value> entry : exec.entrySet()) if(entry.getValue() == null) clear = true;
				else if(entry.getValue().type == 0) put.put(entry.getKey(), entry.getValue().value);
				else if(entry.getValue().type == 1) remove.add(entry.getKey());
				SessionStore<K> ss = null;
				Long now = Calendar.getInstance().getTimeInMillis();
				if(token != null){
					ss = resource.find(token);
					if(ss != null && ss.timestamp + timeout * 1000 <= now){
						resource.discard(token);
						ss = null;
					}else if(ss != null) if(comparator != null) for(Entry<K, Object> entry : compare.entrySet()){
						if(!comparator.process(new Object[]{performer, entry.getKey(), entry.getValue(), ss.store.get(entry.getKey())})) return false;
					} else for(Entry<K, Object> entry : compare.entrySet()) {
						Object newvalue = ss.store.get(entry.getKey());
						if(!(newvalue == null ? entry.getValue() == null : newvalue.equals(entry.getValue()))) return false;	
					}
				}
				if(ss == null) {
					if(keygen == null) token = (K)TypeConvertor.randomUuid();
					else token = keygen.process(null);
					if(tokenOutput != null) tokenOutput.store(token, outputParas);
					ss = new SessionStore<K>(new ConcurrentHashMap<K, Object>(), now);
				}
				if(clear) ss.store = new ConcurrentHashMap<K, Object>();
				if(put.size() > 0) ss.store.putAll(put);
				if(remove.size() > 0) for(K key : remove) ss.store.remove(key);
				ss.timestamp = now;
				resource.store(ss, token);
			}
			return true;
		}
	}

	@Override
	public R execute(final IProcessor<IListableResource<K, Object>, R> processor) {
		R ret = null;
		boolean suc = false;
		int count = 0;
		int max = 52;
		do if(this.<IProcessor<Void, Boolean>>extend(SessionMethod.isAlive).process(null) || !exitOnTimeout) try {
			Resource resource = new Resource(processor);
			ret = processor.process(resource);
			suc = resource.perform();		
			if(!suc && sleep > 0){
				if(fluctuate) {
					long s = (long)sleep << ((count < max ? count: max) + 10);
					if(s < 0){
						max = count - 1;
						s = (long)sleep << ((count < max ? count: max) + 10);
					}
					Thread.sleep(s);
				} else  Thread.sleep((long)sleep << 10);
			} 
			count ++;
		} catch (Exception e) {
			log.error("ResourceSession: sleep interrupted:", e);
			throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL, e);
		} while(!suc && (retry == 0 || count < retry));
		if(suc) return ret;
		else throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL);
	}
}
