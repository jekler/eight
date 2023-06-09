package net.yeeyaa.eight.service.session;

import java.util.ArrayList;
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

import net.spy.memcached.CASMutation;
import net.spy.memcached.CASMutator;
import net.spy.memcached.CASResponse;
import net.spy.memcached.CASValue;
import net.spy.memcached.MemcachedClient;
import net.spy.memcached.transcoders.Transcoder;
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

public class MemcacheSession<K, K1, R, M> implements IListableTransaction<K, Object, IListableResource<K, Object>, R>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{SessionMethod.isAlive, SessionMethod.create, SessionMethod.destroy, ResourceMethod.count, ResourceMethod.status});
	protected static final MapperSet<Object> resourceMethods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final Logger log;
	protected MemcachedClient mc;
	protected CASMutator<Map<K, Object>> mutator;
	protected volatile K token;
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
			if(token != null) try{
				return mc.get(token.toString()) != null;
			}catch(Exception e){
				log.error("memcache error:", e);
			}
			return false;
		}
	};
	protected final IProcessor<Object, Object> destroy = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			if(token != null) mc.delete(token.toString());
			return null;
		}
	};
	protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			if(token != null) {
				CASValue<Object> v = mc.getAndTouch(token.toString(), timeout);
				if(v != null) return new Long(((Map<K, Object>) v.getValue()).size());
			}
			return new Long(0);
		}
	};
	protected final IProcessor<Object, Object> status =  new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			if(token != null && object != null) {
				if (object instanceof Object[] && ((Object[])object).length > 0 && ((Object[])object)[0] != null) object = ((Object[])object)[0];
				CASValue<Object> v = mc.getAndTouch(token.toString(), timeout);
				if (v != null) return (R) (((Map<K, Object>) v.getValue()).get(object) == null ? new Integer(-1) : new Integer(0));
			}
			return (R) new Integer(-1);
		}	
	};
	
	public MemcacheSession() {
		log = LoggerFactory.getLogger(MemcacheSession.class);
	}

	public MemcacheSession(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(MemcacheSession.class) : log;
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

	public void setMc(MemcachedClient mc) {
		this.mc = mc;
	}
	
	public void setTimeout(Integer timeout) {
		if(timeout != null && timeout > 0) this.timeout = timeout;
	}
	
	@PostConstruct
	public void initialize() {
		Transcoder tc = mc.getTranscoder();
		this.mutator = new CASMutator<Map<K, Object>>(mc, tc, retry);
		if(tokenInput != null){
			K token = tokenInput.find(inputParas);
			if(token != null && mc.get(token.toString()) != null){
				this.token = token;
				if(tokenOutput != null) tokenOutput.store(token, outputParas);
			}
		} 
		if(this.token == null && tokenOutput != null) tokenOutput.discard(outputParas);
	}
	
	@Override
	public Object find(K... keys){
		if(token != null && keys !=null && keys.length > 0){
			CASValue<Object> value = mc.getAndTouch(token.toString(), timeout);
			if(value != null) { 
				Map<K, Object> map = (Map<K, Object>) value.getValue();
				Object ret = null;
				if(keys.length == 1) ret = map.get(keys[0]);
				else {
					ret = new ArrayList<Object>(keys.length);
					for(K key : keys) ((List<Object>) ret).add(map.get(key));
				}
				return ret;
			} 
		} 
		return null;
	}

	@Override
	public <R> R discard(final K... keys) {
		if(token != null && keys !=null && keys.length > 0) try {
			CASMutation<Map<K, Object>> mutation=new CASMutation<Map<K, Object>>() {
			     public Map<K, Object> getNewValue(Map<K, Object> current) {
			    	 for(K key : keys) current.remove(key);
			         return current;
			     }
			 };
			 mutator.cas(token.toString(), null, timeout, mutation);
		}catch(Exception e){
			log.error("memc error:", e);
		}
		return null;
	}

	@Override
	public <R> R store(final Object value, final K... keys) {
		K newToken = null;
		if((keys != null && keys.length > 0 && keys[0] != null) || value instanceof Map) try{
			if(token == null) synchronized(this){
				if(token == null){
					if(keygen == null) token = (K)TypeConvertor.randomUuid();
					else token = keygen.process(null);
					newToken = token;
					if(tokenOutput != null) tokenOutput.store(token, outputParas);
				}
			}	
			Map<K, Object> current = new ConcurrentHashMap<K, Object>();
			if(keys == null || keys.length == 0 || keys[0] == null) current.putAll((Map<K, Object>) value);
	    	else current.put(keys[0], value);
			CASMutation<Map<K, Object>> mutation=new CASMutation<Map<K, Object>>() {
			     public Map<K, Object> getNewValue(Map<K, Object> current) {
			    	 if(keys == null) current.putAll((Map<K, Object>) value);
			    	 else current.put(keys[0], value);
			         return current;
			     }
			 };
			 mutator.cas(token.toString(), current, timeout, mutation);
		}catch(Exception e){
			log.error("memc error:", e);
		}
		return (R) newToken;
	}

	@Override
	public Collection<K[]> keys(K ... paras) {
		if(token != null){
			CASValue<Object> value = mc.getAndTouch(token.toString(), timeout);
			if(value != null) {
				Collection<K> keys = ((Map<K, Object>) value.getValue()).keySet();
				Collection<K[]> ret = new HashSet<K[]>(keys.size() * 2);
				for (K key : keys) {
					K[] k = PlatformUtil.newArrayOf(1, key);
					k[0] = key;
					ret.add(k);
				}
				return ret;
			}
		} 
		return null;
	}
	
	@Override
	public Map<K[], Object> all(K ... paras) {
		if(token != null){
			CASValue<Object> value = mc.getAndTouch(token.toString(), timeout);
			if(value != null) {
				Map<K, Object> map = (Map<K, Object>) value.getValue();
				Map<K[], Object> ret = new HashMap<K[], Object>(map.size() * 2);
				for (Entry<K, Object> entry : map.entrySet()) {
					K[] k = PlatformUtil.newArrayOf(1, entry.getKey());
					k[0] = entry.getKey();
					ret.put(k, entry.getValue());
				}
				return ret;
			}
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

	@Override
	public <R> R empty(K... paras) {
		if(token != null) mc.set(token.toString(), timeout, new ConcurrentHashMap<K, Object>());
		return null;
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
		protected volatile Boolean needInit = true;
		protected Long version;
		protected Map<K, Object> cache;
		protected Map<K, Value> exec = Collections.synchronizedMap(new LinkedHashMap<K, Value>());
		protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
			@Override
			public Object process(Object object) {
				if(needInit) initialize();
				return new Long(cache == null ? 0 : cache.size());
			}
		};
		
		protected synchronized void initialize(){
			if(needInit && token != null){
				CASValue<Object> value = mc.getAndTouch(token.toString(), timeout);
				if(value != null) { 
					version = value.getCas();
					cache = (Map<K, Object>) value.getValue();
				}
			}
			needInit = false;
		}
		
		@Override
		public Object find(K... paras) {
			if(paras != null && paras.length > 0) {
				if(needInit) initialize();
				if(cache != null) return cache.get(paras[0]);
			}
			return null;
		}

		@Override
		public <R> R store(Object value, K... paras) {
			if(paras != null && paras.length > 0) {
				exec.remove(paras[0]);
				exec.put(paras[0], new Value(0, value));
			}
			return null;
		}

		@Override
		public <R> R discard(K... paras){
			if(paras != null && paras.length > 0){
				exec.remove(paras[0]);
				exec.put(paras[0], new Value(1, null));
			}
			return null;
		}

		@Override
		public <R> R empty(K... paras) {
			exec.clear();
			exec.put(null, null);
			return null;
		}	

		@Override
		public Collection<K[]> keys(K... paras) {
			if(needInit) initialize();
			Collection<K[]> ls = new ArrayList<K[]>(cache == null ? 0 : cache.size());
			if(cache != null) for(K o : cache.keySet()) {
				K[] k = PlatformUtil.newArrayOf(1, o);
				k[0] = o;
				ls.add(k);
			}
			return ls;
		}

		@Override
		public Map<K[], Object> all(K... paras) {
			if(needInit) initialize();
			Map<K[], Object> ret = new HashMap<K[], Object>(cache == null ? 0 : cache.size() * 2);
			if(cache != null) for(Entry<K, Object> o : cache.entrySet()) {
				K[] k = PlatformUtil.newArrayOf(1, o.getKey());
				k[0] = o.getKey();
				ret.put(k, o.getValue());
			}
			return ret;
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
				if(token == null) synchronized(MemcacheSession.this){
					if(token == null) {
						if(keygen == null) token = (K)TypeConvertor.randomUuid();
						else token = keygen.process(null);
						if(tokenOutput != null) tokenOutput.store(token, outputParas);
					}else return false;
				}
				CASValue<Object> value = mc.gets(token.toString());
				if((version != null && value != null && value.getCas() != version) || (version == null && value != null)) return false;
				final Boolean[] clear = new Boolean[]{false};
				final ConcurrentHashMap<K, Object> put = new ConcurrentHashMap<K, Object>();
				final HashSet<K> remove = new HashSet<K>();
				for(Entry<K, Value> entry : exec.entrySet()) if(entry.getValue() == null) clear[0] = true;
				else if(entry.getValue().type == 0) put.put(entry.getKey(), entry.getValue().value);
				else if(entry.getValue().type == 1) remove.add(entry.getKey());
				if(value == null) try{
					return mc.add(token.toString(), timeout, put).get();
				}catch(Exception e){ return false;}
				else {
					Map<K, Object> current = (Map<K, Object>)value.getValue();
			    	if(clear[0]) current = new ConcurrentHashMap<K, Object>();
					if(put.size() > 0) current.putAll(put);
					if(remove.size() > 0) for(K key : remove) current.remove(key);
					CASResponse resp = mc.cas(token.toString(), value.getCas(), timeout, current);
					if(CASResponse.NOT_FOUND.equals(resp)) try{
						return mc.add(token.toString(), timeout, put).get();
					}catch(Exception e){return false;}
					else if(CASResponse.OK.equals(resp) || CASResponse.OBSERVE_MODIFIED.equals(resp)) return true;
					else if(CASResponse.OBSERVE_ERROR_IN_ARGS.equals(resp) || CASResponse.OBSERVE_TIMEOUT.equals(resp)) 
						throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL);
					else return false;
				}
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
			count ++;
			Resource resource = new Resource();
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
		} catch (Exception e) {
			log.error("MemcacheSession: sleep interrupted:", e);
			throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL, e);
		} else throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL);
		while(!suc && (retry == 0 || count < retry));
		if(suc) return ret;
		else throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL);
	}
}
