package net.yeeyaa.eight.service.session;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

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
import org.springframework.dao.DataAccessException;
import org.springframework.data.redis.core.HashOperations;
import org.springframework.data.redis.core.RedisOperations;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.SessionCallback;

public class RedisSession<K, K1, R> implements IListableTransaction<K, Object, IListableResource<K, Object>, R>, IExtendable<Object> {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{SessionMethod.isAlive, SessionMethod.create, SessionMethod.destroy, ResourceMethod.count, ResourceMethod.status});
	protected static final MapperSet<Object> resourceMethods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected final Logger log;
	protected RedisTemplate<K, Object> template;
	protected HashOperations<K, K, Object> ho;
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
				return ho.size(token) > 0;
			}catch(Exception e){
				log.error("redis error:", e);
			}
			return false;
		}
	};
	protected final IProcessor<Object, Object> destroy = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			if(token != null) template.delete(token);
			return null;
		}
	};
	protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
		@Override
		public Object process(Object object) {
			List<Object> ls = template.executePipelined(new SessionCallback<Object>() {
			    @Override
			    public <T, V> Object execute(RedisOperations<T, V> operations) throws DataAccessException {
			    	ho.size(token);
			        return template.expire(token, timeout, TimeUnit.SECONDS);
			    }
			}, template.getHashKeySerializer());
			return ls.get(0);
		}
	};
	protected final IProcessor<Object, Object> status = new IProcessor<Object, Object> () {
		@Override
		public Object process(final Object object) {
			if(token != null && object != null) {
				List<Object> ls = template.executePipelined(new SessionCallback<Object>() {
				    @Override
				    public <T, V> Object execute(RedisOperations<T, V> operations) throws DataAccessException {
				    	ho.get(token, object instanceof Object[] && ((Object[])object).length > 0 && ((Object[])object)[0] != null ? ((Object[])object)[0] : object);
				        return template.expire(token, timeout, TimeUnit.SECONDS);
				    }
				}, template.getHashValueSerializer());
				return (R) (ls.get(0) == null ? new Long(-1) : new Long(0));
			}
			return (R) new Long(-1);
		}
	};
	
	public RedisSession() {
		log = LoggerFactory.getLogger(RedisSession.class);
	}

	public RedisSession(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(RedisSession.class) : log;
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

	public void setTemplate(RedisTemplate<K, Object> template) {
		if(template != null) {
			this.template = template;
			this.ho = template.opsForHash();
		}
	}
	
	public void setTimeout(Integer timeout) {
		if(timeout != null && timeout > 0) this.timeout = timeout;
	}

	@PostConstruct
	public void initialize() {
		if(tokenInput != null){
			K token = tokenInput.find(inputParas);
			if(token != null && ho.size(token) > 0){
				this.token = token;
				if(tokenOutput != null) tokenOutput.store(token, outputParas);
			}
		}
		if(this.token == null && tokenOutput != null) tokenOutput.discard(outputParas);
	}
	
	@Override
	public Object find(final K... keys){
		if(token != null && keys !=null && keys.length > 0){
			List<Object> ls = template.executePipelined(new SessionCallback<Object>() {
			    @Override
			    public <T, V> Object execute(RedisOperations<T, V> operations) throws DataAccessException {
			    	if(keys.length > 1) ho.multiGet(token, Arrays.asList(keys));
			    	else ho.get(token, keys[0]);		    	
			        return template.expire(token, timeout, TimeUnit.SECONDS);
			    }
			}, template.getHashValueSerializer());
			return ls.get(0);
		} else return null;
	}

	@Override
	public <R> R discard(final K... keys) {
		if(token != null && keys !=null && keys.length > 0) template.executePipelined(new SessionCallback<Object>() {
		    @Override
		    public <T, V> Object execute(RedisOperations<T, V> operations) throws DataAccessException {
		    	template.multi();
		    	if(keys.length > 1) ho.delete(token, keys);
		    	else ho.delete(token, keys[0]);
		    	template.expire(token, timeout, TimeUnit.SECONDS);
		        return operations.exec();
		    }
		}, template.getHashValueSerializer());
		return null;
	}

	@Override
	public <R> R empty(K... paras) {
		if(token != null) template.delete(token);
		return null;
	}
	
	@Override
	public <R> R store(final Object value, final K... keys) {
		K newToken = null;
		if((keys != null && keys.length > 0 && keys[0] != null) || value instanceof Map) {
			if(token == null) synchronized(this){
				if(token == null) {
					if(keygen == null) token = (K)TypeConvertor.randomUuid();
					else token = keygen.process(null);
					newToken = token;
					if(tokenOutput != null) tokenOutput.store(token, outputParas);
				}
			}
			template.executePipelined(new SessionCallback<Object>() {
			    @Override
			    public <T, V> Object execute(RedisOperations<T, V> operations) throws DataAccessException {
					if(keys == null || keys.length == 0 || keys[0] == null) ho.putAll(token, (Map) value);
					else ho.put(token, keys[0], value);
			        return template.expire(token, timeout, TimeUnit.SECONDS);
			    }
			}, template.getHashValueSerializer());
		}
		return (R) newToken;
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		if(token != null) {
			List<Object> ls = template.executePipelined(new SessionCallback<Object>() {
			    @Override
			    public <T, V> Object execute(RedisOperations<T, V> operations) throws DataAccessException {
			    	ho.keys(token);
			        return template.expire(token, timeout, TimeUnit.SECONDS);
			    }
			}, template.getHashKeySerializer());
			Collection<K> keys = (Collection<K>)ls.get(0);
			Collection<K[]> ret = new HashSet<K[]>();
			if (keys != null && keys.size() > 0) for (K key : keys) {
				K[] k = PlatformUtil.newArrayOf(1, key);
				k[0] = key;
				ret.add(k);
			}
			return ret;
		} else return null;
	}
	
	@Override
	public Map<K[], Object> all(K... paras) {
		if(token != null) {
			List<Object> ls = template.executePipelined(new SessionCallback<Object>() {
			    @Override
			    public <T, V> Object execute(RedisOperations<T, V> operations) throws DataAccessException {
			    	ho.entries(token);
			        return template.expire(token, timeout, TimeUnit.SECONDS);
			    }
			}, template.getHashValueSerializer());
			Map<K, Object> map = (Map<K, Object>) (Map<K, Object>)ls.get(0);
			Map<K[], Object> ret = new HashMap<K[], Object>();
			if (map != null && map.size() > 0) for (Entry<K, Object> entry : map.entrySet()) {
				K[] k = PlatformUtil.newArrayOf(1, entry.getKey());
				k[0] = entry.getKey();
				ret.put(k, entry.getValue());
			}
			return ret;
		} else return null;
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
		protected RedisOperations<K, Object> template;
		protected HashOperations<K, K, Object> ho;
		protected volatile Boolean needInit = true;
		protected ConcurrentHashMap<K, Object> cache = new ConcurrentHashMap<K, Object>();
		protected Map<K, Value> exec = Collections.synchronizedMap(new LinkedHashMap<K, Value>());
		protected final IProcessor<Object, Object> count = new IProcessor<Object, Object>(){
			@Override
			public Object process(Object object) {
				if(needInit) initialize();
				return new Long(cache == null ? 0 : cache.size());
			}
		};
		
		public Resource(RedisOperations template) {
			this.template = template;
			this.ho = template.opsForHash();
		}
		
		protected synchronized void initialize(){
			if(needInit && token != null){
				List<Object> ls = template.executePipelined(new SessionCallback<Object>() {
					@Override
					public <K, V> Object execute(RedisOperations<K, V> operations) throws DataAccessException {
				        template.watch(token);
				        ho.entries(token);
				        return template.expire(token, timeout, TimeUnit.SECONDS);  
					}
				}, template.getHashValueSerializer());
				cache.putAll((Map<K, Object>)ls.get(0));
			}
			needInit = false;
		}
		
		@Override
		public Object find(final K... paras) {
			if(paras != null && paras.length > 0) {
				if(needInit) initialize();
				return cache.get(paras[0]);
			} else return null;
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
		
		@Override
		public Collection<K[]> keys(K... paras) {
			if(needInit) initialize();
			Collection<K[]> ls = new ArrayList<K[]>(cache == null ? 0 : cache.size());
			for(K o : cache.keySet()) {
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
			for(Entry<K, Object> o : cache.entrySet()) {
				K[] k = PlatformUtil.newArrayOf(1, o.getKey());
				k[0] = o.getKey();
				ret.put(k, o.getValue());
			}
			return ret;
		}
		
		protected boolean perform(){
			if(exec.size() > 0) {
				if(token == null) synchronized(RedisSession.this){
					if(token == null){
						if(keygen == null) token = (K)TypeConvertor.randomUuid();
						else token = keygen.process(null);
						if(tokenOutput != null) tokenOutput.store(token, outputParas);
					}else return false;
				}
				final Boolean[] clear = {false};
				final HashMap<K, Object> put = new HashMap<K, Object>();
				final HashSet<K> remove = new HashSet<K>();
				for(Entry<K, Value> entry : exec.entrySet()) if(entry.getValue() == null) clear[0] = true;
				else if(entry.getValue().type == 0) put.put(entry.getKey(), entry.getValue().value);
				else if(entry.getValue().type == 1) remove.add(entry.getKey());
				List<Object> ret = template.executePipelined(new SessionCallback<Object>(){
					@Override
					public <T, V> Object execute(RedisOperations<T, V> operations) throws DataAccessException {
						template.multi();
						if(clear[0]) ho.delete(token);
						if(put.size() > 0) ho.putAll(token, put);
						if(remove.size() > 0) ho.delete(token, remove.toArray());
						template.expire(token, timeout, TimeUnit.SECONDS);
						return template.exec();
					}	
				}, template.getHashValueSerializer()); 
				if(ret == null || (ret.size() == 1 && ret.get(0) == null)) return false;
			}
			return true;
		}
	}

	@Override
	public R execute(final IProcessor<IListableResource<K, Object>, R> processor) {
		if(template != null && processor != null){
			R ret = null;
			final Boolean[] suc = {false};
			int count = 0;
			int max = 52;
			do if(this.<IProcessor<Void, Boolean>>extend(SessionMethod.isAlive).process(null) || !exitOnTimeout) try {
				count ++;
				ret = template.execute(new SessionCallback<R>(){
					@Override
					public <K, V> R execute(RedisOperations<K, V> operations)	throws DataAccessException {
						Resource resource = new Resource(operations);
						R ret = processor.process(resource);
						suc[0] = resource.perform();
						return ret;
					}
				});
				if(!suc[0] && sleep > 0){
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
				log.error("RedisSession: sleep interrupted:", e);
				throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL, e);
			}  else throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL);
			while(!suc[0] && (retry == 0 || count < retry));
			if(suc[0]) return ret;
		}
		throw new PlatformException(PlatformError.ERROR_TRANSACTION_FAIL);
	}
}
