package net.yeeyaa.eight.core.processor;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IUniversal;


public class UniversalProxy<K, V, U extends IListableResource<K, V>, T, R> implements IUniversal<K, V, U, T, R>{
	protected IProcessor<Object[], Object> invoker;

	public void setInvoker(IProcessor<Object[], Object> invoker) {
		this.invoker = invoker;
	}

	@Override
	public V find(K... paras) {
		return (V) invoker.process(new Object[]{"find", paras});
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		return (Collection<K[]>) invoker.process(new Object[]{"keys", paras});
	}

	@Override
	public Map<K[], V> all(K... paras) {
		return (Map<K[], V>) invoker.process(new Object[]{"all", paras});
	}

	@Override
	public R perform(K first, V second) {
		return (R)invoker.process(new Object[]{"perform", new Object[]{first, second}});
	}
	
	@Override
	public R operate(T first, K second, V third) {
		return (R)invoker.process(new Object[]{"operate", new Object[]{first, second, third}});
	}

	@Override
	public R process(T object) {
		return (R)invoker.process(new Object[]{"process", object});
	}

	@Override
	public <P> P store(V value, K... paras) {
		return (P) invoker.process(new Object[]{"store", new Object[]{value, paras}});
	}

	@Override
	public <P> P discard(K... paras) {
		return (P) invoker.process(new Object[]{"delete", paras});
	}

	@Override
	public <P> P empty(K... paras) {
		return (P) invoker.process(new Object[]{"empty", paras});
	}

	@Override
	public R execute(IProcessor<U, R> processor) {
		return (R) invoker.process(new Object[]{"execute", processor});
	}

	@Override
	public <N> N extend(T method) {
		return (N)invoker.process(new Object[]{"extend", method});
	}

	@Override
	public Collection<T> methods() {
		return (Collection<T>)invoker.process(new Object[]{"methods", null});
	}


	@Override
	public <L> L present(Class<L> clazz) {
		return (L)invoker.process(new Object[]{"present", clazz});
	}
	
	@Override
	public <O> O realObject() {
		return (O)invoker.process(new Object[]{"realObject", null});
	}
	
	public static class MatchProcessor implements IProcessor<Object[], Object>{
		protected Map<Object, IProcessor<Object, Object>> matchers;
		protected IProcessor<Object, Object> defaultMatcher;
		protected Boolean passMethod = false;
		
		public void setPassMethod(Boolean passMethod) {
			if(passMethod != null)this.passMethod = passMethod;
		}

		public void setMatchers(Map<Object, IProcessor<Object, Object>> matchers) {
			this.matchers = matchers;
		}

		public void setDefaultMatcher(IProcessor<Object, Object> defaultMatcher) {
			this.defaultMatcher = defaultMatcher;
		}

		@Override
		public Object process(Object[] instance) {
			if(instance != null && instance.length > 1){
				Object ret = null;
				if(matchers != null && matchers.containsKey(instance[0])) {
					if(passMethod) ret = matchers.get(instance[0]).process(instance);
					else ret = matchers.get(instance[0]).process(instance[1]);
				}else if(defaultMatcher != null) if(passMethod)ret = defaultMatcher.process(instance);
				else ret = defaultMatcher.process(instance[1]);
				return ret;
			}
			return null;
		}		
	}
	
	public static class PrefixProcessor implements IProcessor<Object[], Object[]>{
		protected Map<Object, List<Object>> prefixs;
		protected List<Object> defaultPrefix;
		protected Boolean origin = true;
		
		public void setOrigin(Boolean origin) {
			if(origin != null) this.origin = origin;
		}
		
		public void setPrefixs(Map<Object, List<Object>> prefixs) {
			this.prefixs = prefixs;
		}

		public void setDefaultPrefix(List<Object> defaultPrefix) {
			this.defaultPrefix = defaultPrefix;
		}

		@Override
		public Object[] process(Object[] instance) {
			if(instance != null && instance.length > 0){
				List<Object> prefix = defaultPrefix;
				if(prefixs != null && prefixs.get(instance[0]) != null) prefix = prefixs.get(instance[0]);
				if(prefix != null){
					Object[] ret;
					if(origin) ret = new Object[instance.length + prefix.size()];
					else ret = new Object[instance.length + prefix.size() - 1];
					int i = 0;
					for(; i < prefix.size(); i++) ret[i] = prefix.get(i);
					int j = origin ? 0 : 1;
					for(; j < instance.length; j++, i++) ret[i] = instance[j];
					return ret;
				} else if(origin) return instance;
				else{
					Object[] ret = new Object[instance.length - 1];
					for(int i = 0; i < ret.length; i++) ret[i] = instance[i + 1];
					return ret;
				}
			}
			return null;
		}		
	}
}
