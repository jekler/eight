package net.yeeyaa.eight.core.resource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IListable;
import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.core.util.PlatformUtil;


public class ProcessorPResource<K, V, U extends IResource<K, V>> extends ProxyResource<U> implements IResource<K, V> {
	protected IProcessor<V, V> in;
	protected IProcessor<V, V> out;
	protected IProcessor<Object[], K[]> paraConvertor;

	public void setIn(IProcessor<V, V> in) {
		this.in = in;
	}

	public void setOut(IProcessor<V, V> out) {
		this.out = out;
	}

	public void setParaConvertor(IProcessor<Object[], K[]> paraConvertor) {
		this.paraConvertor = paraConvertor;
	}

	@Override
	public V find(K ... paras) {
		if(paraConvertor != null) paras = paraConvertor.process(new Object[]{"find", paras});
		V ret = resource.find(paras);
		if(in != null) ret = in.process(ret);
		return ret;
	}

	@Override
	public <P> P store(V value, K ... paras) {
		if(paraConvertor != null) paras = paraConvertor.process(new Object[]{"store", paras});
		if(out != null) value = out.process(value);
		return resource.<P>store(value, paras);
	}

	@Override
	public <P> P discard(K ... paras) {
		if(paraConvertor != null) paras = paraConvertor.process(new Object[]{"discard", paras});
		return resource.discard(paras);
	}

	@Override
	public <P> P empty(K... paras) {
		if(resource instanceof IOutputResource) {
			if(paraConvertor != null) paras = paraConvertor.process(new Object[]{"empty", paras});
			return ((IOutputResource<K, V>)resource).<P>empty(paras);
		}
		return null;
	}

	public static class Listable<K, V, U extends IListable<K, V>> extends ProxyResource<U> implements IListable<K, V>{
		protected IProcessor<Object[], K[]> paraConvertor;
		protected IProcessor<V, V> in;
		
		public void setIn(IProcessor<V, V> in) {
			this.in = in;
		}

		public void setParaConvertor(IProcessor<Object[], K[]> paraConvertor) {
			this.paraConvertor = paraConvertor;
		}
		
		@Override
		public Collection<K[]> keys(K... paras) {
			if(paraConvertor != null) paras = paraConvertor.process(new Object[]{"keys", paras});
			return resource.keys(paras);
		}

		@Override
		public Map<K[], V> all(K... paras) {
			if(paraConvertor != null) paras = paraConvertor.process(new Object[]{"all", paras});
			Map<K[], V> map = resource.all(paras);
			if (in != null) for (Entry<K[], V> entry : map.entrySet()) entry.setValue(in.process(entry.getValue()));
			return map;
		}
	}
	
	public static class MatchProcessor<K> implements IProcessor<Object[], K[]>{
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
		public K[] process(Object[] instance) {
			if(instance != null && instance.length > 1){
				Object ret = null;
				if(matchers != null && matchers.containsKey(instance[0])) {
					if(passMethod) ret = matchers.get(instance[0]).process(instance);
					else ret = matchers.get(instance[0]).process(instance[1]);
				}else if(defaultMatcher != null) if(passMethod)ret = defaultMatcher.process(instance);
				else ret = defaultMatcher.process(instance[1]);
				else ret = instance[1];
				if(ret != null) if(ret.getClass().isArray()) return (K[]) ret;
				else{
					Object[] a = PlatformUtil.newArrayOf(1, ret);
					a[0] = ret;
					return (K[])a;
				}
			}
			return null;
		}
	}
	
	public static class PrefixProcessor<K> implements IProcessor<Object[], K[]>{
		protected Map<Object, List<K>> prefixs;
		protected List<K> defaultPrefix;
		protected Boolean origin = false;
		
		public void setOrigin(Boolean origin) {
			if(origin != null) this.origin = origin;
		}
		
		public void setPrefixs(Map<Object, List<K>> prefixs) {
			this.prefixs = prefixs;
		}

		public void setDefaultPrefix(List<K> defaultPrefix) {
			this.defaultPrefix = defaultPrefix;
		}

		@Override
		public K[] process(Object[] instance) {
			if(instance != null && instance.length > 0){
				List<K> prefix = defaultPrefix;
				if(prefixs != null && prefixs.get(instance[0]) != null) prefix = prefixs.get(instance[0]);
				if(prefix != null){
					K[] paras;
					if(instance.length == 1) paras = (K[])new Object[0];
					else if(instance[1] == null) paras = (K[]) new Object[]{null};
					else if(instance[1] instanceof Object[]) paras = (K[]) instance[1];
					else if(origin) {
						paras = (K[]) PlatformUtil.newArrayOf(1, instance[1]);
						paras[0] = (K)instance[1];
					}else paras = (K[]) new Object[]{instance[1]};
					List<K> ls  = new ArrayList<K>(prefix.size() + paras.length);
					ls.addAll(prefix);
					ls.addAll(Arrays.asList(paras));
					if(origin) return ls.toArray(PlatformUtil.newArrayOf(paras, ls.size()));
					else return (K[]) ls.toArray();
				} else if(instance.length > 1) if(instance[1] == null) return (K[]) new Object[]{null};
				else if(instance[1] instanceof Object[]) return (K[]) instance[1];
				else if(origin) {
					K[] paras = (K[]) PlatformUtil.newArrayOf(1, instance[1]);
					paras[0] = (K)instance[1];
					return paras;
				}else return (K[]) new Object[]{instance[1]};
				else return (K[]) new Object[0];
			}
			return null;
		}		
	}
}
