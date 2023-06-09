package net.yeeyaa.eight.core.resource;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;

import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IListableTransaction;
import net.yeeyaa.eight.IProcessor;


public class GenericExtendableResource<K, V, T extends IListableResource<K, V>, R> implements IListableTransaction<K, V, T, R>, IExtendable<Object> {
	protected IListableTransaction<K, V, T, R> resource;
	protected IExtendable<Object> extendible; 

	public void setResource(IListableTransaction<K, V, T, R> resource) {
		this.resource = resource;
	}

	public void setExtendible(IExtendable<Object> extendible) {
		this.extendible = extendible;
	}

	@Override
	public V find(K... paras) {
		return resource.find(paras);
	}

	@Override
	public <P> P store(V value, K... paras) {
		return resource.store(value, paras);
	}

	@Override
	public <P> P discard(K... paras) {
		return resource.discard(paras);
	}

	@Override
	public <P> P empty(K... paras) {
		return resource.empty(paras);
	}

	@Override
	public R execute(IProcessor<T, R> processor) {
		return resource.execute(processor);
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		return resource.keys(paras);
	}

	@Override
	public Map<K[], V> all(K... paras) {
		return resource.all(paras);
	}

	@Override
	public <N> N extend(Object method) {
		N ret = null;
		if (resource instanceof IExtendable) ret = ((IExtendable<Object>)resource).<N>extend(method);
		if (ret == null) ret = extendible.<N>extend(method);
		return ret;
	}

	@Override
	public Collection<Object> methods() {
		if (resource instanceof IExtendable) {
			Collection<Object> ret = new HashSet<Object>();
			ret.addAll(((IExtendable<Object>)resource).methods());
			ret.addAll(extendible.methods());
			return ret;
		} else return extendible.methods();
	}
}
