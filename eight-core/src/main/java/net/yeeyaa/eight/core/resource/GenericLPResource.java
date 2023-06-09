package net.yeeyaa.eight.core.resource;

import java.util.Collection;
import java.util.Map;

import net.yeeyaa.eight.IListable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;


public class GenericLPResource<K, V, U> implements IListableResource<K, V>, IProcessor<U, U> {
	protected IResource<K, V> resource;
	protected IListable<K, V> listable;

	public void setResource(IResource<K, V> resource) {
		this.resource = resource;
	}
	
	public void setListable(IListable<K, V> listable) {
		this.listable = listable;
	}

	@Override
	public U process(U instance) {
		if(resource instanceof IProcessor) ((IProcessor<U, U>)resource).process(instance);
		if(listable instanceof IProcessor) ((IProcessor<U, U>)listable).process(instance);
		return (U) this;
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
	public Collection<K[]> keys(K... paras) {
		return listable.keys(paras);
	}

	@Override
	public Map<K[], V> all(K... paras) {
		return listable.all(paras);
	}
}
