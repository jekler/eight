package net.yeeyaa.eight.core.resource;

import java.util.Collection;
import java.util.Map;

import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IListableTransaction;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.ITransaction;


public class GenericLTransaction<K, V, T extends IListableResource<K, V>, R> implements IListableTransaction<K, V, T, R> {
	protected IListableResource<K, V> resource;
	protected ITransaction<K, V, T, R> transaction; 
	
	public void setResource(IListableResource<K, V> resource) {
		this.resource = resource;
	}

	public void setTransaction(ITransaction<K, V, T, R> transaction) {
		this.transaction = transaction;
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
		return transaction.execute(processor);
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		return resource.keys(paras);
	}

	@Override
	public Map<K[], V> all(K... paras) {
		return resource.all(paras);
	}
}
