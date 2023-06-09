package net.yeeyaa.eight.core.resource;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.ITransaction;
import net.yeeyaa.eight.ITransactionResource;

public class GenericTResource<K, V, T extends IResource<K, V>, R> implements ITransactionResource<K, V, T, R> {
	protected IResource<K, V> resource;
	protected ITransaction<K, V, T, R> transaction; 
	
	public void setResource(IResource<K, V> resource) {
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
}
