package net.yeeyaa.eight.core.resource;

import java.util.Collection;
import java.util.Map;

import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IListableTransaction;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.ITransaction;


public class SyncLTransaction<K, V, T extends IListableResource<K, V>, R> implements IListableTransaction<K, V, T, R> {
	protected IListableResource<K, V> resource;
	protected ITransaction<K, V, T, R> transaction; 
	
	public void setResource(IListableResource<K, V> resource) {
		this.resource = resource;
	}

	public void setTransaction(ITransaction<K, V, T, R> transaction) {
		this.transaction = transaction;
	}

	@Override
	public synchronized V find(K... paras) {
		return resource.find(paras);
	}

	@Override
	public synchronized <P> P store(V value, K... paras) {
		return resource.<P>store(value, paras);
	}

	@Override
	public synchronized <P> P discard(K... paras) {
		return resource.<P>discard(paras);
	}

	@Override
	public synchronized <P> P empty(K... paras) {
		return resource.<P>empty(paras);
	}

	@Override
	public synchronized R execute(IProcessor<T, R> processor) {
		return transaction.execute(processor);
	}

	@Override
	public synchronized Collection<K[]> keys(K... paras) {
		return resource.keys(paras);
	}

	@Override
	public synchronized Map<K[], V> all(K... paras) {
		return resource.all(paras);
	}
}
