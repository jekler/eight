package net.yeeyaa.eight.core.resource;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.ITransaction;

public class ProxyTransaction<K, V, T extends IResource<K, V>, R> implements ITransaction<K, V, T, R> {
	protected IProcessor<T, T> resource; 
	protected ITransaction<K, V, T, R> transaction;
	
	public void setResource(IProcessor<T, T> resource) {
		this.resource = resource;
	}
	
	public void setTransaction(ITransaction<K, V, T, R> transaction) {
		this.transaction = transaction;
	}

	@Override
	public R execute(final IProcessor<T, R> processor) {
		return transaction.execute(new IProcessor<T, R>(){
			@Override
			public R process(T in) {
				return processor.process(resource == null ? in : resource.process(in));
			}		
		});
	}
}
