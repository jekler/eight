package net.yeeyaa.eight;

import net.yeeyaa.eight.IProcessor;

public interface ITransaction<K, V, U extends IResource<K, V>, R> {
	public R execute(IProcessor<U, R> processor);
}
