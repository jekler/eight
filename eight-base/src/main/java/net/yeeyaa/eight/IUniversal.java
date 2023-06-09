package net.yeeyaa.eight;

public interface IUniversal <K, V, U extends IListableResource<K, V>, T, R> extends IProcessor<T,R>, IBiProcessor<K, V, R>, ITriProcessor<T, K, V, R>, IListableTransaction<K, V, U, R>, IExtendable<T>, IThing {
	public <O> O realObject();
}
