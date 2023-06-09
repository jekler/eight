package net.yeeyaa.eight;

public interface IOutputResource<K, V> {
	public <P> P store(V value, K ... paras); 
	public <P> P discard(K ... paras);
	public <P> P empty(K ... paras);
}
