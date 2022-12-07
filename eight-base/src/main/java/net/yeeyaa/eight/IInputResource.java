package net.yeeyaa.eight;

public interface IInputResource<K, V> {
	public V find(K ... paras);
	//public <T, R> IProcessor<T, R> state(K ... paras); 
	//public Long size(K ... paras);//-3:resource, -2:unknown, -1:noexist
	//public Long modified(K ... paras); //-2:unknown, -1:noexist
	//public <T, R> IProcessor<T, R> status(K ... paras); //-2:unknown, -1:noexist, 0:file instance 1:resource -- "size", "modified", "status"
}
