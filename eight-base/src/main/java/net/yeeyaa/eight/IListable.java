package net.yeeyaa.eight;

import java.util.Collection;
import java.util.Map;

public interface IListable<K, V> {
	public Collection<K[]> keys(K ... paras); //keys
	public Map<K[], V> all(K ... paras); //all
	//public Long count(K ... paras);//--replace by state
	//public <T, R> IProcessor<T, R> state(K ... paras); 
}
