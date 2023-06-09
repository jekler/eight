package net.yeeyaa.eight;

import java.util.Collection;
import java.util.Map;

public interface IListable<K, V> {
	public Collection<K[]> keys(K ... paras); 
	public Map<K[], V> all(K ... paras); 


}
