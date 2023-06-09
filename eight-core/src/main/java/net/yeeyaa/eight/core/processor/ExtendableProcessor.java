package net.yeeyaa.eight.core.processor;

import java.util.Collection;
import java.util.Map;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IProcessor;

public class ExtendableProcessor<M, K, V, R> implements IExtendable<M>, IBiProcessor<K, V, R> {
	protected Map<Object, IProcessor<Object, Object>> map;
	
	public void setMap(Map<Object, IProcessor<Object, Object>> map) {
		this.map = map;
	}

	@Override
	public <N> N extend(M method) {
		return (N)map.get(method);
	}

	@Override
	public Collection<M> methods() {
		return (Collection<M>)map.keySet();
	}

	@Override
	public R perform(K first, V second) {
		IProcessor<Object, Object> processor = map.get(first);
		if (processor != null) return (R)processor.process(second);
		return null;
	}
}
