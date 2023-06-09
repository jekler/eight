package net.yeeyaa.eight.core.processor;

import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IOutputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IReadonlyListable;


public class ResourceCopyProcessor<K, V> implements IProcessor<Object, Object> {
	protected IReadonlyListable<K, V> input;
	protected IOutputResource<K, V> output;

	public void setInput(IReadonlyListable<K, V> input) {
		this.input = input;
	}

	public void setOutput(IOutputResource<K, V> output) {
		this.output = output;
	}

	@Override
	public Object process(Object in) {
		if(input != null && output != null) {
			Map<K[], V> map;
			if (in == null) map = input.all();
			else if (in instanceof Object[]) map = input.all((K[])in);
			else map = input.all((K)in);
			for (Entry<K[], V> entry : map.entrySet()) output.store(entry.getValue(), entry.getKey());
		}
		return in;
	}
	
	public void initialize(){
		process(null);
	}
	
	public void destroy(){
		if(input != null && output != null) {
			for (K[] key : input.keys()) output.discard(key);
		}
	}
}
