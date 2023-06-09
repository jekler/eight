package net.yeeyaa.eight.core.resource;

import java.util.Collection;
import java.util.Map;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IListable;
import net.yeeyaa.eight.IReadonlyListable;


public class GenericOResource<K, V> implements IReadonlyListable<K, V> {
	protected IInputResource<K, V> input;
	protected IListable<K, V> listable;

	public void setInput(IInputResource<K, V> input) {
		this.input = input;
	}

	public void setListable(IListable<K, V> listable) {
		this.listable = listable;
	}

	@Override
	public V find(K... paras) {
		return input.find(paras);
	}

	@Override
	public Collection<K[]> keys(K... paras) {
		return listable.keys(paras);
	}

	@Override
	public Map<K[], V> all(K... paras) {
		return listable.all(paras);
	}
}
