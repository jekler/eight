package net.yeeyaa.eight.core.processor;

import java.util.Arrays;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IProcessor;


public class InputProcessor<K, V> implements IProcessor<K, V> {
	protected IInputResource<K, V> resource;
	protected K[] paras;
	protected IProcessor<K, V> nullProcessor;

	public void setNullProcessor(IProcessor<K, V> nullProcessor) {
		this.nullProcessor = nullProcessor;
	}

	public void setResource(IInputResource<K, V> resource) {
		this.resource = resource;
	}

	public void setParas(K[] paras) {
		this.paras = paras;
	}

	@Override
	public V process(K in) {	
		V ret = null;
		if (in == null) ret = resource.find(paras);
		else if(paras == null || paras.length == 0) if (in instanceof Object[]) ret = resource.find((K[])in);
		else ret = resource.find(in);
		else if (in instanceof Object[]) {
			K[] temp = Arrays.copyOf(paras, paras.length + ((Object[])in).length);
			for (int i = 0; i < ((Object[])in).length; i++) temp[paras.length + i] = (K)((Object[])in)[i];
			ret = resource.find(temp);
		} else {
			K[] temp = Arrays.copyOf(paras, paras.length + 1);
			temp[paras.length + 1] = in;
			ret = resource.find(temp);
		}
		if (ret == null && nullProcessor != null) return nullProcessor.process(in);
		else return ret;
	}
}
