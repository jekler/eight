package net.yeeyaa.eight.core.processor;

import java.util.Arrays;

import net.yeeyaa.eight.IListable;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.util.PlatformUtil;


public class ListableProcessor<K, V> implements IProcessor<K, Object> {
	protected IListable<K, V> resource;
	protected K[] paras;
	protected Boolean map = false;
	
	public void setResource(IListable<K, V> resource) {
		this.resource = resource;
	}

	public void setParas(K[] paras) {
		this.paras = paras;
	}

	public void setMap(Boolean map) {
		if (map != null) this.map = map;
	}

	@Override
	public Object process(K in) {	
		K[] tmp = paras;
		if (in != null) if (tmp == null) if (in instanceof Object[]) tmp = (K[]) in;
		else {
			tmp = PlatformUtil.newArrayOf(1, in);
			tmp[0] = in;
		} else if (in instanceof Object[]) {
			tmp = Arrays.copyOf(paras, paras.length + ((Object[])in).length);
			for (int i = 0; i < ((Object[])in).length; i++) tmp[paras.length + i] = (K)((Object[])in)[i];
		} else {
			tmp = Arrays.copyOf(paras, paras.length + 1);
			tmp[paras.length] = in;
		}
		if (map) if (tmp == null) return resource.all();
		else return resource.all(tmp);
		else if (tmp == null) return resource.keys();
		else return resource.keys(tmp);
	}
}
