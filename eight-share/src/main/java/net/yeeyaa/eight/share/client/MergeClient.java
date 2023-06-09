package net.yeeyaa.eight.share.client;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;

public class MergeClient<K, V, R> implements IBiProcessor<K, V, R>, IProcessor<Object, R> {
	protected IProcessor<Object, R> service;
	protected String split = ".";
	protected Boolean select; 
	
	public void setSelect(Boolean select) {
		this.select = select;
	}

	public void setSplit(String split) {
		this.split = split;
	}

	public void setService(IProcessor<Object, R> service) {
		this.service = service;
	}

	@Override
	public R perform(K name, V content) {
		if (select == null) {
			StringBuilder sb = new StringBuilder();
			if (name != null) sb.append(name);
			if (split != null) sb.append(split);
			if (content != null) sb.append(content);
			return service.process(sb.toString());
		} else if (select) return service.process(name);
		else return service.process(content);
	}

	@Override
	public R process(Object instance) {
		return service.process(instance);
	}
}
