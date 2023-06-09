package net.yeeyaa.eight.ss.util;

import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;


public class SsClient<K, V, R> implements IProcessor<HttpServletRequest, IBiProcessor<K, V, R>> {
	protected IProcessor<IProcessor<Object, Object>[], IBiProcessor<K, V, R>> client;
	protected Map<String, IBiProcessor<K, V, R>> map = Collections.synchronizedMap(new WeakHashMap<String, IBiProcessor<K, V, R>>());
	protected String id;

	public void setId(String id) {
		this.id = id;
	}

	public void setClient(IProcessor<IProcessor<Object, Object>[], IBiProcessor<K, V, R>> client) {
		this.client = client;
	}

	@Override
	public IBiProcessor<K, V, R> process(HttpServletRequest request){
		if (request != null) {
			final HttpSession session = request.getSession();
			final Object identity = this.id == null ? request.getRemoteUser() : request.getAttribute(this.id);
			String id = session.getId();
			IBiProcessor<K, V, R> client = map.get(id);
			if(client == null && this.client != null) {
				client =this.client.process(new IProcessor[]{new IProcessor<Object, Object>(){
					@Override
					public Object process(Object instance) {
						return identity;
					}			
				}, new IProcessor<Object, HttpSession>(){
					@Override
					public HttpSession process(Object instance) {
						return session;
					}			
				}});
				if(id != null) map.put(id, client);
			}
			return client;
		}
		return null;
	}
}
