package net.yeeyaa.eight.ss.util;

import java.lang.ref.WeakReference;
import java.util.Collections;
import java.util.Map;
import java.util.WeakHashMap;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;

public class SsClient<K, V, R> implements IProcessor<HttpServletRequest, IBiProcessor<K, V, R>> {
	protected IProcessor<IProcessor<Object, Object>[], IBiProcessor<K, V, R>> client;
	protected Map<String, ClientHolder> map = Collections.synchronizedMap(new WeakHashMap<String, ClientHolder>());
	
	public void setClient(IProcessor<IProcessor<Object, Object>[], IBiProcessor<K, V, R>> client) {
		this.client = client;
	}

	@Override
	public IBiProcessor<K, V, R> process(final HttpServletRequest request){
		if (request != null) {
			String id = request.getSession().getId();
			ClientHolder client = map.get(id);
			if(client == null && this.client != null) {
				final ClientHolder c = new ClientHolder();
				c.setClient(this.client.process(new IProcessor[]{new IProcessor<Object, Object>(){
					@Override
					public Object process(Object instance) {
						return c.getRemoteUser();
					}			
				}, new IProcessor<Object, Object>(){
					@Override
					public Object process(Object instance) {
						return c.getSession();
					}			
				}}));
				if(id != null) map.put(id, c);
				client = c;
			}
			if (client != null) {
				client.setRequest(request);
				return client.getClient();
			}
		}
		return null;
	}
	
	protected class ClientHolder {
		protected IBiProcessor<K, V, R> client;
		protected Object remoteUser;
		protected WeakReference<HttpSession> session;
		
		public IBiProcessor<K, V, R> getClient() {
			return client;
		}
		
		public void setClient(IBiProcessor<K, V, R> client) {
			this.client = client;
		}
		
		public Object getRemoteUser() {
			return remoteUser;
		}

		public HttpSession getSession() {
			if (session != null) return session.get();
			else return null;
		}

		public void setRequest(HttpServletRequest request) {
			if (request != null) {
				session = new WeakReference<HttpSession>(request.getSession());
				remoteUser = request.getRemoteUser();
			}
		}
	}
}