package net.yeeyaa.eight.client;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.PlatformException.DefaultError;
import net.yeeyaa.eight.client.data.ClientMsg;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class DockClient<K, V, R> implements IProcessor<Object, Object> {
	protected final Logger log;
	protected IBiProcessor<K, V, List<ClientMsg>> client;
	protected K name;
	protected IProcessor<ClientMsg, ClientMsg> preProcessor;		
	protected IProcessor<ClientMsg, ClientMsg> postProcessor;
	protected volatile Map<ClientMsg, IProcessor<IProcessor<Void, R>, Void>> callbacks = new ConcurrentHashMap<ClientMsg, IProcessor<IProcessor<Void, R>, Void>>();
	
	public DockClient() {
		this.log = LoggerFactory.getLogger(DockClient.class);
	}

	public DockClient(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(DockClient.class) : log;
	}
	
	public void setPreProcessor(IProcessor<ClientMsg, ClientMsg> preProcessor) {
		this.preProcessor = preProcessor;
	}

	public void setPostProcessor(IProcessor<ClientMsg, ClientMsg> postProcessor) {
		this.postProcessor = postProcessor;
	}

	public void setClient(IBiProcessor<K, V, List<ClientMsg>> client) {
		this.client = client;
	}

	public void setName(K name) {
		if(name != null) this.name = name;
	}
	
	@Override
	public Object process(Object in) {
		Map<ClientMsg, IProcessor<IProcessor<Void, R>, Void>> callbacks = null;
		synchronized(this){
			callbacks = this.callbacks;
			this.callbacks = new ConcurrentHashMap<ClientMsg, IProcessor<IProcessor<Void, R>, Void>>();
		}
		if(callbacks != null && callbacks.size() > 0){
			Map<String, IProcessor<IProcessor<Void, R>, Void>> tmp = new HashMap<String, IProcessor<IProcessor<Void, R>, Void>>();
			List<ClientMsg> msgls = new ArrayList<ClientMsg>(callbacks.size());
			Integer i = 0;
			for(Entry<ClientMsg, IProcessor<IProcessor<Void, R>, Void>> entry : callbacks.entrySet()){
				ClientMsg msg = entry.getKey();
				if(preProcessor != null) msg = preProcessor.process(msg);
				if (msg.id == null) msg.id = (i++).toString();
				msgls.add(msg);
				tmp.put(msg.id, entry.getValue());
			}
			try{
				msgls = client.perform(name, (V)msgls);
			}catch(Exception e){
				msgls = null;
				log.error("DockClient: invoke fail.", e);
			}
			if(msgls != null) for(final ClientMsg msg : msgls) if(tmp.get(msg.id) != null) try{
				tmp.remove(msg.id).process(new IProcessor<Void, R>(){
					@Override
					public R process(Void instance) {
						ClientMsg m;
						if(postProcessor != null) m = postProcessor.process(msg);
						else m = msg;
						if (m == null) return null;
						else {
							if(m.error != null) throw new PlatformException(m.error);
							else return (R) m.content;
						}
					}		
			});}catch(Exception e){
				log.error("DockClient: invoke fail.", e);
			}
			final DefaultError error = new DefaultError("ClientError", 100, "UNEXPECTED_CLIENT_ERROR");
			for(IProcessor<IProcessor<Void, R>, Void> processor : tmp.values()) try{
				processor.process(new IProcessor<Void, R>(){
					@Override
					public R process(Void instance) {
						throw new PlatformException(error);
					}			
			});}catch(Exception e){
				log.error("DockClient: invoke fail.", e);
			}
		}
		return in;
	}
	
	public void syncInvoke(IProcessor<IProcessor<Void, R>, Void> callback, K name, V parameters){
		syncInvoke(callback, name, null, parameters);
	}
	
	public void syncInvoke(IProcessor<IProcessor<Void, R>, Void> callback, K name, String token, V parameters){
		ClientMsg msg = new ClientMsg();
		msg.name = name == null ? null : name.toString();
		msg.token = token;
		msg.content = parameters;
		callbacks.put(msg, callback);
	}	
}
