package net.yeeyaa.eight.client;

import java.util.concurrent.Semaphore;
import java.util.concurrent.atomic.AtomicReference;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.client.data.ClientMsg;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class SyncClient<K, V, R> implements IBiProcessor<K, V, R>, IProcessor<ClientMsg, R> {
	protected final Logger log;
	protected DockClient<K, V, R> dc;
	protected Integer batchSize = 10;
	protected Boolean auto = true;
	protected volatile int count = 0;
	
	public SyncClient() {
		this.log = LoggerFactory.getLogger(SyncClient.class);
	}

	public SyncClient(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(SyncClient.class) : log;
	}
	
	public void setBatchSize(Integer batchSize) {
		if(batchSize != null && batchSize > 1) this.batchSize = batchSize;
	}

	public void setAuto(Boolean auto) {
		if(auto != null) this.auto = auto;
	}

	public void setDc(DockClient<K, V, R> dc) {
		this.dc = dc;
	}

	public DockClient<K, V, R> getDc() {
		return dc;
	}

	@Override
	public R process(ClientMsg msg) {
		return sync((K)msg.name, msg.token, (V)msg.content);
	}

	@Override
	public R perform(K name, V parameters) {
		return sync(name, null, parameters);
	}
	
	protected R sync(K name, String token, V parameters){
		final AtomicReference<R> ret = new AtomicReference<R>();
		try {
			final Semaphore mutex = new Semaphore(0);
			dc.syncInvoke(new IProcessor<IProcessor<Void, R>, Void>(){
				@Override
				public Void process(IProcessor<Void, R> processor) {
					try{
						ret.set(processor.process(null));
					}catch(Exception e){
						log.error("SyncClient: invoke fail.", e);
					}finally{
						mutex.release();
					}
					return null;
				}		
			}, name, token, parameters);
			if(auto) {
				count ++;
				if(count >= batchSize) {
					count = 0;
					dc.process(null);
				}
			}
			mutex.acquire();
		} catch (Exception e) {
			log.error("SyncClient: invoke fail.", e);
		}
		return ret.get();
	}
	
	public void manualInvoke(){
		dc.process(null);
	}
	
	public class Mannual implements IProcessor<Object, Object>{
		@Override
		public Object process(Object instance) {
			manualInvoke();
			return instance;
		}
	}
}
