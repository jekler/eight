package net.yeeyaa.eight.core.task;

import java.io.IOException;
import java.io.Serializable;
import java.lang.reflect.Field;
import java.util.UUID;
import java.util.concurrent.ExecutorService;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.IResource;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.PlatformException.DefaultError;
import net.yeeyaa.eight.core.util.TypeConvertor;
import net.yeeyaa.eight.core.PlatformError;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class PlatformAsync implements IProcessor<String, Object>, IBiProcessor<IProcessor<Object, Object>, Object, Object>{
	protected final Logger log;
	protected static Field inheritable;
	protected static Field threadLocal;
	protected volatile long serial = System.nanoTime();
	protected long span = 7200000L;
	protected ExecutorService executor;
	protected IResource<String, Object> resource;
	protected String prefix;
	protected IProcessor<Object, Object> contextProcessor;
	protected IProcessor<Object, Object> preProcessor;
	protected IProcessor<Object, Object> postProcessor;
	protected IProcessor<Object, Object> retProcessor;
	protected IProcessor<Object[], Integer> estimator;
	protected IProcessor<UUID, String> encoder;
	protected IProcessor<String, UUID> decoder;
	protected volatile long total;
	protected volatile long finish;
	protected volatile long started;
	protected volatile long count;
	protected volatile double cost;
	static {
		try {
			inheritable = Thread.class.getDeclaredField("inheritableThreadLocals");
			inheritable.setAccessible(true);
            threadLocal = Thread.class.getDeclaredField("threadLocals");
            threadLocal.setAccessible(true);
		} catch (Exception e) {
			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
		} 
	}
	
	protected static class Null implements Serializable{}
	
	public PlatformAsync() {
		this.log = LoggerFactory.getLogger(PlatformAsync.class);
	}
	
	public PlatformAsync(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(PlatformAsync.class) : log;
	}
	
	public void setSpan(Long span) {
		if (span != null && span > 0) this.span = span;
	}

	public void setExecutor(ExecutorService executor) {
		this.executor = executor;
	}

	public void setResource(IResource<String, Object> resource) {
		this.resource = resource;
	}

	public void setPrefix(String prefix) {
		this.prefix = prefix;
	}

	public void setContextProcessor(IProcessor<Object, Object> contextProcessor) {
		this.contextProcessor = contextProcessor;
	}

	public void setPreProcessor(IProcessor<Object, Object> preProcessor) {
		this.preProcessor = preProcessor;
	}

	public void setPostProcessor(IProcessor<Object, Object> postProcessor) {
		this.postProcessor = postProcessor;
	}

	public void setRetProcessor(IProcessor<Object, Object> retProcessor) {
		this.retProcessor = retProcessor;
	}

	public void setEstimator(IProcessor<Object[], Integer> estimator) {
		this.estimator = estimator;
	}

	public void setEncoder(IProcessor<UUID, String> encoder) {
		this.encoder = encoder;
	}

	public void setDecoder(IProcessor<String, UUID> decoder) {
		this.decoder = decoder;
	}

	@Override
	public Object perform(final IProcessor<Object, Object> func, Object para) {
		if (executor == null || resource == null) return func.process(para);
		else {
			Long low;
			Integer high;
			synchronized (this) {
				low = serial++;
				total++;
			}
			if (estimator != null) high = estimator.process(new Object[]{func, para, total, finish, started, count, cost});
			else if (count > 0) high = (int) (cost * (0.5 + (double) (total - started) / count));
			else high = (int) cost;
			UUID uuid = new UUID(high + System.currentTimeMillis(), low);
			final String key = prefix == null ? encoder == null ? TypeConvertor.uuidToStr(uuid) : encoder.process(uuid) : prefix + encoder == null ? TypeConvertor.uuidToStr(uuid) : encoder.process(uuid);
			final Object context = contextProcessor == null ? para : contextProcessor.process(para);
			executor.execute(new Runnable(){
				@Override
				public void run() {
					synchronized (PlatformAsync.this) {
						started++;
						long c = started - finish;
						if (c > count) count = c;
					}
					long time = System.currentTimeMillis();
					try {
						Object o = func.process(preProcessor == null ? context : preProcessor.process(context));
						if (postProcessor != null) o = postProcessor.process(o);
						resource.store(o == null ? new Null() : o, key);
					} catch (Exception e) {
						log.error("PlatformAsync : performing error.", e);
					} finally {
						synchronized (PlatformAsync.this) {
							cost = cost / (finish + 1) * finish + (double) (System.currentTimeMillis() - time) / (finish + 1);
							finish++;
						}
					}
			}});
			throw new PlatformException(new DefaultError("AsyncError", high, key));
		}
	}

	@Override
	public Object process(String key) {
		try {
			UUID uuid = decoder == null ? TypeConvertor.strToUuid(prefix == null ? key : key.substring(prefix.length())) : decoder.process(prefix == null ? key : key.substring(prefix.length()));
			Long time = uuid.getMostSignificantBits();
			Long current = System.currentTimeMillis();
			if (time > current) throw new PlatformException(new DefaultError("AsyncErrorUnavailable", (int) (time - current), "Please wait"));
			else if (current > time + span) throw new PlatformException(new DefaultError("AsyncErrorUnavailable", -1, "Timeout"));
			else {
				Object ret = resource.find(key);
				if (ret == null) throw new PlatformException(new DefaultError("AsyncErrorUnavailable", 0, "Please wait"));
				else if (ret instanceof Null) ret = null;
				if (retProcessor != null) ret = retProcessor.process(ret);
				return ret;
			}
		} catch (Exception e) {
			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
		}

	}
	
	public static class ContextProcessor implements IProcessor<Object, Object[]> {
		protected final Logger log;
		
		public ContextProcessor() {
			this.log = LoggerFactory.getLogger(ContextProcessor.class);
		}
		
		public ContextProcessor(Logger log) {
			this.log = log == null ? LoggerFactory.getLogger(ContextProcessor.class) : log;
		}
		
		@Override
		public Object[] process(Object instance) {
            try{
	            return new Object[]{instance, inheritable.get(Thread.currentThread()), threadLocal.get(Thread.currentThread())};
            }catch(Exception e){
                log.error("PlatformPool: error when gets threalocals.", e);
            }
			return new Object[]{instance};
		}
	}
	
	public static class PreProcessor implements IProcessor<Object[], Object> {
		protected final Logger log;
		
		public PreProcessor() {
			this.log = LoggerFactory.getLogger(PreProcessor.class);
		}
		
		public PreProcessor(Logger log) {
			this.log = log == null ? LoggerFactory.getLogger(PreProcessor.class) : log;
		}
		
		@Override
		public Object process(Object[] instance) {
			if (instance == null || instance.length == 0) return null;
			else if (instance.length > 2) try{
				inheritable.set(Thread.currentThread(), instance[1]);
				threadLocal.set(Thread.currentThread(), instance[2]);
            }catch(Exception e){
                log.error("PlatformPool: error when gets threalocals.", e);
            }
			return instance[0];
		}
	}
}
