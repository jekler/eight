package net.yeeyaa.eight.core;

import java.lang.ref.WeakReference;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.UUID;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.util.ConcurrentWeakIdentityHashMap;
import net.yeeyaa.eight.core.util.Content.Couple;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class PlatformPool implements IProcessor<Object, Object>, ITriProcessor<Object, Object, Object, Object>, ScheduledExecutorService, RejectedExecutionHandler{
	protected static volatile PlatformPool instance;
	protected static Field inheritable;
	protected static Field threadLocal;
	protected static volatile Field tableField;
	protected static volatile Field valueField;	
	protected static Method create;
	protected final Logger log;

	static {
		try {
            threadLocal = Thread.class.getDeclaredField("threadLocals");
            threadLocal.setAccessible(true);
			inheritable = Thread.class.getDeclaredField("inheritableThreadLocals");
			inheritable.setAccessible(true);
			create = ThreadLocal.class.getDeclaredMethod("createInheritedMap", inheritable.getType());
			create.setAccessible(true);
		} catch (Exception e) {}
	}
	
	public PlatformPool() {
		this.log = LoggerFactory.getLogger(PlatformPool.class);
	}
	
	public PlatformPool(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(PlatformPool.class) : log;
	}
	
	protected class LocalRunnable implements Runnable, IBiProcessor<Object, Object, Object> {
		protected Object locals;
        protected Runnable task;
        protected Boolean mode;
        protected UUID id;
        
		public LocalRunnable(Runnable task, Boolean mode) {
            this.task = task;
            this.mode = mode;
            this.id = UUID.randomUUID();
            if (cache != null && (cache & 2) > 0 && (task instanceof IBiProcessor || (cache & 1) > 0)) 
            	tasks.put(this.id, new WeakReference<Entry<IBiProcessor<Object, Object, Object>, Thread>>(new Couple<IBiProcessor<Object, Object, Object>, Thread>(this, null)));
            if (inheritable != null && !Boolean.FALSE.equals(mode)) try {
            	Object o = inheritable.get(Thread.currentThread());
	            if (o != null) locals = create.invoke(null, o);
            }catch(Exception e){
                log.debug("PlatformPool: error when gets threadlocals.", e);
            }
        }
        
		@Override
		public void run() {
			if (cache != null && (task instanceof IBiProcessor || (cache & 1) > 0)) 
				tasks.put(this.id, new WeakReference<Entry<IBiProcessor<Object, Object, Object>, Thread>>(new Couple<IBiProcessor<Object, Object, Object>, Thread>(this, Thread.currentThread())));
			if (inheritable != null && !Boolean.FALSE.equals(mode)) try{
				inheritable.set(Thread.currentThread(), locals);
			}catch(Exception e){
                log.debug("PlatformPool: error when gets threadlocals.", e);
            }
			if (Boolean.TRUE.equals(mode)) try {
				task.run();
			} finally {
				if (cache == null || (cache & 4) == 0) tasks.remove(id);
			} else try {
				task.run();
			} catch (Exception e) {
				log.error("PlatformPool: perform fail.", e);
			} finally {
				if (cache == null || (cache & 4) == 0) tasks.remove(id);
			} 
		}

		@Override
		public Object perform(Object first, Object second) {
			if (task instanceof IBiProcessor) return ((IBiProcessor<Object, Object, Object>) task).perform(first, second);
			else if (first == null && second == null) return toString();
			else throw new PlatformException(PlatformError.ERROR_NOT_SUPPORT);
		}

		@Override
		public String toString() {
			return "LocalRunnable [locals=" + locals + ", task=" + task	+ ", mode=" + mode + "]";
		}
	}

	protected class LocalCallable<T> implements Callable<T>, IBiProcessor<Object, Object, Object> {
		protected Object locals;
        protected Callable<T> task;
        protected Boolean mode;
        protected UUID id;
        
        public LocalCallable(Callable<T> task, Boolean mode) {
            this.task = task;
            this.mode = mode;
            this.id = UUID.randomUUID();
            if (cache != null && (cache & 2) > 0 && (task instanceof IBiProcessor || (cache & 1) > 0)) 
            	tasks.put(this.id, new WeakReference<Entry<IBiProcessor<Object, Object, Object>, Thread>>(new Couple<IBiProcessor<Object, Object, Object>, Thread>(this, null)));
            if (inheritable != null && !Boolean.FALSE.equals(mode)) try{
            	Object o = inheritable.get(Thread.currentThread());
	            if (o != null) locals = create.invoke(null, o);
            } catch(Exception e){
                log.debug("PlatformPool: error when gets threadlocals.", e);
            }
        }
        
		@Override
		public T call() throws Exception {
			if (cache != null && (task instanceof IBiProcessor || (cache & 1) > 0)) 
				tasks.put(this.id, new WeakReference<Entry<IBiProcessor<Object, Object, Object>, Thread>>(new Couple<IBiProcessor<Object, Object, Object>, Thread>(this, Thread.currentThread())));
			if (inheritable != null && !Boolean.FALSE.equals(mode)) try{
				inheritable.set(Thread.currentThread(), locals);
			}catch(Exception e){
                log.debug("PlatformPool: error when gets threadlocals.", e);
            }
			if (Boolean.TRUE.equals(mode)) try {
				return task.call();
			} finally {
				if (cache == null || (cache & 4) == 0) tasks.remove(id);
			} else try {
				return task.call();
			} catch (Exception e) {
				log.error("PlatformPool: perform fail.", e);
				return null;
			} finally {
				if (cache == null || (cache & 4) == 0) tasks.remove(id);
			}
		}

		@Override
		public Object perform(Object first, Object second) {
			if (task instanceof IBiProcessor) return ((IBiProcessor<Object, Object, Object>) task).perform(first, second);
			else if (first == null && second == null) return toString();
			else throw new PlatformException(PlatformError.ERROR_NOT_SUPPORT);
		}
		
		@Override
		public String toString() {
			return "LocalCallable [locals=" + locals + ", task=" + task + ", mode=" + mode + "]";
		}
	}
	
	protected final class ThreadExecutor extends ThreadPoolExecutor{
		protected Boolean mode;
		
		public ThreadExecutor(int corePoolSize, int maximumPoolSize, long keepAliveTime, TimeUnit unit, BlockingQueue<Runnable> workQueue, 
				ThreadFactory threadFactory, RejectedExecutionHandler handler, Boolean mode) {
			super(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue, threadFactory, handler);
			if(mode != null) this.mode = mode;
		}

		@Override
		public void execute(Runnable task) {
			super.execute(new LocalRunnable(task, mode));
		}

		@Override
		protected void afterExecute(Runnable r, Throwable t) {
			super.afterExecute(r, t);
			if (inheritable != null && !Boolean.FALSE.equals(mode)) try {
	        	inheritable.set(Thread.currentThread(), null);
	            threadLocal.set(Thread.currentThread(), null);
	        } catch(Exception e) {
	            log.debug("PlatformPool: error when gets threadlocals.", e);
	        }
		}
	}
	
	protected final class ScheduledThreadExecutor extends ScheduledThreadPoolExecutor{
		protected Boolean mode;
		
		public ScheduledThreadExecutor(int corePoolSize, ThreadFactory threadFactory, RejectedExecutionHandler handler, Boolean mode) {
			super(corePoolSize, threadFactory, handler);
			if(mode != null) this.mode = mode;
		}
		
		@Override
		public <V> ScheduledFuture<V> schedule(Callable<V> task, long delay, TimeUnit unit) {
			if (task == null || unit == null) throw new NullPointerException();
			return super.schedule(new LocalCallable<V>(task, mode), delay, unit);
		}

		@Override
		public ScheduledFuture<?> schedule(Runnable task, long delay, TimeUnit unit) {
			if (task == null || unit == null) throw new NullPointerException();
			return super.schedule(new LocalRunnable(task, mode), delay, unit);
		}

		@Override
		public ScheduledFuture<?> scheduleAtFixedRate(Runnable task, long initialDelay, long period, TimeUnit unit) {
			if (task == null || unit == null) throw new NullPointerException();
			return super.scheduleAtFixedRate(new LocalRunnable(task, mode), initialDelay, period, unit);
		}

		@Override
		public ScheduledFuture<?> scheduleWithFixedDelay(Runnable task, long initialDelay, long delay, TimeUnit unit) {
			if (task == null || unit == null) throw new NullPointerException();
			return super.scheduleWithFixedDelay(new LocalRunnable(task, mode), initialDelay, delay, unit);
		}

		@Override
		protected void afterExecute(Runnable r, Throwable t) {
			super.afterExecute(r, t);
			if (inheritable != null && !Boolean.FALSE.equals(mode)) try {
	        	inheritable.set(Thread.currentThread(), null);
	            threadLocal.set(Thread.currentThread(), null);
	        } catch(Exception e) {
	            log.debug("PlatformPool: error when gets threadlocals.", e);
	        }
		}	
	}
	
	public static PlatformPool getInstance() {
		if (instance == null) synchronized(PlatformPool.class) {
			if (instance == null) instance = new PlatformPool(null);
		}
		return instance;
	}
	
	public static ScheduledExecutorService getScheduledPool(int corePoolSize, ThreadFactory threadFactory, RejectedExecutionHandler handler, Boolean ignoreError) {
		return getInstance().new ScheduledThreadExecutor(corePoolSize, threadFactory, handler, ignoreError);
	}
	
	public static ScheduledExecutorService getScheduledPool(int corePoolSize, Boolean ignoreError) {
		return getInstance().new ScheduledThreadExecutor(corePoolSize, Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), ignoreError);
	}

	public static ScheduledExecutorService getScheduledPool(int corePoolSize) {
		return getInstance().new ScheduledThreadExecutor(corePoolSize, Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), false);
	}
	
	public static ExecutorService getPool(int corePoolSize, int maximumPoolSize, long keepAliveTime, TimeUnit unit, BlockingQueue<Runnable> workQueue, 
			ThreadFactory threadFactory, RejectedExecutionHandler handler, Boolean ignoreError, Boolean allowCoreThreadTimeOut) {
		ThreadExecutor exec = getInstance().new ThreadExecutor(corePoolSize, maximumPoolSize, keepAliveTime, unit, workQueue, threadFactory, handler, ignoreError);
		exec.allowCoreThreadTimeOut(allowCoreThreadTimeOut);
		return exec;
	}	

	public static ExecutorService getPool(int corePoolSize, int maximumPoolSize, long keepAliveTime, int queue, Object comparator, ThreadFactory threadFactory, RejectedExecutionHandler handler, Boolean ignoreError, Boolean allowCoreThreadTimeOut) {
		ThreadExecutor exec = getInstance().new ThreadExecutor(corePoolSize, maximumPoolSize, keepAliveTime, TimeUnit.NANOSECONDS, queue == 0 ? new SynchronousQueue<Runnable>() : queue < 0 ? new LinkedBlockingQueue<Runnable>(-queue) : comparator == null ? new ArrayBlockingQueue<Runnable>(queue) 
				: new PriorityBlockingQueue<Runnable>(queue, comparator instanceof Comparator ? (Comparator<Runnable>) comparator : null), threadFactory == null ? Executors.defaultThreadFactory() : threadFactory, handler == null ? new ThreadPoolExecutor.AbortPolicy() : handler, ignoreError);
		exec.allowCoreThreadTimeOut(allowCoreThreadTimeOut);
		return exec;
	}	

	public static ExecutorService getPool(int size) {
		ThreadExecutor exec = getInstance().new ThreadExecutor(size, size, 60000000000L, TimeUnit.NANOSECONDS, new  LinkedBlockingQueue<Runnable>(), Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), false);
		exec.allowCoreThreadTimeOut(true);
		return exec;
	}
	
	public static ExecutorService getPool(int size, Boolean ignoreError) {
		ThreadExecutor exec = getInstance().new ThreadExecutor(size, size, 60000000000L, TimeUnit.NANOSECONDS, new  LinkedBlockingQueue<Runnable>(), Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), ignoreError);
		exec.allowCoreThreadTimeOut(true);
		return exec;
	}

	public static ExecutorService getPool(int size, long keepAliveTime, Boolean ignoreError) {
		ThreadExecutor exec =  getInstance().new ThreadExecutor(size, size, keepAliveTime, TimeUnit.NANOSECONDS, new  LinkedBlockingQueue<Runnable>(), Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), ignoreError);
		exec.allowCoreThreadTimeOut(true);
		return exec;
	}

	public static ExecutorService getLimitPool(int size, int max) {
		ThreadExecutor exec = getInstance().new ThreadExecutor(size, max, 60000000000L, TimeUnit.NANOSECONDS, new  SynchronousQueue<Runnable>(), Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), false);
		return exec;
	}
	
	public static ExecutorService getLimitPool(int size, int max, Boolean ignoreError) {
		ThreadExecutor exec = getInstance().new ThreadExecutor(size, max, 60000000000L, TimeUnit.NANOSECONDS, new  SynchronousQueue<Runnable>(), Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), ignoreError);
		return exec;
	}

	public static ExecutorService getLimitPool(int size, int max, long keepAliveTime, Boolean ignoreError) {
		ThreadExecutor exec =  getInstance().new ThreadExecutor(size, size, keepAliveTime, TimeUnit.NANOSECONDS, new  SynchronousQueue<Runnable>(), Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), ignoreError);
		return exec;
	}
	
	public static ExecutorService getQueuePool(int size, int queue) {
		ThreadExecutor exec = getInstance().new ThreadExecutor(size, size, 60000000000L, TimeUnit.NANOSECONDS, new ArrayBlockingQueue<Runnable>(queue), Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), false);
		exec.allowCoreThreadTimeOut(true);
		return exec;
	}
	
	public static ExecutorService getQueuePool(int size, int queue, Boolean ignoreError) {
		ThreadExecutor exec = getInstance().new ThreadExecutor(size, size, 60000000000L, TimeUnit.NANOSECONDS, new ArrayBlockingQueue<Runnable>(queue), Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), ignoreError);
		exec.allowCoreThreadTimeOut(true);
		return exec;
	}

	public static ExecutorService getQueuePool(int size, int queue, long keepAliveTime, Boolean ignoreError) {
		ThreadExecutor exec =  getInstance().new ThreadExecutor(size, size, keepAliveTime, TimeUnit.NANOSECONDS, new ArrayBlockingQueue<Runnable>(queue), Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), ignoreError);
		exec.allowCoreThreadTimeOut(true);
		return exec;
	}
	
	public static ExecutorService getFixQueuePool(int size, int queue) {
		ThreadExecutor exec = getInstance().new ThreadExecutor(size, size, 0L, TimeUnit.NANOSECONDS, new ArrayBlockingQueue<Runnable>(queue), Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), false);
		return exec;
	}
	
	public static ExecutorService getFixQueuePool(int size, int queue, Boolean ignoreError) {
		ThreadExecutor exec = getInstance().new ThreadExecutor(size, size, 0L, TimeUnit.NANOSECONDS, new ArrayBlockingQueue<Runnable>(queue), Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), ignoreError);
		return exec;
	}
	
	public static ExecutorService getFixedPool(int size) {
		return getInstance().new ThreadExecutor(size, size, 0L, TimeUnit.NANOSECONDS, new  LinkedBlockingQueue<Runnable>(), Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), false);
	}	
	
	public static ExecutorService getFixedPool(int size, Boolean ignoreError) {
		return getInstance().new ThreadExecutor(size, size, 0L, TimeUnit.NANOSECONDS, new  LinkedBlockingQueue<Runnable>(), Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), ignoreError);
	}	

	public static ExecutorService getCachedPool() {
		return getInstance().new ThreadExecutor(0, Integer.MAX_VALUE, 60000000000L, TimeUnit.NANOSECONDS,  new SynchronousQueue<Runnable>(), Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), false);
	}
	
	public static ExecutorService getCachedPool(Boolean ignoreError) {
		return getInstance().new ThreadExecutor(0, Integer.MAX_VALUE, 60000000000L, TimeUnit.NANOSECONDS,  new SynchronousQueue<Runnable>(), Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), ignoreError);
	}
	
	public static ExecutorService getCachedPool(long keepAliveTime, Boolean ignoreError) {
		return getInstance().new ThreadExecutor(0, Integer.MAX_VALUE, keepAliveTime, TimeUnit.NANOSECONDS,  new SynchronousQueue<Runnable>(), Executors.defaultThreadFactory(), new ThreadPoolExecutor.AbortPolicy(), ignoreError);
	}
	
	protected volatile Map<UUID, WeakReference<Entry<IBiProcessor<Object, Object, Object>, Thread>>> tasks = new ConcurrentWeakIdentityHashMap<UUID, WeakReference<Entry<IBiProcessor<Object, Object, Object>, Thread>>>(false);
	protected volatile ExecutorService executor;
	protected RejectedExecutionHandler handler;
	protected Object comparator;
	protected Integer size;
	protected Integer max;
	protected Integer queue = 0x80000001; 
	protected Boolean mode;
	protected Boolean timeout = false; 
	protected Long alive = 60000000000L;
	protected Boolean now = false; 
	protected Boolean chain = true; 
	protected Boolean type; 
	protected volatile Integer cache;
	
	public void setCache(Integer cache) {
		this.cache = cache;
	}

	public void setComparator(Object comparator) {
		this.comparator = comparator;
	}

	public void setType(Boolean type) {
		this.type = type;
	}

	public void setChain(Boolean chain) {
		if (chain != null) this.chain = chain;
	}

	public void setNow(Boolean now) {
		if (now != null) this.now = now;
	}

	public void setSize(Integer size) {
		if (size != null && size >= 0) this.size = size;
	}

	public void setMax(Integer max) {
		if (max != null && max >= 0) this.max = max;
	}

	public void setHandler(RejectedExecutionHandler handler) {
		this.handler = handler;
	}

	public void setQueue(Integer queue) {
		if (queue != null) this.queue = queue;
		if (this.queue == 0x80000000) this.queue = 0x80000001;
	}

	public void setMode(Boolean mode) {
		if (mode != null) this.mode = mode;
	}

	public void setTimeout(Boolean timeout) {
		if (timeout != null) this.timeout = timeout;
	}

	public void setAlive(Long alive) {
		if (alive != null && alive >= 0) this.alive = alive;
	}

	@PostConstruct
	public void initialize() {
		if(size == null) size = Runtime.getRuntime().availableProcessors();
		if (max == null || max < size) max = size;
		if(executor != null) if (now) executor.shutdownNow();
		else executor.shutdown();
		if (Boolean.FALSE.equals(type)) executor = new ScheduledThreadExecutor(size, Executors.defaultThreadFactory(),  handler == null ? new ThreadPoolExecutor.AbortPolicy() : handler, mode);
		else {
			ThreadExecutor exec = new ThreadExecutor(size, max, alive, TimeUnit.NANOSECONDS, queue == 0 ? new SynchronousQueue<Runnable>() : queue < 0 ? new LinkedBlockingQueue<Runnable>(-queue) : comparator == null ? new ArrayBlockingQueue<Runnable>(queue) :
				new PriorityBlockingQueue<Runnable>(queue, comparator instanceof Comparator ? (Comparator<Runnable>) comparator : null), Executors.defaultThreadFactory(), handler == null ? new ThreadPoolExecutor.AbortPolicy() : handler, mode);
			exec.allowCoreThreadTimeOut(timeout);
			executor = exec;
		}
	}

	@PreDestroy
	public synchronized void destroy() throws Throwable {
		try {
			if (chain && handler instanceof ExecutorService) {
				if (now) ((ExecutorService)handler).shutdownNow();
				else ((ExecutorService)handler).shutdown();
				handler = null;
			}
		} finally {
			if (executor != null) {
				if (now) executor.shutdownNow();
				else executor.shutdown();
				executor = null;
			}
		}
	}
	
	public Object process(Object in){
		return this;
	}

	@Override
	public void rejectedExecution(Runnable r, ThreadPoolExecutor exec) {
		executor.execute(r);
	}
	
	@Override
	public void execute(Runnable task) {
		executor.execute(task);
	}

	@Override
	public void shutdown() {
		try {
			tasks = new ConcurrentWeakIdentityHashMap<UUID, WeakReference<Entry<IBiProcessor<Object, Object, Object>, Thread>>>(false);
			if (chain && handler instanceof ExecutorService) ((ExecutorService) handler).shutdown();
		} finally {
			executor.shutdown();
		}
	}

	@Override
	public List<Runnable> shutdownNow() {
		List<Runnable> ls = null;
		try {
			tasks = new ConcurrentWeakIdentityHashMap<UUID, WeakReference<Entry<IBiProcessor<Object, Object, Object>, Thread>>>(false);
			if (chain && handler instanceof ExecutorService) ls = ((ExecutorService) handler).shutdownNow();
		} finally {
			if (ls == null) ls = executor.shutdownNow();
			else ls.addAll(executor.shutdownNow());
		}
		return ls;
	}

	@Override
	public boolean isShutdown() {
		return executor.isShutdown();
	}

	@Override
	public boolean isTerminated() {
		return executor.isTerminated();
	}

	@Override
	public boolean awaitTermination(long timeout, TimeUnit unit) throws InterruptedException {
		return executor.awaitTermination(timeout, unit);
	}

	@Override
	public <T> Future<T> submit(Callable<T> task) {
		return executor.submit(task);
	}

	@Override
	public <T> Future<T> submit(Runnable task, T result) {
		return executor.submit(task, result);
	}

	@Override
	public Future<?> submit(Runnable task) {
		return executor.submit(task);
	}

	@Override
	public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks) throws InterruptedException {
		return executor.invokeAll(tasks);
	}

	@Override
	public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks, long timeout, TimeUnit unit) throws InterruptedException {
		return executor.invokeAll(tasks, timeout, unit);
	}

	@Override
	public <T> T invokeAny(Collection<? extends Callable<T>> tasks) throws InterruptedException, ExecutionException {
		return executor.invokeAny(tasks);
	}

	@Override
	public <T> T invokeAny(Collection<? extends Callable<T>> tasks, long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
		return executor.invokeAny(tasks, timeout, unit);
	}
	
	@Override
	public ScheduledFuture<?> schedule(Runnable task, long delay, TimeUnit unit) {
		if (executor instanceof ScheduledExecutorService) return ((ScheduledExecutorService)executor).schedule(task, delay, unit);
		else throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL);
	}

	@Override
	public <V> ScheduledFuture<V> schedule(Callable<V> task, long delay, TimeUnit unit) {
		if (executor instanceof ScheduledExecutorService) return ((ScheduledExecutorService)executor).schedule(task, delay, unit);
		else throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL);
	}

	@Override
	public ScheduledFuture<?> scheduleAtFixedRate(Runnable task, long initialDelay, long period, TimeUnit unit) {
		if (executor instanceof ScheduledExecutorService) return ((ScheduledExecutorService)executor).scheduleAtFixedRate(task, initialDelay, period, unit);
		else throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL);
	}

	@Override
	public ScheduledFuture<?> scheduleWithFixedDelay(Runnable task, long initialDelay, long delay, TimeUnit unit) {
		if (executor instanceof ScheduledExecutorService) return ((ScheduledExecutorService)executor).scheduleWithFixedDelay(task, initialDelay, delay, unit);
		else throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL);
	}
	
	public class Destroy implements IProcessor<Object, Object>{
		protected Boolean now;
		
		public void setNow(Boolean now) {
			this.now = now;
		}
		
		public Object process(Object in) {
			try {
				if (in instanceof PlatformPool) {
					if (now == null) ((PlatformPool) in).finalize();
					else try {
						if (((PlatformPool) in).chain && ((PlatformPool) in).handler instanceof ExecutorService) {
							if (now) ((ExecutorService)((PlatformPool) in).handler).shutdownNow();
							else ((ExecutorService)((PlatformPool) in).handler).shutdown();
							((PlatformPool) in).handler = null;
						}
					} finally {
						if (((PlatformPool) in).executor != null) {
							if (now) ((PlatformPool) in).shutdownNow();
							else ((PlatformPool) in).shutdown();
							((PlatformPool) in).executor = null;
						}
					}
				} else if (now == null) finalize();
				else try {
					if (chain && handler instanceof ExecutorService) {
						if (now) ((ExecutorService)handler).shutdownNow();
						else ((ExecutorService)handler).shutdown();
						handler = null;
					}
				} finally {
					if (executor != null) {
						if (now) shutdownNow();
						else shutdown();
						executor = null;
					}
				}
			} catch (Throwable e) {
	            log.error("PlatformPool: error when finalize.", e);
	            throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
			}
			return in;
		}
	}
	
	public static class Local<T, R> extends ThreadLocal<R> implements Comparable<T> {
		protected T key;

		public Local() {}

		public Local(T key) {
			this.key = key;
		}

		public void setKey(T key) {
			this.key = key;
		}

		@Override
		public int compareTo(T o) {
			if (key != null && key.equals(o)) return 0;
			else if (o instanceof Object[] && ((Object[])o).length > 0)  {
				((Object[])o)[0] = key;
				return 1;
			} else return -1;
		}
	}
	
	public static class InheritableLocal<T, R> extends InheritableThreadLocal<R> implements Comparable<T> {
		protected T key;

		public InheritableLocal() {}

		public InheritableLocal(T key) {
			this.key = key;
		}

		public void setKey(T key) {
			this.key = key;
		}

		@Override
		public int compareTo(T o) {
			if (key != null && key.equals(o)) return 0;
			else if (o instanceof Object[] && ((Object[])o).length > 0)  {
				((Object[])o)[0] = key;
				return 1;
			} else return -1;
		}
	}
	
	public static class Cleaner<T> implements IProcessor<T, T> {
		protected final Logger log;
		protected Boolean mode; 
		protected Boolean inherit; 
		
		public Cleaner(){
			this.log = LoggerFactory.getLogger(Cleaner.class);
		}
		
		public Cleaner(Logger log) {
			this.log = log == null ? LoggerFactory.getLogger(Cleaner.class) : log;
		}

		public void setMode(Boolean mode) {
			this.mode = mode;
		}

		public void setInherit(Boolean inherit) {
			this.inherit = inherit;
		}

		@Override
		public T process(T instance) {
			if (inheritable != null && !Boolean.TRUE.equals(mode)) try {
	        	if (!Boolean.FALSE.equals(inherit)) {
		            Object threadLocalTable = inheritable.get(Thread.currentThread());
		            if(threadLocalTable != null) {
		            	if (tableField == null) {
		            		tableField = threadLocalTable.getClass().getDeclaredField("table");
		            		tableField.setAccessible(true);
		            	}
			            Object[] table = (Object[]) tableField.get(threadLocalTable);
			            if(table != null) for (int j = 0; j < table.length; j++) if (table[j] instanceof WeakReference) try {
			            	if (valueField == null) {
			            		valueField = table[j].getClass().getDeclaredField("value");
			            		valueField.setAccessible(true);
			            	}
			            	if (((WeakReference<ThreadLocal>)table[j]).get() instanceof InheritableLocal) {
			            		Object value = valueField.get(table[j]);
			            		if (value instanceof IProcessor) try {
			            			((IProcessor<T, T>) value).process(instance);
			            		} finally {
			            			Array.set(table, j, null);
			            		}
			            	}
		            	} catch(Exception e) {
	                        log.debug("PlatformPool: execute callback fail.", e);
	                    } 
		            }
	        	}
	        	if (!Boolean.TRUE.equals(inheritable)) {
		            Object threadLocalTable = threadLocal.get(Thread.currentThread());
		            if(threadLocalTable != null){
		            	if (tableField == null) {
		            		tableField = threadLocalTable.getClass().getDeclaredField("table");
		            		tableField.setAccessible(true);
		            	}
			            Object[] table = (Object[]) tableField.get(threadLocalTable);
			            if(table != null) for (int j = 0; j < table.length; j++) if (table[j] instanceof WeakReference) try {
			            	if (valueField == null) {
			            		valueField = table[j].getClass().getDeclaredField("value");
			            		valueField.setAccessible(true);
			            	}
			            	if (((WeakReference<ThreadLocal>)table[j]).get() instanceof Local) {
			            		Object value = valueField.get(table[j]);
			            		if (value instanceof IProcessor) try{
			            			((IProcessor<T, T>) value).process(instance);
			            		} finally {
			            			Array.set(table, j, null);
			            		}
			            	}
		            	} catch(Exception e) {
	                        log.debug("PlatformPool: execute callback fail.", e);
	                    } 
		            }
	            }  
	        } catch(Exception e) {
	            log.debug("PlatformPool: error when gets threadlocals.", e);
	        }
			if (inheritable != null && !Boolean.FALSE.equals(mode)) try {
	        	inheritable.set(Thread.currentThread(), null);
	            threadLocal.set(Thread.currentThread(), null);
	        } catch(Exception e) {
	            log.debug("PlatformPool: error when gets threadlocals.", e);
	        }
			return instance;
		}
	}
	
	@Override
	public Object operate(Object first, Object second, Object third) {
		if (first == null) {
			HashMap<UUID, Entry<Object, Boolean>> map = new HashMap<UUID, Entry<Object, Boolean>>();
			for (Entry<UUID, WeakReference<Entry<IBiProcessor<Object, Object, Object>, Thread>>> entry : tasks.entrySet()) try {
				map.put(entry.getKey(), new Couple<Object, Boolean>(entry.getValue().get().getKey().perform(second, third), entry.getValue().get().getValue() == null));
			} catch (Exception e) {
				Entry<IBiProcessor<Object, Object, Object>, Thread> value = entry.getValue().get();
				if (value != null) map.put(entry.getKey(), new Couple<Object, Boolean>(value.getKey().toString(), value.getValue() == null));
			}
			return map;
		} else if (Boolean.FALSE.equals(first)) {
			UUID id = third instanceof UUID ? (UUID) third : UUID.fromString(third.toString());
			WeakReference<Entry<IBiProcessor<Object, Object, Object>, Thread>> ref = tasks.get(id);
			if (ref != null) {
				Entry<IBiProcessor<Object, Object, Object>, Thread> entry = ref.get();
				if (entry != null) {
					Thread thread = entry.getValue();
					if (thread != null) if (second instanceof Integer) switch((Integer) second) {
						case 0 : return thread.getId();
						case 1 : return thread.getName();
						case 2 : return thread.getPriority();
						case 3 : return thread.getState();
						case 4 : return thread.getThreadGroup();
						case 5 : return thread.isAlive();
						case 6 : return thread.isDaemon();
						case 7 : return thread.isInterrupted();
						case 8 : return thread.activeCount();
						case 9 : return thread.interrupted();
						case 10 : return thread.getStackTrace();
						case 11 : return thread.getUncaughtExceptionHandler();
						default : thread.dumpStack();
					} else return thread;
				}
			}
		} else if (Boolean.TRUE.equals(first)) {
			if (second instanceof Integer) switch((Integer) second) {
				case 0 : if (third == null) {
					for (WeakReference<Entry<IBiProcessor<Object, Object, Object>, Thread>> ref : tasks.values()) try {
						Entry<IBiProcessor<Object, Object, Object>, Thread> entry = ref.get();
						if (entry != null) entry.getValue().interrupt();
					} catch(Exception e) {
			            log.error("PlatformPool: error when kill thread.", e);
			        }
					tasks = new ConcurrentWeakIdentityHashMap<UUID, WeakReference<Entry<IBiProcessor<Object, Object, Object>, Thread>>>(false);
				}else {
		        	UUID id = third instanceof UUID ? (UUID) third : UUID.fromString(third.toString());
					WeakReference<Entry<IBiProcessor<Object, Object, Object>, Thread>> ref = tasks.remove(id);
					if (ref != null) {
						Entry<IBiProcessor<Object, Object, Object>, Thread> entry = ref.get();
						if (entry != null) entry.getValue().interrupt();
					}
		        }
				break;
				case 1 : if (third == null) for (WeakReference<Entry<IBiProcessor<Object, Object, Object>, Thread>> ref : tasks.values()) try {
					Entry<IBiProcessor<Object, Object, Object>, Thread> entry = ref.get();
					if (entry != null) entry.getValue().interrupt();
				} catch(Exception e) {
		            log.error("PlatformPool: error when kill thread.", e);
		        } else {
		        	UUID id = third instanceof UUID ? (UUID) third : UUID.fromString(third.toString());
					WeakReference<Entry<IBiProcessor<Object, Object, Object>, Thread>> ref = tasks.get(id);
					if (ref != null) {
						Entry<IBiProcessor<Object, Object, Object>, Thread> entry = ref.get();
						if (entry != null) entry.getValue().interrupt();
					}
		        }
				break;
				default : if (third == null) tasks = new ConcurrentWeakIdentityHashMap<UUID, WeakReference<Entry<IBiProcessor<Object, Object, Object>, Thread>>>(false);
				else tasks.remove(third instanceof UUID ? (UUID) third : UUID.fromString(third.toString()));
				break;
			}
		} else {
			UUID id = first instanceof UUID ? (UUID) first : UUID.fromString(first.toString());
			WeakReference<Entry<IBiProcessor<Object, Object, Object>, Thread>> ref = tasks.get(id);
			if (ref != null) {
				Entry<IBiProcessor<Object, Object, Object>, Thread> entry = ref.get();
				if (entry != null) return entry.getKey().perform(second, third);
			}
		}
		return null;
	}
}
