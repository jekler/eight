package net.yeeyaa.eight.core.task;

import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.core.PlatformPool;
import net.yeeyaa.eight.core.util.Interval;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class PlatformTask<K, K1> implements IProcessor<String, Void>{
	protected final Logger log;
	protected ConcurrentHashMap<String, Runnable> taskMap = new ConcurrentHashMap<String, Runnable>();
	protected IInputResource<K, Map<String, Object>> resource;
	protected K[] paras;
	protected IProcessor<Object, ScheduledExecutorService> pool;
	protected IProcessor<Object, Object> destroy;
	protected Integer threadCount = 5;
	protected ScheduledExecutorService executor;
	protected IProcessor<Object, K1> paraConvertor;
	protected Collection<TaskEntity> tasks;
	protected IProcessor<Object, IProcessor<K1, Void>> beanHolder;

	public enum Method{setTasks, destroy, reload, clearTask, stopTask, loadTask, removeTask, removeTaskbyBean, addTask, addTaskbyBegin};	

	public PlatformTask() {
		this.log = LoggerFactory.getLogger(PlatformTask.class);
	}
	
	public PlatformTask(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(PlatformTask.class) : log;
	}
	
	public class Invoker implements IProcessor<Object, Object>{
		protected Method method;
		
		public void setMethod(Method method) {
			this.method = method;
		}

		@Override
		public Object process(Object instance) {
			switch(method){
				case setTasks: if (instance instanceof Collection) setTasks((Collection<TaskEntity>) instance);
				break;
				case destroy: destroy();
				break;
				case reload: reload();
				break;
				case clearTask: clearTask();
				break;
				case stopTask: stopTask();
				break;
				case loadTask: loadTask();
				break;
				case removeTask: if (instance != null) removeTask(instance.toString());
				break;
				case addTask: if (instance instanceof TaskEntity) addTask((TaskEntity) instance);
				break;	
				case addTaskbyBegin: if (instance instanceof Object[] && ((Object[]) instance).length > 6) addTaskbyBegin((String)((Object[]) instance)[0], 
						(Long)((Object[]) instance)[1], (Interval)((Object[]) instance)[2],	(String)((Object[]) instance)[3], ((Object[]) instance)[4], 
						(Boolean)((Object[]) instance)[5], (Short)((Object[]) instance)[0]);
			}
			return instance;
		}
	}
	
	public void setPool(IProcessor<Object, ScheduledExecutorService> pool) {
		this.pool = pool;
	}

	public void setDestroy(IProcessor<Object, Object> destroy) {
		this.destroy = destroy;
	}
	
	public void setParaConvertor(IProcessor<Object, K1> paraConvertor) {
		this.paraConvertor = paraConvertor;
	}

	public void setTasks(Collection<TaskEntity> tasks) {
		this.tasks = tasks;
	}
	
	public void setBeanHolder(IProcessor<Object, IProcessor<K1, Void>> beanHolder) {
		this.beanHolder = beanHolder;
	}

	public void setResource(IInputResource<K, Map<String, Object>> iresource) {
		this.resource = iresource;
	}

	public void setThreadCount(Integer threadCount) {
		if(threadCount != null && threadCount >= 0) this.threadCount = threadCount;
	}	

	public void setParas(K[] paras) {
		this.paras = paras;
	}

	public class PoolRunnable implements Runnable {
        private Runnable task;

        public PoolRunnable(Runnable task) {
            this.task = task;
        }
        
        @Override
        public final void run(){
            try {
                task.run();
            } catch (Throwable e) {
            	log.error("PlatformTask: task perform error.", e);
            }
        }
    }
	
	protected static class SubTimer<K> implements Runnable{
		protected IProcessor<K, Void> bean;
		protected K paras;
		
		protected SubTimer(IProcessor<K, Void> bean, K paras){
			this.bean=bean;
			this.paras = paras;
		}

		public void run() {
			bean.process(paras);
		}
	}

	@PreDestroy
	public synchronized void destroy(){
		if (destroy != null) destroy.process(pool);
		else if(executor != null) executor.shutdown();
		executor = null;
	}
	
	public Boolean addTask(TaskEntity task) {
		return addTaskbyBegin(task.getName(), task.getBegin(), task.getSpan(), task.getBean(), task.getParameter(), task.getIgnore(), task.getType());
	}
	
	public Boolean addTaskbyBegin(String taskname, Long begin, Interval interval, Object bean, Object paras, Boolean ignoreError, Short type){
		if(taskname != null && bean != null && taskname.trim().length()>0 && begin != null && interval != null && type != null){
			K1 para;
			if(paraConvertor != null) para = paraConvertor.process(paras);
			else para = (K1) paras;
			Runnable task = bean instanceof Runnable ? (Runnable) bean : bean instanceof IProcessor ? new SubTimer<K1>((IProcessor<K1, Void>) bean, para) : new SubTimer<K1>(beanHolder.process(bean), para);
			taskMap.put(taskname, task);
			long span = interval.getTime(TimeUnit.MILLISECONDS);
			long time = begin < 0 ? -begin : begin - System.currentTimeMillis();
			if(time < 0) if(span > 0) time += Math.ceil((double) (-time) / span) * span;
			else if (span == 0) return false;
			if(executor != null && !executor.isShutdown()) {
				if(span > 0) {
					if(type.shortValue() == 0) if(ignoreError) executor.scheduleAtFixedRate(new PoolRunnable(task), time, span, TimeUnit.MILLISECONDS);
					else executor.scheduleAtFixedRate(task, time, span, TimeUnit.MILLISECONDS);
					else if(ignoreError) executor.scheduleWithFixedDelay(new PoolRunnable(task), time, span, TimeUnit.MILLISECONDS);
					else executor.scheduleWithFixedDelay(task, time, span, TimeUnit.MILLISECONDS);
				} else if(ignoreError) executor.schedule(new PoolRunnable(task), time, TimeUnit.MILLISECONDS);
				else executor.schedule(task, time, TimeUnit.MILLISECONDS);
				return true;
			} else return false;
		}else return false;
	}
	
	public synchronized void reload(){
		if (destroy != null) destroy.process(pool);
		else if (executor != null) executor.shutdown();
		if (pool == null) executor = PlatformPool.getScheduledPool(threadCount, false);	
		else executor = pool.process(null);
		taskMap.clear();
		loadTask();
	}
	
	public void removeTask(String taskname){
		if(taskname != null && taskname.trim().length()>0) taskMap.remove(taskname.trim());
	}

	public Void process(String taskname){
		if(taskMap.containsKey(taskname)) try{
			taskMap.get(taskname).run();
		}catch(Exception e){
			log.error(taskname+": task failed.", e);
		}
		return null;
	}

	public void clearTask(){
		taskMap.clear();
	}

	public void stopTask(){
		if (destroy != null) destroy.process(pool);
		else if (executor != null) executor.shutdown();
	}
	
	protected void loadTask(){
		if(tasks != null) for(TaskEntity task : tasks) addTaskbyBegin(task.getName(), task.getBegin(), task.getSpan(), task.getBean(),
				task.getParameter(), task.getIgnore(), task.getType());
		if(resource != null) {
			Object o = resource.find(paras);
			if(Collection.class.isInstance(o)) for(Object task : ((Collection)resource.find(paras))) if(task instanceof TaskEntity)
					addTaskbyBegin(((TaskEntity)task).getName(), ((TaskEntity)task).getBegin(), ((TaskEntity)task).getSpan(), 
							((TaskEntity)task).getBean(), ((TaskEntity)task).getParameter(), ((TaskEntity)task).getIgnore(), ((TaskEntity)task).getType());
		}
	}
	
	public class PipeNode implements IProcessor<Map<String, String>, Void>{
		@Override
		public Void process(Map<String, String> paras) {
			PlatformTask.this.process((String)paras.get("name"));
			return null;
		}
	}
}
