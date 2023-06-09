package net.yeeyaa.eight.common.spring;

import java.util.concurrent.ExecutorService;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IProcessor;

import org.springframework.context.ApplicationContext;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.support.AbstractApplicationContext;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.context.support.FileSystemXmlApplicationContext;
import org.springframework.context.support.GenericXmlApplicationContext;


public class RestartProcessor implements IProcessor<Object, Object> {
	protected String begin;
	protected String close;
	protected String self;
	protected IProcessor<Void, ApplicationContext> creator;
	protected ApplicationContext context;
	protected String[] args;
	protected Integer type = 0; 
	protected IProcessor<Object, Object> construct;
	protected IProcessor<Object, Object> destroy;
	protected Object constructPara;
	protected Object destroyPara;
	protected ExecutorService executor; 

	public void setExecutor(ExecutorService executor) {
		this.executor = executor;
	}

	public void setConstruct(IProcessor<Object, Object> construct) {
		this.construct = construct;
	}

	public void setDestroy(IProcessor<Object, Object> destroy) {
		this.destroy = destroy;
	}

	public void setConstructPara(Object constructPara) {
		this.constructPara = constructPara;
	}

	public void setDestroyPara(Object destroyPara) {
		this.destroyPara = destroyPara;
	}

	public void setCreator(IProcessor<Void, ApplicationContext> creator) {
		this.creator = creator;
	}

	public void setBegin(String begin) {
		this.begin = begin;
	}

	public void setClose(String close) {
		this.close = close;
	}

	public void setSelf(String self) {
		this.self = self;
	}

	public void setContext(ApplicationContext context) {
		this.context = context;
	}

	public void setArgs(String[] args) {
		this.args = args;
	}

	public void setType(Integer type) {
		if(type != null) this.type = type;
	}

	@PostConstruct
	public void initialize(){
		if (construct != null) construct.process(constructPara);
	}
	
	@PreDestroy
	public void destroy(){
		if (destroy != null) destroy.process(destroyPara);
	}
	
	public synchronized void start(){
		if(creator != null) context = creator.process(null);
		else {
			AbstractApplicationContext context;
			switch(type){
				case 0: context = new ClassPathXmlApplicationContext(args);
				break;
				case 1: context = new FileSystemXmlApplicationContext(args);
				break;
				default: context = new GenericXmlApplicationContext(args);
			}
			context.refresh();
			context.start();
			this.context = context;
		}
		if(self != null) {
			Object b = context.getBean(self);
			if(b instanceof RestartProcessor) {
				RestartProcessor restart = (RestartProcessor) b;
				restart.args = args;
				restart.begin = begin;
				restart.close = close;
				restart.self = self;
				restart.type = type;
				restart.creator = creator;
				restart.context = context;
			}
		}
		if(begin != null) {
			Object b = context.getBean(begin);
			if(b instanceof IProcessor) ((IProcessor<Object, Object>)b).process(null);
		}
	}

	public synchronized void stop(){
		if(close != null) {
			Object b = context.getBean(close);
			if(b instanceof IProcessor) ((IProcessor<Object, Object>)b).process(null);
		}
		if(context instanceof ConfigurableApplicationContext) try {
			((ConfigurableApplicationContext)context).stop();
		} finally {
			((ConfigurableApplicationContext)context).close();
		}
	}
	
	public synchronized void restart() {
		try{
			stop();
		} finally {
			start();
		}
	}
	
	@Override
	public Object process(Object instance) {
		Runnable run = new Runnable(){
			@Override
			public void run() {
				restart();
			}
		};
		if (executor != null) executor.execute(run); 
		else new Thread(run).start();
		return instance;
	}
}
