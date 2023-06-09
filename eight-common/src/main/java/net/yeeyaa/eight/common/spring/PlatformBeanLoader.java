package net.yeeyaa.eight.common.spring;

import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IProcessor;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.RootBeanDefinition;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;


public class PlatformBeanLoader implements ApplicationContextAware, IProcessor<Collection<Resource>, Void> {
	protected static String scriptProcessor = "org.springframework.scripting.config.scriptFactoryPostProcessor";
	protected ApplicationContext parent;
	protected GenericApplicationContext context;
	protected ClassLoader classLoader;
	protected ResourceLoader resourceLoader;
	protected Integer priority = 50;	
	protected IProcessor<String, String> nameCreator;
	protected IProcessor<Resource, BeanDefinition> beanCreator;
	protected Map<String, Resource> resources = new ConcurrentHashMap<String, Resource>();
	protected String prefix = "scriptFactory.";
	protected PlatformBean platformBean;
	
	public void setPlatformBean(PlatformBean platformBean) {
		this.platformBean = platformBean;
	}

	protected class LocalResourceLoader implements ResourceLoader{
		@Override
		public Resource getResource(String location) {
			if(resourceLoader != null) return resourceLoader.getResource(location);
			else return resources.get(location);
		}

		@Override
		public ClassLoader getClassLoader() {
			if(classLoader != null) return classLoader;
			else return getClassLoader();
		}
	}
	
	public void setResourceLoader(ResourceLoader resourceLoader) {
		this.resourceLoader = resourceLoader;
	}

	public void setScriptProcessor(String scriptProcessor) {
		if(scriptProcessor != null) PlatformBeanLoader.scriptProcessor = scriptProcessor;
	}

	public void setNameCreator(IProcessor<String, String> nameCreator) {
		this.nameCreator = nameCreator;
	}

	public void setBeanCreator(IProcessor<Resource, BeanDefinition> beanCreator) {
		this.beanCreator = beanCreator;
	}

	public void setPriority(Integer priority) {
		if(priority != null && priority > 0) this.priority = priority;
	}

	public void setParent(ApplicationContext parent) {
		this.parent = parent;
	}

	public void setPrefix(String prefix) {
		if(prefix != null) this.prefix = prefix;
	}

	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		if(parent == null)parent = applicationContext;
	}
	
	public void setClassLoader(ClassLoader classLoader) {
		this.classLoader = classLoader;
	}

	@PostConstruct
	public void initialize(){
		context = new GenericApplicationContext(parent);
		if(classLoader != null) context.setClassLoader(classLoader);
		context.setResourceLoader(new LocalResourceLoader());
		context.registerBeanDefinition(scriptProcessor, new RootBeanDefinition(PlatformScriptFactoryPostProcessor.class));
		context.refresh();
		context.start();
		platformBean.register(context, priority);
	}
	
	@PreDestroy
	public void destroy(){
		try {
			context.stop();
		} finally {
			context.close();
		}
	}
	
	@Override
	public Void process(Collection<Resource> resources) {
		if(resources != null && resources.size() > 0) {
			HashSet<String> set = new HashSet<String>();
			for(Resource resource : resources){
				String name = resource.getFilename();
				if(nameCreator != null) name = nameCreator.process(name);
				BeanDefinition bd = beanCreator.process(resource);
				this.resources.put(prefix + name, resource);
				if(name != null && bd != null){
					context.registerBeanDefinition(name, bd);
					set.add(name);
				}
			}
			platformBean.register(context, set.toArray(new String[set.size()]));
		}
		return null;
	}
}
