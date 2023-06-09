package net.yeeyaa.eight.common.spring;

import net.yeeyaa.eight.IProcessor;

import org.springframework.beans.factory.groovy.GroovyBeanDefinitionReader;
import org.springframework.beans.factory.support.BeanDefinitionReader;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.PropertiesBeanDefinitionReader;
import org.springframework.beans.factory.xml.XmlBeanDefinitionReader;


public class ContextReaderProcessor implements IProcessor<BeanDefinitionRegistry, BeanDefinitionReader> {
	protected String type = "xml";
	
	public void setType(String type) {
		if(type != null)this.type = type;
	}

	@Override
	public BeanDefinitionReader process(BeanDefinitionRegistry instance) {
		if("xml".equals(type)) return new XmlBeanDefinitionReader(instance);
		else if("groovy".equals(type)) return new GroovyBeanDefinitionReader(instance);
		else return new PropertiesBeanDefinitionReader(instance);
	}
}
