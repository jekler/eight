package net.yeeyaa.eight.common.spring;

import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;

import org.springframework.beans.MutablePropertyValues;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.ConstructorArgumentValues;
import org.springframework.beans.factory.support.GenericBeanDefinition;
import org.springframework.beans.factory.support.MethodOverrides;
import org.springframework.core.io.Resource;


public class BeanDefinationProcessor implements IProcessor<Resource, BeanDefinition> {
	protected String beanClassName;
	protected String factoryBeanName;
	protected String factoryMethodName;
	protected Boolean lazyInit = true;
	protected Map<String, Object> attributes;
	protected Integer autowireMode;
	protected Boolean autowireCandidate;
	protected Integer dependencyCheck;
	protected Boolean synthetic;
	protected Object source;
	protected String scope;
	protected Integer role;
	protected Boolean abstractFlag;
	protected Class<?> beanClass;
	protected String[] dependsOn;
	protected String description;
	protected String destroyMethodName;
	protected Boolean enforceDestroyMethod;
	protected Boolean enforceInitMethod;
	protected String initMethodName;
	protected Boolean lenientConstructorResolution;
	protected MethodOverrides methodOverrides;
	protected Boolean nonPublicAccessAllowed;
	protected BeanDefinition originatingBd;
	protected String parentName;
	protected Boolean primary;
	protected Map<Integer, Object> constructors;
	protected MutablePropertyValues propertyValues;
	protected Resource resource;
	protected String resourceDescription;
	
	public void setBeanClassName(String beanClassName) {
		this.beanClassName = beanClassName;
	}

	public void setFactoryBeanName(String factoryBeanName) {
		this.factoryBeanName = factoryBeanName;
	}

	public void setFactoryMethodName(String factoryMethodName) {
		this.factoryMethodName = factoryMethodName;
	}

	public void setLazyInit(Boolean lazyInit) {
		if(lazyInit != null)this.lazyInit = lazyInit;
	}

	public void setAttributes(Map<String, Object> attributes) {
		this.attributes = attributes;
	}

	public void setAutowireMode(Integer autowireMode) {
		this.autowireMode = autowireMode;
	}

	public void setAutowireCandidate(Boolean autowireCandidate) {
		this.autowireCandidate = autowireCandidate;
	}

	public void setDependencyCheck(Integer dependencyCheck) {
		this.dependencyCheck = dependencyCheck;
	}

	public void setSynthetic(Boolean synthetic) {
		this.synthetic = synthetic;
	}

	public void setSource(Object source) {
		this.source = source;
	}

	public void setScope(String scope) {
		this.scope = scope;
	}

	public void setRole(Integer role) {
		this.role = role;
	}

	public void setAbstractFlag(Boolean abstractFlag) {
		this.abstractFlag = abstractFlag;
	}

	public void setBeanClass(Class<?> beanClass) {
		this.beanClass = beanClass;
	}

	public void setDependsOn(String[] dependsOn) {
		this.dependsOn = dependsOn;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public void setDestroyMethodName(String destroyMethodName) {
		this.destroyMethodName = destroyMethodName;
	}

	public void setEnforceDestroyMethod(Boolean enforceDestroyMethod) {
		this.enforceDestroyMethod = enforceDestroyMethod;
	}

	public void setEnforceInitMethod(Boolean enforceInitMethod) {
		this.enforceInitMethod = enforceInitMethod;
	}

	public void setInitMethodName(String initMethodName) {
		this.initMethodName = initMethodName;
	}

	public void setLenientConstructorResolution(Boolean lenientConstructorResolution) {
		this.lenientConstructorResolution = lenientConstructorResolution;
	}

	public void setMethodOverrides(MethodOverrides methodOverrides) {
		this.methodOverrides = methodOverrides;
	}

	public void setNonPublicAccessAllowed(Boolean nonPublicAccessAllowed) {
		this.nonPublicAccessAllowed = nonPublicAccessAllowed;
	}

	public void setOriginatingBd(BeanDefinition originatingBd) {
		this.originatingBd = originatingBd;
	}

	public void setParentName(String parentName) {
		this.parentName = parentName;
	}

	public void setPrimary(Boolean primary) {
		this.primary = primary;
	}

	public void setConstructors(Map<Integer, Object> constructors) {
		this.constructors = constructors;
	}

	public void setPropertyValues(MutablePropertyValues propertyValues) {
		this.propertyValues = propertyValues;
	}

	public void setResource(Resource resource) {
		this.resource = resource;
	}

	public void setResourceDescription(String resourceDescription) {
		this.resourceDescription = resourceDescription;
	}

	@Override
	public BeanDefinition process(Resource resource) {
		GenericBeanDefinition bd = new GenericBeanDefinition();
		if(beanClassName != null) bd.setBeanClassName(beanClassName);
		if(factoryBeanName != null) {
			bd.setFactoryBeanName(factoryBeanName);
			bd.setFactoryMethodName(factoryMethodName);
		}
		if(autowireMode != null)bd.setAutowireMode(autowireMode);
		if(autowireCandidate != null)bd.setAutowireCandidate(autowireCandidate);
		if(dependencyCheck != null) bd.setDependencyCheck(dependencyCheck);
		if(synthetic != null) bd.setSynthetic(synthetic);
		if(source != null)bd.setSource(source);
		if(scope != null)bd.setScope(scope);
		if(scope != null)bd.setRole(role);
		if(abstractFlag != null)bd.setAbstract(abstractFlag);
		if(beanClass != null)bd.setBeanClass(beanClass);
		if(dependsOn != null)bd.setDependsOn(dependsOn);
		if(description != null)bd.setDescription(description);
		if(destroyMethodName != null)bd.setDestroyMethodName(destroyMethodName);
		if(enforceDestroyMethod != null)bd.setEnforceDestroyMethod(enforceDestroyMethod);
		if(enforceInitMethod != null)bd.setEnforceInitMethod(enforceInitMethod);
		if(initMethodName != null)bd.setInitMethodName(initMethodName);
		if(lenientConstructorResolution != null)bd.setLenientConstructorResolution(lenientConstructorResolution);
		if(methodOverrides != null)bd.setMethodOverrides(methodOverrides);
		if(nonPublicAccessAllowed != null)bd.setNonPublicAccessAllowed(nonPublicAccessAllowed);
		if(originatingBd != null)bd.setOriginatingBeanDefinition(originatingBd);
		if(parentName != null)bd.setParentName(parentName);
		if(primary != null)bd.setPrimary(primary);
		if(propertyValues != null)bd.setPropertyValues(propertyValues);
		if(constructors != null && constructors.size() > 0) {
			ConstructorArgumentValues  cav = bd.getConstructorArgumentValues();
			for(Entry<Integer, Object> entry : constructors.entrySet()) cav.addIndexedArgumentValue(entry.getKey(), entry.getValue());
		}
		bd.setLazyInit(lazyInit);
		if(attributes != null) for(Entry<String, Object> entry : attributes.entrySet()) bd.setAttribute(entry.getKey(), entry.getValue());
		if(resource != null) bd.setResource(resource);
		else if(this.resource != null) bd.setResource(this.resource);
		if(resourceDescription != null)bd.setResourceDescription(resourceDescription);
		return bd;
	}
}
