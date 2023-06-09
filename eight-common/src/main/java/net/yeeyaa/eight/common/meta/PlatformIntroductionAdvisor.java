package net.yeeyaa.eight.common.meta;

import java.io.Serializable;
import java.util.Collections;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.aopalliance.aop.Advice;
import org.springframework.aop.ClassFilter;
import org.springframework.aop.DynamicIntroductionAdvice;
import org.springframework.aop.IntroductionAdvisor;
import org.springframework.aop.IntroductionInfo;
import org.springframework.aop.aspectj.TypePatternClassFilter;
import org.springframework.aop.support.ClassFilters;
import org.springframework.aop.support.DelegatePerTargetObjectIntroductionInterceptor;
import org.springframework.aop.support.DelegatingIntroductionInterceptor;
import org.springframework.core.Ordered;
import org.springframework.util.Assert;


public class PlatformIntroductionAdvisor implements IntroductionAdvisor, ClassFilter, Ordered, Serializable{
	private static final long serialVersionUID = -545523402988417234L;
	protected final Set<Class<?>> interfaces = Collections.newSetFromMap(new ConcurrentHashMap<Class<?>, Boolean>());
	protected boolean perInstance = true;
	protected int order = Ordered.LOWEST_PRECEDENCE;
	protected final Advice advice;
	protected ClassFilter classFilter;
	protected boolean enforce = false; 
	
	public PlatformIntroductionAdvisor(Object advice) {
		Assert.notNull(advice, "Advice must not be null");
		if(advice instanceof Advice) this.advice = (Advice) advice;
		else this.advice = new DelegatingIntroductionInterceptor(advice);
		if (this.advice instanceof IntroductionInfo) setAdditionalInterfaces(((IntroductionInfo)this.advice).getInterfaces());
	}

	public PlatformIntroductionAdvisor(Class<?> interfaceType, Class<?> defaultImpl) {
		Assert.notNull(interfaceType, "interfaceType must not be null");
		Assert.notNull(defaultImpl, "defaultImpl must not be null");
		DelegatePerTargetObjectIntroductionInterceptor introduction = new DelegatePerTargetObjectIntroductionInterceptor(defaultImpl, interfaceType);
		this.advice = introduction;
		setAdditionalInterfaces(introduction.getInterfaces());
	}

	public void setInterface(Class<?> clz) {
		interfaces.clear();
		if(clz != null && clz.isInterface()) interfaces.add(clz);
	}
	
	public void setInterfaces(Class<?>[] classes) {
		interfaces.clear();
		if(classes != null && classes.length > 0) for (Class<?> clz : classes) if (clz != null && clz.isInterface()) interfaces.add(clz);
	}

	public void setAdditionalInterface(Class<?> clz) {
		if(clz != null && clz.isInterface()) interfaces.add(clz);
	}
	
	public void setAdditionalInterfaces(Class<?>[] classes) {
		if(classes != null && classes.length > 0) for (Class<?> clz : classes) if (clz != null && clz.isInterface()) interfaces.add(clz);
	}
	
	public void setEnforce(boolean enforce) {
		this.enforce = enforce;
	}

	public void setPerInstance(boolean perInstance) {
		this.perInstance = perInstance;
	}

	public void setTypePattern(String typePattern) {
		if (classFilter == null) classFilter = ClassFilters.intersection(new TypePatternClassFilter(typePattern), this);
	}

	public void setClassFilter(ClassFilter classFilter) {
		this.classFilter = classFilter;
	}
	
	public void setOrder(int order) {
		if(order > Ordered.LOWEST_PRECEDENCE && order <= Ordered.HIGHEST_PRECEDENCE) this.order = order;
	}

	@Override
	public int getOrder() {
		return order;
	}

	@Override
	public ClassFilter getClassFilter() {
		if(classFilter == null) return this;
		else return classFilter;
	}

	@Override
	public boolean isPerInstance() {
		return perInstance;
	}

	@Override
	public Advice getAdvice() {
		return advice;
	}

	@Override
	public Class<?>[] getInterfaces() {
		return interfaces.toArray(new Class<?>[interfaces.size()]);
	}

	@Override
	public void validateInterfaces() throws IllegalArgumentException {
		for (Class<?> ifc : interfaces) if (advice instanceof DynamicIntroductionAdvice && !((DynamicIntroductionAdvice) this.advice).implementsInterface(ifc)) 
			 throw new IllegalArgumentException("PlatformIntroductionAdvisor: DynamicIntroductionAdvice [" + advice + "] " + "does not implement interface [" + ifc.getName() + "] specified for introduction");
	}

	@Override
	public boolean matches(Class<?> clazz) {
		if (enforce) return true;
		for (Class<?> clz : interfaces) if (!clz.isAssignableFrom(clazz)) return true;
		return false;
	}
	
	@Override
	public boolean equals(Object obj) {
		if (this == obj) return true;
		else if ((obj instanceof PlatformIntroductionAdvisor)) {
			PlatformIntroductionAdvisor other = (PlatformIntroductionAdvisor) obj;
			return (advice == other.advice ||(advice != null && advice.equals(other.advice))) && (classFilter == other.classFilter ||(classFilter != null && classFilter.equals(other.classFilter))) && perInstance == other.perInstance && enforce == other.enforce && interfaces.equals(other.interfaces);
		}
		return false;
	}

	@Override
	public int hashCode() {
		int hash = advice.hashCode() * 11 + (perInstance ? 1231 : 1237) + (enforce ? 1153 : 1179);
		if (classFilter != null) hash = hash * 13 + classFilter.hashCode();
		if (interfaces.size() > 0) hash = hash * 17 + interfaces.hashCode();
		return hash;
	}
}
