package net.yeeyaa.eight.annotation.processor;

import java.lang.annotation.Annotation;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;

import net.yeeyaa.eight.IProcessor;

import org.reflections.Reflections;
import org.reflections.scanners.SubTypesScanner;
import org.reflections.scanners.TypeAnnotationsScanner;
import org.reflections.util.ClasspathHelper;
import org.reflections.util.ConfigurationBuilder;


public class PackageSearchProcessor implements IProcessor<Collection<String>, Collection<Class<?>>> {
	protected Collection<String> packages;
	protected ClassLoader[] classLoaders;
	protected Class<? extends Annotation> target;
	protected Boolean honorInherited = true;
	
	public void setTarget(Class<? extends Annotation> target) {
		this.target = target;
	}

	public void setHonorInherited(Boolean honorInherited) {
		if(honorInherited != null) this.honorInherited = honorInherited;
	}

	public void setPackages(String packages) {
		if(packages != null) {
			LinkedList<String> ps = new LinkedList<String>();
			for(String p : packages.split(":")) if(p.trim().length() > 0) ps.add(p.trim());
			if(ps.size() > 0) this.packages = ps;
		}
	}
	
	public void setClassLoaders(Collection<ClassLoader> classLoaders) {
		if(classLoaders != null && classLoaders.size() > 0) this.classLoaders = classLoaders.toArray(new ClassLoader[classLoaders.size()]);
	}

	@Override
	public Collection<Class<?>> process(Collection<String> packages) {
		if(packages == null) packages = this.packages;
		if(packages == null) return new HashSet<Class<?>>();
        ConfigurationBuilder cb = new ConfigurationBuilder().setScanners(new TypeAnnotationsScanner(), new SubTypesScanner());
        if(classLoaders != null) cb.setClassLoaders(classLoaders);
        if(packages != null) for(String pkgName : packages) cb.setUrls(ClasspathHelper.forPackage(pkgName));
    	Reflections reflections = new Reflections(cb);
    	return reflections.getTypesAnnotatedWith(target, honorInherited);
	}
}
