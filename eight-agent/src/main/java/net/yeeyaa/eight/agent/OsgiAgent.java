package net.yeeyaa.eight.agent;

import java.lang.instrument.ClassFileTransformer;
import java.security.ProtectionDomain;
import java.util.Hashtable;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;
import org.osgi.framework.hooks.weaving.WeavingHook;
import org.osgi.framework.hooks.weaving.WovenClass;

public final class OsgiAgent implements BundleActivator, WeavingHook{
	private ServiceRegistration<WeavingHook> registration;
	private Bundle bundle;

	private void initialize(BundleContext context) {
		if("true".equals(System.getProperty("config.agent.prefix"))) ClassCenter.classloaderPrefix = true;
		String regex = System.getProperty("config.agent.regex");
		if(regex != null && regex.trim().length() > 0) ClassCenter.regRegex(regex.trim());
		String transformers = System.getProperty("config.agent.transformers");
		if(transformers != null && transformers.trim().length() > 0) for(String transformer : transformers.split(":"))try{
			Class<?> clz = context.getBundle().loadClass(transformer.trim());
			if(ClassFileTransformer.class.isAssignableFrom(clz)) ClassCenter.regTransformer((ClassFileTransformer)clz.newInstance());
		}catch(Exception e){
			e.printStackTrace();
		}
		WeavingHook hook = null;
		String hname = System.getProperty("config.agent.hook");
		if(hname != null && regex.trim().length() > 0) try{
			Class<?> clz = context.getBundle().loadClass(hname.trim());
			if(WeavingHook.class.isAssignableFrom(clz)) hook = (WeavingHook)clz.newInstance();
		}catch(Exception e){
			e.printStackTrace();
		}
		if(hook == null) {
			bundle = context.getBundle();
			hook = this;
		}
		registration = context.registerService(WeavingHook.class, hook, new Hashtable<String, Object>());
    }
	
	@Override
	public void start(BundleContext context) throws Exception {
		if("true".equals(System.getProperty("config.agent.singleton"))) {
			if(!ClassCenter.initialized) synchronized(ClassCenter.class){
				if(!ClassCenter.initialized) {
					initialize(context);
					ClassCenter.initialized = true;
				}
			}
		}else initialize(context);
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		if(registration != null) registration.unregister();
	}
	
	@Override
	public void weave(WovenClass wovenClass) {
		if(!wovenClass.getBundleWiring().getBundle().equals(bundle)){
			byte[] classfileBuffer = wovenClass.getBytes();
			byte[] tmp = classfileBuffer;
			ClassLoader loader = wovenClass.getBundleWiring().getClassLoader();
			String className = wovenClass.getClassName();
			ProtectionDomain protectionDomain = wovenClass.getProtectionDomain();
			Class<?> classBeingRedefined = wovenClass.getDefinedClass();
			for(ClassFileTransformer transformer : ClassCenter.getTransformers()) try{
				classfileBuffer = transformer.transform(loader, className, classBeingRedefined, protectionDomain, classfileBuffer);
			}catch(Exception e){
				e.printStackTrace();
			}
			ClassCenter.store(classfileBuffer, className, loader.toString());
			if(tmp != classfileBuffer) wovenClass.setBytes(classfileBuffer);
		}
	}
}
