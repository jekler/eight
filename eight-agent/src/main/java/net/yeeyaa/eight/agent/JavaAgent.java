package net.yeeyaa.eight.agent;

import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.IllegalClassFormatException;
import java.lang.instrument.Instrumentation;
import java.lang.management.ManagementFactory;
import java.security.ProtectionDomain;
import java.util.HashMap;
import java.util.Map.Entry;

import com.sun.tools.attach.VirtualMachine;

public final class JavaAgent implements ClassFileTransformer {
    private JavaAgent() {}

	private static void initialize(String args, Instrumentation inst) {
		if("true".equals(System.getProperty("config.agent.prefix"))) ClassCenter.classloaderPrefix = true;
		String regex = System.getProperty("config.agent.regex");
		if(regex != null && regex.trim().length() > 0) ClassCenter.regRegex(regex.trim());
		String transformers = System.getProperty("config.agent.transformers");
		ClassLoader cl = JavaAgent.class.getClassLoader();
		if(cl == null) cl = ClassLoader.getSystemClassLoader();
		if(transformers != null && transformers.trim().length() > 0 && cl != null) for(String transformer : transformers.split(":"))try{
			Class<?> clz = cl.loadClass(transformer.trim());
			if(ClassFileTransformer.class.isAssignableFrom(clz)) ClassCenter.regTransformer((ClassFileTransformer)clz.newInstance());
		}catch(Exception e){
			e.printStackTrace();
		}
		ClassFileTransformer hook = null;
		if(args != null && args.trim().length() > 0 && cl != null) try{
			Class<?> clz = cl.loadClass(args.trim());
			if(ClassFileTransformer.class.isAssignableFrom(clz)) hook = (ClassFileTransformer)clz.newInstance();
		}catch(Exception e){
			e.printStackTrace();
		}
		if(hook == null) hook = new JavaAgent();
		inst.addTransformer(hook);
    }
	
    public static void premain(String args, Instrumentation inst) throws Exception {
		if("true".equals(System.getProperty("config.agent.singleton"))) {
			if(!ClassCenter.initialized) synchronized(ClassCenter.class){
				if(!ClassCenter.initialized) {
					initialize(args, inst);
					ClassCenter.initialized = true;
				}
			}
		}else initialize(args, inst);
    }

    public static void agentmain(String args, Instrumentation inst) throws Exception {
    	if(args != null && args.trim().length() > 0){
    		HashMap<String, String> map = new HashMap<String, String>();
    		Boolean overlap = false;
    		for(String prop : args.split("##")){
    			String[] kv = prop.split("#");
    			if(kv.length > 1 && kv[0].trim().length() > 0 && kv[1].trim().length() > 0){
    				String key = kv[0].replace("`b", "#").replace("`c", "`");
    				String value = kv[1].replace("`b", "#").replace("`c", "`");
    				if("overlap".equals(key)){
    					if("true".equals(value)) overlap =true;
    				} else map.put(key, value);
    			}
    		}
    		if(map.size() > 0) args = map.remove("args");
    		for(Entry<String, String> entry : map.entrySet()) if(overlap || System.getProperty(entry.getKey()) == null) System.setProperty(entry.getKey(), entry.getValue());
    	}
		if("true".equals(System.getProperty("config.agent.singleton"))) {
			if(!ClassCenter.initialized) synchronized(ClassCenter.class){
				if(!ClassCenter.initialized) {
					initialize(args, inst);
					ClassCenter.initialized = true;
				}
			}
		}else initialize(args, inst);
    }
    
    public static void main(String[] args) {
    	if(args != null && args.length > 0){
    		String pid = null;
    		String hook = null;
    		if(args.length > 1) for(int i = 1; i < args.length; i++) if('@' == args[i].charAt(0)) pid = args[i].substring(1);
    		else hook = args[i];
    		if(pid == null || pid.trim().length() == 0) {
		        String nameOfRunningVM = ManagementFactory.getRuntimeMXBean().getName();
		        int p = nameOfRunningVM.indexOf('@');
		        pid = nameOfRunningVM.substring(0, p);
    		}
	        try {
	            VirtualMachine vm = VirtualMachine.attach(pid);
	            vm.loadAgent(args[0], hook);
	            vm.detach();
	        } catch (Exception e) {
	            e.printStackTrace();
	        }
    	}
    }
    
	@Override
    public byte[] transform(ClassLoader loader, String className, Class<?> classBeingRedefined, ProtectionDomain protectionDomain, byte[] classfileBuffer) throws IllegalClassFormatException {
		className = className.replace('/', '.');
		for(ClassFileTransformer transformer : ClassCenter.getTransformers()) try{
			classfileBuffer = transformer.transform(loader, className, classBeingRedefined, protectionDomain, classfileBuffer);
		}catch(Exception e){
			e.printStackTrace();
		}
		ClassCenter.store(classfileBuffer, className, loader.toString());
        return classfileBuffer;
    }
}
