package net.yeeyaa.eight.common.aop;

import java.io.ByteArrayInputStream;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.ITriProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.asm.ClassReader;
import org.springframework.asm.ClassVisitor;
import org.springframework.asm.ClassWriter;
import org.springframework.asm.MethodVisitor;
import org.springframework.asm.Opcodes;
import org.springframework.asm.Type;


public class SubClassProcessor implements Opcodes, IProcessor<Object, byte[]>, IBiProcessor<Object, Object, byte[]>, ITriProcessor<Object, Object, Collection<Object>, byte[]> {
	protected static final HashMap<Type, Integer> RETURN_OPCODES = new HashMap<Type, Integer>();
	protected static final HashMap<Type, Integer> LOAD_OPCODES = new HashMap<Type, Integer>();
	static {
	    RETURN_OPCODES.put(Type.VOID_TYPE, RETURN);
	    RETURN_OPCODES.put(Type.BOOLEAN_TYPE, IRETURN);
	    RETURN_OPCODES.put(Type.BYTE_TYPE, IRETURN);
	    RETURN_OPCODES.put(Type.CHAR_TYPE, IRETURN);
	    RETURN_OPCODES.put(Type.SHORT_TYPE, IRETURN);
	    RETURN_OPCODES.put(Type.INT_TYPE, IRETURN);
	    RETURN_OPCODES.put(Type.LONG_TYPE, LRETURN);
	    RETURN_OPCODES.put(Type.FLOAT_TYPE, FRETURN);
	    RETURN_OPCODES.put(Type.DOUBLE_TYPE, DRETURN);

	    LOAD_OPCODES.put(Type.BOOLEAN_TYPE, ILOAD);
	    LOAD_OPCODES.put(Type.BYTE_TYPE, ILOAD);
	    LOAD_OPCODES.put(Type.CHAR_TYPE, ILOAD);	    
	    LOAD_OPCODES.put(Type.SHORT_TYPE, ILOAD);
	    LOAD_OPCODES.put(Type.INT_TYPE, ILOAD);
	    LOAD_OPCODES.put(Type.LONG_TYPE, LLOAD);
	    LOAD_OPCODES.put(Type.FLOAT_TYPE, FLOAD);
	    LOAD_OPCODES.put(Type.DOUBLE_TYPE, DLOAD);
	}
	protected static final IProcessor<Object, String> defaultPolicy = new IProcessor<Object, String>(){
		@Override
		public String process(Object object) {
			return object == null ? null : object.toString().replace('.', '/');
		}
	};
	protected final Logger log;
	protected Integer api = ASM4;
	protected Object superClass;
	protected ITriProcessor<String, Object, String[], ClassWriter> inject;
	protected final IProcessor<Object, String> namingPolicy;
	protected final IProcessor<Object, String> superPolicy;
	protected Integer version = V1_6;
	protected Integer access = ACC_PUBLIC + ACC_SUPER;
	protected String signature;
	protected String[] interfaces;
	
	public SubClassProcessor() {
		this.namingPolicy = this.superPolicy = defaultPolicy;
		this.log = LoggerFactory.getLogger(SubClassProcessor.class);
	}

	public SubClassProcessor(Logger log) {
		this.namingPolicy = this.superPolicy = defaultPolicy;
		this.log = log == null ? LoggerFactory.getLogger(ByteClassProcessor.class) : log;
	}

	public SubClassProcessor(IProcessor<Object, String> defaultPolicy) {
		this.namingPolicy = this.superPolicy = defaultPolicy;
		this.log = LoggerFactory.getLogger(SubClassProcessor.class);
	}
	
	public SubClassProcessor(IProcessor<Object, String> defaultPolicy, Logger log) {
		this.namingPolicy = this.superPolicy = defaultPolicy;
		this.log = log == null ? LoggerFactory.getLogger(ByteClassProcessor.class) : log;
	}

	public SubClassProcessor(IProcessor<Object, String> namingPolicy, IProcessor<Object, String> superPolicy) {
		this.namingPolicy = namingPolicy;
		this.superPolicy = superPolicy;
		this.log = LoggerFactory.getLogger(SubClassProcessor.class);
	}
	
	public SubClassProcessor(IProcessor<Object, String> namingPolicy, IProcessor<Object, String> superPolicy, Logger log) {
		this.namingPolicy = namingPolicy;
		this.superPolicy = superPolicy;
		this.log = log == null ? LoggerFactory.getLogger(ByteClassProcessor.class) : log;
	}
	
	public void setApi(Integer api) {
		if(api != null) this.api = api;
	}

	public void setInject(ITriProcessor<String, Object, String[], ClassWriter> inject) {
		this.inject = inject;
	}

	public void setVersion(Integer version) {
		if (version != null) this.version = version;
	}

	public void setAccess(Integer access) {
		if (access != null) this.access = access;
	}

	public void setSignature(String signature) {
		this.signature = signature;
	}

	public void setSuperClass(Object superClass) {
		if (superClass != null) if (superClass instanceof Map) {
			Map<String, Object> tmp = new LinkedHashMap<String, Object>();
			for (Entry<String, Object> e : ((Map<String, Object>)superClass).entrySet()) tmp.put(superPolicy.process(e.getKey()), e.getValue());
			if (tmp.size() > 0) this.superClass = tmp;
		} else if (superClass instanceof String) this.superClass =  superPolicy.process(superClass.toString());
		else if (superClass instanceof Class) this.superClass = superPolicy.process(((Class<?>) superClass).getName());
		else this.superClass = superPolicy.process(superClass.getClass().getName());
	}
	
	public void setInterfaces(Collection<Object> interfaces) {
		if (interfaces != null && interfaces.size() > 0) {
			ArrayList<String> ls = new ArrayList<String>(interfaces.size());
			for (Object intf : interfaces) if (intf instanceof String) ls.add(superPolicy.process(intf.toString()));
			else if (intf instanceof Class) ls.add(superPolicy.process(((Class<?>) intf).getName()));
			else if (intf != null) ls.add(superPolicy.process(intf.getClass().getName()));
			if (ls.size() > 0) this.interfaces = ls.toArray(new String[ls.size()]);
		}
	}

	protected byte[] generate(String name, Object superClass, String[] interfaces) {
		if (inject == null) try {
			final ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES);
			if (superClass instanceof Map) {
				final HashSet<String> methods = new HashSet<String>();
				Entry<String, byte[]>[] es = ((Map<String, byte[]>)superClass).entrySet().toArray(new Entry[ ((Map<String, byte[]>)superClass).size()]);
				for (int i = 0; i < es.length; i++) {
					final int index = i;
					final Entry<String, byte[]> parent = es[index];
					if (index == 0) {
						cw.visit(version, access, name, signature, parent.getKey(), interfaces);
						cw.visitSource(name.concat(".java"), null);
					}
					new ClassReader(new ByteArrayInputStream(parent.getValue())).accept(new ClassVisitor(api){		
						@Override
						public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
						    if (methods.contains(name + "|" + desc) || Modifier.isStatic(access) || Modifier.isPrivate(access) || Modifier.isFinal(access) || (name.equals("<init>") && index != 0) || (access & 7) == 0) return null;
						    MethodVisitor mv = cw.visitMethod(name.equals("<init>") || Modifier.isProtected(access)  || Modifier.isNative(access) || Modifier.isAbstract(access) ? ACC_PUBLIC : access, name, desc, null, exceptions);
						    Type methodType = Type.getMethodType(desc);
						    Type[] argumentTypes = methodType.getArgumentTypes();
						    mv.visitVarInsn(ALOAD, 0);
						    for (int i = 0; i < argumentTypes.length; i++) {
						    	Integer code = LOAD_OPCODES.get(argumentTypes[i]);
						    	mv.visitVarInsn(code == null ? ALOAD : code, i + 1);
						    }
						    Integer code = RETURN_OPCODES.get(methodType.getReturnType());
						    if (code == null) code = ARETURN;
						    if (!Modifier.isAbstract(access) && !Modifier.isInterface(access))  mv.visitMethodInsn(INVOKESPECIAL, parent.getKey(), name, desc, false);
						    else switch (code) {
						    	case ARETURN: mv.visitInsn(ACONST_NULL);
						    		break;
						    	case IRETURN: mv.visitLdcInsn(0);
						    		break;
						    	case FRETURN: mv.visitLdcInsn(new Float(0));
					    			break;
						    	case DRETURN: mv.visitLdcInsn(new Double(0));
				    				break;
						    	case LRETURN: mv.visitLdcInsn(new Long(0));
						    }
						    mv.visitInsn(code);
						    int maxLocals = argumentTypes.length + 1;
						    mv.visitMaxs(maxLocals + 2, maxLocals);
						    mv.visitEnd();
						    methods.add(name + "|" + desc);
						    return null;
						}
			        }, ClassReader.EXPAND_FRAMES);
				}
			} else {
				cw.visit(version, access, name, signature, superClass.toString(), interfaces);
				MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
				mv.visitCode();
				mv.visitVarInsn(ALOAD,0);
				mv.visitMethodInsn(INVOKESPECIAL, superClass.toString(), "<init>", "()V", false);
				mv.visitInsn(RETURN);
				mv.visitMaxs(0,0);
				mv.visitEnd();
			}
			cw.visitEnd();
			return cw.toByteArray();
		} catch (Throwable e) {
			log.error("SubClassProcessor : create class error.", e);
			return null;
		} else return inject.operate(name, superClass, interfaces).toByteArray();
	}
	
	@Override
	public byte[] process(Object instance) {
		if (superClass != null) {
			String name = namingPolicy.process(instance);
			if (name != null) return generate(name, superClass, interfaces);
		}
		return null;
	}

	@Override
	public byte[] perform(Object first, Object second) {
		String name = namingPolicy.process(first);
		Object superClass = null;
		if (second != null) if (second instanceof Map) {
			Map<String, Object> tmp = new LinkedHashMap<String, Object>();
			for (Entry<String, Object> e : ((Map<String, Object>)second).entrySet()) tmp.put(superPolicy.process(e.getKey()), e.getValue());
			if (tmp.size() > 0) superClass = tmp;
		} else if (second instanceof String) superClass =  superPolicy.process(second.toString());
		else if (second instanceof Class) superClass = superPolicy.process(((Class<?>) second).getName());
		else superClass = superPolicy.process(second.getClass().getName());
		if (name != null && superClass != null) return generate(name, superClass, interfaces);
		else return null;
	}

	@Override
	public byte[] operate(Object first, Object second, Collection<Object> third) {
		String name = namingPolicy.process(first);
		Object superClass = null;
		if (second != null) if (second instanceof Map) {
			Map<String, Object> tmp = new LinkedHashMap<String, Object>();
			for (Entry<String, Object> e : ((Map<String, Object>)second).entrySet()) tmp.put(superPolicy.process(e.getKey()), e.getValue());
			if (tmp.size() > 0) superClass = tmp;
		} else if (second instanceof String) superClass =  superPolicy.process(second.toString());
		else if (second instanceof Class) superClass = superPolicy.process(((Class<?>) second).getName());
		else superClass = superPolicy.process(second.getClass().getName());
		String[] interfaces;
		if (third == null || third.size() == 0) interfaces = null;
		else {
			ArrayList<String> ls = new ArrayList<String>(third.size());
			for (Object intf : third) if (intf instanceof String) ls.add(superPolicy.process(intf.toString()));
			else if (intf instanceof Class) ls.add(superPolicy.process(((Class<?>) intf).getName()));
			else if (intf != null) ls.add(superPolicy.process(intf.getClass().getName()));
			interfaces = ls.size() == 0 ? null : ls.toArray(new String[ls.size()]);
		}
		if (name != null && superClass != null) return generate(name, superClass, interfaces);
		else return null;
	}
}
