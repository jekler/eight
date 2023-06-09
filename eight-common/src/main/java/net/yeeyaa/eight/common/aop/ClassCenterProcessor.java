package net.yeeyaa.eight.common.aop;

import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.IllegalClassFormatException;
import java.security.ProtectionDomain;
import java.util.Collection;
import java.util.Map;

import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.agent.ClassCenter;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;

public class ClassCenterProcessor implements IBiProcessor<Boolean, Object, Object>, IListableResource<Object, byte[]>, IExtendable<Object>, ClassFileTransformer {
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[] { ResourceMethod.count });
	protected ITriProcessor<String, byte[], Object, byte[]> transformer;
	protected Boolean mode;
	protected final IProcessor<Object[], Object> count = new IProcessor<Object[], Object>() {
		@Override
		public Object process(Object[] paras) {
			return ClassCenter.count();
		}
	};

	public void setMode(Boolean mode) {
		this.mode = mode;
	}

	public void setTransformer(ITriProcessor<String, byte[], Object, byte[]> transformer) {
		if (transformer != null) {
			this.transformer = transformer;
			ClassCenter.regTransformer(this);
		}
	}

	@PreDestroy
	public void destroy(){
		ClassCenter.unregTransformer(this);
	}
	
	@Override
	public Object perform(Boolean cond, Object instance) {
		if (mode == null) if (instance == null) return null;
		else if (instance instanceof Class) return ClassCenter.find((Class<?>) instance);
		else return ClassCenter.find(instance.getClass());
		else if (mode) if (cond == null)  ClassCenter.clearTransformer();
		else if (cond) {
			if (instance instanceof ClassFileTransformer) ClassCenter.regTransformer((ClassFileTransformer) instance);
		} else {
			if (instance instanceof ClassFileTransformer) ClassCenter.unregTransformer((ClassFileTransformer) instance);
		} else if (cond == null)  ClassCenter.clearRegex();
		else if (cond) {
			if (instance instanceof String) ClassCenter.regRegex((String) instance);
		} else if (instance instanceof String) ClassCenter.unregRegex((String) instance);
		return null;
	}

	@Override
	public byte[] find(Object... paras) {
		if (paras == null || paras.length == 0 || paras[0] == null) return null;
		else if (paras[0] instanceof Class) return ClassCenter.find((Class<?>) paras[0]);
		else return ClassCenter.find(paras[0].getClass());
	}

	@Override
	public <P> P store(byte[] value, Object... paras) {
		if (paras != null && paras.length > 1 && paras[0] != null && paras[1] != null) ClassCenter.store(value, paras[0].toString(), paras[1].toString());
		return null;
	}

	@Override
	public <P> P discard(Object... paras) {
		if (paras != null && paras.length > 1 && paras[0] != null) if (paras[0] instanceof Class) return (P) ClassCenter.discard((Class<?>) paras[0]);
		else return (P) ClassCenter.discard(paras[0].getClass());
		else return null;
	}

	@Override
	public <P> P empty(Object... paras) {
		ClassCenter.empty();
		return null;
	}

	@Override
	public Collection<Object[]> keys(Object... paras) {
		return ClassCenter.keys();
	}

	@Override
	public Map<Object[], byte[]> all(Object... paras) {
		return ClassCenter.all();
	}

	@Override
	public <N> N extend(Object object) {
		if (object != null) {
			Object method = object instanceof ResourceMethod ? object : methods.process(object);
			if (method != null) switch ((ResourceMethod) method) {
				case count: return (N) count;
			}
		}
		return null;
	}

	@Override
	public Collection<Object> methods() {
		return methods;
	}

	@Override
	public byte[] transform(ClassLoader loader, String className, Class<?> classBeingRedefined, ProtectionDomain protectionDomain, byte[] classfileBuffer) throws IllegalClassFormatException {
		return transformer.operate(className, classfileBuffer, new Object[]{loader, classBeingRedefined, protectionDomain});
	}
}
