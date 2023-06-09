package net.yeeyaa.eight.common.util;

import javax.xml.bind.annotation.adapters.XmlAdapter;

import net.yeeyaa.eight.IProcessor;

import org.springframework.asm.ClassWriter;
import org.springframework.asm.MethodVisitor;
import org.springframework.asm.Opcodes;


public class TypeAdaptor<T, R> extends XmlAdapter<T, R> {
	protected IProcessor<R, T> marshal;
	protected IProcessor<T, R> unmarshal;
	protected Boolean nullable = false;

	public void setMarshal(IProcessor<R, T> marshal) {
		this.marshal = marshal;
	}

	public void setUnmarshal(IProcessor<T, R> unmarshal) {
		this.unmarshal = unmarshal;
	}

	public void setNullable(Boolean nullable) {
		if (nullable != null) this.nullable = nullable;
	}

	@Override
    public R unmarshal(T v) throws Exception {
        if ((v != null || nullable) && unmarshal != null) return unmarshal.process(v);
        else return (R) v;
    }

    @Override
    public T marshal(R v) throws Exception {
        if ((v != null || nullable) && marshal != null) return marshal.process(v);
        else return (T) v;
    }
    
    public static class SubClass implements Opcodes, IProcessor<Object, byte[]>{
    	protected String bundle;
    	protected String value; 
    	protected String bundlecast;
    	protected String valuecast;
    	protected String superName = "net/yeeyaa/eight/common/util/TypeAdaptor";
    	protected IProcessor<Object, String> namingPolicy;
    	
    	protected String[] name(String in) {
    		Boolean array = false;
    		int index = in.lastIndexOf("[]");
    		if (index > -1) {
    			in = in.substring(0, index).trim();
    			array = true;
    		}
    		if ("int".equals(in)) return array ? new String[]{"[I", "[I"} : new String[]{"I", "I"};
    		else if ("boolean".equals(in)) return array ? new String[]{"[Z", "[Z"} : new String[]{"Z", "Z"};
    		else if ("byte".equals(in)) return array ? new String[]{"[B", "[B"} : new String[]{"B", "B"};
    		else if ("char".equals(in)) return array ? new String[]{"[C", "[C"} : new String[]{"C", "C"};
    		else if ("short".equals(in)) return array ? new String[]{"[S", "[S"} : new String[]{"S", "S"};
    		else if ("float".equals(in)) return array ? new String[]{"[F", "[F"} : new String[]{"F", "F"};
    		else if ("double".equals(in)) return array ? new String[]{"[D", "[D"} : new String[]{"D", "D"};
    		else if ("long".equals(in)) return array ? new String[]{"[J", "[J"} : new String[]{"J", "J"};
    		else if ("void".equals(in)) return new String[]{"V", "V"};
    		else return array ? new String[]{"[L" + in.replace('.', '/') + ";", "[L" + in.replace('.', '/') + ";"} : new String[] {"L" + in.replace('.', '/') + ";", in.replace('.', '/')};
    	}
    	
		public void setNamingPolicy(IProcessor<Object, String> namingPolicy) {
			this.namingPolicy = namingPolicy;
		}

		public void setBundle(String bundle) {
			if (bundle != null) {
				String[] kv = name(bundle);
				this.bundle = kv[0];
				this.bundlecast = kv[1];
			}
		}

		public void setValue(String value) {
			if (value != null) {
				String[] kv = name(value);
				this.value = kv[0];
				this.valuecast = kv[1];
			}
		}

		@Override
		public byte[] process(Object instance) {
			String name;
			if (namingPolicy == null) name = instance == null ? null : instance.toString().replace('.', '/');
			else name = namingPolicy.process(instance);
			if (name != null && bundle != null && value != null) {
				ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
				cw.visit(V1_6, ACC_PUBLIC + ACC_SUPER, name, "L" + superName + "<" + value + bundle + ">;", superName, null);
				cw.visitSource(name + ".java", null);
				MethodVisitor mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
				mv.visitCode();
				mv.visitVarInsn(ALOAD,0);
				mv.visitMethodInsn(INVOKESPECIAL, superName, "<init>", "()V", false);
				mv.visitInsn(RETURN);
				mv.visitMaxs(0,0);
				mv.visitEnd();
				mv = cw.visitMethod(ACC_PUBLIC, "unmarshal", "(" + value + ")" + bundle, null, new String[] { "java/lang/Exception" });
				mv.visitCode();
				mv.visitVarInsn(ALOAD, 0);
				mv.visitVarInsn(ALOAD, 1);
				mv.visitMethodInsn(INVOKESPECIAL, superName, "unmarshal", "(Ljava/lang/Object;)Ljava/lang/Object;", false);
				mv.visitTypeInsn(CHECKCAST, bundlecast);
				mv.visitInsn(ARETURN);
				mv.visitMaxs(2, 2);
				mv.visitEnd();
				mv = cw.visitMethod(ACC_PUBLIC, "marshal", "(" + bundle + ")" + value, null, new String[] { "java/lang/Exception" });
				mv.visitCode();
				mv.visitVarInsn(ALOAD, 0);
				mv.visitVarInsn(ALOAD, 1);
				mv.visitMethodInsn(INVOKESPECIAL, superName, "marshal", "(Ljava/lang/Object;)Ljava/lang/Object;", false);
				mv.visitTypeInsn(CHECKCAST, valuecast);
				mv.visitInsn(ARETURN);
				mv.visitMaxs(2, 2);
				mv.visitEnd();
				mv = cw.visitMethod(ACC_PUBLIC + ACC_BRIDGE + ACC_SYNTHETIC, "unmarshal", "(Ljava/lang/Object;)Ljava/lang/Object;", null, new String[] { "java/lang/Exception" });
				mv.visitCode();
				mv.visitVarInsn(ALOAD, 0);
				mv.visitVarInsn(ALOAD, 1);
				mv.visitTypeInsn(CHECKCAST, valuecast);
				mv.visitMethodInsn(INVOKEVIRTUAL, name, "unmarshal", "(" + value +  ")" + bundle, false);
				mv.visitInsn(ARETURN);
				mv.visitMaxs(2, 2);
				mv.visitEnd();
				mv = cw.visitMethod(ACC_PUBLIC + ACC_BRIDGE + ACC_SYNTHETIC, "marshal", "(Ljava/lang/Object;)Ljava/lang/Object;", null, new String[] { "java/lang/Exception" });
				mv.visitCode();
				mv.visitVarInsn(ALOAD, 0);
				mv.visitVarInsn(ALOAD, 1);
				mv.visitTypeInsn(CHECKCAST, bundlecast);
				mv.visitMethodInsn(INVOKEVIRTUAL, name, "marshal", "(" + bundle + ")" + value , false);
				mv.visitInsn(ARETURN);
				mv.visitMaxs(2, 2);
				mv.visitEnd();
				cw.visitEnd();
				return cw.toByteArray();
			}
			return null;
		}
    }
}
