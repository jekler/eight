package net.yeeyaa.eight.common.aop;

import java.util.Map;

import net.yeeyaa.eight.IProcessor;

import org.springframework.asm.AnnotationVisitor;
import org.springframework.asm.Attribute;
import org.springframework.asm.ClassVisitor;
import org.springframework.asm.FieldVisitor;
import org.springframework.asm.MethodVisitor;
import org.springframework.asm.Opcodes;
import org.springframework.asm.TypePath;


public class VisitorProcessor extends ClassVisitor implements IProcessor<ClassVisitor, ClassVisitor>{
	protected Integer api = Opcodes.ASM4;
	protected Map<String, IProcessor<Object, Object>> visitors;
	protected IProcessor<Object, Object> defaultVisitor;
	
	public void setApi(Integer api) {
		if(api != null) this.api = api;
	}

	public void setClassVisitor(ClassVisitor cv) {
		if(cv != null) this.cv = cv;
	}
	
	public void setVisitors(Map<String, IProcessor<Object, Object>> visitors) {
		this.visitors = visitors;
	}

	public void setDefaultVisitor(IProcessor<Object, Object> defaultVisitor) {
		this.defaultVisitor = defaultVisitor;
	}

	public VisitorProcessor() {
		super(Opcodes.ASM4);
	}

	public VisitorProcessor(int api, ClassVisitor cv) {
		super(api, cv);
	}

	public VisitorProcessor(int api) {
		super(api);
	}

	@Override
	public ClassVisitor process(ClassVisitor instance) {
		VisitorProcessor visitor;
		if(instance == null) visitor = new VisitorProcessor(api, cv);
		else visitor = new VisitorProcessor(api, instance);
		if(visitors != null) visitor.visitors = visitors;
		return visitor;
	}

	protected IProcessor<Object, Object> visit(String key) {
		IProcessor<Object, Object> visitor = defaultVisitor;
		if(visitors != null && visitors.containsKey(key)) visitor = visitors.get(key);
		return visitor;
	}
	
	@Override
	public void visit(int version, int access, String name, String signature, String superName, String[] interfaces) {
		IProcessor<Object, Object> visitor = visit("visit");
		if(visitor != null) visitor.process(new Object[]{version, access, name, signature, superName, interfaces, cv});
		else if(cv != null) cv.visit(version, access, name, signature, superName, interfaces);
	}

	@Override
	public void visitSource(String source, String debug) {
		IProcessor<Object, Object> visitor = visit("visitSource");
		if(visitor != null) visitor.process(new Object[]{source, debug, cv});
		else if(cv != null) cv.visitSource(source, debug);
	}

	@Override
	public void visitOuterClass(String owner, String name, String desc) {
		IProcessor<Object, Object> visitor = visit("visitOuterClass");
		if(visitor != null) visitor.process(new Object[]{owner, name, desc, cv});
		else if(cv != null) cv.visitOuterClass(owner, name, desc);
	}

	@Override
	public void visitAttribute(Attribute attr) {
		IProcessor<Object, Object> visitor = visit("visitAttribute");
		if(visitor != null) visitor.process(new Object[]{attr, cv});
		else if(cv != null) cv.visitAttribute(attr);
	}

	@Override
	public void visitInnerClass(String name, String outerName, String innerName, int access) {
		IProcessor<Object, Object> visitor = visit("visitInnerClass");
		if(visitor != null) visitor.process(new Object[]{name, outerName, innerName, access, cv});
		else if(cv != null) cv.visitInnerClass(name, outerName, innerName, access);
	}

	@Override
	public FieldVisitor visitField(int access, String name, String desc, String signature, Object value) {
		FieldVisitor field = null;
		if(cv != null)  field = cv.visitField(access, name, desc, signature, value);
		IProcessor<Object, Object> visitor = visit("visitField");
		if(visitor != null) field = (FieldVisitor) visitor.process(new Object[]{access, name, desc, signature, value, field});
		return field;
	}

	@Override
	public MethodVisitor visitMethod(int access, String name, String desc, String signature, String[] exceptions) {
		MethodVisitor method = null;
		if(cv != null)  method = cv.visitMethod(access, name, desc, signature, exceptions);
		IProcessor<Object, Object> visitor = visit("visitMethod");
		if(visitor != null) method = (MethodVisitor) visitor.process(new Object[]{access, name, desc, signature, exceptions, method});
		return method;
	}

	@Override
	public AnnotationVisitor visitAnnotation(String desc, boolean visible) {
		AnnotationVisitor annotation = null;
		if(cv != null)  annotation = cv.visitAnnotation(desc, visible);
		IProcessor<Object, Object> visitor = visit("visitAnnotation");
		if(visitor != null) annotation = (AnnotationVisitor) visitor.process(new Object[]{desc, visible, annotation});
		return annotation;
	}

	@Override
	public AnnotationVisitor visitTypeAnnotation(int typeRef, TypePath typePath, String desc, boolean visible) {
		AnnotationVisitor annotation = null;
		if(cv != null)  annotation = cv.visitTypeAnnotation(typeRef, typePath, desc, visible);
		IProcessor<Object, Object> visitor = visit("visitTypeAnnotation");
		if(visitor != null) annotation = (AnnotationVisitor) visitor.process(new Object[]{typeRef, typePath, desc, visible, annotation});
		return annotation;
	}
	
	@Override
	public void visitEnd() {
		IProcessor<Object, Object> visitor = visit("visitEnd");
		if(visitor != null) visitor.process(new Object[]{cv});
		else if(cv != null) cv.visitEnd();
	}
}
