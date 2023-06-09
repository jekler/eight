package net.yeeyaa.eight.common.aop;

import net.yeeyaa.eight.IProcessor;

import org.springframework.asm.Attribute;
import org.springframework.asm.ClassReader;
import org.springframework.asm.ClassVisitor;
import org.springframework.asm.ClassWriter;


public class AsmProcessor implements IProcessor<byte[], byte[]>{
	protected Integer writerFlag = ClassWriter.COMPUTE_FRAMES;
	protected Integer readerFlag = ClassReader.SKIP_DEBUG;
	protected Attribute[] attributes;
	protected IProcessor<ClassVisitor, ClassVisitor> visitor;
	
	public void setWriterFlag(Integer writerFlag) {
		this.writerFlag = writerFlag;
	}

	public void setReaderFlag(Integer readerFlag) {
		this.readerFlag = readerFlag;
	}

	public void setAttributes(Attribute[] attributes) {
		this.attributes = attributes;
	}

	public void setVisitor(IProcessor<ClassVisitor, ClassVisitor> visitor) {
		this.visitor = visitor;
	}

	@Override
	public byte[] process(byte[] instance) {
		ClassWriter cw = new ClassWriter(writerFlag);
		ClassVisitor visitor;
		if(this.visitor == null) visitor = cw;
		else visitor = this.visitor.process(cw);
		ClassReader reader = new ClassReader(instance);
		if(attributes == null) reader.accept(visitor, readerFlag);
		else reader.accept(visitor, attributes, readerFlag);
		return cw.toByteArray();
	}
}
