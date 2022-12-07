package net.yeeyaa.eight.common.processor;

import java.io.StringWriter;

import javax.xml.transform.stream.StreamResult;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.springframework.oxm.Marshaller;


public class MarshallerProcessor<T> implements IProcessor<T, String> {
	protected Marshaller marshaller;
	
	public void setMarshaller(Marshaller marshaller) {
		this.marshaller = marshaller;
	}

	@Override
	public String process(T in) {	
		if(marshaller != null && in != null) try{
			StringWriter writer = new StringWriter();
			marshaller.marshal(in, new StreamResult(writer));
			return writer.toString();
		}catch(Exception e){
			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
		}
		return null;
	}
}