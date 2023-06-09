package net.yeeyaa.eight.common.processor;

import java.io.Writer;

import javax.xml.transform.stream.StreamResult;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.util.FragmentStringWriter;

import org.springframework.oxm.Marshaller;


public class MarshallerProcessor<T> implements IProcessor<T, String>, IBiProcessor<T, Integer, String> {
	protected Marshaller marshaller;
	protected int size = 8192;
	
	public void setMarshaller(Marshaller marshaller) {
		this.marshaller = marshaller;
	}

	public void setSize(Integer size) {
		if (size != null && size > 0) this.size = size;
	}

	@Override
	public String process(T in) {	
		return perform(in, size);
	}

	@Override
	public String perform(T in, Integer length) {
		if(marshaller != null && in != null) try{
			Writer writer = length == null ? new FragmentStringWriter() : new FragmentStringWriter(length);
			marshaller.marshal(in, new StreamResult(writer));
			return writer.toString();
		}catch(Exception e){
			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
		}
		return null;
	}
}
