package net.yeeyaa.eight.common.processor;

import java.io.StringReader;

import javax.xml.transform.stream.StreamSource;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.springframework.oxm.Unmarshaller;


public class UnmarshallerProcessor implements IProcessor<String, Object> {
	protected Unmarshaller unmarshaller;
	
	public void setUnmarshaller(Unmarshaller unmarshaller) {
		this.unmarshaller = unmarshaller;
	}

	@Override
	public Object process(String in) {	
		if(unmarshaller != null && in != null && in.length() > 0) try{	
			return unmarshaller.unmarshal(new StreamSource(new StringReader(in)));
		}catch(Exception e){
			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
		}
		return null;
	}
}
