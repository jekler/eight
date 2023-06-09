package net.yeeyaa.eight.core.processor;

import java.io.IOException;
import java.io.StringWriter;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.SchemaOutputResolver;
import javax.xml.transform.Result;
import javax.xml.transform.stream.StreamResult;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;


public class SchemaProcessor<T> implements IProcessor<JAXBContext, String> {
	protected IProcessor<String[], String> systemId;

	public void setSystemId(IProcessor<String[], String> systemId) {
		this.systemId = systemId;
	}

	@Override
	public String process(JAXBContext in) {	
		if(in != null) try{
			final StringWriter writer = new StringWriter();
			in.generateSchema(new SchemaOutputResolver() {
				@Override
				public Result createOutput(String namespaceUri, String suggestedFileName) throws IOException {
					Result result = new StreamResult(writer);
					if (systemId != null) result.setSystemId(systemId.process(new String[]{namespaceUri, suggestedFileName}));
					return result;
				}
			});
			return writer.toString();
		}catch(Exception e){
			throw new PlatformException(PlatformError.ERROR_PERFORMING_FAIL, e);
		}
		return null;
	}
}
