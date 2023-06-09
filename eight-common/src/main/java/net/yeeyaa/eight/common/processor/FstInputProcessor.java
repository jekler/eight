package net.yeeyaa.eight.common.processor;

import java.io.ByteArrayInputStream;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


import de.ruedigermoeller.serialization.FSTConfiguration;
import de.ruedigermoeller.serialization.FSTObjectInput;

public class FstInputProcessor implements IProcessor<byte[], Object> {
	protected final Logger log;
	protected IProcessor<Object, FSTConfiguration> conf;

	public FstInputProcessor() {
		this.log = LoggerFactory.getLogger(FstInputProcessor.class);
	}

	public FstInputProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(FstInputProcessor.class) : log;
	}
	
    public void setConf(IProcessor<Object, FSTConfiguration> conf) {
		this.conf = conf;
	}
	
	@Override
	public Object process(byte[] bytes) {
        if (bytes != null && bytes.length > 0) try {
            ByteArrayInputStream is = new ByteArrayInputStream(bytes);
            FSTObjectInput ois = conf.process(null).getObjectInput(is);
            return ois.readObject();
        } catch (Exception e) {
            log.error("FstInputProcessor: fail to deserialize bytes.", e);
        }
        return null;
	}
}
