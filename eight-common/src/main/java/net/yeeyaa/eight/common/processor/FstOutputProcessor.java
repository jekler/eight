package net.yeeyaa.eight.common.processor;

import java.io.ByteArrayOutputStream;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


import de.ruedigermoeller.serialization.FSTConfiguration;
import de.ruedigermoeller.serialization.FSTObjectOutput;

public class FstOutputProcessor implements IProcessor<Object, byte[]> {
	protected final Logger log;
	protected IProcessor<Object, FSTConfiguration> conf;
	protected static final byte[] EMPTY = new byte[0];
	
	public FstOutputProcessor() {
		this.log = LoggerFactory.getLogger(FstOutputProcessor.class);
	}

	public FstOutputProcessor(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(FstOutputProcessor.class) : log;
	}
	
    public void setConf(IProcessor<Object, FSTConfiguration> conf) {
		this.conf = conf;
	}
	
	@Override
	public byte[] process(Object graph) {
        if (graph != null) try {
            ByteArrayOutputStream os = new ByteArrayOutputStream();
            FSTObjectOutput oos = conf.process(null).getObjectOutput(os);
            oos.writeObject(graph);
            oos.flush();
            return os.toByteArray();
        } catch (Exception e) {
            log.error("FstOutputProcessor: fail to serializer graph. graph=" + graph, e);
        } 
        return EMPTY;
	}
}
