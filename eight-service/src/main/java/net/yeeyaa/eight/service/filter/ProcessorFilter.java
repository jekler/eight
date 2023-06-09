package net.yeeyaa.eight.service.filter;

import net.yeeyaa.eight.IProcessor;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class ProcessorFilter implements IProcessor<Object, Object>{
	protected final Logger log;
	protected IProcessor<Object, Object> next;
	protected IProcessor<Object, Object> in;
	protected IProcessor<Object, Object> out;	
	protected Boolean ignoreError;

	public ProcessorFilter() {
		log = LoggerFactory.getLogger(ProcessorFilter.class);
	}

	public ProcessorFilter(Logger log) {
		this.log = log == null ? LoggerFactory.getLogger(ProcessorFilter.class) : log;
	}
	
	public void setIgnoreError(Boolean ignoreError) {
		this.ignoreError = ignoreError;
	}

	public void setNext(IProcessor<Object, Object> next) {
		this.next = next;
	}

	public void setIn(IProcessor<Object, Object> in) {
		this.in = in;
	}

	public void setOut(IProcessor<Object, Object> out) {
		this.out = out;
	}

	@Override
	public Object process(Object msg) {
		if (ignoreError  == null) try {
			if(in != null) msg = in.process(msg);
		} finally {
			try {
				msg = next.process(msg);
			} finally {
				if(out != null) msg = out.process(msg);
			}
		} else if (ignoreError) {
			if(in != null) try {
				msg = in.process(msg);
			} catch (Exception e) {
				log.error("ProcessorFilter: processor failed.", e);
			}
			try {
				msg = next.process(msg);
			} catch (Exception e) {
				log.error("ProcessorFilter: processor failed.", e);
			}
			if(out != null) try {
				msg = out.process(msg);
			} catch (Exception e) {
				log.error("ProcessorFilter: processor failed.", e);
			}
		} else {
			if(in != null) msg = in.process(msg);
			msg = next.process(msg);
			if(out != null) msg = out.process(msg);
		}
		return msg;
	}
}
