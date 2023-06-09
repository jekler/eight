package net.yeeyaa.eight.data.util;

import java.util.UUID;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;

import org.hibernate.engine.spi.SessionImplementor;
import org.hibernate.id.UUIDGenerationStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class UuidGenerationStrategy implements UUIDGenerationStrategy {
	protected final static Logger log = LoggerFactory.getLogger(UuidGenerationStrategy.class);
	protected final Pattern gen = Pattern.compile("[\\w.$]*[.$]gen_(\\d+)[\\w.$]*");
	protected final Pattern ver = Pattern.compile("[\\w.$]*[.$]ver_(\\d+)[\\w.$]*");
	protected IBiProcessor<String, Object, Object> bi;
	protected String classname;
	protected int version = 4;
	protected IProcessor<SessionImplementor, UUID> generator;
	
	protected class Processor implements IProcessor<SessionImplementor, UUID> {
		protected volatile int resume; 
		protected volatile IProcessor<SessionImplementor, UUID> processor;
		
		public Processor(int resume) {
			this.resume = resume;
		}

		@Override
		public UUID process(SessionImplementor instance) {
			if (resume >= 0) {
				Object processor = bi.perform(classname, "gen");
				if (processor instanceof IProcessor) this.processor = (IProcessor<SessionImplementor, UUID>) processor;
				switch (resume) {
					case 0: resume = -1;
					break;
					case 1: if (processor != null) resume = -1;
					break;
					case 2: if (this.processor != null) resume = -1;
				}
			}
			if (processor == null) return null;
			else return processor.process(instance);
		}
	}	
	
	public UuidGenerationStrategy() {
		ClassLoader classloader = getClass().getClassLoader();
		if (classloader instanceof IBiProcessor) {
			bi = (IBiProcessor<String, Object, Object>) classloader;
			classname = getClass().getName();
			Matcher matcher = gen.matcher(classname);
			if(matcher.find()) try {
				generator = new Processor(Integer.parseInt(matcher.group(1)));
			} catch (Exception e) {
				log.error("UuidGenerationStrategy: init param error", e);
			}
			matcher = ver.matcher(classname);
			if(matcher.find()) try { 
				version = Integer.parseInt(matcher.group(1));
			} catch (Exception e) {
				log.error("UuidGenerationStrategy: init param error", e);
			}
		}
	}

	@Override
	public int getGeneratedVersion() {
		return version;
	}

	@Override
	public UUID generateUUID(SessionImplementor session) {
		if (generator == null) return UUID.randomUUID();
		else return generator.process(session);
	}
}
