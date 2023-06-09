package net.yeeyaa.eight.common.processor;

import net.yeeyaa.eight.IProcessor;

import de.ruedigermoeller.serialization.FSTConfiguration;

public class FstProcessor implements IProcessor<Object, FSTConfiguration> {
	protected final FSTConfiguration conf = FSTConfiguration.createDefaultConfiguration();
	
	@Override
	public FSTConfiguration process(Object paras) {
		return conf;
	}
}
