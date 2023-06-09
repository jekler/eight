package net.yeeyaa.eight.common.spring;

import org.springframework.scripting.groovy.GroovyObjectCustomizer;
import org.springframework.scripting.groovy.GroovyScriptFactory;

public class PlatformGroovyScriptFactory extends GroovyScriptFactory {
	public PlatformGroovyScriptFactory() {
		super("null");
	}

	public PlatformGroovyScriptFactory(GroovyObjectCustomizer groovyObjectCustomizer) {
		super("null", groovyObjectCustomizer);
	}
}
