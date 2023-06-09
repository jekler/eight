package net.yeeyaa.eight.common.spring;

import org.springframework.core.io.ResourceLoader;
import org.springframework.scripting.ScriptSource;
import org.springframework.scripting.support.ResourceScriptSource;
import org.springframework.scripting.support.ScriptFactoryPostProcessor;
import org.springframework.scripting.support.StaticScriptSource;

public class PlatformScriptFactoryPostProcessor extends ScriptFactoryPostProcessor {
	@Override
	protected ScriptSource convertToScriptSource(String beanName, String scriptSourceLocator, ResourceLoader resourceLoader) {
		if (scriptSourceLocator.startsWith(INLINE_SCRIPT_PREFIX)) return new StaticScriptSource(scriptSourceLocator.substring(INLINE_SCRIPT_PREFIX.length()), beanName);
		else return new ResourceScriptSource(resourceLoader.getResource(beanName));
	}
}
