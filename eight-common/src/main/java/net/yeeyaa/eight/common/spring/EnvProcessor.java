package net.yeeyaa.eight.common.spring;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IProcessor;

import org.springframework.context.EnvironmentAware;
import org.springframework.core.env.Environment;

public class EnvProcessor implements EnvironmentAware, IProcessor<String, Object>, IBiProcessor<String, Object, Object> {
	protected Environment env;

	@Override
	public void setEnvironment(Environment env) {
		this.env = env;
	}

	@Override
	public Object process(String object) {
		return env.getProperty(object);
	}

	@Override
	public Object perform(String paras, Object flag) {
		if (flag == null) return  env.getProperty(paras);
		else if (flag instanceof Class) return env.getProperty(paras, (Class) flag);
		else if (Boolean.TRUE.equals(flag)) return System.getenv(paras);
		else return System.getProperty(paras);
	}
}
