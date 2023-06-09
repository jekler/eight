package net.yeeyaa.eight.data.processor;

import java.util.Map.Entry;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.data.entity.SettingEntity;
import net.yeeyaa.eight.core.util.Content.Couple;

public class SettingBeanProcessor implements IProcessor<SettingEntity, Entry<String, String>>{
	@Override
	public Entry<String, String> process(SettingEntity in) {
		return new Couple<String, String>(((SettingEntity)in).getVariable(), ((SettingEntity)in).getData());
	}
}
