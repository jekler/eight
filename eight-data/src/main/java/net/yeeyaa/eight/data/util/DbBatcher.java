package net.yeeyaa.eight.data.util;

import java.util.Collection;
import java.util.HashSet;

import javax.annotation.PreDestroy;

import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.data.dao.PlatformDao;


public class DbBatcher implements IProcessor<Object, Object>{
	protected OperatorType optype = OperatorType.SAVEORUPDATE;
	protected Integer batchsize = 1;
	protected final HashSet<Object> batchset = new HashSet<Object>();
	protected PlatformDao platformDao;
	
	public void setPlatformDao(PlatformDao platformDao) {
		if(platformDao != null) this.platformDao = platformDao;
	}
		
	public enum OperatorType{
		SAVE,
		UPDATE,
		SAVEORUPDATE;	
	}

	public OperatorType getOptype() {
		return optype;
	}

	public void setOptype(OperatorType optype) {
		this.optype = optype;
	}

	public Integer getBatchsize() {
		return batchsize;
	}

	public void setBatchsize(Integer batchsize) {
		if(batchsize > 0) this.batchsize = batchsize;
	}
	
	@PreDestroy
	public synchronized void finalized(){
		if(batchset.size() > 0){
			if(OperatorType.SAVE.equals(optype)) platformDao.batchSave(batchset);
			else if(OperatorType.UPDATE.equals(optype)) platformDao.batchUpdate(batchset);
			else platformDao.batchSaveOrUpdate(batchset);
			batchset.clear();	
		}
	}

	@Override
	public synchronized Object process(Object instance) {
		if (instance != null) {
			if (instance instanceof Collection) batchset.addAll((Collection<Object>)instance);	
			else batchset.add(instance);
			if(batchset.size() > batchsize) {
				if(OperatorType.SAVE.equals(optype)) platformDao.batchSave(batchset);
				else if(OperatorType.UPDATE.equals(optype)) platformDao.batchUpdate(batchset);
				else platformDao.batchSaveOrUpdate(batchset);
				batchset.clear();	
			}
		}
		return instance;
	}
}
