package net.yeeyaa.eight.core.processor;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.ITriProcessor;
import net.yeeyaa.eight.IProcessor;

public class AccommodateProcessor implements IBiProcessor<Object, Object, Object>, IProcessor<Object, Object>, ITriProcessor<Object, Object, Object, Object> {
	protected IProcessor<Object, Object> processor;
	protected IBiProcessor<Object, Object, Object> bi;
	protected ITriProcessor<Object, Object, Object, Object> tri;
	
	public void setProcessor(IProcessor<Object, Object> processor) {
		this.processor = processor;
	}

	public void setBi(IBiProcessor<Object, Object, Object> bi) {
		this.bi = bi;
	}

	public void setTri(ITriProcessor<Object, Object, Object, Object> tri) {
		this.tri = tri;
	}

	@Override
	public Object operate(Object first, Object second, Object third) {
		if (processor != null) return processor.process(new Object[]{first, second, third});
		else if (bi != null) return bi.perform(new Object[]{first, second}, third);
		else if (tri != null) return tri.operate(first, second, third);
		else return null;
	}

	@Override
	public Object process(Object instance) {
		if (processor != null) return processor.process(instance);
		else if (bi != null) {
			if (instance instanceof Object[] && ((Object[])instance).length > 1) return bi.perform(((Object[])instance)[0], ((Object[])instance)[1]);
		} else if (tri != null && instance instanceof Object[] && ((Object[])instance).length > 2) return tri.operate(((Object[])instance)[0], ((Object[])instance)[1], ((Object[])instance)[2]);
		return null;
	}

	@Override
	public Object perform(Object first, Object second) {
		if (processor != null) return processor.process(new Object[]{first, second});
		else if (bi != null) return bi.perform(first, second);
		else if (tri != null && first instanceof Object[] && ((Object[])first).length > 1) return tri.operate(((Object[])first)[0], ((Object[])first)[1], second);
		else return null;
	}
}
