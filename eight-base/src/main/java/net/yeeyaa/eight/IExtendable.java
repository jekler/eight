package net.yeeyaa.eight;

import java.util.Collection;

public interface IExtendable<M> {
	public Collection<M> methods();
	public <N> N extend(M method);
}
