package net.yeeyaa.eight;

public interface IBiProcessor<K, V, R> {
	public R perform(K first, V second);
}
