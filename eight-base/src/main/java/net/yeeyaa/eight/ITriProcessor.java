package net.yeeyaa.eight;

public interface ITriProcessor<T, K, V, R> {
	public R operate(T first, K second, V third);
}
