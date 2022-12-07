package net.yeeyaa.eight;

public interface IProcessor<T,R> {
	public R process(T object);
/*	public R x(T... object);
	public <P(maybe string), K, V> IThing<K, V, P> on(U aspect);
	public <P extends IThing> Collection<U> e();*/
}
