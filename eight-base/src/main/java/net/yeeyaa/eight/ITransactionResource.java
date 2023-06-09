package net.yeeyaa.eight;


public interface ITransactionResource<K, V, U extends IResource<K, V>, R> extends IResource<K, V>, ITransaction<K, V, U, R> {}
