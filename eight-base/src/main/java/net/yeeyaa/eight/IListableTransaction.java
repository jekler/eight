package net.yeeyaa.eight;

public interface IListableTransaction<K, V, U extends IResource<K, V>, R> extends IListableResource<K, V>, ITransactionResource<K, V, U, R> {}
