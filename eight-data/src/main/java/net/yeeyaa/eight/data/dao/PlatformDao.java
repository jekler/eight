package net.yeeyaa.eight.data.dao;

import static org.hibernate.criterion.Example.create;

import java.io.Serializable;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javassist.Modifier;

import net.yeeyaa.eight.IBiProcessor;
import net.yeeyaa.eight.IExtendable;
import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IListableResource;
import net.yeeyaa.eight.IListableTransaction;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.ITransaction;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.common.util.Pojo;
import net.yeeyaa.eight.core.util.TypeConvertor;
import net.yeeyaa.eight.core.PlatformError;
import net.yeeyaa.eight.core.enumerate.ResourceMethod;
import net.yeeyaa.eight.core.util.MapperSet;
import net.yeeyaa.eight.core.util.PlatformUtil;
import net.yeeyaa.eight.data.util.MultipleCountProjection;

import org.hibernate.Criteria;
import org.hibernate.type.Type;
import org.hibernate.CacheMode;
import org.hibernate.FetchMode;
import org.hibernate.Query;
import org.hibernate.SQLQuery;
import org.hibernate.Session;
import org.hibernate.sql.JoinType;
import org.hibernate.SessionFactory;
import org.hibernate.collection.internal.AbstractPersistentCollection;
import org.hibernate.criterion.Criterion;
import org.hibernate.criterion.DetachedCriteria;
import org.hibernate.criterion.MatchMode;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projection;
import org.hibernate.criterion.ProjectionList;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.criterion.SimpleProjection;
import org.hibernate.criterion.Subqueries;
import org.hibernate.proxy.HibernateProxy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class PlatformDao<PK extends Serializable, K, V, R> implements IListableTransaction<K, V, IListableResource<K, V>, R>, IProcessor<Object, Object>, IBiProcessor<IProcessor<V, R>, V, R>, IExtendable<Object> {
	protected final static Logger log = LoggerFactory.getLogger(PlatformDao.class);
	protected static final MapperSet<Object> methods = new MapperSet<Object>(new Object[]{ResourceMethod.count});
	protected IProcessor<Object, Class<?>> defaultLoader;
	protected IProcessor<Class<?>, Object> newInstance;	
	protected Integer type = 0;
	protected Integer putMode = 0;
	protected SessionFactory sessionFactory;
	protected Boolean cache;
	protected CacheMode mode;
	protected String region;
	protected final IProcessor<K[], Object> count = new IProcessor<K[], Object>(){
		@Override
		public Object process(K[] paras) {
			Long o = null;
			if(paras != null && paras.length > 0) try {
				Query query = getSession().createQuery((String)paras[0]);
				if (Boolean.TRUE.equals(cache)) {
					query.setCacheable(true);
					if (mode != null) query.setCacheMode(mode);
					if (region != null) query.setCacheRegion(region);
				}	
				o = (Long)query.uniqueResult();
			} catch (Exception e) {
				log.error("PlatformDao: count resource failed", e);	
			}
			return o;
		}
	};
	
	public void setCache(Boolean cache) {
		this.cache = cache;
	}

	public void setMode(CacheMode mode) {
		this.mode = mode;
	}

	public void setRegion(String region) {
		this.region = region;
	}
	
	public void setDefaultLoader(IProcessor<Object, Class<?>> defaultLoader) {
		this.defaultLoader = defaultLoader;
	}
	
	public void setPutMode(Integer putMode) {
		if(putMode != null && putMode > 0) this.putMode = putMode;
	}

	public void setType(Integer type) {
		if(type != null && (type == 1 || type == 2)) this.type = type;
	}
	
	public void setSessionFactory(SessionFactory sessionFactory) {
		this.sessionFactory = sessionFactory;
	}

	protected Session getSession(){
		return sessionFactory.getCurrentSession();
	}

	protected Session openSession(){
		return sessionFactory.openSession();
	}
	
	protected Class<?> findClass(Object o, IProcessor<String, Class<?>> cl){
		if (cl == null)return defaultLoader.process(o);
		else return cl.process((String)o);
	}
	
	protected Object newInstance(Class<?> clz) throws Exception {
		if (newInstance == null) return clz.newInstance();
		else return newInstance.process(clz);
	}
	
	public void setNewInstance(IProcessor<Class<?>, Object> newInstance) {
		this.newInstance = newInstance;
	}

	protected void unproxy(Object obj, Set<Object> set){
		if(obj instanceof Pojo) try {
			for(Field field : PlatformUtil.getAllField(obj.getClass())) if(!Modifier.isStatic(field.getModifiers())){
				Class<?> clz = field.getType();
				if(Pojo.class.isAssignableFrom(clz) || Collection.class.isAssignableFrom(clz)){
					field.setAccessible(true);
					Object o = field.get(obj);
					if (o instanceof AbstractPersistentCollection) if(((AbstractPersistentCollection)o).wasInitialized()){
						Object o1 = ((AbstractPersistentCollection)o).getValue();
						if(o1 != null && !set.contains(o1)) {
							set.add(o1);
							unproxy(o1, set);
						}
					}else if(type == 1) field.set(obj, null);
					else {
						Method m = AbstractPersistentCollection.class.getDeclaredMethod("read");
						m.setAccessible(true);
						m.invoke(o);
						Object o1 = ((AbstractPersistentCollection)o).getValue();
						if(o1 != null && !set.contains(o1)) {
							set.add(o1);
							unproxy(o1, set);
						}
					} else if(o instanceof HibernateProxy) if(type == 1) field.set(obj, null);
					else {
						Object o1 = ((HibernateProxy)o).getHibernateLazyInitializer().getImplementation();
						field.set(obj, o1);
						set.add(o1);
						unproxy(o1, set);
					} else if(o instanceof Pojo && !set.contains(o)) {
						set.add(o);
						unproxy(o, set);
					}
				}
			}
		} catch(Exception e) {
			log.error("PlatformDao: reset persistent failed", e);		
		} else if(obj instanceof Map) for(Entry<Object, Object> entry : ((Map<Object, Object>) obj).entrySet()) {
			Object key = entry.getKey();
			if (key != null && !set.contains(key)) {
				set.add(key);
				unproxy(key, set);
			}
			Object value = entry.getValue();
			if (value != null && !set.contains(value)) {
				set.add(value);
				unproxy(value, set);
			}
		} else if(obj.getClass().isArray()) for (int i = 0; i < Array.getLength(obj); i ++) {
			Object so = Array.get(obj, i);
			if(so != null && !set.contains(so)) {
				set.add(so);
				unproxy(so, set);
			}
		} else if(obj instanceof Collection) for(Object so : (Collection<Object>) obj) if(so != null && !set.contains(so)) {
			set.add(so);
			unproxy(so, set);
		} 
	}
	
	public Object process(Object obj){
	    if (type != 0 && obj != null) unproxy(obj, new HashSet<Object>());
	    return obj;
	}

	@Override
	public R perform(IProcessor<V, R> processor, V param) {
		if (processor == null) return null;
		else return processor.process(param);
	}
	
	public void save(V instance) {
		try {
			getSession().save(instance);
		} catch (RuntimeException re) {
			log.error("PlatformDao: save fail", re);
			throw re;
		}
	}

	public void saveOrUpdate(V instance) {
		try {
			getSession().saveOrUpdate(instance);
		} catch (RuntimeException re) {
			log.error("PlatformDao: save or update ", re);
			throw re;
		}
	}
	
	public void update(V instance) {
		try {
			getSession().update(instance);
		} catch (RuntimeException re) {
			log.error("PlatformDao: update fail", re);
			throw re;
		}
	}
	
	public void batchSaveOrUpdate(Collection<V> instance, Integer batchsize) {
		if(instance!= null && instance.size() > 0 && batchsize > 0)try {
			Session session = getSession();
			int i = 1;
			for (V inst : instance) {
			    session.saveOrUpdate(inst);
			    if ( i % batchsize == 0 ) {
			        session.flush();
			        session.clear();
			    }
			    i++;
			}
		} catch (RuntimeException re) {
			log.error("PlatformDao: batch save or update ", re);
			throw re;
		}
	}
	
	public void batchSaveOrUpdate(Collection<V> instance) {
		if(instance!= null && instance.size() > 0)try {
			Session session = getSession();
			for (V inst : instance) session.saveOrUpdate(inst);
		} catch (RuntimeException re) {
			log.error("PlatformDao: batch save or update ", re);
			throw re;
		}
	}
	
	public void batchSave(Collection<V> instance, Integer batchsize) {
		if(instance!= null && instance.size() > 0 && batchsize > 0)try {
			Session session = getSession();
			int i = 1;
			for (V inst : instance) {
			    session.save(inst);
			    if ( i % batchsize == 0 ) {
			        session.flush();
			        session.clear();
			    }
			    i++;
			}
		} catch (RuntimeException re) {
			log.error("PlatformDao: batch save ", re);
			throw re;
		}
	}

	public void batchSave(Collection<V> instance) {
		if(instance!= null && instance.size() > 0)try {
			Session session = getSession();
			for (V inst : instance) session.save(inst);
		} catch (RuntimeException re) {
			log.error("PlatformDao: batch save ", re);
			throw re;
		}
	}
	
	public void batchUpdate(Collection<V> instance, Integer batchsize) {
		if(instance!= null && instance.size() > 0 && batchsize > 0)try {
			Session session = getSession();
			int i = 1;
			for (V inst : instance) {
			    session.update(inst);
			    if ( i % batchsize == 0 ) {
			        session.flush();
			        session.clear();
			    }
			    i++;
			}
		} catch (RuntimeException re) {
			log.error("PlatformDao: batch update ", re);
			throw re;
		}
	}
	
	public void batchUpdate(Collection<V> instance) {
		if(instance!= null && instance.size() > 0)try {
			Session session = getSession();
			for (V inst : instance) session.update(inst);
		} catch (RuntimeException re) {
			log.error("PlatformDao: batch update ", re);
			throw re;
		}
	}
	
	public void delete(V persistentInstance) throws Exception {
		try {
			getSession().delete(persistentInstance);
		} catch (Exception re) {
			log.error("PlatformDao: delete failed", re);
			throw re;
		}
	}

	public V merge(V detachedInstance) throws Exception {
		try {
			V result = (V) getSession().merge(detachedInstance);
			return result;
		} catch (Exception re) {
			log.error("PlatformDao: merge failed", re);
			throw re;
		}
	}
	
	public V findById(Class<V> cls, PK id) throws Exception {
		try {
			V instance = (V) getSession().get(cls, id);
			return (V)process(instance);
		} catch (Exception re) {
			log.error("PlatformDao: find by id failed", re);
			throw re;
		}
	}

	protected Criterion createCriterion(Object[] cond, IProcessor<String, Class<?>> cl){
		if(cond.length > 1 && cond[1] != null){
			if("eq".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] != null) return Restrictions.eq(cond[0].toString(), cond[2]);
			} else if("eqProperty".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] != null) return Restrictions.eqProperty(cond[0].toString(), cond[2].toString());
			} else if("ge".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] != null) return Restrictions.ge(cond[0].toString(), cond[2]);
			} else if("geProperty".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] != null) return Restrictions.geProperty(cond[0].toString(), cond[2].toString());
			} else if("gt".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] != null) return Restrictions.gt(cond[0].toString(), cond[2]);
			} else if("gtProperty".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] != null) return Restrictions.gtProperty(cond[0].toString(), cond[2].toString());
			} else if("le".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] != null) return Restrictions.le(cond[0].toString(), cond[2]);
			} else if("leProperty".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] != null) return Restrictions.leProperty(cond[0].toString(), cond[2].toString());
			} else if("lt".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] != null) return Restrictions.lt(cond[0].toString(), cond[2]);
			} else if("ltProperty".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] != null) return Restrictions.ltProperty(cond[0].toString(), cond[2].toString());
			} else if("ne".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] != null) return Restrictions.ne(cond[0].toString(), cond[2]);
			} else if("neProperty".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] != null) return Restrictions.neProperty(cond[0].toString(), cond[2].toString());
			} else if("ilike".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] != null) return Restrictions.ilike(cond[0].toString(), cond[2]);
			} else if("ilikeMode".equals(cond[1])) {
				if(cond.length > 3 && cond[0] != null && cond[2] != null && cond[3] != null) 
					return Restrictions.ilike(cond[0].toString(), cond[2].toString(), MatchMode.valueOf(cond[3].toString()));
			} else if("like".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] != null) return Restrictions.like(cond[0].toString(), cond[2]);
			} else if("likeMode".equals(cond[1])) {
				if(cond.length > 3 && cond[0] != null && cond[2] != null && cond[3] != null) 
					return Restrictions.like(cond[0].toString(), cond[2].toString(), MatchMode.valueOf(cond[3].toString()));
			} else if("allEq".equals(cond[1])) {
				if(cond.length > 2 && cond[2] instanceof Map) return Restrictions.allEq((Map<String,?>)cond[2]);
			} else if("between".equals(cond[1])) {
				if(cond.length > 3 && cond[0] != null && cond[2] != null && cond[3] != null) return Restrictions.between(cond[0].toString(), cond[2], cond[3]);
			} else if("eqOrIsNull".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] != null) return Restrictions.eqOrIsNull(cond[0].toString(), cond[2]);
			} else if("and".equals(cond[1])) {
				if(cond.length > 2) {
					List<Criterion> ls = new ArrayList<Criterion>(cond.length - 2);
					for(int i = 2; i < cond.length; i++) if(cond[i] instanceof Object[]) ls.add(createCriterion((Object[])cond[i], cl));
					return Restrictions.and(ls.toArray(new Criterion[ls.size()]));
				}
			} else if("andOther".equals(cond[1])) {
				if(cond.length > 3 && cond[2] instanceof Object[] && cond[3] instanceof Object[]) 
					return Restrictions.and(createCriterion((Object[])cond[2], cl), createCriterion((Object[])cond[3], cl));
			} else if("or".equals(cond[1])) {
				if(cond.length > 2) {
					List<Criterion> ls = new ArrayList<Criterion>(cond.length - 2);
					for(int i = 2; i < cond.length; i++) if(cond[i] instanceof Object[]) ls.add(createCriterion((Object[])cond[i], cl));
					return Restrictions.or(ls.toArray(new Criterion[ls.size()]));
				}
			} else if("orOther".equals(cond[1])) {
				if(cond.length > 3 && cond[2] instanceof Object[] && cond[3] instanceof Object[]) 
					return Restrictions.or(createCriterion((Object[])cond[2], cl), createCriterion((Object[])cond[3], cl));
			} else if("idEq".equals(cond[1])) {
				if(cond.length > 2 && cond[2] != null) return Restrictions.idEq(cond[2]);
			} else if("in".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null) if(cond[2] instanceof Collection) return Restrictions.in(cond[0].toString(), (Collection)cond[2]);
				else if(cond[2] instanceof Object[]) return Restrictions.in(cond[0].toString(), (Object[])cond[2]);
			} else if("isEmpty".equals(cond[1])) {
				if(cond[0] != null) return Restrictions.isEmpty(cond[0].toString());
			} else if("isNotEmpty".equals(cond[1])) {
				if(cond[0] != null) return Restrictions.isNotEmpty(cond[0].toString());
			} else if("isNotNull".equals(cond[1])) {
				if(cond[0] != null) return Restrictions.isNotNull(cond[0].toString());
			} else if("isNull".equals(cond[1])) {
				if(cond[0] != null) return Restrictions.isNull(cond[0].toString());
			} else if("neOrIsNotNull".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] != null) return Restrictions.neOrIsNotNull(cond[0].toString(), cond[2]);
			} else if("not".equals(cond[1])) {
				if(cond.length > 3 && cond[3] != null) {
					Object[] newcond = new Object[cond.length - 2];
					for(int i = 2; i < cond.length; i ++) newcond[i - 2] = cond[i];
					return Restrictions.not(createCriterion(newcond, cl));
				}
			} else if("sizeEq".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] instanceof Integer) return Restrictions.sizeEq(cond[0].toString(), (Integer)cond[2]);
			} else if("sizeGe".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] instanceof Integer) return Restrictions.sizeGe(cond[0].toString(), (Integer)cond[2]);
			} else if("sizeGt".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] instanceof Integer) return Restrictions.sizeGt(cond[0].toString(), (Integer)cond[2]);
			} else if("sizeLe".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] instanceof Integer) return Restrictions.sizeLe(cond[0].toString(), (Integer)cond[2]);
			} else if("sizeLt".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] instanceof Integer) return Restrictions.sizeLt(cond[0].toString(), (Integer)cond[2]);
			} else if("sizeNe".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null && cond[2] instanceof Integer) return Restrictions.sizeNe(cond[0].toString(), (Integer)cond[2]);
			} else if("sqlRestriction".equals(cond[1])) {
				if(cond.length > 2 && cond[2] != null) return Restrictions.sqlRestriction(cond[2].toString());
			} else if("sqlRestrictionWithType".equals(cond[1])) {
				if(cond.length > 4 && cond[2] != null && cond[3] instanceof Object[] && cond[4] instanceof Type[]) 
					return Restrictions.sqlRestriction(cond[2].toString(), (Object[])cond[3], (Type[])cond[4]);
			} else if("sqlRestrictionWithValue".equals(cond[1])) {
				if(cond.length > 4 && cond[2] != null && cond[3] instanceof Object && cond[4] instanceof Type) 
					return Restrictions.sqlRestriction(cond[2].toString(), cond[3], (Type)cond[4]);
			} else if("naturalId".equals(cond[1])) return Restrictions.naturalId();
			else if("conjunction".equals(cond[1])) return Restrictions.conjunction();
			else if("disjunction".equals(cond[1])) return Restrictions.disjunction();
			else if("conjunctionMulti".equals(cond[1])) {
				if(cond.length > 2) {
					List<Criterion> ls = new ArrayList<Criterion>(cond.length - 2);
					for(int i = 2; i < cond.length; i++) if(cond[i] instanceof Object[]) ls.add(createCriterion((Object[])cond[i], cl));
					return Restrictions.conjunction(ls.toArray(new Criterion[ls.size()]));
				}
			} else if("disjunctionMulti".equals(cond[1])) {
				if(cond.length > 2) {
					List<Criterion> ls = new ArrayList<Criterion>(cond.length - 2);
					for(int i = 2; i < cond.length; i++) if(cond[i] instanceof Object[]) ls.add(createCriterion((Object[])cond[i], cl));
					return Restrictions.disjunction(ls.toArray(new Criterion[ls.size()]));
				}
			} else if("seq".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.eq(cond[0], subqueries(cond, cl));
			} else if("seqAll".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.eqAll(cond[0], subqueries(cond, cl));
			} else if("sexists".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.exists(subqueries(cond, cl));
			} else if("sge".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.ge(cond[0], subqueries(cond, cl));
			} else if("sgeAll".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.geAll(cond[0], subqueries(cond, cl));
			} else if("sgeSome".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.geSome(cond[0], subqueries(cond, cl));
			} else if("sgt".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.gt(cond[0], subqueries(cond, cl));
			} else if("sgtAll".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.gtAll(cond[0], subqueries(cond, cl));
			} else if("sgtSome".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.gtSome(cond[0], subqueries(cond, cl));
			} else if("sin".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.in(cond[0], subqueries(cond, cl));
			} else if("sle".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.le(cond[0], subqueries(cond, cl));
			} else if("sleAll".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.leAll(cond[0], subqueries(cond, cl));
			} else if("sleSome".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.leSome(cond[0], subqueries(cond, cl));
			} else if("slt".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.lt(cond[0], subqueries(cond, cl));
			} else if("sltAll".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.ltAll(cond[0], subqueries(cond, cl));
			} else if("sltSome".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.ltSome(cond[0], subqueries(cond, cl));
			} else if("sne".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.ne(cond[0], subqueries(cond, cl));
			} else if("snotExists".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.notExists(subqueries(cond, cl));
			} else if("snotIn".equals(cond[1])) {
				if(cond.length > 2 && cond[0] != null &&  cond[2] instanceof String) return Subqueries.notIn(cond[0], subqueries(cond, cl));
			} else if("spsEq".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String[] && cond[2] instanceof String) return Subqueries.propertiesEq((String[])cond[0], subqueries(cond, cl));
			} else if("spsIn".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String[] && cond[2] instanceof String) return Subqueries.propertiesIn((String[])cond[0], subqueries(cond, cl));
			} else if("spsNotEq".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String[] && cond[2] instanceof String) return Subqueries.propertiesNotEq((String[])cond[0], subqueries(cond, cl));
			} else if("spsNotIn".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String[] && cond[2] instanceof String) return Subqueries.propertiesNotIn((String[])cond[0], subqueries(cond, cl));
			} else if("spIn".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String && cond[2] instanceof String) return Subqueries.propertyIn((String)cond[0], subqueries(cond, cl));
			} else if("spNotIn".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String && cond[2] instanceof String) return Subqueries.propertyNotIn((String)cond[0], subqueries(cond, cl));
			} else if("spEq".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String && cond[2] instanceof String) return Subqueries.propertyEq((String)cond[0], subqueries(cond, cl));
			} else if("spNe".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String && cond[2] instanceof String) return Subqueries.propertyNe((String)cond[0], subqueries(cond, cl));
			} else if("spEqAll".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String && cond[2] instanceof String) return Subqueries.propertyEqAll((String)cond[0], subqueries(cond, cl));
			} else if("spGe".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String && cond[2] instanceof String) return Subqueries.propertyGe((String)cond[0], subqueries(cond, cl));
			} else if("spGeAll".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String && cond[2] instanceof String) return Subqueries.propertyGeAll((String)cond[0], subqueries(cond, cl));
			} else if("spGeSome".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String && cond[2] instanceof String) return Subqueries.propertyGeSome((String)cond[0], subqueries(cond, cl));
			} else if("spGt".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String && cond[2] instanceof String) return Subqueries.propertyGt((String)cond[0], subqueries(cond, cl));
			} else if("spGtAll".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String && cond[2] instanceof String) return Subqueries.propertyGtAll((String)cond[0], subqueries(cond, cl));
			} else if("spGtSome".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String && cond[2] instanceof String) return Subqueries.propertyGtSome((String)cond[0], subqueries(cond, cl));
			} else if("spLt".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String && cond[2] instanceof String) return Subqueries.propertyLt((String)cond[0], subqueries(cond, cl));
			} else if("spLtAll".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String && cond[2] instanceof String) return Subqueries.propertyLtAll((String)cond[0], subqueries(cond, cl));
			} else if("spLtSome".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String && cond[2] instanceof String) return Subqueries.propertyLtSome((String)cond[0], subqueries(cond, cl));
			} else if("spLe".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String && cond[2] instanceof String) return Subqueries.propertyLe((String)cond[0], subqueries(cond, cl));
			} else if("spLeAll".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String && cond[2] instanceof String) return Subqueries.propertyLeAll((String)cond[0], subqueries(cond, cl));
			} else if("spLeSome".equals(cond[1])) {
				if(cond.length > 2 && cond[0] instanceof String && cond[2] instanceof String) return Subqueries.propertyLeSome((String)cond[0], subqueries(cond, cl));
			}
		}
		return null;
	}
	
	protected DetachedCriteria subqueries(Object[] cond, IProcessor<String, Class<?>> cl) {
		DetachedCriteria dc;
		if(cond.length > 3 && cond[3] instanceof String) dc = DetachedCriteria.forEntityName((String) cond[2], (String) cond[3]);
		else dc = DetachedCriteria.forEntityName((String) cond[2]);
		if(cond.length > 6 && cond[6] instanceof Object[] && ((Object[])cond[6]).length > 0) {
			Criterion restriction = createCriterion((Object[])cond[6], cl);
			if(restriction != null) dc.add(restriction);
		}
		if(cond.length > 4 && cond[4] instanceof Object[] && ((Object[])cond[4]).length > 0) {
			Projection projection = createProjection((Object[])cond[4], cl);
			if(projection != null) dc.setProjection(projection);
		}
		if(cond.length > 5 && cond[5] instanceof Object[] && ((Object[])cond[5]).length > 0) for(Object c : (Object[])cond[5]) if(c != null && c instanceof Object[] && ((Object[]) c).length > 0 && ((Object[]) c)[0] instanceof String){ 
			Object[] fetch = (Object[]) c;
			FetchMode mode = FetchMode.JOIN;
			if(((Object[]) fetch).length > 1 && ((Object[]) fetch)[1] instanceof String) try{
				mode = FetchMode.valueOf((String)fetch[1]);
			} catch (Exception re) {
				log.error("PlatformDao: make fetch failed");
			}
			dc.setFetchMode((String)fetch[0], mode);
			if(fetch.length > 2 && fetch[2] instanceof String) dc.createAlias((String)fetch[0], (String)fetch[2]);
		}
		return dc;
	}
	
	protected Projection createProjection(Object[] proj, IProcessor<String, Class<?>> cl){
		if(proj.length > 0) if(proj[0] instanceof String){
			if("property".equals(proj[0])) {
				if(proj.length > 1 && proj[1] instanceof String) {
					SimpleProjection ret = Projections.property((String)proj[1]);
					if (proj.length > 2 && proj[2] instanceof String) ret.as((String)proj[2]);
					return ret;
				}
			} else if("distinct".equals(proj[0])) {
				if(proj.length > 1 && proj[1] instanceof Object[]) return Projections.distinct(createProjection((Object[])proj[1], cl));
			} else if("count".equals(proj[0])) {
				if(proj.length > 1 && proj[1] instanceof String) {
					SimpleProjection ret = Projections.count((String)proj[1]);
					if (proj.length > 2 && proj[2] instanceof String) ret.as((String)proj[2]);
					return ret;
				}
			} else if("countDistinct".equals(proj[0])) {
				if(proj.length > 1 && proj[1] instanceof String) {
					SimpleProjection ret = Projections.countDistinct((String)proj[1]);
					if (proj.length > 2 && proj[2] instanceof String) ret.as((String)proj[2]);
					return ret;
				}
			} else if("multiCount".equals(proj[0])) {
				if(proj.length > 4 && (proj[1] instanceof String || proj[1] == null)) {
					StringBuilder sb = new StringBuilder();
					for (int i = 4; i < proj.length; i++) if (proj != null) sb.append(";").append(proj[i]);
					if (sb.length() > 0) {
						MultipleCountProjection ret = new MultipleCountProjection(sb.substring(1), (String) proj[1]);
						if (proj[3] instanceof Integer) {
							Integer flag = (Integer) proj[3];
							if ((flag & 1) > 0) ret = ret.setDistinct();
							if ((flag & 2) > 0) ret =ret.setConcat();
						}
						if (proj[2] instanceof String) ret.as((String)proj[2]);
						return ret;
					}
				}
			} else if("sum".equals(proj[0])) {
				if(proj.length > 1 && proj[1] instanceof String) {
					SimpleProjection ret = Projections.sum((String)proj[1]);
					if (proj.length > 2 && proj[2] instanceof String) ret.as((String)proj[2]);
					return ret;
				}
			} else if("avg".equals(proj[0])) {
				if(proj.length > 1 && proj[1] instanceof String) {
					SimpleProjection ret = Projections.avg((String)proj[1]);
					if (proj.length > 2 && proj[2] instanceof String) ret.as((String)proj[2]);
					return ret;
				}
			} else if("max".equals(proj[0])) {
				if(proj.length > 1 && proj[1] instanceof String) {
					SimpleProjection ret = Projections.max((String)proj[1]);
					if (proj.length > 2 && proj[2] instanceof String) ret.as((String)proj[2]);
					return ret;
				}
			} else if("min".equals(proj[0])) {
				if(proj.length > 1 && proj[1] instanceof String) {
					SimpleProjection ret = Projections.min((String)proj[1]);
					if (proj.length > 2 && proj[2] instanceof String) ret.as((String)proj[2]);
					return ret;
				}
			} else if("group".equals(proj[0])) {
				if(proj.length > 1 && proj[1] instanceof String) {
					SimpleProjection ret = Projections.groupProperty((String)proj[1]);
					if (proj.length > 2 && proj[2] instanceof String) ret.as((String)proj[2]);
					return ret;
				}
			} else if("alias".equals(proj[0])) {
				if(proj.length > 2 && proj[2] instanceof String && proj[1] instanceof Object[])
					return Projections.alias(createProjection((Object[])proj[1], cl), (String)proj[2]);
			} else if("rowCount".equals(proj[0])) return Projections.rowCount();
			else if("sql".equals(proj[0])) {
				if(proj.length > 3 && proj[1] instanceof String && proj[2] instanceof Object[] && proj[3] instanceof Object[]){
					List<String> ls = new ArrayList<String>(Array.getLength(proj[2]));
					for(Object o : (Object[])proj[2]) if(o instanceof String) ls.add((String)o);
					List<Type> types = new ArrayList<Type>(Array.getLength(proj[3]));
					for(Object o : (Object[])proj[3]) if(o instanceof String) try{
						Object i = newInstance(findClass(o, cl));
						if(i instanceof Type) types.add((Type)i);
					}catch(Exception e){
						log.error("PlatformDao: create projection fail." + o, e);
					}
					return Projections.sqlProjection((String)proj[1], ls.toArray(new String[ls.size()]), types.toArray(new Type[types.size()]));
				}
			} else if("sqlGroup".equals(proj[0])) {
				if(proj.length > 4 && proj[1] instanceof String && proj[2] instanceof String && proj[3] instanceof Object[] && proj[4] instanceof Object[]){
					List<String> ls = new ArrayList<String>(Array.getLength(proj[3]));
					for(Object o : (Object[])proj[3]) if(o instanceof String) ls.add((String)o);
					List<Type> types = new ArrayList<Type>(Array.getLength(proj[4]));
					for(Object o : (Object[])proj[4]) if(o instanceof String) try{
						Object i = newInstance(findClass(o, cl));
						if(i instanceof Type) types.add((Type)i);
					}catch(Exception e){
						log.error("PlatformDao: create projection fail." + o, e);
					}
					return Projections.sqlGroupProjection((String)proj[1], (String)proj[2], ls.toArray(new String[ls.size()]), types.toArray(new Type[types.size()]));
				}
			} else if("id".equals(proj[0])) {
				SimpleProjection ret = Projections.id();
				if (proj.length > 1 && proj[1] instanceof String) ret.as((String)proj[1]);
				return ret;
			} else if("list".equals(proj[0])) return Projections.projectionList();
		} else {
			ProjectionList list = Projections.projectionList();
			for(Object p : proj) if (p instanceof Object[]) {
				Projection projection = createProjection((Object[])p, cl);
				if(projection != null) list.add(projection);
			}
			if(list.getLength() > 0) return list;
		} 
		return null;
	}
	
	public List<V> findByCond(String cls, Object[][] conds, Object[][] fetchs, Object[][] projs, Object[][] orders, Integer index, Integer count, IProcessor<String, Class<?>> cl, Boolean cache, CacheMode cm, String region) throws Exception {
		try {
			Criteria  crit = getSession().createCriteria(findClass(cls, cl));
			if(fetchs != null) for(Object[] fetch : fetchs) if(fetch != null && fetch.length > 0 && fetch[0] instanceof String){ 
				FetchMode mode = FetchMode.JOIN;
				if(fetch.length > 1 && fetch[1] instanceof String) try{
					mode = FetchMode.valueOf((String)fetch[1]);
				} catch (Exception re) {
					log.error("PlatformDao: make fetch failed");
				}
				crit.setFetchMode((String)fetch[0], mode);
				if(fetch.length > 2 && fetch[2] instanceof String) if(fetch.length > 3 && fetch[3] instanceof String) try{
					JoinType join = JoinType.valueOf((String)fetch[3]);
					Criterion with = null;
					if (fetch.length > 4 && fetch[4] instanceof Object[]) with = createCriterion((Object[]) fetch[3], cl);
					if (with == null) crit.createAlias((String)fetch[0], (String)fetch[2], join);
					else crit.createAlias((String)fetch[0], (String)fetch[2], join, with);
				} catch (Exception re) {
					log.error("PlatformDao: make fetch failed");
					crit.createAlias((String)fetch[0], (String)fetch[2]);
				} else crit.createAlias((String)fetch[0], (String)fetch[2]);
			}
			if(projs != null && projs.length > 0) {
				ProjectionList list = Projections.projectionList();
				for(Object[] proj : projs) {
					Projection projection = createProjection(proj, cl);
					if(projection != null) list.add(projection);
				}	
				if (list.getLength() > 0) crit.setProjection(list);
			}
			if(conds != null && conds.length > 0) for(Object[] cond : conds) {
				Criterion restriction = createCriterion(cond, cl);
				if(restriction != null) crit.add(restriction);
			}		
			if(orders != null && orders.length > 0) for(Object[] order : orders) if(order != null && order.length > 1 && order[0] != null) 
				crit.addOrder(Boolean.FALSE.equals(order[1]) ? Order.desc(order[0].toString()) : Order.asc(order[0].toString()));
			if(index != null && count != null){
				crit.setFirstResult(index);
				crit.setMaxResults(count);
			}
			if (Boolean.TRUE.equals(cache)) {
				crit.setCacheable(true);
				if (cm != null) crit.setCacheMode(cm);
				if (region != null) crit.setCacheRegion(region);
			}
			List<V> results = (List<V>) process(crit.list());
			return results;
		} catch (Exception re) {
			log.error("PlatformDao: find by cond failed", re);
			throw re;
		}
	}

	public class FindByCondProcessor implements IProcessor<Object[], List<V>> {
		@Override
		public List<V> process(Object[] instance) {
			try {
				return findByCond((String) instance[0], (Object[][]) instance[1], (Object[][]) instance[2], (Object[][]) instance[3], (Object[][]) instance[4],  (Integer) instance[5], (Integer) instance[6], (IProcessor<String, Class<?>>) instance[7], (Boolean) instance[8], (CacheMode) instance[9], (String) instance[10]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_DATA_ACCESS, e);
			}
		}
	}
	
	public static class FindByCond<V> implements IInputResource<Object, List<V>>{
		protected String cls;
		protected Object[][] conds;
		protected Object[][] fetchs;
		protected Object[][] projs;
		protected Object[][] orders;
		protected IProcessor<String, Class<?>> cl;
		protected IProcessor<Object[], List<V>> findByCond;
		protected Boolean cache;
		protected CacheMode mode;
		protected String region;
		
		public void setCache(Boolean cache) {
			this.cache = cache;
		}

		public void setMode(CacheMode mode) {
			this.mode = mode;
		}

		public void setRegion(String region) {
			this.region = region;
		}

		public void setClassLoader(IProcessor<String, Class<?>> cl) {
			this.cl = cl;
		}
				
		public void setFetchs(Object[][] fetchs) {
			this.fetchs = fetchs;
		}

		public void setProjs(Object[][] projs) {
			this.projs = projs;
		}

		public void setCls(String cls) {
			this.cls = cls;
		}

		public void setConds(Object[][] conds) {
			this.conds = conds;
		}

		public void setOrders(Object[][] orders) {
			this.orders = orders;
		}

		public void setFindByCond(IProcessor<Object[], List<V>> findByCond) {
			this.findByCond = findByCond;
		}

		@Override
		public List<V> find(Object... paras) {
			String cls = this.cls;
			Object[][] conds = this.conds;
			Object[][] fetchs = this.fetchs;
			Object[][] projs = this.projs;
			Object[][] orders = this.orders;
			Integer index = null;
			Integer count = null;
			if(paras != null && paras.length > 0){
				if(cls == null && paras[0] instanceof String) cls = (String) paras[0];
				if(conds == null && paras.length > 1 && paras[1] instanceof Object[][]) conds = (Object[][])paras[1];
				if(fetchs == null && paras.length > 2 && paras[2] instanceof Object[][]) fetchs = (Object[][])paras[2];
				if(projs == null && paras.length > 3 && paras[3] instanceof Object[][]) projs = (Object[][])paras[3];
				if(orders == null && paras.length > 4 && paras[4] instanceof Object[][]) orders = (Object[][])paras[4];
				if(paras.length > 5 && paras[5] instanceof Integer) index = (Integer)paras[5];
				if(paras.length > 6 && paras[6] instanceof Integer) count = (Integer)paras[6];
			} 
			List<V> ls = null;
			try{
				ls =  findByCond.process(new Object[]{cls, conds, fetchs, projs, orders, index, count, cl, cache, mode, region});
			}catch(Exception e){
				log.error("PlatformDao: find by cond failed", e);	
			}
			return ls;
		}	
	}
	
	public Long countByCond(String cls, Object[][] conds, Object[][] fetchs, Object[][] projs, IProcessor<String, Class<?>> cl, Boolean cache, CacheMode cm, String region) throws Exception {
		try {
			Criteria  crit = getSession().createCriteria(findClass(cls, cl));
			if(fetchs != null) for(Object[] fetch : fetchs) if(fetch != null && fetch.length > 0 && fetch[0] instanceof String){ 
				FetchMode mode = FetchMode.JOIN;
				if(fetch.length > 1 && fetch[1] instanceof String) try{
					mode = FetchMode.valueOf((String)fetch[1]);
				} catch (Exception re) {
					log.error("PlatformDao: make fetch failed");
				}
				crit.setFetchMode((String)fetch[0], mode);
				if(fetch.length > 2 && fetch[2] instanceof String) if(fetch.length > 3 && fetch[3] instanceof String) try{
					JoinType join = JoinType.valueOf((String)fetch[3]);
					Criterion with = null;
					if (fetch.length > 4 && fetch[4] instanceof Object[]) with = createCriterion((Object[]) fetch[3], cl);
					if (with == null) crit.createAlias((String)fetch[0], (String)fetch[2], join);
					else crit.createAlias((String)fetch[0], (String)fetch[2], join, with);
				} catch (Exception re) {
					log.error("PlatformDao: make fetch failed");
					crit.createAlias((String)fetch[0], (String)fetch[2]);
				} else crit.createAlias((String)fetch[0], (String)fetch[2]);
			}
			ProjectionList list = Projections.projectionList();
			if(projs != null && projs.length > 0) for(Object[] proj : projs) {
				Projection projection = createProjection(proj, cl);
				if(projection != null) list.add(projection);
			}	
			if (list.getLength() > 0) crit.setProjection(list);
			else crit.setProjection(Projections.rowCount());
			if(conds != null && conds.length > 0) for(Object[] cond : conds) {
				Criterion restriction = createCriterion(cond, cl);
				if(restriction != null) crit.add(restriction);
			}
			if (Boolean.TRUE.equals(cache)) {
				crit.setCacheable(true);
				if (cm != null) crit.setCacheMode(cm);
				if (region != null) crit.setCacheRegion(region);
			}
			Object result = crit.uniqueResult();
			Long count = 0L;
			if (result instanceof Long) count = (Long) result;
			else if (result instanceof Integer) count = new Long((Integer) result);
			else if (result instanceof Object[] && ((Object[]) result).length > 0 && ((Object[]) result)[0] instanceof Long) count = (Long) ((Object[]) result)[0];
			else if (result instanceof Object[] && ((Object[]) result).length > 0 && ((Object[]) result)[0] instanceof Integer) count = new Long((Integer) ((Object[]) result)[0]);
			return count;
		} catch (Exception re) {
			log.error("PlatformDao: count by cond failed", re);
			throw re;
		}
	}
	
	public class CountByCondProcessor implements IProcessor<Object[], Long> {
		@Override
		public Long process(Object[] instance) {
			try {
				return countByCond((String) instance[0], (Object[][]) instance[1], (Object[][]) instance[2], (Object[][]) instance[3], (IProcessor<String, Class<?>>) instance[4], (Boolean) instance[5], (CacheMode) instance[6], (String) instance[7]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_DATA_ACCESS, e);
			}
		}
	}
	
	public static class CountByCond implements IInputResource<Object, Long>{
		protected String cls;
		protected Object[][] conds;
		protected Object[][] fetchs;
		protected Object[][] projs;
		protected IProcessor<String, Class<?>> cl;
		protected IProcessor<Object[], Long> countByCond;
		protected Boolean cache;
		protected CacheMode mode;
		protected String region;
		
		public void setCache(Boolean cache) {
			this.cache = cache;
		}

		public void setMode(CacheMode mode) {
			this.mode = mode;
		}

		public void setRegion(String region) {
			this.region = region;
		}
		
		public void setCountByCond(IProcessor<Object[], Long> countByCond) {
			this.countByCond = countByCond;
		}

		public void setClassLoader(IProcessor<String, Class<?>> cl) {
			this.cl = cl;
		}
				
		public void setFetchs(Object[][] fetchs) {
			this.fetchs = fetchs;
		}

		public void setProjs(Object[][] projs) {
			this.projs = projs;
		}

		public void setCls(String cls) {
			this.cls = cls;
		}

		public void setConds(Object[][] conds) {
			this.conds = conds;
		}

		@Override
		public Long find(Object... paras) {
			String cls = this.cls;
			Object[][] conds = this.conds;
			Object[][] fetchs = this.fetchs;
			Object[][] projs = this.projs;
			if(paras != null && paras.length > 0) {
				if(cls == null && paras[0] instanceof String) cls = (String) paras[0];
				if(conds == null && paras.length > 1 && paras[1] instanceof Object[][]) conds = (Object[][])paras[1];
				if(fetchs == null && paras.length > 2 && paras[2] instanceof Object[][]) fetchs = (Object[][])paras[2];
				if(projs == null && paras.length > 3 && paras[3] instanceof Object[][]) projs = (Object[][])paras[3];
			} 
			Long count = null;
			try{
				count =  countByCond.process(new Object[]{cls, conds, fetchs, projs, cl, cache, mode, region});
			}catch(Exception e){
				log.error("PlatformDao: count by cond failed", e);	
			}
			return count;
		}	
	}
	
	public List<V> findByExample(V instance, Object[][] conds, Object[][] fetchs, Object[][] projs, Object[][] orders, Integer index, Integer count, IProcessor<String, Class<?>> cl, Boolean cache, CacheMode cm, String region) throws Exception {
		try {
			Criteria  crit = getSession().createCriteria(instance.getClass()).add(create(instance));
			if(fetchs != null) for(Object[] fetch : fetchs) if(fetch != null && fetch.length > 0 && fetch[0] instanceof String){ 
				FetchMode mode = FetchMode.JOIN;
				if(fetch.length > 1 && fetch[1] instanceof String) try{
					mode = FetchMode.valueOf((String)fetch[1]);
				} catch (Exception re) {
					log.error("PlatformDao: make fetch failed");
				}
				crit.setFetchMode((String)fetch[0], mode);
				if(fetch.length > 2 && fetch[2] instanceof String) if(fetch.length > 3 && fetch[3] instanceof String) try{
					JoinType join = JoinType.valueOf((String)fetch[3]);
					Criterion with = null;
					if (fetch.length > 4 && fetch[4] instanceof Object[]) with = createCriterion((Object[]) fetch[3], cl);
					if (with == null) crit.createAlias((String)fetch[0], (String)fetch[2], join);
					else crit.createAlias((String)fetch[0], (String)fetch[2], join, with);
				} catch (Exception re) {
					log.error("PlatformDao: make fetch failed");
					crit.createAlias((String)fetch[0], (String)fetch[2]);
				} else crit.createAlias((String)fetch[0], (String)fetch[2]);
			}
			if(projs != null && projs.length > 0)  {
				ProjectionList list = Projections.projectionList();
				for(Object[] proj : projs) {
					Projection projection = createProjection(proj, cl);
					if(projection != null) list.add(projection);
				}	
				if (list.getLength() > 0) crit.setProjection(list);
			}
			if(conds != null && conds.length > 0) for(Object[] cond : conds) {
				Criterion restriction = createCriterion(cond, cl);
				if(restriction != null) crit.add(restriction);
			}
			if(orders != null && orders.length > 0) for(Object[] order : orders) if(order != null && order.length > 1 && order[0] != null) 
				crit.addOrder(Boolean.FALSE.equals(order[1]) ? Order.desc(order[0].toString()) : Order.asc(order[0].toString()));
			if(index != null && count != null){
				crit.setFirstResult(index);
				crit.setMaxResults(count);
			}
			if (Boolean.TRUE.equals(cache)) {
				crit.setCacheable(true);
				if (cm != null) crit.setCacheMode(cm);
				if (region != null) crit.setCacheRegion(region);
			}
			List<V> results = (List<V>) process(crit.list());
			return results;
		} catch (Exception re) {
			log.error("PlatformDao: find by example failed", re);
			throw re;
		}
	}

	public class FindByExampleProcessor implements IProcessor<Object[], List<V>> {
		@Override
		public List<V> process(Object[] instance) {
			try {
				return findByExample((V) instance[0], (Object[][]) instance[1], (Object[][]) instance[2], (Object[][]) instance[3], (Object[][]) instance[4], (Integer) instance[5], (Integer) instance[6], (IProcessor<String, Class<?>>) instance[7], (Boolean) instance[8], (CacheMode) instance[9], (String) instance[10]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_DATA_ACCESS, e);
			}
		}
	}
	
	public static class FindByExample<V> implements IInputResource<V, List<V>>{
		protected Object[][] conds;
		protected Object[][] fetchs;
		protected Object[][] projs;
		protected Object[][] orders;
		protected IProcessor<String, Class<?>> cl;
		protected IProcessor<Object[], List<V>> findByExample;		
		protected Boolean cache;
		protected CacheMode mode;
		protected String region;
		
		public void setCache(Boolean cache) {
			this.cache = cache;
		}

		public void setMode(CacheMode mode) {
			this.mode = mode;
		}

		public void setRegion(String region) {
			this.region = region;
		}
		
		public void setClassLoader(IProcessor<String, Class<?>> cl) {
			this.cl = cl;
		}
		
		public void setFetchs(Object[][] fetchs) {
			this.fetchs = fetchs;
		}

		public void setProjs(Object[][] projs) {
			this.projs = projs;
		}

		public void setConds(Object[][] conds) {
			this.conds = conds;
		}

		public void setOrders(Object[][] orders) {
			this.orders = orders;
		}

		public void setFindByExample(IProcessor<Object[], List<V>> findByExample) {
			this.findByExample = findByExample;
		}

		@Override
		public List<V> find(V... paras) {
			Object[][] orders = this.orders;
			Object[][] conds = this.conds;
			Object[][] fetchs = this.fetchs;
			Object[][] projs = this.projs;
			Integer index = null;
			Integer count = null;
			V instance = null;
			if(paras != null && paras.length > 0) {
				instance = (V) paras[0];
				if(conds == null && paras.length > 1 && paras[1] instanceof Object[][]) conds = (Object[][])paras[1];
				if(fetchs == null && paras.length > 2 && paras[2] instanceof Object[][]) fetchs = (Object[][])paras[2];
				if(projs == null && paras.length > 3 && paras[3] instanceof Object[][]) projs = (Object[][])paras[3];
				if(orders == null && paras.length > 4 && paras[4] instanceof Object[][]) orders = (Object[][])paras[4];
				if(paras.length > 5 && paras[5] instanceof Integer) index = (Integer)paras[5];
				if(paras.length > 6 && paras[6] instanceof Integer) count = (Integer)paras[6];
			} 
			List<V> ls = null;
			try{
				ls =  findByExample.process(new Object[]{instance, conds, fetchs, projs, orders, index, count, cl, cache, mode, region});
			}catch(Exception e){
				log.error("PlatformDao: find by example failed", e);	
			}
			return ls;
		}	
	}
	
	public Long countByExample(V instance, Object[][] conds, Object[][] fetchs, Object[][] projs, IProcessor<String, Class<?>> cl, Boolean cache, CacheMode cm, String region) throws Exception {
		try {
			Criteria  crit = getSession().createCriteria(instance.getClass()).add(create(instance));
			if(fetchs != null) for(Object[] fetch : fetchs) if(fetch != null && fetch.length > 0 && fetch[0] instanceof String){ 
				FetchMode mode = FetchMode.JOIN;
				if(fetch.length > 1 && fetch[1] instanceof String) try{
					mode = FetchMode.valueOf((String)fetch[1]);
				} catch (Exception re) {
					log.error("PlatformDao: make fetch failed");
				}
				crit.setFetchMode((String)fetch[0], mode);
				if(fetch.length > 2 && fetch[2] instanceof String) if(fetch.length > 3 && fetch[3] instanceof String) try{
					JoinType join = JoinType.valueOf((String)fetch[3]);
					Criterion with = null;
					if (fetch.length > 4 && fetch[4] instanceof Object[]) with = createCriterion((Object[]) fetch[3], cl);
					if (with == null) crit.createAlias((String)fetch[0], (String)fetch[2], join);
					else crit.createAlias((String)fetch[0], (String)fetch[2], join, with);
				} catch (Exception re) {
					log.error("PlatformDao: make fetch failed");
					crit.createAlias((String)fetch[0], (String)fetch[2]);
				} else crit.createAlias((String)fetch[0], (String)fetch[2]);
			}
			ProjectionList list = Projections.projectionList();
			if(projs != null && projs.length > 0) for(Object[] proj : projs) {
				Projection projection = createProjection(proj, cl);
				if(projection != null) list.add(projection);
			}	
			if (list.getLength() > 0) crit.setProjection(list);
			else crit.setProjection(Projections.rowCount());
			if(conds != null && conds.length > 0) for(Object[] cond : conds) {
				Criterion restriction = createCriterion(cond, cl);
				if(restriction != null) crit.add(restriction);
			}
			if (Boolean.TRUE.equals(cache)) {
				crit.setCacheable(true);
				if (cm != null) crit.setCacheMode(cm);
				if (region != null) crit.setCacheRegion(region);
			}
			Object result = crit.uniqueResult();
			Long count = 0L;
			if (result instanceof Long) count = (Long) result;
			else if (result instanceof Integer) count = new Long((Integer) result);
			else if (result instanceof Object[] && ((Object[]) result).length > 0 && ((Object[]) result)[0] instanceof Long) count = (Long) ((Object[]) result)[0];
			else if (result instanceof Object[] && ((Object[]) result).length > 0 && ((Object[]) result)[0] instanceof Integer) count = new Long((Integer) ((Object[]) result)[0]);
			return count;
		} catch (Exception re) {
			log.error("PlatformDao: find count by example failed", re);
			throw re;
		}
	}

	public class CountByExampleProcessor implements IProcessor<Object[], Long> {
		@Override
		public Long process(Object[] instance) {
			try {
				return countByExample((V) instance[0], (Object[][]) instance[1], (Object[][]) instance[2], (Object[][]) instance[3], (IProcessor<String, Class<?>>) instance[4], (Boolean) instance[5], (CacheMode) instance[6], (String) instance[7]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_DATA_ACCESS, e);
			}
		}
	}
	
	public static class CountByExample<V> implements IInputResource<V, Long>{
		protected Object[][] conds;
		protected Object[][] fetchs;
		protected Object[][] projs;
		protected IProcessor<String, Class<?>> cl;
		protected IProcessor<Object[], Long> countByExample;
		protected Boolean cache;
		protected CacheMode mode;
		protected String region;
		
		public void setCache(Boolean cache) {
			this.cache = cache;
		}

		public void setMode(CacheMode mode) {
			this.mode = mode;
		}

		public void setRegion(String region) {
			this.region = region;
		}
		
		public void setClassLoader(IProcessor<String, Class<?>> cl) {
			this.cl = cl;
		}
		
		public void setConds(Object[][] conds) {
			this.conds = conds;
		}

		public void setFetchs(Object[][] fetchs) {
			this.fetchs = fetchs;
		}

		public void setProjs(Object[][] projs) {
			this.projs = projs;
		}

		public void setCountByExample(IProcessor<Object[], Long> countByExample) {
			this.countByExample = countByExample;
		}

		@Override
		public Long find(V... paras) {
			Object[][] conds = this.conds;
			Object[][] fetchs = this.fetchs;
			Object[][] projs = this.projs;
			V instance = null;
			if(paras != null && paras.length > 0) {
				instance = (V) paras[0];
				if(conds == null && paras.length > 1 && paras[1] instanceof Object[][]) conds = (Object[][])paras[1];
				if(fetchs == null && paras.length > 2 && paras[2] instanceof Object[][]) fetchs = (Object[][])paras[2];
				if(projs == null && paras.length > 3 && paras[3] instanceof Object[][]) projs = (Object[][])paras[3];
			} 
			Long count = null;
			try{
				count =  countByExample.process(new Object[]{instance, conds, fetchs, projs, cl, cache, mode, region});
			}catch(Exception e){
				log.error("PlatformDao: count by example failed", e);	
			}
			return count;
		}	
	}
	
	public List<V> findAll(Class<V> cls) throws Exception {
		try {
			List<V> results = (List<V>) process(getSession().createCriteria(cls).list());
			return results;
		} catch (Exception re) {
			log.error("PlatformDao: find all failed", re);
			throw re;
		}
	}
	
	public Long countAll(Class<V> cls) throws Exception {
		try {
			Long results = (Long)getSession().createCriteria(cls).setProjection(Projections.rowCount()).uniqueResult();
			return results;
		} catch (Exception re) {
			log.error("PlatformDao: find all count failed", re);
			throw re;
		}
	}
	
	public Integer executeSql(String sql, Map<Object, Object> paras) throws Exception {
		try {
			SQLQuery  query = getSession().createSQLQuery(sql);
			if(paras != null) for (Entry<Object, Object> entry : paras.entrySet())
				if(entry.getValue() instanceof Pojo) if (entry.getKey() instanceof Integer) query.setEntity((Integer)entry.getKey(), entry.getValue());
				else query.setEntity(entry.getKey().toString(), entry.getValue());
				else if(entry.getValue() instanceof Collection) query.setParameterList(entry.getKey().toString(), (Collection)entry.getValue());
				else if(entry.getValue() instanceof Object[]) query.setParameterList(entry.getKey().toString(), (Object[])entry.getValue());
				else if (entry.getKey() instanceof Integer) query.setParameter((Integer)entry.getKey(), entry.getValue());
				else query.setParameter(entry.getKey().toString(), entry.getValue());
			Integer result = query.executeUpdate();
			return result;
		} catch (Exception re) {
			log.error("PlatformDao: execute sql failed", re);
			throw re;
		}
	}

	public class ExecuteSqlProcessor implements IProcessor<Object[], Integer> {
		@Override
		public Integer process(Object[] instance) {
			try {
				return executeSql((String) instance[0], (Map<Object, Object>) instance[1]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_DATA_ACCESS, e);
			}
		}
	}
	
	public static class ExecuteSql implements IInputResource<Object, Integer>{
		protected String sql;
		protected Map<Object, Object> paras;
		protected IProcessor<Object[], Integer> executeSql;
		
		public void setSql(String sql) {
			this.sql = sql;
		}

		public void setParas(Map<Object, Object> paras) {
			this.paras = paras;
		}
		
		public void setExecuteSql(IProcessor<Object[], Integer> executeSql) {
			this.executeSql = executeSql;
		}

		@Override
		public Integer find(Object... paras) {
			String sql = this.sql;
			Map<Object, Object> ps = this.paras;
			if(paras != null && paras.length > 0){
				if(sql == null && paras[0] instanceof String) sql = (String)paras[0];
				if(ps == null && paras.length > 1 && paras[1] instanceof Map) ps = (Map<Object, Object>) paras[1];
			}
			Integer ret = null;
			try{
				ret = executeSql.process(new Object[]{sql, ps});
			}catch(Exception e){
				log.error("PlatformDao: execute sql failed", e);
			}
			return ret;
		}	
	}
	
	public List<V> findBySql(String sql, String cls, Object[][] scalar, String result, Integer index, Integer count, Map<Object, Object> paras, IProcessor<String, Class<?>> cl, Boolean cache, CacheMode cm, String region) throws Exception {
		try {
			SQLQuery  query = getSession().createSQLQuery(sql);
			if (cls != null) query.addEntity(findClass(cls, cl));
			if (scalar != null && scalar.length > 0) for (Object[] s : scalar) if (s.length > 1 && s[0] instanceof String && s[1] instanceof Type) query.addScalar((String) s[0], (Type) s[1]);
			else if (s.length > 0 && s[0] instanceof String) query.addScalar((String)s[0]);
			if(paras != null) for (Entry<Object, Object> entry : paras.entrySet())
				if(entry.getValue() instanceof Pojo) if (entry.getKey() instanceof Integer) query.setEntity((Integer)entry.getKey(), entry.getValue());
				else query.setEntity(entry.getKey().toString(), entry.getValue());
				else if(entry.getValue() instanceof Collection) query.setParameterList(entry.getKey().toString(), (Collection)entry.getValue());
				else if(entry.getValue() instanceof Object[]) query.setParameterList(entry.getKey().toString(), (Object[])entry.getValue());
				else if (entry.getKey() instanceof Integer) query.setParameter((Integer)entry.getKey(), entry.getValue());
				else query.setParameter(entry.getKey().toString(), entry.getValue());
			if(index != null && count != null){
				query.setFirstResult(index);
				query.setMaxResults(count);
			}
			if (Boolean.TRUE.equals(cache)) {
				query.setCacheable(true);
				if (cm != null) query.setCacheMode(cm);
				if (region != null) query.setCacheRegion(region);
			}
			if (result != null) query.setResultSetMapping(result);
			List<V> results = (List<V>) process(query.list());
			return results;
		} catch (Exception re) {
			log.error("PlatformDao: find by sql failed", re);
			throw re;
		}
	}

	public class FindBySqlProcessor implements IProcessor<Object[], List<V>> {
		@Override
		public List<V> process(Object[] instance) {
			try {
				return findBySql((String) instance[0],  (String) instance[1], (Object[][]) instance[2], (String) instance[3], (Integer) instance[4], (Integer) instance[5], (Map<Object, Object>) instance[6], (IProcessor<String, Class<?>>)instance[7], (Boolean) instance[8], (CacheMode) instance[9], (String) instance[10]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_DATA_ACCESS, e);
			}
		}
	}
	
	public static class FindBySql<V> implements IInputResource<Object, List<V>>{
		protected String sql;
		protected String cls;
		protected String result;
		protected Object[][] scalar;
		protected Map<Object, Object> paras;
		protected IProcessor<String, Class<?>> cl;
		protected IProcessor<Object[], List<V>> findBySql;
		protected Boolean cache;
		protected CacheMode mode;
		protected String region;
		
		public void setCache(Boolean cache) {
			this.cache = cache;
		}

		public void setMode(CacheMode mode) {
			this.mode = mode;
		}

		public void setRegion(String region) {
			this.region = region;
		}
				
		public void setSql(String sql) {
			this.sql = sql;
		}

		public void setParas(Map<Object, Object> paras) {
			this.paras = paras;
		}

		public void setFindBySql(IProcessor<Object[], List<V>> findBySql) {
			this.findBySql = findBySql;
		}

		public void setCls(String cls) {
			this.cls = cls;
		}

		public void setScalar(Object[][] scalar) {
			this.scalar = scalar;
		}

		public void setCl(IProcessor<String, Class<?>> cl) {
			this.cl = cl;
		}

		@Override
		public List<V> find(Object... paras) {
			String sql = this.sql;
			String cls = this.cls;
			String result = this.result;
			Object[][] scalar = this.scalar;
			Map<Object, Object> ps = this.paras;
			Integer index = null;
			Integer count = null;
			if(paras != null && paras.length > 0){
				if(sql == null && paras[0] instanceof String) sql = (String)paras[0];
				if(cls == null && paras.length > 1 && paras[1] instanceof String) cls = (String) paras[1];
				if(scalar == null && paras.length > 2 && paras[2] instanceof Object[][]) scalar = (Object[][])paras[2];
				if(result == null && paras.length > 3 && paras[3] instanceof String) result = (String) paras[3];
				if(paras.length > 4 && paras[4] instanceof Integer) index = (Integer)paras[4];
				if(paras.length > 5 && paras[5] instanceof Integer) count = (Integer)paras[5];
				if(ps == null && paras.length > 6 && paras[6] instanceof Map) ps = (Map<Object, Object>) paras[6];
			} 
			List<V> ret = null;
			try{
				ret =  findBySql.process(new Object[]{sql, cls, scalar, index, count, paras, cl, cache, mode, region});
			}catch(Exception e){
				log.error("PlatformDao: find by sql failed", e);	
			}
			return ret;
		}
	}

	public Long countBySql(String sql, Map<Object, Object> paras, Boolean cache, CacheMode cm, String region) throws Exception {
		try {
			SQLQuery query = getSession().createSQLQuery(sql);
			if(paras != null) for (Entry<Object, Object> entry : paras.entrySet())
				if(entry.getValue() instanceof Pojo) if (entry.getKey() instanceof Integer) query.setEntity((Integer)entry.getKey(), entry.getValue());
				else query.setEntity(entry.getKey().toString(), entry.getValue());
				else if(entry.getValue() instanceof Collection) query.setParameterList(entry.getKey().toString(), (Collection)entry.getValue());
				else if(entry.getValue() instanceof Object[]) query.setParameterList(entry.getKey().toString(), (Object[])entry.getValue());
				else if (entry.getKey() instanceof Integer) query.setParameter((Integer)entry.getKey(), entry.getValue());
				else query.setParameter(entry.getKey().toString(), entry.getValue());
			if (Boolean.TRUE.equals(cache)) {
				query.setCacheable(true);
				if (cm != null) query.setCacheMode(cm);
				if (region != null) query.setCacheRegion(region);
			}
			Long count = (Long)query.uniqueResult();
			return count;
		} catch (Exception re) {
			log.error("PlatformDao: count by sql failed", re);
			throw re;
		}
	}
	
	public class CountBySqlProcessor implements IProcessor<Object[], Long> {
		@Override
		public Long process(Object[] instance) {
			try {
				return countBySql((String) instance[0], (Map<Object, Object>) instance[1], (Boolean) instance[2], (CacheMode) instance[3], (String) instance[4]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_DATA_ACCESS, e);
			}
		}
	}
	
	public static class CountBySql implements IInputResource<Object, Long>{
		protected String sql;
		protected Map<Object, Object> paras;
		protected IProcessor<Object[], Long> countBySql;
		protected Boolean cache;
		protected CacheMode mode;
		protected String region;
		
		public void setCache(Boolean cache) {
			this.cache = cache;
		}

		public void setMode(CacheMode mode) {
			this.mode = mode;
		}

		public void setRegion(String region) {
			this.region = region;
		}
		
		public void setSql(String sql) {
			this.sql = sql;
		}

		public void setParas(Map<Object, Object> paras) {
			this.paras = paras;
		}

		public void setCountBySql(IProcessor<Object[], Long> countBySql) {
			this.countBySql = countBySql;
		}

		@Override
		public Long find(Object... paras) {
			String sql = this.sql;
			Map<Object, Object> ps = this.paras;
			if(paras != null && paras.length > 0){
				if(sql == null && paras[0] instanceof String) sql = (String)paras[0];
				if(ps == null && paras.length > 1 && paras[1] instanceof Map) ps = (Map<Object, Object>) paras[1];
			}
			Long count = null;
			try{
				count = countBySql.process(new Object[]{sql, ps, cache, mode, region});
			}catch(Exception e){
				log.error("PlatformDao: count by sql failed", e);
			}
			return count;
		}
	}

	public Integer executeNamedQuery(String name, Map<Object, Object> paras) throws Exception {
		try {
			Query query = getSession().getNamedQuery(name);
			if(paras != null) for (Entry<Object, Object> entry : paras.entrySet())
				if(entry.getValue() instanceof Pojo) if (entry.getKey() instanceof Integer) query.setEntity((Integer)entry.getKey(), entry.getValue());
				else query.setEntity(entry.getKey().toString(), entry.getValue());
				else if(entry.getValue() instanceof Collection) query.setParameterList(entry.getKey().toString(), (Collection)entry.getValue());
				else if(entry.getValue() instanceof Object[]) query.setParameterList(entry.getKey().toString(), (Object[])entry.getValue());
				else if (entry.getKey() instanceof Integer) query.setParameter((Integer)entry.getKey(), entry.getValue());
				else query.setParameter(entry.getKey().toString(), entry.getValue());
			Integer result = query.executeUpdate();
			return result;
		} catch (Exception re) {
			log.error("PlatformDao: execute named query failed", re);
			throw re;
		}
	}
	
	public class ExecuteNamedQueryProcessor implements IProcessor<Object[], Integer> {
		@Override
		public Integer process(Object[] instance) {
			try {
				return executeNamedQuery((String) instance[0], (Map<Object, Object>) instance[1]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_DATA_ACCESS, e);
			}
		}
	}
	
	public static class ExecuteNamedQuery implements IInputResource<Object, Integer>{
		protected String name;
		protected Map<Object, Object> paras;
		protected IProcessor<Object[], Integer> executeNamedQuery;

		public void setName(String name) {
			this.name = name;
		}

		public void setParas(Map<Object, Object> paras) {
			this.paras = paras;
		}
		
		public void setExecuteNamedQuery(IProcessor<Object[], Integer> executeNamedQuery) {
			this.executeNamedQuery = executeNamedQuery;
		}

		@Override
		public Integer find(Object... paras) {
			String name = this.name;
			Map<Object, Object> ps = this.paras;
			if(paras != null && paras.length > 0){
				if(name == null && paras[0] instanceof String) name = (String)paras[0];
				if(ps == null && paras.length > 1 && paras[1] instanceof Map) ps = (Map<Object, Object>) paras[1];
			}
			Integer ret = null;
			try{
				ret = executeNamedQuery.process(new Object[]{name, ps});
			}catch(Exception e){
				log.error("PlatformDao: execute named query failed", e);
			}
			return ret;
		}	
	}
	
	public List<V> findByNamedQuery(String name, Integer index, Integer count, Map<Object, Object> paras, Boolean cache, CacheMode cm, String region) throws Exception {
		try {
			Query query = getSession().getNamedQuery(name);
			if(paras != null) for (Entry<Object, Object> entry : paras.entrySet())
				if(entry.getValue() instanceof Pojo) if (entry.getKey() instanceof Integer) query.setEntity((Integer)entry.getKey(), entry.getValue());
				else query.setEntity(entry.getKey().toString(), entry.getValue());
				else if(entry.getValue() instanceof Collection) query.setParameterList(entry.getKey().toString(), (Collection)entry.getValue());
				else if(entry.getValue() instanceof Object[]) query.setParameterList(entry.getKey().toString(), (Object[])entry.getValue());
				else if (entry.getKey() instanceof Integer) query.setParameter((Integer)entry.getKey(), entry.getValue());
				else query.setParameter(entry.getKey().toString(), entry.getValue());
			if(index != null && count != null){
				query.setFirstResult(index);
				query.setMaxResults(count);
			}
			if (Boolean.TRUE.equals(cache)) {
				query.setCacheable(true);
				if (cm != null) query.setCacheMode(cm);
				if (region != null) query.setCacheRegion(region);
			}
			List<V> results = (List<V>) process(query.list());
			return results;
		} catch (Exception re) {
			log.error("PlatformDao: find by named query failed", re);
			throw re;
		}
	}
	
	public class FindByNamedQueryProcessor implements IProcessor<Object[], List<V>> {
		@Override
		public List<V> process(Object[] instance) {
			try {
				return findByNamedQuery((String) instance[0], (Integer) instance[1], (Integer) instance[2], (Map<Object, Object>) instance[3], (Boolean) instance[4], (CacheMode) instance[5], (String) instance[6]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_DATA_ACCESS, e);
			}
		}
	}
	
	public static class FindByNamedQuery<V> implements IInputResource<Object, List<V>>{
		protected String name;
		protected Map<Object, Object> paras;
		protected IProcessor<Object[], List<V>> findByNamedQuery;
		protected Boolean cache;
		protected CacheMode mode;
		protected String region;
		
		public void setCache(Boolean cache) {
			this.cache = cache;
		}

		public void setMode(CacheMode mode) {
			this.mode = mode;
		}

		public void setRegion(String region) {
			this.region = region;
		}
		
		public void setParas(Map<Object, Object> paras) {
			this.paras = paras;
		}

		public void setName(String name) {
			this.name = name;
		}

		public void setFindByNamedQuery(IProcessor<Object[], List<V>> findByNamedQuery) {
			this.findByNamedQuery = findByNamedQuery;
		}

		@Override
		public List<V> find(Object... paras) {
			String name = this.name;
			Map<Object, Object> ps = this.paras;
			Integer index = null;
			Integer count = null;
			if(paras != null && paras.length > 0){
				if(name == null && paras[0] instanceof String) name = (String)paras[0];
				if(paras.length > 1 && paras[1] instanceof Integer) index = (Integer)paras[1];
				if(paras.length > 2 && paras[2] instanceof Integer) count = (Integer)paras[2];
				if(ps == null && paras.length > 3 && paras[3] instanceof Map) ps = (Map<Object, Object>) paras[3];
			}
			List<V> ret = null;
			try{
				ret = findByNamedQuery.process(new Object[]{name, index, count, ps, cache, mode, region});
			}catch(Exception e){
				log.error("PlatformDao: find by named query failed", e);
			}
			return ret;
		}
	}

	public Long countByNamedQuery(String name, Map<Object, Object> paras, Boolean cache, CacheMode cm, String region) throws Exception {
		try {
			Query query = getSession().getNamedQuery(name);
			if(paras != null) for (Entry<Object, Object> entry : paras.entrySet())
				if(entry.getValue() instanceof Pojo) if (entry.getKey() instanceof Integer) query.setEntity((Integer)entry.getKey(), entry.getValue());
				else query.setEntity(entry.getKey().toString(), entry.getValue());
				else if(entry.getValue() instanceof Collection) query.setParameterList(entry.getKey().toString(), (Collection)entry.getValue());
				else if(entry.getValue() instanceof Object[]) query.setParameterList(entry.getKey().toString(), (Object[])entry.getValue());
				else if (entry.getKey() instanceof Integer) query.setParameter((Integer)entry.getKey(), entry.getValue());
				else query.setParameter(entry.getKey().toString(), entry.getValue());
			if (Boolean.TRUE.equals(cache)) {
				query.setCacheable(true);
				if (cm != null) query.setCacheMode(cm);
				if (region != null) query.setCacheRegion(region);
			}
			Long count = (Long)query.uniqueResult();
			return count;
		} catch (Exception re) {
			log.error("PlatformDao: count by named query failed", re);
			throw re;
		}
	}
	
	public class CountByNamedQueryProcessor implements IProcessor<Object[], Long> {
		@Override
		public Long process(Object[] instance) {
			try {
				return countByNamedQuery((String) instance[0], (Map<Object, Object>) instance[1], (Boolean) instance[2], (CacheMode) instance[3], (String) instance[4]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_DATA_ACCESS, e);
			}
		}
	}
	
	public static class CountByNamedQuery implements IInputResource<Object, Long>{
		protected String name;
		protected Map<Object, Object> paras;
		protected IProcessor<Object[], Long> countByNamedQuery;
		protected Boolean cache;
		protected CacheMode mode;
		protected String region;
		
		public void setCache(Boolean cache) {
			this.cache = cache;
		}

		public void setMode(CacheMode mode) {
			this.mode = mode;
		}

		public void setRegion(String region) {
			this.region = region;
		}
		
		public void setParas(Map<Object, Object> paras) {
			this.paras = paras;
		}

		public void setCountByNamedQuery(IProcessor<Object[], Long> countByNamedQuery) {
			this.countByNamedQuery = countByNamedQuery;
		}

		@Override
		public Long find(Object... paras) {
			String name = this.name;
			Map<Object, Object> ps = this.paras;
			if(paras != null && paras.length > 0){
				if(name == null && paras[0] instanceof String) name = (String)paras[0];
				if(ps == null && paras.length > 1 && paras[1] instanceof Map) ps = (Map<Object, Object>) paras[1];
			}
			Long count = null;
			try{
				count = countByNamedQuery.process(new Object[]{name, ps, cache, mode, region});
			}catch(Exception e){
				log.error("PlatformDao: count by named query failed", e);
			}
			return count;
		}
	}
	
	public Integer executeHql(String hql, Map<Object, Object> paras) throws Exception {
		try {
			Query query = getSession().createQuery(hql);
			if(paras != null) for (Entry<Object, Object> entry : paras.entrySet())
				if(entry.getValue() instanceof Pojo) if (entry.getKey() instanceof Integer) query.setEntity((Integer)entry.getKey(), entry.getValue());
				else query.setEntity(entry.getKey().toString(), entry.getValue());
				else if(entry.getValue() instanceof Collection) query.setParameterList(entry.getKey().toString(), (Collection)entry.getValue());
				else if(entry.getValue() instanceof Object[]) query.setParameterList(entry.getKey().toString(), (Object[])entry.getValue());
				else if (entry.getKey() instanceof Integer) query.setParameter((Integer)entry.getKey(), entry.getValue());
				else query.setParameter(entry.getKey().toString(), entry.getValue());
			Integer result = query.executeUpdate();
			return result;
		} catch (Exception re) {
			log.error("PlatformDao: execute hql failed", re);
			throw re;
		}
	}
	
	public class ExecuteHqlProcessor implements IProcessor<Object[], Integer> {
		@Override
		public Integer process(Object[] instance) {
			try {
				return executeHql((String) instance[0], (Map<Object, Object>) instance[1]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_DATA_ACCESS, e);
			}
		}
	}
	
	public static class ExecuteHql implements IInputResource<Object, Integer>{
		protected String hql;
		protected Map<Object, Object> paras;
		protected IProcessor<Object[], Integer> executeHql;
		
		public void setHql(String hql) {
			this.hql = hql;
		}

		public void setParas(Map<Object, Object> paras) {
			this.paras = paras;
		}
		
		public void setExecuteHql(IProcessor<Object[], Integer> executeHql) {
			this.executeHql = executeHql;
		}

		@Override
		public Integer find(Object... paras) {
			String hql = this.hql;
			Map<Object, Object> ps = this.paras;
			if(paras != null && paras.length > 0){
				if(hql == null && paras[0] instanceof String) hql = (String)paras[0];
				if(ps == null && paras.length > 1 && paras[1] instanceof Map) ps = (Map<Object, Object>) paras[1];
			}
			Integer ret = null;
			try{
				ret = executeHql.process(new Object[]{hql, ps});
			}catch(Exception e){
				log.error("PlatformDao: execute hql failed", e);
			}
			return ret;
		}	
	}
	
	public List<V> findByHql(String hql, Integer index, Integer count, Map<Object, Object> paras, Boolean cache, CacheMode cm, String region) throws Exception {
		try {
			Query query = getSession().createQuery(hql);
			if(paras != null) for (Entry<Object, Object> entry : paras.entrySet())
				if(entry.getValue() instanceof Pojo) if (entry.getKey() instanceof Integer) query.setEntity((Integer)entry.getKey(), entry.getValue());
				else query.setEntity(entry.getKey().toString(), entry.getValue());
				else if(entry.getValue() instanceof Collection) query.setParameterList(entry.getKey().toString(), (Collection)entry.getValue());
				else if(entry.getValue() instanceof Object[]) query.setParameterList(entry.getKey().toString(), (Object[])entry.getValue());
				else if (entry.getKey() instanceof Integer) query.setParameter((Integer)entry.getKey(), entry.getValue());
				else query.setParameter(entry.getKey().toString(), entry.getValue());
			if(index != null && count != null){
				query.setFirstResult(index);
				query.setMaxResults(count);
			}
			if (Boolean.TRUE.equals(cache)) {
				query.setCacheable(true);
				if (cm != null) query.setCacheMode(cm);
				if (region != null) query.setCacheRegion(region);
			}
			List<V> results = (List<V>) process(query.list());
			return results;
		} catch (Exception re) {
			log.error("PlatformDao: find by hql failed", re);
			throw re;
		}
	}
	
	public class FindByHqlProcessor implements IProcessor<Object[], List<V>> {
		@Override
		public List<V> process(Object[] instance) {
			try {
				return findByHql((String) instance[0], (Integer) instance[1], (Integer) instance[2], (Map<Object, Object>) instance[3], (Boolean) instance[4], (CacheMode) instance[5], (String) instance[6]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_DATA_ACCESS, e);
			}
		}
	}
	
	public static class FindByHql<V> implements IInputResource<Object, List<V>>{
		protected String hql;
		protected Map<Object, Object> paras;
		protected IProcessor<Object[], List<V>> findByHql;
		protected Boolean cache;
		protected CacheMode mode;
		protected String region;
		
		public void setCache(Boolean cache) {
			this.cache = cache;
		}

		public void setMode(CacheMode mode) {
			this.mode = mode;
		}

		public void setRegion(String region) {
			this.region = region;
		}
		
		public void setHql(String hql) {
			this.hql = hql;
		}

		public void setParas(Map<Object, Object> paras) {
			this.paras = paras;
		}

		public void setFindByHql(IProcessor<Object[], List<V>> findByHql) {
			this.findByHql = findByHql;
		}

		@Override
		public List<V> find(Object... paras) {
			String hql = this.hql;
			Map<Object, Object> ps = this.paras;
			Integer index = null;
			Integer count = null;
			if(paras != null && paras.length > 0){
				if(hql == null && paras[0] instanceof String) hql = (String)paras[0];
				if(paras.length > 1 && paras[1] instanceof Integer) index = (Integer)paras[1];
				if(paras.length > 2 && paras[2] instanceof Integer) count = (Integer)paras[2];
				if(ps == null && paras.length > 3 && paras[3] instanceof Map) ps = (Map<Object, Object>) paras[3];
			}
			List<V> ret = null;
			try{
				ret = findByHql.process(new Object[]{hql, index, count, ps, cache, mode, region});
			}catch(Exception e){
				log.error("PlatformDao: find by hql failed", e);
			}
			return ret;
		}
	}

	public Long countByHql(String hql, Map<Object, Object> paras, Boolean cache, CacheMode cm, String region) throws Exception {
		try {
			Query query = getSession().createQuery(hql);
			if(paras != null) for (Entry<Object, Object> entry : paras.entrySet())
				if(entry.getValue() instanceof Pojo) if (entry.getKey() instanceof Integer) query.setEntity((Integer)entry.getKey(), entry.getValue());
				else query.setEntity(entry.getKey().toString(), entry.getValue());
				else if(entry.getValue() instanceof Collection) query.setParameterList(entry.getKey().toString(), (Collection)entry.getValue());
				else if(entry.getValue() instanceof Object[]) query.setParameterList(entry.getKey().toString(), (Object[])entry.getValue());
				else if (entry.getKey() instanceof Integer) query.setParameter((Integer)entry.getKey(), entry.getValue());
				else query.setParameter(entry.getKey().toString(), entry.getValue());
			if (Boolean.TRUE.equals(cache)) {
				query.setCacheable(true);
				if (cm != null) query.setCacheMode(cm);
				if (region != null) query.setCacheRegion(region);
			}
			Long count = (Long)query.uniqueResult();
			return count;
		} catch (Exception re) {
			log.error("PlatformDao: count by hql failed", re);
			throw re;
		}
	}
	
	public class CountByHqlProcessor implements IProcessor<Object[], Long> {
		@Override
		public Long process(Object[] instance) {
			try {
				return countByHql((String) instance[0], (Map<Object, Object>) instance[1], (Boolean) instance[2], (CacheMode) instance[3], (String) instance[4]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_DATA_ACCESS, e);
			}
		}
	}
	
	public static class CountByHql implements IInputResource<Object, Long>{
		protected String hql;
		protected Map<Object, Object> paras;
		protected IProcessor<Object[], Long> countByHql;
		protected Boolean cache;
		protected CacheMode mode;
		protected String region;
		
		public void setCache(Boolean cache) {
			this.cache = cache;
		}

		public void setMode(CacheMode mode) {
			this.mode = mode;
		}

		public void setRegion(String region) {
			this.region = region;
		}
				
		public void setHql(String hql) {
			this.hql = hql;
		}

		public void setParas(Map<Object, Object> paras) {
			this.paras = paras;
		}

		public void setCountByHql(IProcessor<Object[], Long> countByHql) {
			this.countByHql = countByHql;
		}

		@Override
		public Long find(Object... paras) {
			String hql = this.hql;
			Map<Object, Object> ps = this.paras;
			if(paras != null && paras.length > 0){
				if(hql == null && paras[0] instanceof String) hql = (String)paras[0];
				if(ps == null && paras.length > 1 && paras[1] instanceof Map) ps = (Map<Object, Object>) paras[1];
			}
			Long count = null;
			try{
				count = countByHql.process(new Object[]{hql, ps, cache, mode, region});
			}catch(Exception e){
				log.error("PlatformDao: count by hql failed", e);
			}
			return count;
		}
	}
	
	protected static class IdPair{
		final protected Object min;
		final protected Object max;
		
		public IdPair(Object min, Object max) {
			this.min = min;
			this.max = max;
		}
		
		public Object getMin() {
			return min;
		}
		
		public Object getMax() {
			return max;
		}
	}
	
	@Override
	public <P> P store(V resource, K... paras) {
		if(putMode == 0) {
			if(resource != null){
				if (paras == null || paras.length == 0 || !(paras[0] instanceof Boolean)) if(resource.getClass().isArray()) batchSaveOrUpdate((Collection)TypeConvertor.asCollection(resource));
				else if(Collection.class.isInstance(resource)) batchSaveOrUpdate((Collection)resource);
				else saveOrUpdate(resource);
				else if (Boolean.TRUE.equals(paras[0])) if(resource.getClass().isArray()) batchSave((Collection)TypeConvertor.asCollection(resource));
				else if(Collection.class.isInstance(resource)) batchSave((Collection)resource);
				else save(resource);
				else if(resource.getClass().isArray()) batchUpdate((Collection)TypeConvertor.asCollection(resource));
				else if(Collection.class.isInstance(resource)) batchUpdate((Collection)resource);
				else update(resource);
			}
		} else discard(paras);
		return null;
	}

	@Override
	public <P> P discard(K... paras) {
		if(paras != null && paras.length > 0) try {
			getSession().createQuery((String)paras[0]).executeUpdate();
		} catch (Exception e) {
			log.error("PlatformDao: remove resource failed", e);			
		}
		return null;
	}

	public V find(K... paras) {
		V o = null;
		if(paras != null && paras.length > 0) try {
			Query query = getSession().createQuery((String)paras[0]);
			if (Boolean.TRUE.equals(cache)) {
				query.setCacheable(true);
				if (mode != null) query.setCacheMode(mode);
				if (region != null) query.setCacheRegion(region);
			}			
			o = (V)process(query.list());
		} catch (Exception e) {
			log.error("PlatformDao: find resource failed", e);	
		}
		return o;
	}
	
	@Override
	public <P> P empty(K... paras) {
		if(paras != null && paras.length > 0) try {
			getSession().createQuery((String)paras[0]).executeUpdate();
		} catch (Exception e) {
			log.error("PlatformDao: clear resource failed", e);			
		}	
		return null;
	}
	
	@Override
	public Collection<K[]> keys(K... paras) {
		Collection<K[]> o = null;
		if(paras != null && paras.length > 0) try {
			Query query = getSession().createQuery((String)paras[0]);
			if (Boolean.TRUE.equals(cache)) {
				query.setCacheable(true);
				if (mode != null) query.setCacheMode(mode);
				if (region != null) query.setCacheRegion(region);
			}	
			o = (Collection)query.list();
		} catch (Exception e) {
			log.error("PlatformDao: list keys failed", e);	
		}
		return o;
	}

	@Override
	public Map<K[], V> all(K... paras) {
		Map<K[], V> o = null;
		if(paras != null && paras.length > 0) try {
			Query query = getSession().createQuery((String)paras[0]);
			if (Boolean.TRUE.equals(cache)) {
				query.setCacheable(true);
				if (mode != null) query.setCacheMode(mode);
				if (region != null) query.setCacheRegion(region);
			}	
			Collection<K[]> r = (Collection<K[]>)query.list();
			if(r != null) {
				o = new HashMap<K[], V>();
				for(K[] key : r) if(key != null && key.length > 0) 
					o.put(Arrays.copyOf(key, key.length - 1), (V)key[key.length - 1]);
			}
		} catch (Exception e) {
			log.error("PlatformDao: list keys failed", e);	
		}
		return o;
	}	

	@Override
	public <N> N extend(Object object) {
		if (object != null) {
			Object method = object instanceof ResourceMethod ? object : methods.process(object);
			if (method!= null) switch((ResourceMethod) method) {
				case count : return (N) count;
			}
		}
		return null;
	}
	
	@Override
	public Collection<Object> methods() {
		return methods;
	}
	
	@Override
	public R execute(IProcessor<IListableResource<K, V>, R> processor) {
		return processor.process(this);
	}
	
	public R execute(IProcessor<IListableResource<K, V>, R> processor, IListableResource<K, V> resource) {
		return processor.process(resource);
	}
	
	public class Transaction implements ITransaction<K, V, IListableResource<K, V>, R>{
		protected IListableResource<K, V> resource;
		
		public void setResource(IListableResource<K, V> resource) {
			this.resource = resource;
		}

		@Override
		public R execute(IProcessor<IListableResource<K, V>, R> processor) {
			return PlatformDao.this.execute(processor, resource);
		}
	}
	
	public static class AdaptorResource implements IListableResource<Object, Object>{
		protected IInputResource<Object, Object> find;
		protected IInputResource<Object, Object> put;
		protected IInputResource<Object, Object> remove;
		protected IInputResource<Object, Object> clear;
		protected IInputResource<Object, Object> listKeys;
		protected IInputResource<Object, Object> listAll;
		
		public void setFind(IInputResource<Object, Object> find) {
			this.find = find;
		}

		public void setPut(IInputResource<Object, Object> put) {
			this.put = put;
		}

		public void setRemove(IInputResource<Object, Object> remove) {
			this.remove = remove;
		}

		public void setClear(IInputResource<Object, Object> clear) {
			this.clear = clear;
		}

		public void setListKeys(IInputResource<Object, Object> listKeys) {
			this.listKeys = listKeys;
		}

		public void setListAll(IInputResource<Object, Object> listAll) {
			this.listAll = listAll;
		}

		@Override
		public Object find(Object... paras) {
			return find.find(paras);
		}

		@Override
		public <P> P store(Object value, Object... paras) {
			put.find(paras);
			return null;
		}

		@Override
		public <P> P discard(Object... paras) {
			remove.find(paras);
			return null;
		}

		@Override
		public <P> P empty(Object... paras) {
			clear.find(paras);
			return null;
		}

		@Override
		public Collection<Object[]> keys(Object... paras) {
			return (Collection<Object[]>)listKeys.find(paras);
		}

		@Override
		public Map<Object[], Object> all(Object... paras) {
			return (Map<Object[], Object>)listAll.find(paras);
		}
	}
}
