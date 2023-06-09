package net.yeeyaa.eight.data.dao;

import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Semaphore;

import net.yeeyaa.eight.IInputResource;
import net.yeeyaa.eight.IProcessor;
import net.yeeyaa.eight.PlatformException;
import net.yeeyaa.eight.core.PlatformError;

import org.apache.lucene.index.Term;
import org.apache.lucene.index.Terms;

import org.apache.lucene.search.BooleanClause;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.ConstantScoreQuery;
import org.apache.lucene.search.DisjunctionMaxQuery;
import org.apache.lucene.search.FuzzyQuery;
import org.apache.lucene.search.MatchAllDocsQuery;
import org.apache.lucene.search.MultiPhraseQuery;
import org.apache.lucene.search.NumericRangeQuery;
import org.apache.lucene.search.PhraseQuery;
import org.apache.lucene.search.PrefixQuery;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.RegexpQuery;
import org.apache.lucene.search.TermQuery;
import org.apache.lucene.search.TermRangeQuery;
import org.apache.lucene.search.WildcardQuery;
import org.apache.lucene.util.BytesRef;

import org.hibernate.CacheMode;
import org.hibernate.Criteria;
import org.hibernate.criterion.Order;
import org.hibernate.criterion.Projections;
import org.hibernate.criterion.Restrictions;
import org.hibernate.search.FullTextQuery;
import org.hibernate.search.FullTextSession;
import org.hibernate.search.Search;


public class SearchDao<PK extends Serializable, K, V, R> extends PlatformDao<PK, K, V, R> {
	protected FullTextSession getSession(){
		return Search.getFullTextSession(sessionFactory.getCurrentSession());
	}

	protected FullTextSession openSession(){
		return Search.getFullTextSession(sessionFactory.openSession());
	}
	
	protected class IndexerRunner implements Runnable{
		protected final ConcurrentLinkedQueue<IdPair> cachelist;
		protected final Class<V> cls;
		protected final Semaphore customer;
		@Override
		public void run() {
			FullTextSession fullTextSession = openSession();
			try {
				customer.acquire();
				IdPair idpair = cachelist.poll();
				while(idpair != null){
					for(Object o : fullTextSession.createCriteria(cls).add(Restrictions.ge(Projections.id().toString(),idpair.getMin())).add(Restrictions.le(Projections.id().toString(),idpair.getMax())).list()) fullTextSession.index(o);
					fullTextSession.flushToIndexes();
					fullTextSession.clear();
					customer.acquire();
					idpair = cachelist.poll();}
			} catch (Exception e) {
				log.error("SearchDao: indexing runner failed", e);
			}finally{
				fullTextSession.close();
			}
		}
		
		public IndexerRunner(ConcurrentLinkedQueue<IdPair> cachelist, Class<V> cls,	Semaphore customer) {
			this.cachelist = cachelist;
			this.cls = cls;
			this.customer = customer;
		}
	}
		
	public void createIndex(Class<V> cls, Integer threadnum, Integer batchsize, Integer idsize) throws Exception{
		LinkedList<Thread> ls = new LinkedList<Thread>();
		try {
			FullTextSession fullTextSession = getSession();
			Criteria criteria = fullTextSession.createCriteria(cls).setProjection(Projections.id()).addOrder(Order.asc(Projections.id().toString())).setMaxResults(idsize);
			ConcurrentLinkedQueue<IdPair> cachelist = new ConcurrentLinkedQueue<IdPair>();
			Semaphore customer = new Semaphore(0);
			for(int i = 0 ; i < threadnum; i ++){
				Thread t = new Thread(new IndexerRunner(cachelist, cls, customer));
				ls.add(t);
				t.start();
			}
			int count = 0;
			int index = 0;
			do{
				criteria.setFirstResult(index * idsize);
				List<V> results = criteria.list();
				count = results.size();
				int times = count/batchsize;
				for(int i = 0; i < times; i++) cachelist.add(new IdPair(results.get(i*batchsize), results.get(i*batchsize + batchsize -1)));
				if(count - times * batchsize > 0)  {
					cachelist.add(new IdPair(results.get(times*batchsize), results.get(count - 1)));
					times ++;
				}
				index ++;
				customer.release(times);
			}while(idsize.equals(count));
			customer.release(threadnum);
			for(Thread t : ls) t.join();
			fullTextSession.getSearchFactory().optimize(cls);
		} catch (Exception re) {
			log.error("SearchDao: indexing failed.", re);
			throw re;
		} finally{
			for(Thread t : ls) if(t.isAlive()) t.interrupt();
		}
	}
	
	public class IndexCreator implements IProcessor<String, Void> {
		protected String regex="~";
		
		public void setRegex(String regex) {
			if(regex != null) this.regex = regex;
		}
		
		@Override
		public Void process(String paras) {
			if(paras != null && paras.length() > 0){
				for(String para : paras.split(regex+regex+regex)) try{
					Class<V> cls = null;
					Integer threadnum = 1;
					Integer batchsize = 10000;
					Integer idsize = 100000;
					for (String subpara : para.split(regex+regex)){
						String[] kv = subpara.split(regex);
						if(kv.length > 1){
							if(kv[0].trim().equals("class")) {
								cls = (Class<V>) Class.forName(kv[1]);
							}else if(kv[0].trim().equals("thread-num")){
								threadnum = Integer.parseInt(kv[1]);
								if (threadnum < 1) threadnum = 1;
							}else if(kv[0].trim().equals("batchsize")){
								batchsize = Integer.parseInt(kv[1]);
								if (batchsize < 1) batchsize = 10000;
							}else if(kv[0].trim().equals("idsize")){
								idsize = Integer.parseInt(kv[1]);
								if (idsize < 1) idsize = 10000;
							}
						}
					}
					if(cls != null) createIndex(cls, threadnum, batchsize,idsize);
				}catch(Exception e){
					log.error("IndexCreator: job failed.", e);
				}
			}
			return null;
		}
	}
	
	public void removeIndex(Class<V> cls) throws Exception{
		try {
			getSession().purgeAll(cls);
		} catch (Exception re) {
			log.error("SearchDao: remove indexing failed.", re);
			throw re;
		}		
	}
	
	public void optimizeIndex(Class<V> cls) throws Exception{
		try {
			getSession().getSearchFactory().optimize(cls);
		} catch (Exception re) {
			log.error("SearchDao: optimize indexing failed.", re);
			throw re;
		}		
	}
	
	protected Query createQuery(Object... paras){
		if(paras != null && paras.length > 0 && paras[0] != null){
			if("term".equals(paras[0])){
				if(paras.length > 2 && paras[1] instanceof String) {
					if(paras[2] instanceof String) return new TermQuery(new Term((String) paras[1], (String)paras[2]));
					else return new TermQuery(new Term((String) paras[1]));
				}
			} else if("boolean".equals(paras[0])){
				BooleanQuery.Builder query = new BooleanQuery.Builder();
				if(paras.length > 1 && paras[1] instanceof Boolean) query.setDisableCoord((Boolean) paras[1]);
				if(paras.length > 2 && paras[2] instanceof Integer) query.setMinimumNumberShouldMatch((Integer)paras[2]);
				if(paras.length > 3) for(int i = 3; i < paras.length; i++) if(paras[i] instanceof Object[] && Array.getLength(paras[i]) > 1 && ((Object[])paras[i])[0] instanceof Object[] && ((Object[])paras[i])[1] instanceof String) try{
					BooleanClause.Occur occur = BooleanClause.Occur.valueOf((String)((Object[])paras[i])[1]);
					query.add(createQuery((Object[])((Object[])paras[i])[0]), occur);
				} catch(Exception e){
					log.error("PlatformDao.createQuery: create query error.", e);
				}
				return query.build();
			} else if("wildcard".equals(paras[0])){
				if(paras.length > 3 && paras[1] instanceof String) {
					if(paras[2] instanceof String) if (paras[3] instanceof Integer) return new WildcardQuery(new Term((String) paras[1], (String)paras[2]), (Integer)paras[3]);
					else return new WildcardQuery(new Term((String) paras[1], (String)paras[2]));
					else if (paras[3] instanceof Integer) return new WildcardQuery(new Term((String) paras[1]), (Integer) paras[3]);
					else return new WildcardQuery(new Term((String) paras[1]));
				}
			} else if("phrase".equals(paras[0])){
				PhraseQuery.Builder query = new PhraseQuery.Builder();
				if (paras.length > 1 && paras[1] instanceof Integer) query.setSlop((Integer) paras[1]);
				if(paras.length > 2) for(int i = 2; i < paras.length; i++) if(paras[i] instanceof Object[] && Array.getLength(paras[i]) > 1 && ((Object[])paras[i])[0] instanceof String) {
					Term term;
					if(((Object[])paras[i])[1] instanceof String) term = new Term((String)((Object[])paras[i])[0], (String)((Object[])paras[i])[1]);
					else term = new Term((String)((Object[])paras[i])[0]);
					if(Array.getLength(paras[i]) > 2 && ((Object[])paras[i])[2] instanceof Integer) query.add(term, (Integer) ((Object[])paras[i])[2]);
					else query.add(term);
				}
				return query.build();
			} else if("prefix".equals(paras[0])){
				if(paras.length > 2 && paras[1] instanceof String) {
					if(paras[2] instanceof String) return new PrefixQuery(new Term((String) paras[1], (String)paras[2]));
					else return new PrefixQuery(new Term((String) paras[1]));
				}
			} else if("multiPhrase".equals(paras[0])){
				MultiPhraseQuery query = new MultiPhraseQuery();
				if (paras.length > 1 && paras[1] instanceof Integer) query.setSlop((Integer) paras[1]);
				if(paras.length > 2) for(int i = 2; i < paras.length; i++) if(paras[i] instanceof Object[]) {
					if(Array.getLength(paras[i]) > 1 && ((Object[])paras[i])[0] instanceof String){
						Term term;
						if(((Object[])paras[i])[1] instanceof String) term = new Term((String)((Object[])paras[i])[0], (String)((Object[])paras[i])[1]);
						else term = new Term((String)((Object[])paras[i])[0]);
						query.add(term);
					}else {
						List<Term> terms = new ArrayList<Term>(Array.getLength(paras[i]));
						Integer pos = null;
						for(Object o : (Object[])paras[i]) if(o instanceof Object[]){
							if(Array.getLength(o) > 1 && ((Object[])o)[0] instanceof String && ((Object[])o)[1] instanceof String) 
								terms.add(new Term((String)((Object[])o)[0], (String)((Object[])o)[1]));
							else if(Array.getLength(o) > 0 && ((Object[])o)[0] instanceof String) terms.add(new Term((String)((Object[])o)[0]));
						}else if(o instanceof Integer && pos == null) pos = (Integer)o;
						if(pos == null) query.add(terms.toArray(new Term[terms.size()]));
						else query.add(terms.toArray(new Term[terms.size()]), pos);
					}
				}
				return query;
			} else if("fuzzy".equals(paras[0])){
				Term term = null;
				if(paras.length > 1 && paras[0] instanceof String && paras[1] instanceof String) term = new Term((String)paras[0], (String)paras[1]);
				else if(paras.length > 0 && paras[0] instanceof String) term = new Term((String)paras[0]);
				Integer maxEdit = null;
				Integer prefixLength = null;
				Integer maxExpansions = null;
				Boolean transpositions = null;
				if(term != null && paras.length > 2 && paras[2] instanceof Integer) maxEdit = (Integer)paras[2];
				if(maxEdit != null && paras.length > 3 && paras[3] instanceof Integer) prefixLength = (Integer)paras[3];
				if(prefixLength != null && paras.length > 4 && paras[4] instanceof Integer) maxExpansions = (Integer)paras[4];
				if(maxExpansions != null && paras.length > 5 && paras[5] instanceof Boolean) transpositions = (Boolean)paras[5];
				if(transpositions != null) return new FuzzyQuery(term, maxEdit, prefixLength, maxExpansions, transpositions);
				else if(maxExpansions != null) return new FuzzyQuery(term, maxEdit, prefixLength, maxExpansions,FuzzyQuery.defaultTranspositions);
				else if(prefixLength != null) return new FuzzyQuery(term, maxEdit, prefixLength);
				else if(maxEdit != null) return new FuzzyQuery(term, maxEdit);
				else if(term != null) return new FuzzyQuery(term);
			}  else if("regexp".equals(paras[0])){
				Term term = null;
				if(paras.length > 1 && paras[0] instanceof String && paras[1] instanceof String) term = new Term((String)paras[0], (String)paras[1]);
				else if(paras.length > 0 && paras[0] instanceof String) term = new Term((String)paras[0]);
				Integer flags = null;
				Integer maxDeterminizedStates = null;
				if(term != null && paras.length > 2 && paras[2] instanceof Integer) flags = (Integer)paras[2];
				if(flags != null && paras.length > 3 && paras[3] instanceof Integer) maxDeterminizedStates = (Integer)paras[3];
				if(maxDeterminizedStates != null) return new RegexpQuery(term, flags, maxDeterminizedStates);
				else if(flags != null) return new RegexpQuery(term, flags);
				else if(term != null) return new RegexpQuery(term);
			}  else if("termRange".equals(paras[0])){
				if(paras.length > 5){
					String field = (paras[1] instanceof String) ? (String) paras[1] : null;
					BytesRef lowerTerm = (paras[2] instanceof String) ? new BytesRef((String)paras[2]) : (paras[2] instanceof byte[]) ? new BytesRef((byte[])paras[2]) : null;
					BytesRef upperTerm = (paras[3] instanceof String) ? new BytesRef((String)paras[3]) : (paras[3] instanceof byte[]) ? new BytesRef((byte[])paras[3]) : null;
					Boolean includeLower = (paras[4] instanceof Boolean) ? (Boolean) paras[4] : null;
					Boolean includeUpper = (paras[5] instanceof Boolean) ? (Boolean) paras[5] : null;					
					if(field != null && lowerTerm != null && upperTerm != null && includeLower != null && includeUpper != null)
						return new TermRangeQuery(field, lowerTerm, upperTerm, includeLower, includeUpper);
				}
			}  else if("numericRange".equals(paras[0])){
				if(paras.length > 5){
					String field = (paras[1] instanceof String) ? (String) paras[1] : null;
					Number min = (paras[2] instanceof Number) ? (Number) paras[2] : null;
					Number max = (paras[3] instanceof Number) ? (Number) paras[3] : null;
					Boolean minInclusive = (paras[4] instanceof Boolean) ? (Boolean) paras[4] : null;
					Boolean maxInclusive = (paras[5] instanceof Boolean) ? (Boolean) paras[5] : null;	
					Integer precisionStep = (paras.length > 6 && paras[6] instanceof Integer) ? (Integer) paras[6] : null;
					if(field != null && min != null && max != null && minInclusive != null && maxInclusive != null)
						if(min instanceof Integer) if(precisionStep == null) return NumericRangeQuery.newIntRange(field, min.intValue(), max.intValue(), minInclusive, maxInclusive);
						else return NumericRangeQuery.newIntRange(field, precisionStep, min.intValue(), max.intValue(), minInclusive, maxInclusive);
						else if(min instanceof Long) if(precisionStep == null) return NumericRangeQuery.newLongRange(field, min.longValue(), max.longValue(), minInclusive, maxInclusive);
						else return NumericRangeQuery.newLongRange(field, precisionStep, min.longValue(), max.longValue(), minInclusive, maxInclusive);
						else if(min instanceof Double) if(precisionStep == null) return NumericRangeQuery.newDoubleRange(field, min.doubleValue(), max.doubleValue(), minInclusive, maxInclusive);
						else return NumericRangeQuery.newDoubleRange(field, precisionStep, min.doubleValue(), max.doubleValue(), minInclusive, maxInclusive);
						else if(min instanceof Float) if(precisionStep == null) return NumericRangeQuery.newFloatRange(field, min.floatValue(), max.floatValue(), minInclusive, maxInclusive);
						else return NumericRangeQuery.newFloatRange(field, precisionStep, min.floatValue(), max.floatValue(), minInclusive, maxInclusive);
				}
			} else if("constantScore".equals(paras[0])){
				Object[] newparas = new Object[paras.length - 1];
				for(int i = 0; i < paras.length - 1; i++) newparas[i] = paras[i + 1];
				if(newparas.length > 0) return new ConstantScoreQuery(createQuery(newparas));
			} else if("disjunctionMax".equals(paras[0])){
				if(paras.length > 1 && paras[1] instanceof Float){
					List<Query> qs = new ArrayList<Query>(paras.length - 2);
					for(int i = 2; i < paras.length; i++) if(paras[i] instanceof Object[]) qs.add(createQuery((Object[])paras[i]));
					DisjunctionMaxQuery query = new DisjunctionMaxQuery(qs, (Float)paras[1]);
					return query;
				}
			} else if("matchAllDocs".equals(paras[0])) return new MatchAllDocsQuery();
		}
		return null;
	}
	
	public List<V> findByFullIndex(Object[] query, String[] projections, Integer count, IProcessor<String, Class<?>> cl, Boolean cache, CacheMode cm, String region, String ... clses) throws Exception {
		try {
			Query q = createQuery(query);
			List<Class<?>> ls = new ArrayList<Class<?>>(clses == null ? 0 : clses.length);
			if (clses != null && clses.length > 0) for(String cls : clses) try{
				ls.add(findClass(cls, cl));
			} catch(Exception e){
				log.error("SearchDao: finding "+q+" by fulltext", e);
			}
			FullTextQuery fullTextQuery = getSession().createFullTextQuery(q, ls.toArray(new Class<?>[ls.size()]));
			if(projections != null && projections.length > 0) fullTextQuery.setProjection(projections);
			if(count != null && count > 0)fullTextQuery.setMaxResults(count);
			if (Boolean.TRUE.equals(cache)) {
				fullTextQuery.setCacheable(true);
				if (cm != null) fullTextQuery.setCacheMode(cm);
				if (region != null) fullTextQuery.setCacheRegion(region);
			}
			List<V> results = (List<V>) process(fullTextQuery.list());
			return results;
		} catch (Exception re) {
			log.error("SearchDao: finding by fulltext", re);
			throw re;
		}	
	}
	
	public class FindByFullIndexProcessor implements IProcessor<Object[], List<V>> {
		@Override
		public List<V> process(Object[] instance) {
			try {
				return findByFullIndex((Object[]) instance[0], (String[]) instance[1], (Integer) instance[2], (IProcessor<String, Class<?>>) instance[3], (Boolean) instance[4], (CacheMode) instance[5], (String) instance[6], (String[]) instance[7]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_DATA_ACCESS, e);
			}
		}
	}
	
	public static class FindByFullIndex<V> implements IInputResource<Object, List<V>>{
		protected Object[] query;
		protected String[] projections;
		protected Integer count;
		protected String[] clses;
		protected IProcessor<String, Class<?>> cl;
		protected IProcessor<Object[], List<V>> findByFullIndex;
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
		
		public void setQuery(Object[] query) {
			this.query = query;
		}

		public void setProjections(String[] projections) {
			this.projections = projections;
		}

		public void setCount(Integer count) {
			this.count = count;
		}

		public void setClses(String[] clses) {
			this.clses = clses;
		}

		public void setFindByFullIndex(IProcessor<Object[], List<V>> findByFullIndex) {
			this.findByFullIndex = findByFullIndex;
		}

		@Override
		public List<V> find(Object... paras) {
			Object[] query = this.query;
			String[] projections = this.projections;
			Integer count = this.count;
			String[] clses = this.clses;
			if(paras != null && paras.length > 0) {
				if(query == null && paras[0] instanceof Object[]) query = (Object[])paras[0];
				if(projections == null && paras.length > 1 && paras[1] instanceof Object[]){
					List<String> ls = new ArrayList<String>(Array.getLength(paras[1]));
					for(Object so : (Object[])paras[1]) if(so instanceof String) ls.add((String)so);
					projections = ls.toArray(new String[ls.size()]);
				}
				if(count == null && paras.length > 2 && paras[2] instanceof Integer) count = (Integer) paras[2];
				if(clses == null && paras.length > 3 && paras[3] instanceof Object[]){
					List<String> ls = new ArrayList<String>(Array.getLength(paras[3]));
					for(Object so : (Object[])paras[3]) if(so instanceof String) ls.add((String)so);
					clses = ls.toArray(new String[ls.size()]);
				}
			}
			List<V> ret = null;
			try{
				ret = findByFullIndex.process(new Object[]{query, projections, count, cl, cache, mode, region, clses});
			}catch(Exception e){
				log.error("SearchDao: finding "+query+" by fulltext", e);
			}
			return ret;
		}	
	}
	
	public Long countByFullIndex(Object[] query, String[] projections, IProcessor<String, Class<?>> cl, Boolean cache, CacheMode cm, String region, String ... clses) throws Exception {
		try {
			Query q = createQuery(query);
			List<Class<?>> ls = new ArrayList<Class<?>>(clses == null ? 0 : clses.length);
			if (clses != null && clses.length > 0) for(String cls : clses) try{
				ls.add(findClass(cls, cl));
			} catch(Exception e){
				log.error("SearchDao: finding "+q+" by fulltext", e);
			}
			FullTextQuery fullTextQuery = getSession().createFullTextQuery(q, ls.toArray(new Class<?>[ls.size()]));
			if(projections != null && projections.length > 0) fullTextQuery.setProjection(projections);
			if (Boolean.TRUE.equals(cache)) {
				fullTextQuery.setCacheable(true);
				if (cm != null) fullTextQuery.setCacheMode(cm);
				if (region != null) fullTextQuery.setCacheRegion(region);
			}
			Long count = new Long(fullTextQuery.getResultSize());
			return count;
		} catch (Exception re) {
			log.error("SearchDao: finding number of by fulltext", re);
			throw re;
		}	
	}

	public class CountByFullIndexProcessor implements IProcessor<Object[], Long> {
		@Override
		public Long process(Object[] instance) {
			try {
				return countByFullIndex((Object[]) instance[0], (String[]) instance[1], (IProcessor<String, Class<?>>) instance[2], (Boolean) instance[3], (CacheMode) instance[4], (String) instance[5], (String[]) instance[6]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_DATA_ACCESS, e);
			}
		}
	}
	
	public static class CountByFullIndex implements IInputResource<Object, Long>{
		protected Object[] query;
		protected String[] projections;
		protected String[] clses;
		protected IProcessor<String, Class<?>> cl;
		protected IProcessor<Object[], Long> countByFullIndex;
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

		public void setQuery(Object[] query) {
			this.query = query;
		}

		public void setClses(String[] clses) {
			this.clses = clses;
		}

		public void setProjections(String[] projections) {
			this.projections = projections;
		}
		
		public void setCountByFullIndex(IProcessor<Object[], Long> countByFullIndex) {
			this.countByFullIndex = countByFullIndex;
		}

		@Override
		public Long find(Object... paras) {
			Object[] query = this.query;
			String[] projections = this.projections;
			String[] clses = this.clses;
			if(paras != null && paras.length > 0) {
				if(query == null && paras[0] instanceof Object[]) query = (Object[])paras[0];
				if(projections == null && paras.length > 1 && paras[1] instanceof Object[]){
					List<String> ls = new ArrayList<String>(Array.getLength(paras[1]));
					for(Object so : (Object[])paras[1]) if(so instanceof String) ls.add((String)so);
					projections = ls.toArray(new String[ls.size()]);
				}
				if(clses == null && paras.length > 2 && paras[2] instanceof Object[]){
					List<String> ls = new ArrayList<String>(Array.getLength(paras[2]));
					for(Object so : (Object[])paras[2]) if(so instanceof String) ls.add((String)so);
					clses = ls.toArray(new String[ls.size()]);
				}
			}
			Long count = null;
			try{
				count = countByFullIndex.process(new Object[]{query, projections, cl, cache, mode, region, clses});
			}catch(Exception e){
				log.error("SearchDao: finding number of "+query+" by fulltext", e);
			}
			return count;
		}	
	}
	
	public Terms findTerms(Integer docid, String field, String cls, IProcessor<String, Class<?>> cl) throws Exception {
		try {
			return getSession().getSearchFactory().getIndexReaderAccessor().open(findClass(cls, cl)).getTermVector(docid, field);
		} catch (Exception re) {
			log.error("SearchDao: finding term vector "+field+" by fulltext id:"+docid, re);
			throw re;
		}	
	}

	public class FindTermsProcessor implements IProcessor<Object[], Terms> {
		@Override
		public Terms process(Object[] instance) {
			try {
				return findTerms((Integer) instance[0], (String) instance[1], (String) instance[2], (IProcessor<String, Class<?>>) instance[3]);
			} catch (Exception e) {
				throw new PlatformException(PlatformError.ERROR_DATA_ACCESS, e);
			}
		}
	}
	
	public static class FindTerms implements IInputResource<Object, Object>{
		protected String field;
		protected Integer docid;
		protected String cls;
		protected IProcessor<String, Class<?>> cl;
		protected IProcessor<Object[], Object> findTerms;
		
		public void setClassLoader(IProcessor<String, Class<?>> cl) {
			this.cl = cl;
		}
		
		public void setField(String field) {
			this.field = field;
		}

		public void setDocid(Integer docid) {
			this.docid = docid;
		}

		public void setCls(String cls) {
			this.cls = cls;
		}

		public void setFindTerms(IProcessor<Object[], Object> findTerms) {
			this.findTerms = findTerms;
		}

		@Override
		public Object find(Object... paras) {
			String field = this.field;
			String cls = this.cls;
			Integer docid = this.docid;
			if(paras != null && paras.length > 0) {
				if(docid == null && paras[0] instanceof Integer) docid = (Integer) paras[0];
				if(field == null && paras.length > 1 && paras[1] instanceof String) field = (String) paras[1];
				if(cls == null && paras.length > 2 && paras[2] instanceof String) cls = (String) paras[2];
			}
			Object ret = null;
			try{
				ret = findTerms.process(new Object[]{docid, field, cls, cl});
			}catch(Exception e){
				log.error("SearchDao: finding term vector "+field+" by fulltext id:"+docid, e);
			}
			return ret;
		}	
	}
}
