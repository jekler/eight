package net.yeeyaa.eight.data.util;

import org.hibernate.Criteria;
import org.hibernate.HibernateException;
import org.hibernate.criterion.AggregateProjection;
import org.hibernate.criterion.CriteriaQuery;
import org.hibernate.type.IntegerType;
import org.hibernate.type.Type;

public class MultipleCountProjection extends AggregateProjection {
   protected boolean distinct;
   protected String flag = ",";
   protected boolean concat;
   
   public MultipleCountProjection(String prop, String flag) {
      super("count", prop);
      if (flag != null) this.flag = flag;
   }

   public String toString() {
      if(distinct) if(concat) return "distinct concat(" + super.toString() + ")";
      else return "distinct " + super.toString();
      else if (concat) return "concat(" + super.toString() + ")";
      else return super.toString();
   }

   public Type[] getTypes(Criteria criteria, CriteriaQuery criteriaQuery) throws HibernateException {
      return new Type[] { IntegerType.INSTANCE };
   }

   public String toSqlString(Criteria criteria, int position, CriteriaQuery criteriaQuery) throws HibernateException {
	    StringBuffer buf = new StringBuffer();
	    buf.append("count(");
	    if (distinct) buf.append("distinct ");
	    if (concat) buf.append("concat(");
        String[] properties = propertyName.split(";");
        for (int i = 0; i < properties.length; i++) {
           buf.append( criteriaQuery.getColumn(criteria, properties[i]) );
           if(i != properties.length - 1) buf.append(" ").append(flag).append(" ");
        }
        if(concat) buf.append(")");
        buf.append(") as y");
        buf.append(position);
        buf.append('_');
        return buf.toString();
   }
   
   public MultipleCountProjection setConcat() {
		this.concat = true;
		return this;
   }

   public MultipleCountProjection setDistinct() {
      distinct = true;
      return this;
   }
}
