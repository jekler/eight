package net.yeeyaa.eight.osgi.loader.config;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.Map.Entry;
import java.util.Set;

public class Clause {
    protected final String name;
    protected final HashMap<String, LinkedHashSet<String>> directives = new HashMap<String, LinkedHashSet<String>>();
    protected final HashMap<String, LinkedHashSet<String>> attributes = new HashMap<String, LinkedHashSet<String>>();

    public Clause(String name, Entry<String, String>[] directives, Entry<String, String>[] attributes) {
        this.name = name;
        if (directives != null && directives.length > 0) for (Entry<String, String> directive : directives) {
        	LinkedHashSet<String> v = this.directives.get(directive.getKey());
        	if (v == null) {
        		v = new LinkedHashSet<String>();
        		this.directives.put(directive.getKey(), v);
        	}
        	v.add(directive.getValue());
        }
        if (attributes != null && attributes.length > 0) for (Entry<String, String> attribute : attributes) {
        	LinkedHashSet<String> v = this.attributes.get(attribute.getKey());
        	if (v == null) {
        		v = new LinkedHashSet<String>();
        		this.attributes.put(attribute.getKey(), v);
        	}
        	v.add(attribute.getValue());
        }
    }

    public String getName() {
        return name;
    }

    public String[] getDirectiveNames() {
        Set<String> set = directives.keySet();
        return set.toArray(new String[set.size()]);
    }

    public String[] getAttributeNames() {
        Set<String> set = attributes.keySet();
        return set.toArray(new String[set.size()]);
    }

    public String getDirective(String name) {
    	LinkedHashSet<String> directive = directives.get(name);
    	if (directive == null) return null;
    	else {
    		String[] ds = directive.toArray(new String[directive.size()]);
    		if (ds.length > 0) return ds[0];
    		else return null;
    	}
    }

    public String[] getDirectives(String name) {
    	LinkedHashSet<String> directive = directives.get(name);
    	if (directive == null) return new String[0];
    	else return directive.toArray(new String[directive.size()]);
    }
    
    public String getAttribute(String name) {
    	LinkedHashSet<String> attribute = attributes.get(name);
    	if (attribute == null) return null;
    	else {
    		String[] as = attribute.toArray(new String[attribute.size()]);
    		if (as.length > 0) return as[0];
    		else return null;
    	}
    }

    public String[] getAttributes(String name) {
    	LinkedHashSet<String> attribute = attributes.get(name);
    	if (attribute == null) return new String[0];
    	else return attribute.toArray(new String[attribute.size()]);
    }

    public String toString() {
        StringBuffer sb = new StringBuffer();
        sb.append(name);
        for (Entry<String, LinkedHashSet<String>> entry : directives.entrySet()) for (String value : entry.getValue()) {
            sb.append(";").append(entry.getKey()).append(":=");
            if (value.indexOf(",") >= 0) sb.append("\"").append(value).append("\"");
            else sb.append(value);
        }
        for (Entry<String, LinkedHashSet<String>> entry : attributes.entrySet()) for (String value : entry.getValue()) {
            sb.append(";").append(entry.getKey()).append("=");
            if (value.indexOf(",") >= 0) sb.append("\"").append(value).append("\"");
            else sb.append(value);
        }
        return sb.toString();
    }
    
    public static Clause[] parseHeader(String header) throws IllegalArgumentException {
        Clause[] clauses = null;
        if (header != null) {
            if (header.length() == 0) throw new IllegalArgumentException("The header cannot be an empty string.");
            String[] ss = parseDelimitedString(header, ",");
            clauses = parseClauses(ss);
        }
        return (clauses == null) ? new Clause[0] : clauses;
    }

    public static Clause[] parseClauses(String[] ss) throws IllegalArgumentException {
        if (ss == null) return null;
        List<Clause> completeList = new LinkedList<Clause>();
        for (int ssIdx = 0; ssIdx < ss.length; ssIdx++) {
            String[] pieces = parseDelimitedString(ss[ssIdx], ";");
            int pathCount = 0;
            for (int pieceIdx = 0; pieceIdx < pieces.length; pieceIdx++) {
                if (pieces[pieceIdx].indexOf('=') >= 0) break;
                pathCount++;
            }
            if (pathCount == 0) throw new IllegalArgumentException("No path specified on clause: " + ss[ssIdx]);
            Entry<String, String>[] dirs = new Entry[pieces.length - pathCount];
            Entry<String, String>[] attrs = new Entry[pieces.length - pathCount];
            int dirCount = 0, attrCount = 0;
            int idx = -1;
            String sep = null;
            for (int pieceIdx = pathCount; pieceIdx < pieces.length; pieceIdx++) {
                if ((idx = pieces[pieceIdx].indexOf(":=")) >= 0) sep = ":=";
                else if ((idx = pieces[pieceIdx].indexOf("=")) >= 0) sep = "=";
                else throw new IllegalArgumentException("Not a directive/attribute: " + ss[ssIdx]);
                String key = pieces[pieceIdx].substring(0, idx).trim();
                String value = pieces[pieceIdx].substring(idx + sep.length()).trim();
                if (value.startsWith("\"") && value.endsWith("\"")) value = value.substring(1, value.length() - 1);
                if (sep.equals(":=")) dirs[dirCount++] = new SimpleImmutableEntry<String, String>(key, value);
                else attrs[attrCount++] = new SimpleImmutableEntry<String, String>(key, value);
            }
            Entry<String, String>[] dirsFinal = new Entry[dirCount];
            System.arraycopy(dirs, 0, dirsFinal, 0, dirCount);
            Entry<String, String>[] attrsFinal = new Entry[attrCount];
            System.arraycopy(attrs, 0, attrsFinal, 0, attrCount);
            for (int pkgIdx = 0; pkgIdx < pathCount; pkgIdx++) completeList.add(new Clause(pieces[pkgIdx], dirsFinal, attrsFinal));
        }
        return completeList.toArray(new Clause[completeList.size()]);
    }

    public static String[] parseDelimitedString(String value, String delim) {
        if (value == null) value = "";
        List<String> list = new LinkedList<String>();
        int CHAR = 1;
        int DELIMITER = 2;
        int STARTQUOTE = 4;
        int ENDQUOTE = 8;
        StringBuffer sb = new StringBuffer();
        int expecting = (CHAR | DELIMITER | STARTQUOTE);
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            boolean isDelimiter = (delim.indexOf(c) >= 0);
            boolean isQuote = (c == '"');
            if (isDelimiter && ((expecting & DELIMITER) > 0)) {
                list.add(sb.toString().trim());
                sb.delete(0, sb.length());
                expecting = (CHAR | DELIMITER | STARTQUOTE);
            } else if (isQuote && ((expecting & STARTQUOTE) > 0)) {
                sb.append(c);
                expecting = CHAR | ENDQUOTE;
            } else if (isQuote && ((expecting & ENDQUOTE) > 0)) {
                sb.append(c);
                expecting = (CHAR | STARTQUOTE | DELIMITER);
            } else if ((expecting & CHAR) > 0) sb.append(c);
            else throw new IllegalArgumentException("Invalid delimited string: " + value);
        }
        String s = sb.toString().trim();
        if (s.length() > 0) list.add(s);
        return (String[]) list.toArray(new String[list.size()]);
    }
}
