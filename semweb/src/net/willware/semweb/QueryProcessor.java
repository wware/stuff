/* -*- mode: java; c-basic-offset: 4; indent-tabs-mode: nil -*- */

package net.willware.semweb;

import com.hp.hpl.jena.sparql.core.ResultBinding;

/**
 * A QueryProcessor specifies what should be done when we find a
 * result binding satisfying a given query.
 * @see GenealogyModel#query(String, QueryProcessor)
 */
public interface QueryProcessor {
    void process(ResultBinding binding);
}
