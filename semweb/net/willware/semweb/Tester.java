/* -*- mode: java; c-basic-offset: 4; indent-tabs-mode: nil -*- */

package net.willware.semweb;

/**
 * This is just a way to conveniently package a test that runs on
 * any random URI.
 * @see GenealogyModel#crawlSeeAlsos(int, Tester)
 */
public interface Tester {
    boolean test(String uri);
}
