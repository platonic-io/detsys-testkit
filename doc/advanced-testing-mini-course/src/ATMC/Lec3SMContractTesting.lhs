> module ATMC.Lec3SMContractTesting where

Consumer-driven contract testing using state machines
=====================================================

Motivation
----------

  - Components rarely exist in isolation, they almost always depend on some
    other component;

  - When we test we often want to test as if the component existed in isolation,
    e.g. if component A depends on component B, we'd like to test B first and
    then *assume* that B is working when testing A;

  - Assumptions like these can be justified using so called *contract tests*.
