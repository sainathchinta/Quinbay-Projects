package com.gdn.partners.pbp.model.vo;

import java.util.Map;

import jakarta.persistence.criteria.Predicate;

public class JpaPredicateResult {
  Predicate predicate;
  Map<String, Object> predicateParametersMap;

  public Predicate getPredicate() {
    return predicate;
  }

  public void setPredicate(Predicate predicate) {
    this.predicate = predicate;
  }

  public Map<String, Object> getPredicateParametersMap() {
    return predicateParametersMap;
  }

  public void setPredicateParametersMap(Map<String, Object> predicateParametersMap) {
    this.predicateParametersMap = predicateParametersMap;
  }


}
