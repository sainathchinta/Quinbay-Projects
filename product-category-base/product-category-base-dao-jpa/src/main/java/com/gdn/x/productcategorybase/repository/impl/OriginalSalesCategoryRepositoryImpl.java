package com.gdn.x.productcategorybase.repository.impl;

import java.util.List;
import java.util.Objects;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import org.apache.commons.lang.StringUtils;

import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.entity.OriginalSalesCategory;
import com.gdn.x.productcategorybase.repository.OriginalSalesCategoryRepositoryCustom;

public class OriginalSalesCategoryRepositoryImpl implements OriginalSalesCategoryRepositoryCustom {

  @PersistenceContext
  private EntityManager entityManager;

  @Override
  public List<OriginalSalesCategory> findByOscCodeAndNameAndActivated(String oscCode, String keyword,
      Boolean activated) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<OriginalSalesCategory> criteriaQuery = criteriaBuilder.createQuery(OriginalSalesCategory.class);
    Root<OriginalSalesCategory> root = criteriaQuery.from(OriginalSalesCategory.class);
    Predicate predicate = getPredicateForCategoryFilter(oscCode, keyword, activated, criteriaBuilder, root);
    CriteriaQuery<OriginalSalesCategory> resultCriteriaQuery = criteriaQuery.select(root).where(predicate);
    resultCriteriaQuery.orderBy(criteriaBuilder.asc(root.get(Constants.OSC_LONG_TEXT)));
    return entityManager.createQuery(resultCriteriaQuery).getResultList();
  }

  private Predicate getPredicateForCategoryFilter(String oscCode, String keyword, Boolean activated,
      CriteriaBuilder criteriaBuilder, Root<OriginalSalesCategory> root) {
    Predicate predicate = criteriaBuilder.equal(root.get(Constants.MARK_FOR_DELETE), false);
    if (StringUtils.isNotEmpty(oscCode)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal(root.get(Constants.OSC_CODE), oscCode));
    }
    if (StringUtils.isNotEmpty(keyword)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder
          .like(criteriaBuilder.lower(root.get(Constants.OSC_LONG_TEXT)),
              Constants.ANY_STRING + keyword.toLowerCase() + Constants.ANY_STRING));
    }
    if (Objects.nonNull(activated)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal(root.get(Constants.ACTIVATED), activated));
    }
    return predicate;
  }
}
