package com.gdn.x.productcategorybase.repository.impl;

import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.entity.MerchantConfiguration;
import com.gdn.x.productcategorybase.repository.MerchantConfigurationCustomRepository;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

public class MerchantConfigurationRepositoryImpl implements MerchantConfigurationCustomRepository {

  private static final String STORE_ID = "storeId";
  private static final String REVIEW_CONFIG = "reviewConfig";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String MERCHANT_NAME = "merchantName";
  private static final String ANY_STRING = "%";
  private static final String MARK_FOR_DELETE = "markForDelete";
  private static final String CREATED_DATE = "createdDate";
  private static final String ORDER_BY_DESC = "desc";


  @PersistenceContext
  private EntityManager entityManager;

  @Override
  public Page<MerchantConfiguration> findByReviewConfigAndCategoryNameAndKeywordMarkForDeleteFalseOrderByCreatedDate(
      String storeId, String reviewConfig, String categoryName, String searchKey, String sortOrder, Pageable pageable) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();

    CriteriaQuery<MerchantConfiguration> criteriaQuery = criteriaBuilder.createQuery(MerchantConfiguration.class);
    Root<MerchantConfiguration> merchantConfigurationRoot = criteriaQuery.from(MerchantConfiguration.class);
    Predicate predicate = getPredicateForFilterRequest(storeId, reviewConfig, categoryName, searchKey, criteriaBuilder,
        merchantConfigurationRoot);

    CriteriaQuery<Long> countCriteriaQuery = criteriaBuilder.createQuery(Long.class);
    Root<MerchantConfiguration> countRoot = countCriteriaQuery.from(MerchantConfiguration.class);
    Predicate countPredicate =
        getPredicateForFilterRequest(storeId, reviewConfig, categoryName, searchKey, criteriaBuilder, countRoot);

    CriteriaQuery<MerchantConfiguration> resultCriteriaQuery =
        criteriaQuery.select(merchantConfigurationRoot).where(predicate);
    countCriteriaQuery.select(criteriaBuilder.count(countRoot)).where(countPredicate);
    if (ORDER_BY_DESC.equalsIgnoreCase(sortOrder))
      resultCriteriaQuery.orderBy(criteriaBuilder.desc(merchantConfigurationRoot.get(CREATED_DATE)));
    else {
      resultCriteriaQuery.orderBy(criteriaBuilder.asc(merchantConfigurationRoot.get(CREATED_DATE)));
    }

    List<MerchantConfiguration> pagingContent =
        entityManager.createQuery(resultCriteriaQuery).setFirstResult(pageable.getPageNumber() * pageable.getPageSize())
            .setMaxResults(pageable.getPageSize()).getResultList();
    Long totalRecords = (Long) entityManager.createQuery(countCriteriaQuery).getSingleResult();

    return new PageImpl<>(pagingContent, pageable, totalRecords);
  }

  private Predicate getPredicateForFilterRequest(String storeId, String reviewConfig, String categoryName,
      String searchKey, CriteriaBuilder criteriaBuilder, Root<MerchantConfiguration> merchantConfigurationRoot) {
    Predicate predicate = criteriaBuilder.equal(merchantConfigurationRoot.get(STORE_ID), storeId);
    if (StringUtils.isNotEmpty(reviewConfig)) {
      predicate = criteriaBuilder
          .and(predicate, criteriaBuilder.equal(merchantConfigurationRoot.get(REVIEW_CONFIG), reviewConfig));
    }
    if(StringUtils.isNotEmpty(categoryName)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder
          .equal(criteriaBuilder.lower(merchantConfigurationRoot.get(CATEGORY_NAME)), categoryName.toLowerCase()));
    }
    if(StringUtils.isNotEmpty(searchKey)) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.or(criteriaBuilder
              .equal(criteriaBuilder.lower(merchantConfigurationRoot.get(MERCHANT_CODE)), searchKey.toLowerCase()),
          criteriaBuilder.like(criteriaBuilder.lower(merchantConfigurationRoot.get(MERCHANT_NAME)),
              ANY_STRING + searchKey.toLowerCase() + ANY_STRING)));
    }
    predicate =
        criteriaBuilder.and(predicate, criteriaBuilder.equal(merchantConfigurationRoot.get(MARK_FOR_DELETE), false));
    return predicate;
  }
}
