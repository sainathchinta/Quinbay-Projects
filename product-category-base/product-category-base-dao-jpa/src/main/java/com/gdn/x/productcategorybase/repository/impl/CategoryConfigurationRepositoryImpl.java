package com.gdn.x.productcategorybase.repository.impl;

import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryConfiguration;
import com.gdn.x.productcategorybase.repository.CategoryConfigurationCustomRepository;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class CategoryConfigurationRepositoryImpl implements CategoryConfigurationCustomRepository {

  private static final String STORE_ID = "storeId";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATEGORY_NAME = "name";
  private static final String REVIEW_CONFIG = "reviewConfig";
  private static final String MARK_FOR_DELETE = "markForDelete";
  private static final String CATEGORY = "category";
  private static final String ANY_STRING = "%";
  private static final String ORDER_BY_DESC = "desc";
  private static final String CREATED_DATE = "createdDate";

  @PersistenceContext
  private EntityManager entityManager;


  @Override
  public Page<CategoryConfiguration> findByReviewConfigAndCategoryAndMarkForDeleteFalseOrderByCreatedDate(
      String storeId, String reviewConfig, String categoryCode, String searchKey, String sortOrder, Pageable pageable) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();

    CriteriaQuery<CategoryConfiguration> criteriaQuery = criteriaBuilder.createQuery(CategoryConfiguration.class);
    Root<CategoryConfiguration> categoryConfigurationRoot = criteriaQuery.from(CategoryConfiguration.class);
    Predicate predicate =
        getPredicateForCategoryConfigurationListing(storeId, reviewConfig, categoryCode, searchKey,
            criteriaBuilder, categoryConfigurationRoot);

    CriteriaQuery<Long> countCriteriaQuery = criteriaBuilder.createQuery(Long.class);
    Root<CategoryConfiguration> countRoot = countCriteriaQuery.from(CategoryConfiguration.class);
    Predicate countPredicate =
        getPredicateForCategoryConfigurationListing(storeId, reviewConfig, categoryCode, searchKey,
            criteriaBuilder, countRoot);

    if(Objects.isNull(predicate)) {
      return new PageImpl<>(new ArrayList<>(), pageable, 0);
    }

    CriteriaQuery<CategoryConfiguration> resultCriteriaQuery =
        criteriaQuery.select(categoryConfigurationRoot).where(predicate);
    countCriteriaQuery.select(criteriaBuilder.count(countRoot)).where(countPredicate);

    if (ORDER_BY_DESC.equalsIgnoreCase(sortOrder))
      resultCriteriaQuery.orderBy(criteriaBuilder.desc(categoryConfigurationRoot.get(CREATED_DATE)));
    else {
      resultCriteriaQuery.orderBy(criteriaBuilder.asc(categoryConfigurationRoot.get(CREATED_DATE)));
    }

    List<CategoryConfiguration> pagingContent =
        entityManager.createQuery(resultCriteriaQuery).setFirstResult(pageable.getPageNumber() * pageable.getPageSize())
            .setMaxResults(pageable.getPageSize()).getResultList();
    Long totalRecords = (Long) entityManager.createQuery(countCriteriaQuery).getSingleResult();

    return new PageImpl<>(pagingContent, pageable, totalRecords);
  }

  private Predicate getPredicateForCategoryConfigurationListing(String storeId, String reviewConfig,
      String categoryCode, String searchKey, CriteriaBuilder criteriaBuilder,
      Root<CategoryConfiguration> categoryConfigurationRoot) {
    Predicate predicate = criteriaBuilder.equal(categoryConfigurationRoot.get(STORE_ID), storeId);
    if(StringUtils.isNotEmpty(reviewConfig)) {
      predicate = criteriaBuilder
          .and(predicate, criteriaBuilder.equal(categoryConfigurationRoot.get(REVIEW_CONFIG), reviewConfig));
    }
    predicate =
        criteriaBuilder.and(predicate, criteriaBuilder.equal(categoryConfigurationRoot.get(MARK_FOR_DELETE), false));
    if(StringUtils.isNotEmpty(categoryCode)) {
      CriteriaBuilder categoryCriteriaBuilder = entityManager.getCriteriaBuilder();
      CriteriaQuery<Category> categoryCriteriaQuery= categoryCriteriaBuilder.createQuery(Category.class);
      Root<Category> category = categoryCriteriaQuery.from(Category.class);
      categoryCriteriaQuery.where(categoryCriteriaBuilder
          .and(categoryCriteriaBuilder.equal(category.get(CATEGORY_CODE), categoryCode),
              categoryCriteriaBuilder.equal(category.get(MARK_FOR_DELETE), false)));
      TypedQuery<Category> categoryTypedQuery = entityManager.createQuery(categoryCriteriaQuery);
      if(CollectionUtils.isNotEmpty(categoryTypedQuery.getResultList())) {
        predicate = criteriaBuilder
            .and(predicate, categoryConfigurationRoot.get(CATEGORY).in(categoryTypedQuery.getResultList()));
      } else {
        return null;
      }
    }
    if(StringUtils.isNotEmpty(searchKey)) {
      CriteriaBuilder categoryCriteriaBuilder = entityManager.getCriteriaBuilder();
      CriteriaQuery<Category> categoryCriteriaQuery= categoryCriteriaBuilder.createQuery(Category.class);
      Root<Category> category = categoryCriteriaQuery.from(Category.class);
      categoryCriteriaQuery.where(categoryCriteriaBuilder.and((categoryCriteriaBuilder.or(categoryCriteriaBuilder
              .equal(categoryCriteriaBuilder.lower(category.get(CATEGORY_CODE)), searchKey.toLowerCase()),
          categoryCriteriaBuilder.like(criteriaBuilder.lower(category.get(CATEGORY_NAME)),
              ANY_STRING + searchKey.toLowerCase() + ANY_STRING))),
          categoryCriteriaBuilder.equal(category.get(MARK_FOR_DELETE), false)));
      TypedQuery<Category> categoryTypedQuery = entityManager.createQuery(categoryCriteriaQuery);
      if(CollectionUtils.isNotEmpty(categoryTypedQuery.getResultList())) {
        predicate = criteriaBuilder
            .and(predicate, categoryConfigurationRoot.get(CATEGORY).in(categoryTypedQuery.getResultList()));
      } else {
        return null;
      }
    }
    return predicate;
  }
}
