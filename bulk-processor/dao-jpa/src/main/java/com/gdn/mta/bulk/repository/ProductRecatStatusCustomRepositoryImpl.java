package com.gdn.mta.bulk.repository;

import java.util.List;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;

import com.gdn.mta.bulk.entity.ProductRecatStatus;
import com.gdn.partners.bulk.util.RecatConstants;

@Component
public class ProductRecatStatusCustomRepositoryImpl implements ProductRecatStatusCustomRepository {

  @PersistenceContext
  private EntityManager entityManager;

  @Override
  public Page<ProductRecatStatus> findProductRecatStatusByStatusFilterAndKeyword(String storeId, String requestCode,
      String status, String keyword, Pageable pageable) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<ProductRecatStatus> criteriaQuery = builder.createQuery(ProductRecatStatus.class);
    CriteriaQuery<Long> countCriteriaQuery = builder.createQuery(Long.class);
    Root<ProductRecatStatus> productRecatStatusRoot = criteriaQuery.from(ProductRecatStatus.class);
    Root<ProductRecatStatus> countRoot = countCriteriaQuery.from(ProductRecatStatus.class);

    Predicate predicate = getStatusFilterQuery(builder, productRecatStatusRoot, storeId, requestCode, status);
    predicate = getKeywordFilterQuery(builder, productRecatStatusRoot, predicate, keyword);

    Predicate countPredicate = getStatusFilterQuery(builder, countRoot, storeId, requestCode, status);
    countPredicate = getKeywordFilterQuery(builder, countRoot, countPredicate, keyword);

    CriteriaQuery<ProductRecatStatus> resultCriteriaQuery =
        criteriaQuery.select(productRecatStatusRoot).where(predicate);
    countCriteriaQuery.select(builder.count(countRoot)).where(countPredicate);

    resultCriteriaQuery.orderBy(builder.desc(productRecatStatusRoot.get(RecatConstants.FIELD_UPDATED_DATE)));

    List<ProductRecatStatus> pagingContent =
        entityManager.createQuery(resultCriteriaQuery).setFirstResult(pageable.getPageNumber() * pageable.getPageSize())
            .setMaxResults(pageable.getPageSize()).getResultList();
    Long totalRecords = (Long) entityManager.createQuery(countCriteriaQuery).getSingleResult();
    return new PageImpl<>(pagingContent, pageable, totalRecords);
  }

  private Predicate getStatusFilterQuery(CriteriaBuilder builder, Root<ProductRecatStatus> productRecatStatusRoot,
      String storeId, String recatRequestCode, String status) {
    Predicate predicate = builder.equal(productRecatStatusRoot.get(RecatConstants.FIELD_STORE_ID), storeId);
    predicate = builder.and(predicate,
        builder.equal(productRecatStatusRoot.get(RecatConstants.FIELD_RECAT_REQUEST_CODE), recatRequestCode));
    if (StringUtils.isNotBlank(status) && !RecatConstants.ALL_STATUS.equals(status)) {
      if (RecatConstants.PENDING.equals(status)) {
        predicate = builder.and(predicate, builder
            .or(builder.equal(productRecatStatusRoot.get(RecatConstants.FIELD_STATUS), RecatConstants.PENDING),
                builder.equal(productRecatStatusRoot.get(RecatConstants.FIELD_STATUS), RecatConstants.PUBLISHED)));
      } else {
        predicate =
            builder.and(predicate, builder.equal(productRecatStatusRoot.get(RecatConstants.FIELD_STATUS), status));
      }
    }
    return predicate;
  }

  private Predicate getKeywordFilterQuery(CriteriaBuilder builder, Root<ProductRecatStatus> productRecatStatusRoot,
      Predicate predicate, String keyword) {
    if (StringUtils.isNotBlank(keyword)) {
      predicate = builder.and(predicate, builder.or(builder
              .equal(builder.lower(productRecatStatusRoot.get(RecatConstants.FIELD_PRODUCT_NAME)), keyword.toLowerCase()),
          builder.equal(builder.lower(productRecatStatusRoot.get(RecatConstants.FIELD_PRODUCT_CODE)),
              keyword.toLowerCase()), builder
              .equal(builder.lower(productRecatStatusRoot.get(RecatConstants.FIELD_CATEGORY_CODE)),
                  keyword.toLowerCase()), builder
              .equal(builder.lower(productRecatStatusRoot.get(RecatConstants.FIELD_NEW_CATEGORY_CODE)),
                  keyword.toLowerCase())));
    }
    return predicate;
  }
}
