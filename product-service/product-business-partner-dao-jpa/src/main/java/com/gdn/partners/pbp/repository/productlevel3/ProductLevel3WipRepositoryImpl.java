package com.gdn.partners.pbp.repository.productlevel3;

import java.util.List;
import java.util.Objects;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.product.commons.constant.ProductLevel3WipSummaryCriteria;
import com.gdn.partners.pbp.commons.util.DatabaseFieldNames;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3WipSummaryRequest;
import com.gdn.partners.pbp.entity.productlevel3.ProductLevel3Wip;

public class ProductLevel3WipRepositoryImpl implements ProductLevel3CustomRepository {

  private static final String DRAFT = "DRAFT";
  private static final String IN_PROGRESS = "IN_PROGRESS";
  private static final String NEED_CORRECTION = "NEED_CORRECTION";
  private static final String NEED_ACTION = "FAILED";
  private static final String ACTIVE = "active";
  private static final String SUBMITTED_DATE = "submittedDate";
  private static final String EXPECTED_ACTIVATION_DATE = "expectedActivationDate";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String STATE = "state";
  private static final String ASC = "ASC";

  @PersistenceContext
  private EntityManager entityManager;

  @Override
  public Page<ProductLevel3Wip> findSummaryByFilterWithState(String storeId, ProductLevel3WipSummaryRequest request,
      Pageable pageable) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<ProductLevel3Wip> criteriaQuery = builder.createQuery(ProductLevel3Wip.class);
    Root<ProductLevel3Wip> productLevel3WipRoot = criteriaQuery.from(ProductLevel3Wip.class);
    Predicate predicate = getPredicate(storeId, request, builder, productLevel3WipRoot);
    CriteriaQuery<Long> countQuery = builder.createQuery(Long.class);
    Root<ProductLevel3Wip> countRoot = countQuery.from(ProductLevel3Wip.class);
    Predicate countPredicate = getPredicate(storeId, request, builder, countRoot);
    countQuery.select(builder.count(countRoot)).where(countPredicate);
    CriteriaQuery<ProductLevel3Wip> resultCriteriaQuery = criteriaQuery.select(productLevel3WipRoot).where(predicate);

    setFiltersForCriteriaQuery(resultCriteriaQuery, request, productLevel3WipRoot, builder);

    List<ProductLevel3Wip> pagingContent =
        entityManager.createQuery(resultCriteriaQuery).setFirstResult(pageable.getPageNumber() * pageable.getPageSize())
            .setMaxResults(pageable.getPageSize()).getResultList();
    Long totalRecords = entityManager.createQuery(countQuery).getSingleResult();
    return new PageImpl<>(pagingContent, pageable, totalRecords);
  }

  private void setFiltersForCriteriaQuery(CriteriaQuery<ProductLevel3Wip> resultCriteriaQuery,
      ProductLevel3WipSummaryRequest request, Root<ProductLevel3Wip> productLevel3WipRoot, CriteriaBuilder builder) {
    if (ASC.equalsIgnoreCase(request.getSortBy())) {
      if (ProductLevel3WipSummaryCriteria.NEED_CORRECTION.equals(request.getCriteria())) {
        resultCriteriaQuery.orderBy(builder.asc(productLevel3WipRoot.get(SUBMITTED_DATE)));
      } else if (Objects.nonNull(request.getOrderBy()) && request.getOrderBy().equals(SUBMITTED_DATE)) {
        resultCriteriaQuery.orderBy(builder.asc(productLevel3WipRoot.get(SUBMITTED_DATE)));
      } else if (Objects.nonNull(request.getOrderBy()) && request.getOrderBy().equals(EXPECTED_ACTIVATION_DATE)) {
        resultCriteriaQuery.orderBy(builder.asc(productLevel3WipRoot.get(EXPECTED_ACTIVATION_DATE)));
      }
    } else {
      if (ProductLevel3WipSummaryCriteria.NEED_CORRECTION.equals(request.getCriteria())) {
        resultCriteriaQuery.orderBy(builder.desc(productLevel3WipRoot.get(SUBMITTED_DATE)));
      } else if (Objects.nonNull(request.getOrderBy()) && request.getOrderBy().equals(SUBMITTED_DATE)) {
        resultCriteriaQuery.orderBy(builder.desc(productLevel3WipRoot.get(SUBMITTED_DATE)));
      } else if (Objects.nonNull(request.getOrderBy()) && request.getOrderBy().equals(EXPECTED_ACTIVATION_DATE)) {
        resultCriteriaQuery.orderBy(builder.desc(productLevel3WipRoot.get(EXPECTED_ACTIVATION_DATE)));
      }
    }
  }

  private Predicate getPredicate(String storeId, ProductLevel3WipSummaryRequest request, CriteriaBuilder builder,
      Root<ProductLevel3Wip> productLevel3Wip) {
    Predicate predicate = builder.equal(productLevel3Wip.get(DatabaseFieldNames.STORE_ID), storeId);
    predicate = builder.and(predicate, builder
        .equal(productLevel3Wip.get(DatabaseFieldNames.BUSINESS_PARTNER_CODE), request.getBusinessPartnerCode()));
    if (Objects.nonNull(request.getProductName())) {
      predicate = builder.and(predicate, builder
          .like(builder.lower(productLevel3Wip.get(DatabaseFieldNames.PRODUCT_NAME)),
              DatabaseFieldNames.ANY_STRING + request.getProductName().toLowerCase().trim() + DatabaseFieldNames.ANY_STRING));
    }
    if (CollectionUtils.isNotEmpty(request.getCategoryCodes())) {
      predicate = builder.and(predicate, productLevel3Wip.get(CATEGORY_CODE).in(request.getCategoryCodes()));
    }

    if (ProductLevel3WipSummaryCriteria.IN_PROGRESS.equals(request.getCriteria())) {
      predicate = builder.and(predicate, builder.or((builder.equal(productLevel3Wip.get(STATE), DRAFT)),
          builder.equal(productLevel3Wip.get(STATE), IN_PROGRESS), builder.isNull(productLevel3Wip.get(STATE))));
      predicate = builder.and(predicate, builder.equal(productLevel3Wip.get(ACTIVE), false));
    } else if (ProductLevel3WipSummaryCriteria.NEED_CORRECTION.equals(request.getCriteria())) {
      predicate = builder.and(predicate, builder.equal(productLevel3Wip.get(STATE), NEED_CORRECTION));
    } else if (ProductLevel3WipSummaryCriteria.FAILED.equals(request.getCriteria())) {
      predicate = builder.and(predicate, builder.equal(productLevel3Wip.get(ACTIVE), true));
    }
    predicate = builder.and(predicate, builder.equal(productLevel3Wip.get(DatabaseFieldNames.MARK_FOR_DELETE), false));
    return predicate;
  }
}
