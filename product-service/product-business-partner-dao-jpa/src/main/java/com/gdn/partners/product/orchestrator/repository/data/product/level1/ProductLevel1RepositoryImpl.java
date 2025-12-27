package com.gdn.partners.product.orchestrator.repository.data.product.level1;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.util.CollectionUtils;

import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.partners.product.orchestrator.constant.ProductLevel1State;
import com.gdn.partners.product.orchestrator.entity.product.level1.ProductLevel1;
import com.gdn.partners.product.orchestrator.model.product.level1.ProductLevel1Filter;

public class ProductLevel1RepositoryImpl implements ProductLevel1CustomRepository {
  
  private static final String PERCENT = "%";

  @Autowired
  private EntityManager entityManager;

  @Override
  public Page<ProductLevel1> findByStoreIdAndFilterAndMarkForDeleteFalse(String storeId,
      ProductLevel1Filter filter, Pageable pageable) {
    CriteriaBuilder builder = this.entityManager.getCriteriaBuilder();
    CriteriaQuery<ProductLevel1> criteria = builder.createQuery(ProductLevel1.class);
    CriteriaQuery<Long> count = builder.createQuery(Long.class);
    Root<ProductLevel1> root = criteria.from(ProductLevel1.class);
    Root<ProductLevel1> countRoot = count.from(ProductLevel1.class);
    List<Predicate> predicates = getPredicateList(storeId, filter, builder, root);
    List<Predicate> countPredicates = getPredicateList(storeId, filter, builder, countRoot);
    criteria.where(builder.and(predicates.toArray(new Predicate[predicates.size()])));
    if (!StringUtils.isEmpty(filter.getSortedBy()) && !Objects.isNull(filter.getSortDirection())) {
      if (Sort.Direction.ASC.equals(filter.getSortDirection())) {
        criteria.orderBy(builder.asc(root.get(filter.getSortedBy())));
      } else {
        criteria.orderBy(builder.desc(root.get(filter.getSortedBy())));
      }
    }
    count.select(builder.count(countRoot))
        .where(builder.and(countPredicates.toArray(new Predicate[countPredicates.size()])));
    List<ProductLevel1> productLevel1s =
        this.entityManager.createQuery(criteria).setFirstResult((int) pageable.getOffset())
            .setMaxResults(pageable.getPageSize()).getResultList();
    long totalElements = this.entityManager.createQuery(count).getSingleResult();
    return new PageImpl<>(productLevel1s, pageable, totalElements);
  }

  private List<Predicate> getPredicateList(String storeId, ProductLevel1Filter filter, CriteriaBuilder builder,
      Root<ProductLevel1> root) {
    List<Predicate> predicates = new ArrayList<>();
    predicates.add(builder.equal(root.get(ProductLevel1.FIELD_STORE_ID), storeId));
    predicates.add(builder.equal(root.get(ProductLevel1.FIELD_MARK_FOR_DELETE), false));
    if (!StringUtils.isEmpty(filter.getStoreCode())) {
      predicates
          .add(builder.equal(root.get(ProductLevel1.FIELD_STORE_CODE), filter.getStoreCode()));
    }
    if (!StringUtils.isEmpty(filter.getStoreName())) {
      predicates.add(buildContainingIgnoreCasePredicate(builder, root,
          ProductLevel1.FIELD_STORE_NAME, filter.getStoreName()));
    }
    if (!CollectionUtils.isEmpty(filter.getProductIds())) {
      predicates.add(root.get(ProductLevel1.FIELD_PRODUCT_ID).in(filter.getProductIds()));
    }
    if (!StringUtils.isEmpty(filter.getCode())) {
      predicates.add(builder.equal(root.get(ProductLevel1.FIELD_CODE), filter.getCode()));
    }
    if (!StringUtils.isEmpty(filter.getName())) {
      predicates.add(buildContainingIgnoreCasePredicate(builder, root, ProductLevel1.FIELD_NAME,
          filter.getName()));
    }
    if (!StringUtils.isEmpty(filter.getBrandCode())) {
      predicates
          .add(builder.equal(root.get(ProductLevel1.FIELD_BRAND_CODE), filter.getBrandCode()));
    }
    if (!StringUtils.isEmpty(filter.getBrandName())) {
      predicates.add(buildContainingIgnoreCasePredicate(builder, root,
          ProductLevel1.FIELD_BRAND_NAME, filter.getBrandName()));
    }
    if (!StringUtils.isEmpty(filter.getCategoryCode())) {
      predicates.add(
          builder.equal(root.get(ProductLevel1.FIELD_CATEGORY_CODE), filter.getCategoryCode()));
    }
    if (!StringUtils.isEmpty(filter.getCategoryName())) {
      predicates.add(buildContainingIgnoreCasePredicate(builder, root,
          ProductLevel1.FIELD_CATEGORY_NAME, filter.getCategoryName()));
    }
    if (!StringUtils.isEmpty(filter.getCreatedBy())) {
      predicates.add(buildContainingIgnoreCasePredicate(builder, root,
          ProductLevel1.FIELD_CREATED_BY, filter.getCreatedBy()));
    }
    addStatesFilter(filter, builder, root, predicates);
    return predicates;
  }

  private static void addStatesFilter(ProductLevel1Filter filter, CriteriaBuilder builder, Root<ProductLevel1> root,
      List<Predicate> predicates) {
    if (!CollectionUtils.isEmpty(filter.getStates())) {
      List<Predicate> statePredicates = new ArrayList<>();
      statePredicates.add(root.get(ProductLevel1.FIELD_STATE)
          .in(filter.getStates().stream().map(e -> e.replace("-", "_"))
              .collect(Collectors.toSet())));
      if (filter.getStates().contains(ProductLevel1State.ACTIVE)) {
        statePredicates.add(builder.and(builder.isTrue(root.get(ProductLevel1.FIELD_ACTIVATED)),
            builder.isTrue(root.get(ProductLevel1.FIELD_VIEWABLE)),
            builder.isNull(root.get(ProductLevel1.FIELD_STATE))));
      }
      if (filter.getStates().contains(ProductLevel1State.DRAFT) || filter.getStates()
          .contains(ProductLevel1State.NEED_CORRECTION)) {
        statePredicates.add(builder.and(builder.isFalse(root.get(ProductLevel1.FIELD_ACTIVATED)),
            builder.isFalse(root.get(ProductLevel1.FIELD_VIEWABLE)),
            builder.isNull(root.get(ProductLevel1.FIELD_STATE))));
      }
      if (filter.getStates().contains(ProductLevel1State.IN_PROGRESS)) {
        statePredicates.add(builder.and(builder.isTrue(root.get(ProductLevel1.FIELD_ACTIVATED)),
            builder.isFalse(root.get(ProductLevel1.FIELD_VIEWABLE)),
            builder.isNull(root.get(ProductLevel1.FIELD_STATE))));
      }
      predicates.add(builder.or(statePredicates.toArray(new Predicate[statePredicates.size()])));
    }
  }

  private Predicate buildContainingIgnoreCasePredicate(CriteriaBuilder builder,
      Root<ProductLevel1> root, String fieldName, String value) {
    return builder.like(builder.lower(root.get(fieldName)),
        PERCENT + value.toLowerCase() + PERCENT);
  }
}
