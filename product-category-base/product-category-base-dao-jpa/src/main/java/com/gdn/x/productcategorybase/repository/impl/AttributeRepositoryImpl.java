package com.gdn.x.productcategorybase.repository.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import org.apache.commons.lang.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisation;
import com.gdn.x.productcategorybase.repository.AttributeRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

/**
 * Created by govind on 06/11/2018 AD.
 */
public class AttributeRepositoryImpl implements AttributeRepositoryCustom {

  private static final String STORE_ID = "storeId";
  private static final String NAME = "name";
  private static final String CODE = "attributeCode";
  private static final String DESCRIPTION_SEARCH = "descriptionSearch";
  private static final String MARK_FOR_DELETE = "markForDelete";
  private static final String ATTRIBUTE_TYPE = "attributeType";
  private static final String SIZE_ATTRIBUTE = "sizeAttribute";
  private static final String ANY_STRING = "%";

  @PersistenceContext
  private EntityManager entityManager;

  @SuppressWarnings("unchecked")
  @Override
  public Page<Attribute> findByStoreIdAndAttributeTypeAndNameAndSizeAttributeAndMarkForDeleteFalseOrderByName(
      String storeId, AttributeType attributeType, String name, String sortByFieldName,
      String sortOrder, Pageable pageable, Boolean sizeAttribute) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();

    CriteriaQuery<Attribute> criteriaQuery = builder.createQuery(Attribute.class);
    Root<Attribute> attributeRoot = criteriaQuery.from(Attribute.class);
    Predicate predicate = getPredicate(storeId, attributeType, name, builder, attributeRoot, sizeAttribute);

    CriteriaQuery<Long> countCriteriaQuery = builder.createQuery(Long.class);
    Root<Attribute> countRoot = countCriteriaQuery.from(Attribute.class);
    Predicate countPredicate = getPredicate(storeId, attributeType, name, builder, countRoot, sizeAttribute);

    if (predicate == null) {
      return new PageImpl<>(new ArrayList<>(), pageable, 0);
    }

    criteriaQuery.select(attributeRoot).where(predicate);
    countCriteriaQuery.select(builder.count(countRoot)).where(countPredicate);

    sortByFieldName = StringUtils.defaultIfBlank(sortByFieldName, NAME);
    if ("desc".equalsIgnoreCase(sortOrder)) {
      criteriaQuery.orderBy(builder.desc(attributeRoot.get(sortByFieldName)));
    } else {
      criteriaQuery.orderBy(builder.asc(attributeRoot.get(sortByFieldName)));
    }

    List<Attribute> pagingContent = entityManager.createQuery(criteriaQuery)
        .setFirstResult((int) pageable.getOffset())
        .setMaxResults(pageable.getPageSize())
        .getResultList();
    Long totalRecords = entityManager.createQuery(countCriteriaQuery).getSingleResult();

    return new PageImpl<>(pagingContent, pageable, totalRecords);
  }

  /**
   * Create predicate using CriteriaBuilder
   *
   * @param storeId
   * @param attributeType
   * @param name
   * @param builder
   * @param attribute
   * @param sizeAttribute
   * @return
   */
  private Predicate getPredicate(String storeId, AttributeType attributeType, String name,
      CriteriaBuilder builder, Root<Attribute> attribute, Boolean sizeAttribute) {
    Predicate predicate = builder.equal(attribute.get(STORE_ID), storeId);
    if (StringUtils.isNotBlank(name)) {
      predicate = builder.and(predicate, builder.or(
          builder.like(builder.lower(attribute.get(NAME)), ANY_STRING + name.toLowerCase() + ANY_STRING),
          builder.like(builder.lower(attribute.get(CODE)), ANY_STRING + name.toLowerCase() + ANY_STRING),
          builder.like(builder.lower(attribute.get(DESCRIPTION_SEARCH)), ANY_STRING + name.toLowerCase() + ANY_STRING)));
    }
    predicate = builder.and(predicate, builder.equal(attribute.get(MARK_FOR_DELETE), false));
    if (!AttributeType.ALL.equals(attributeType)) {
      predicate =
          builder.and(predicate, builder.equal(attribute.get(ATTRIBUTE_TYPE), attributeType));
    }
    if (Objects.nonNull(sizeAttribute)) {
      predicate =
        builder.and(predicate, builder.equal(attribute.get(SIZE_ATTRIBUTE), sizeAttribute));
    }
    return predicate;
  }
}
