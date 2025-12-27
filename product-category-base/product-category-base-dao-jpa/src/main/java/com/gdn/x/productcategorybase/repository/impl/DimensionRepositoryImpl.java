package com.gdn.x.productcategorybase.repository.impl;

import com.gdn.x.productcategorybase.entity.Dimension;
import com.gdn.x.productcategorybase.repository.DimensionRepositoryCustom;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import java.util.List;

public class DimensionRepositoryImpl implements DimensionRepositoryCustom {
  private static final String STORE_ID = "storeId";
  private static final String NAME = "name";
  private static final String CODE = "dimensionCode";
  private static final String DESCRIPTION_SEARCH = "descriptionSearch";
  private static final String MARK_FOR_DELETE = "markForDelete";
  private static final String ANY_STRING = "%";
  private static final String SORT_DIRECTION_DESCENDING = "desc";

  @PersistenceContext
  private EntityManager entityManager;

  @Override
  public Page<Dimension> findByStoreIdAndKeywordAndMarkForDeleteFalseOrderByName(
      String storeId, String keyword, String sortByFieldName,
      String sortOrder, Pageable pageable) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<Dimension> criteriaQuery = builder.createQuery(Dimension.class);
    Root<Dimension> dimension = criteriaQuery.from(Dimension.class);
    Predicate predicate = getPredicate(storeId, keyword, builder, dimension);

    CriteriaQuery<Long> countCriteriaQuery = builder.createQuery(Long.class);
    Root<Dimension> countRoot = countCriteriaQuery.from(Dimension.class);
    Predicate countPredicate = getPredicate(storeId, keyword, builder, countRoot);

    CriteriaQuery<Dimension> resultCriteriaQuery = criteriaQuery.select(dimension).where(predicate);
    countCriteriaQuery.select(builder.count(countRoot)).where(countPredicate);
    sortByFieldName = StringUtils.isNotBlank(sortByFieldName) ? sortByFieldName : NAME;
    if (SORT_DIRECTION_DESCENDING.equalsIgnoreCase(sortOrder)) {
      resultCriteriaQuery.orderBy(builder.desc(dimension.get(sortByFieldName)));
    } else {
      resultCriteriaQuery.orderBy(builder.asc(dimension.get(sortByFieldName)));
    }

    List<Dimension> pagingContent = entityManager.createQuery(resultCriteriaQuery)
        .setFirstResult(pageable.getPageNumber() * pageable.getPageSize())
        .setMaxResults(pageable.getPageSize()).getResultList();
    Long totalRecords = entityManager.createQuery(countCriteriaQuery).getSingleResult();

    return new PageImpl<>(pagingContent, pageable, totalRecords);
  }

  /**
   * Create predicate using CriteriaBuilder
   *
   * @param storeId
   * @param name
   * @param builder
   * @param dimension
   * @return
   */
  private Predicate getPredicate(String storeId, String name,
      CriteriaBuilder builder, Root<Dimension> dimension) {
    Predicate predicate = builder.equal(dimension.get(STORE_ID), storeId);
    if (StringUtils.isNotBlank(name)) {
      predicate = builder.and(predicate, builder.or(builder.like(builder.lower(dimension.get(NAME)),
              ANY_STRING + name.toLowerCase() + ANY_STRING),
          builder.like(builder.lower(dimension.get(CODE)),
              ANY_STRING + name.toLowerCase() + ANY_STRING),
          builder.like(builder.lower(dimension.get(DESCRIPTION_SEARCH)),
              ANY_STRING + name.toLowerCase() + ANY_STRING)));
    }
    predicate = builder.and(predicate, builder.equal(dimension.get(MARK_FOR_DELETE), false));
    return predicate;
  }
}
