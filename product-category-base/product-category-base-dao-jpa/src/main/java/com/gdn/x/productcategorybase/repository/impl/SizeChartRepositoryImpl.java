package com.gdn.x.productcategorybase.repository.impl;

import com.gdn.x.productcategorybase.dto.SizeChartFilterRequestDTO;
import com.gdn.x.productcategorybase.entity.SizeChart;
import com.gdn.x.productcategorybase.repository.SizeChartRepositoryCustom;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
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
import java.util.Objects;

public class SizeChartRepositoryImpl implements SizeChartRepositoryCustom {

  private static final String STORE_ID = "storeId";
  private static final String SIZE_ATTRIBUTE_CODE = "sizeAttributeCode";
  public static final String BRAND_CODE = "brandCode";
  public static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  public static final String WAITING_DELETION = "waitingDeletion";
  public static final String SIZE_CHART_NAME = "name";
  public static final String CREATED_DATE = "createdDate";
  private static final String ANY_STRING = "%";
  private static final String MARK_FOR_DELETE = "markForDelete";
  private static final String SORT_DIRECTION_DESCENDING = "desc";
  private static final String BUSINESS_PARTNER_CODE_FOR_INTERNAL = "INTERNAL";

  @PersistenceContext
  private EntityManager entityManager;

  @Override
  public Page<SizeChart> findByStoreIdAndFilterAppliedAndBusinessPartnerCodeAndMarkForDeleteFalse(
      String storeId, SizeChartFilterRequestDTO filter, Pageable pageable,
      boolean sortExternalSizeChartsByCreatedDateDescending) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();

    CriteriaQuery<SizeChart> criteriaQuery = builder.createQuery(SizeChart.class);
    Root<SizeChart> sizeChart = criteriaQuery.from(SizeChart.class);
    Predicate predicate = getPredicate(storeId, filter, builder, sizeChart);

    CriteriaQuery<Long> countCriteriaQuery = builder.createQuery(Long.class);
    Root<SizeChart> countRoot = countCriteriaQuery.from(SizeChart.class);
    Predicate countPredicate = getPredicate(storeId, filter, builder, countRoot);

    CriteriaQuery<SizeChart> resultCriteriaQuery = criteriaQuery.select(sizeChart).where(predicate);
    countCriteriaQuery.select(builder.count(countRoot))
        .where(countPredicate);

    filter.setSortByFieldName(StringUtils.isNotBlank(filter.getSortByFieldName()) ?
        filter.getSortByFieldName() :
        SIZE_CHART_NAME);

    /**
     * Here the logic is to prioritize the seller created size charts over the internal in the size
     * chart suggestion drop down during the creation from seller side
     */

      if (SORT_DIRECTION_DESCENDING.equalsIgnoreCase(filter.getSortOrder())) {
        resultCriteriaQuery.orderBy(builder.desc(sizeChart.get(filter.getSortByFieldName())));
      } else {
        if (CollectionUtils.size(filter.getBusinessPartnerCodes()) > 1
            && filter.getBusinessPartnerCodes().contains(BUSINESS_PARTNER_CODE_FOR_INTERNAL)) {
          setOrderByForExternalSizeCharts(builder, sizeChart, filter, resultCriteriaQuery,
              sortExternalSizeChartsByCreatedDateDescending);
        } else {
          resultCriteriaQuery.orderBy(builder.asc(sizeChart.get(filter.getSortByFieldName())));
        }
      }

    List<SizeChart> pagingContent = entityManager.createQuery(resultCriteriaQuery)
        .setFirstResult(pageable.getPageNumber() * pageable.getPageSize())
        .setMaxResults(pageable.getPageSize()).getResultList();
    Long totalRecords = entityManager.createQuery(countCriteriaQuery).getSingleResult();

    return new PageImpl<>(pagingContent, pageable, totalRecords);
  }

  private Predicate getPredicate(String storeId, SizeChartFilterRequestDTO filterRequestDTO,
      CriteriaBuilder builder, Root<SizeChart> sizeChart) {
    Predicate predicate = builder.equal(sizeChart.get(STORE_ID), storeId);
    if (StringUtils.isNotBlank(filterRequestDTO.getSizeAttributeCode())) {
      predicate = builder.and(predicate, builder.equal(sizeChart.get(SIZE_ATTRIBUTE_CODE),
          filterRequestDTO.getSizeAttributeCode()));
    }
    if (StringUtils.isNotBlank(filterRequestDTO.getBrandCode())) {
      predicate = builder.and(predicate,
          builder.equal(sizeChart.get(BRAND_CODE), filterRequestDTO.getBrandCode()));
    }
    if (CollectionUtils.isNotEmpty(filterRequestDTO.getBusinessPartnerCodes())) {
      predicate = builder.and(predicate,
          sizeChart.get(BUSINESS_PARTNER_CODE).in(filterRequestDTO.getBusinessPartnerCodes()));
    }
    if (StringUtils.isNotBlank(filterRequestDTO.getSizeChartName())) {
      predicate = builder.and(predicate, builder.like(builder.lower(sizeChart.get(SIZE_CHART_NAME)),
          ANY_STRING + filterRequestDTO.getSizeChartName().toLowerCase() + ANY_STRING));
    }
    if (Objects.nonNull(filterRequestDTO.getWaitingDeletion())) {
      predicate = builder.and(predicate,
          builder.equal(sizeChart.get(WAITING_DELETION), filterRequestDTO.getWaitingDeletion()));
    }
    predicate = builder.and(predicate, builder.equal(sizeChart.get(MARK_FOR_DELETE), false));
    return predicate;
  }

  public void setOrderByForExternalSizeCharts(CriteriaBuilder builder, Root<SizeChart> sizeChart,
      SizeChartFilterRequestDTO filter, CriteriaQuery<SizeChart> resultCriteriaQuery,
      boolean sortExternalSizeChartsByCreatedDateDescending) {
    Predicate predicateForInternalSizeCharts =
        builder.equal(sizeChart.get(BUSINESS_PARTNER_CODE), BUSINESS_PARTNER_CODE_FOR_INTERNAL);

    /**
     * The external size charts are sorted based on created date when the switch is on,
     * else fallback to the default sorting on size chart name
     */

    if (sortExternalSizeChartsByCreatedDateDescending) {
      filter.setSortByFieldName(CREATED_DATE);
      resultCriteriaQuery.orderBy(
          builder.desc(builder.selectCase().when(predicateForInternalSizeCharts, 0).otherwise(1)),
          builder.desc(sizeChart.get(filter.getSortByFieldName())));
    } else {
      resultCriteriaQuery.orderBy(
          builder.desc(builder.selectCase().when(predicateForInternalSizeCharts, 0).otherwise(1)),
          builder.asc(sizeChart.get(filter.getSortByFieldName())));
    }
  }
}
