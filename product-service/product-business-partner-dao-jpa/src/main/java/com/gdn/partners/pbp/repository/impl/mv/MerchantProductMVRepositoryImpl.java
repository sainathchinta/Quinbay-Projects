package com.gdn.partners.pbp.repository.impl.mv;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import jakarta.persistence.EntityManager;
import jakarta.persistence.Parameter;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.Query;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.ParameterExpression;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import com.gdn.mta.product.commons.constant.ProductLevel3InventoryCriteria;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.entity.mv.MerchantProductMV;
import com.gdn.partners.pbp.entity.mv.MerchantProductMV_;
import com.gdn.partners.pbp.model.vo.JpaPredicateResult;
import com.gdn.partners.pbp.repository.mv.MerchantProductMVRepositoryCustom;

@Repository
public class MerchantProductMVRepositoryImpl implements MerchantProductMVRepositoryCustom {

  @PersistenceContext
  private EntityManager entityManager;

  private static final String PARAM_BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String PARAM_NAME = "name";
  private static final String PARAM_GDN_SKU = "gdnSku";
  private static final String PARAM_CATEGORY_CODE = "categoryCode";
  private static final String PARAM_PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PARAM_BUYABLE = "isBuyable";
  private static final String PARAM_DISPLAYABLE = "isDisplayable";
  private static final String PARAM_ARCHIVED = "isArchived";
  private static final String PARAM_MARK_FOR_DELETE = "markForDelete";
  private static final String PARAM_STORE_ID = "storeId";
  private static final String PARAM_STOCK_STATUS = "stockStatus";

  private JpaPredicateResult buildPredicateFromFilter(ProductLevel3SummaryFilter filter,
      CriteriaBuilder cb, Root<MerchantProductMV> entity) {
    JpaPredicateResult result = new JpaPredicateResult();
    Map<String, Object> predicateParametersMap = new HashMap<>();
    Predicate predicate = cb.conjunction();
    if (StringUtils.isNotEmpty(filter.getStoreId())) {
      ParameterExpression<String> param = cb.parameter(String.class, PARAM_STORE_ID);
      predicate = cb.and(predicate, cb.equal(entity.get(MerchantProductMV_.storeId), param));
      predicateParametersMap.put(PARAM_STORE_ID, filter.getStoreId());
    }

    if (StringUtils.isNotEmpty(filter.getBusinessPartnerCode())) {
      ParameterExpression<String> param = cb.parameter(String.class, PARAM_BUSINESS_PARTNER_CODE);
      predicate =
          cb.and(predicate, cb.equal(entity.get(MerchantProductMV_.businessPartnerCode), param));
      predicateParametersMap.put(PARAM_BUSINESS_PARTNER_CODE, filter.getBusinessPartnerCode());
    }

    if (StringUtils.isNotEmpty(filter.getItemName())) {
      ParameterExpression<String> param = cb.parameter(String.class, PARAM_NAME);
      predicate = cb.and(predicate, cb.like(cb.lower(entity.get(MerchantProductMV_.name)), param));
      predicateParametersMap.put(PARAM_NAME, "%" + filter.getItemName().toLowerCase() + "%");
    }

    if (StringUtils.isNotEmpty(filter.getGdnSku())) {
      ParameterExpression<String> param = cb.parameter(String.class, PARAM_GDN_SKU);
      predicate = cb.and(predicate, cb.equal(entity.get(MerchantProductMV_.itemSku), param));
      predicateParametersMap.put(PARAM_GDN_SKU, filter.getGdnSku());
    }

    if (StringUtils.isNotEmpty(filter.getCategoryCode())) {
      ParameterExpression<String> param = cb.parameter(String.class, PARAM_CATEGORY_CODE);
      predicate = cb.and(predicate, cb.equal(entity.get(MerchantProductMV_.categoryCode), param));
      predicateParametersMap.put(PARAM_CATEGORY_CODE, filter.getCategoryCode());
    }

    if (StringUtils.isNotEmpty(filter.getPickupPointCode())) {
      ParameterExpression<String> param = cb.parameter(String.class, PARAM_PICKUP_POINT_CODE);
      predicate =
          cb.and(predicate, cb.equal(entity.get(MerchantProductMV_.pickupPointCode), param));
      predicateParametersMap.put(PARAM_PICKUP_POINT_CODE, filter.getPickupPointCode());
    }

    if (filter.getBuyable() != null) {
      ParameterExpression<Boolean> param = cb.parameter(Boolean.class, PARAM_BUYABLE);
      predicate = cb.and(predicate, cb.equal(entity.get(MerchantProductMV_.buyable), param));
      predicateParametersMap.put(PARAM_BUYABLE, filter.getBuyable());
    }

    if (filter.getDisplayable() != null) {
      ParameterExpression<Boolean> param = cb.parameter(Boolean.class, PARAM_DISPLAYABLE);
      predicate = cb.and(predicate, cb.equal(entity.get(MerchantProductMV_.displayable), param));
      predicateParametersMap.put(PARAM_DISPLAYABLE, filter.getDisplayable());
    }

    if (filter.getArchived() != null) {
      ParameterExpression<Boolean> param = cb.parameter(Boolean.class, PARAM_ARCHIVED);
      predicate = cb.and(predicate, cb.equal(entity.get(MerchantProductMV_.archived), param));
      predicateParametersMap.put(PARAM_ARCHIVED, filter.getArchived());
    }

    if (filter.getInventoryFilter() != null) {
      ParameterExpression<Boolean> param = cb.parameter(Boolean.class, PARAM_STOCK_STATUS);
      if (filter.getInventoryFilter() == ProductLevel3InventoryCriteria.STOCK_ALERT) {
        predicate =
            cb.and(predicate,
                cb.equal(entity.get(MerchantProductMV_.belowMinimumStockStatus), param));
        predicateParametersMap.put(PARAM_STOCK_STATUS, true);
      } else {
        predicate =
            cb.and(predicate, cb.equal(entity.get(MerchantProductMV_.outOfStockStatus), param));
        predicateParametersMap.put(PARAM_STOCK_STATUS,
            filter.getInventoryFilter() == ProductLevel3InventoryCriteria.OOS);
      }
    }
    result.setPredicate(predicate);
    result.setPredicateParametersMap(predicateParametersMap);
    return result;
  }

  private Query buildQuery(CriteriaQuery<?> criteria, Map<String, Object> queryParameters) {
    Query query = entityManager.createQuery(criteria);
    for (Parameter<?> queryParam : query.getParameters()) {
      String queryParamName = queryParam.getName();
      if (queryParameters.containsKey(queryParamName)) {
        query.setParameter(queryParamName, queryParameters.get(queryParamName));
      }
    }
    return query;
  }

  private List<Order> buildOrderByFilter(SortOrder filter, CriteriaBuilder cb,
      Root<MerchantProductMV> entity) {
    List<Order> result = new ArrayList<>();
    result.add("asc".equalsIgnoreCase(filter.getSortBy()) ? cb.asc(entity.get(filter.getOrderBy()))
        : cb.desc(entity.get(filter.getOrderBy())));
    return result;
  }


  @SuppressWarnings("unchecked")
  @Override
  public Page<MerchantProductMV> findByFilter(ProductLevel3SummaryFilter filter, Pageable pageable,
      SortOrder sort) {
    CriteriaBuilder cb = entityManager.getCriteriaBuilder();
    CriteriaQuery<MerchantProductMV> criteriaQuery = cb.createQuery(MerchantProductMV.class);
    CriteriaQuery<Long> countCriteriaQuery = cb.createQuery(Long.class);

    Root<MerchantProductMV> entity = criteriaQuery.from(MerchantProductMV.class);

    Map<String, Object> queryParameters = new HashMap<>();
    ParameterExpression<Boolean> param = cb.parameter(Boolean.class, PARAM_MARK_FOR_DELETE);
    Predicate predicate = cb.equal(entity.get(MerchantProductMV_.markForDelete), param);
    queryParameters.put(PARAM_MARK_FOR_DELETE, false);
    if (filter != null) {
      JpaPredicateResult predicateResult = buildPredicateFromFilter(filter, cb, entity);
      predicate = cb.and(predicate, predicateResult.getPredicate());
      queryParameters.putAll(predicateResult.getPredicateParametersMap());
    }

    CriteriaQuery<MerchantProductMV> resultCriteriaQuery =
        criteriaQuery.select(entity).where(predicate);

    if (sort != null && StringUtils.isNotEmpty(sort.getOrderBy()) && StringUtils.isNotEmpty(sort.getSortBy())) {
      resultCriteriaQuery.orderBy(buildOrderByFilter(sort, cb, entity));
    }

    countCriteriaQuery.select(cb.count(countCriteriaQuery.from(MerchantProductMV.class))).where(
        predicate);

    Query query = buildQuery(resultCriteriaQuery, queryParameters);
    query.setFirstResult(pageable.getPageNumber());
    query.setMaxResults(pageable.getPageSize());

    Query countQuery = buildQuery(countCriteriaQuery, queryParameters);
    List<MerchantProductMV> pagingContent = (List<MerchantProductMV>) query.getResultList();
    return new PageImpl<>(pagingContent, pageable, (Long) countQuery.getSingleResult());
  }

  @Override
  public boolean isInitialized() {
    TypedQuery<MerchantProductMV> queryResult =
        entityManager.createQuery("SELECT p FROM MerchantProductMV p", MerchantProductMV.class);
    queryResult.setMaxResults(1);
    return CollectionUtils.isNotEmpty(queryResult.getResultList());
  }
}
