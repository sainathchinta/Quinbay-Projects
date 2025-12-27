package com.gdn.mta.product.repository;

import com.gda.mta.product.dto.HistoryUpdateRequest;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.partners.pbp.commons.constants.Constants;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Component;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import java.util.List;

@Slf4j
@Component
public class UpdatedProductHistoryCustomRepositoryImpl
  implements UpdatedProductHistoryCustomRepository {
  private static final String ACCESS_TIME = "accessTime";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String ACTIVITY = "activity";
  private static final String GDN_NAME = "gdnName";
  private static final String ONLINE_STATUS = "onlineStatus";
  private static final String ANY_STRING = "%";

  @PersistenceContext
  private EntityManager entityManager;

  @Override
  public Page<UpdatedProductHistory> findByHistoryUpdateRequest(
          HistoryUpdateRequest historyUpdateRequest, Pageable pageable) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<UpdatedProductHistory> criteriaQuery =
      criteriaBuilder.createQuery(UpdatedProductHistory.class);
    CriteriaQuery<Long> countCriteriaQuery = criteriaBuilder.createQuery(Long.class);
    Root<UpdatedProductHistory> updatedProductHistory =
      criteriaQuery.from(UpdatedProductHistory.class);
    Root<UpdatedProductHistory> countRoot =
        countCriteriaQuery.from(UpdatedProductHistory.class);
    Predicate predicate = getPredicateForHistoryUpdateRequest(historyUpdateRequest, criteriaBuilder,
      updatedProductHistory);
    Predicate countPredicate = getPredicateForHistoryUpdateRequest(historyUpdateRequest, criteriaBuilder,
        countRoot);
    CriteriaQuery<UpdatedProductHistory> resultCriteriaQuery =
      criteriaQuery.select(updatedProductHistory).where(predicate);
    countCriteriaQuery.select(
      criteriaBuilder.count(countRoot)).where(countPredicate);
    resultCriteriaQuery.orderBy(criteriaBuilder.desc(updatedProductHistory.get(ACCESS_TIME)));
    List<UpdatedProductHistory> pagingContent = entityManager.createQuery(resultCriteriaQuery)
      .setFirstResult(pageable.getPageNumber() * pageable.getPageSize())
      .setMaxResults(pageable.getPageSize()).getResultList();
    Long totalRecords = entityManager.createQuery(countCriteriaQuery).getSingleResult();
    return new PageImpl<>(pagingContent, pageable, totalRecords);
  }

  private Predicate getPredicateForHistoryUpdateRequest(HistoryUpdateRequest historyUpdateRequest,
    CriteriaBuilder criteriaBuilder, Root<UpdatedProductHistory> updatedProductHistory) {
    Predicate predicate = criteriaBuilder.between(updatedProductHistory.get(ACCESS_TIME),
      historyUpdateRequest.getStartDate(), historyUpdateRequest.getEndDate());
    if (StringUtils.isNotEmpty(historyUpdateRequest.getProductSku())) {
      predicate = criteriaBuilder.and(predicate,
        criteriaBuilder.equal(updatedProductHistory.get(PRODUCT_SKU),
          historyUpdateRequest.getProductSku()));
    }
    if (StringUtils.isNotEmpty(historyUpdateRequest.getPickupPointCode())) {
      predicate = criteriaBuilder.and(predicate,
        criteriaBuilder.equal(updatedProductHistory.get(PICKUP_POINT_CODE),
          historyUpdateRequest.getPickupPointCode()));
    }
    if (StringUtils.isNotBlank(historyUpdateRequest.getKeyword())) {
      if (Constants.ACTIVITY.equalsIgnoreCase(historyUpdateRequest.getSearchField())) {
        predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.like(criteriaBuilder.lower(updatedProductHistory.get(ACTIVITY)),
            ANY_STRING + historyUpdateRequest.getKeyword().toLowerCase() + ANY_STRING));
      } else {
        predicate = criteriaBuilder.and(predicate,
          criteriaBuilder.like(criteriaBuilder.lower(updatedProductHistory.get(GDN_NAME)),
            ANY_STRING + historyUpdateRequest.getKeyword().toLowerCase() + ANY_STRING));
      }
    }
    return predicate;
  }
}
