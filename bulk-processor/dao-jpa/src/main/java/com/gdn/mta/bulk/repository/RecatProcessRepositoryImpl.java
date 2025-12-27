package com.gdn.mta.bulk.repository;

import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import com.gdn.mta.bulk.entity.RecatProcess;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

@Slf4j
public class RecatProcessRepositoryImpl implements RecatProcessCustomRepository {

  private static final String STORE_ID = "storeId";
  private static final String MARK_FOR_DELETE = "markForDelete";
  private static final String CREATED_BY = "createdBy";
  private static final String RECAT_REQUEST_CODE = "recatRequestCode";
  private static final String SCHEDULED_TIME = "scheduledTime";
  private static final String STATUS = "status";
  private static final String CREATED_DATE = "createdDate";
  private static final String ORDER_BY_DESC = "desc";
  private static final String ORDER_BY_SCHEDULED_TIME = "scheduledDate";
  private static final String RECAT_STATUS_FILTER_ALL = "all";



  @PersistenceContext
  private EntityManager entityManager;

  @Override
  public Page<RecatProcess> findSummaryByFilter(String storeId, RecatProcessSummaryRequest recatProcessSummaryRequest, int page, int size) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<RecatProcess> criteriaQuery = criteriaBuilder.createQuery(RecatProcess.class);
    CriteriaQuery<Long> countCriteriaQuery = criteriaBuilder.createQuery(Long.class);
    Root<RecatProcess> recatProcess = criteriaQuery.from(RecatProcess.class);
    Root<RecatProcess> countRoot = countCriteriaQuery.from(RecatProcess.class);
    List<Order> orderArrayList = new ArrayList<>();
    Predicate predicate =
        getPredicateByRequest(storeId, recatProcessSummaryRequest, criteriaBuilder, recatProcess);
    Predicate countPredicate =
        getPredicateByRequest(storeId, recatProcessSummaryRequest, criteriaBuilder, countRoot);
    CriteriaQuery<RecatProcess> resultCriteriaQuery = criteriaQuery.select(recatProcess).where(predicate);
    countCriteriaQuery.select(criteriaBuilder.count(countRoot)).where(countPredicate);
    if (ORDER_BY_DESC.equalsIgnoreCase(recatProcessSummaryRequest.getSortOrder())) {
      if (ORDER_BY_SCHEDULED_TIME.equalsIgnoreCase(recatProcessSummaryRequest.getSortColumn())) {
        orderArrayList.add(criteriaBuilder.desc(recatProcess.get(SCHEDULED_TIME)));
      } else {
        orderArrayList.add(criteriaBuilder.desc(recatProcess.get(CREATED_DATE)));
      }
    }
    else {
      if (ORDER_BY_SCHEDULED_TIME.equalsIgnoreCase(recatProcessSummaryRequest.getSortColumn())) {
        orderArrayList.add(criteriaBuilder.asc(recatProcess.get(SCHEDULED_TIME)));
      } else {
        orderArrayList.add(criteriaBuilder.asc(recatProcess.get(CREATED_DATE)));
      }
    }
    resultCriteriaQuery.orderBy(orderArrayList);
    List<RecatProcess> pagingContent =
        entityManager.createQuery(resultCriteriaQuery).setFirstResult(page * size)
            .setMaxResults(size).getResultList();
    Long totalRecords = entityManager.createQuery(countCriteriaQuery).getSingleResult();
    return new PageImpl<>(pagingContent,  PageRequest.of(page, size), totalRecords);
  }

  private Predicate getPredicateByRequest(String storeId, RecatProcessSummaryRequest recatProcessSummaryRequest,
      CriteriaBuilder criteriaBuilder, Root<RecatProcess> recatProcess) {
    Predicate predicate = criteriaBuilder.equal((recatProcess.get(STORE_ID)), storeId);
    if (Objects.nonNull(recatProcessSummaryRequest.getRequestEndDate()) && Objects
        .nonNull(recatProcessSummaryRequest.getRequestStartDate())) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder
          .between(recatProcess.get(CREATED_DATE), recatProcessSummaryRequest.getRequestStartDate(),
              recatProcessSummaryRequest.getRequestEndDate()));
    }
    if (StringUtils.isNotEmpty(recatProcessSummaryRequest.getStatus()) && !RECAT_STATUS_FILTER_ALL
        .equalsIgnoreCase(recatProcessSummaryRequest.getStatus())) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder
          .equal(recatProcess.get(STATUS), recatProcessSummaryRequest.getStatus().toUpperCase()));
    }
    if (StringUtils.isNotEmpty(recatProcessSummaryRequest.getKeyword())) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.or(
          criteriaBuilder.equal(recatProcess.get(RECAT_REQUEST_CODE),
              recatProcessSummaryRequest.getKeyword().toUpperCase()),
          criteriaBuilder.equal(recatProcess.get(CREATED_BY),
              recatProcessSummaryRequest.getKeyword())));
    }
    predicate = criteriaBuilder
        .and(predicate, criteriaBuilder.equal(recatProcess.get(MARK_FOR_DELETE), false));
    return predicate;
  }
}
