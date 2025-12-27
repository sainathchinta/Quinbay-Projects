package com.gdn.mta.bulk.repository;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Order;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryRequest;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class BulkInternalProcessRepositoryImpl implements BulkInternalProcessCustomRepository {

  private static final String STORE_ID = "storeId";
  private static final String CREATED_DATE = "createdDate";
  private static final String STATUS_ALL = "ALL";
  private static final String STATUS = "status";
  private static final String SELLER_CODE = "sellerCode";
  private static final String SELLER_NAME = "sellerName";
  private static final String CREATED_BY = "createdBy";
  private static final String INTERNAL_PROCESS_REQUEST_CODE = "internalProcessRequestCode";
  private static final String MARK_FOR_DELETE = "markForDelete";
  private static final String PROCESS_TYPE = "processType";
  private static final String ORDER_BY_DESC = "DESC";
  private static final String ORDER_BY_ASC = "ASC";
  private static final String ORDER_BY_UPLOAD_DATE = "UPLOAD_DATE";
  private static final String IN_PROGRESS = "IN_PROGRESS";
  private static final String PUBLISHED = "PUBLISHED";

  @PersistenceContext
  private EntityManager entityManager;

  @Override
  public Page<BulkInternalProcess> bulkInternalProcessSummary(String storeId,
      BulkInternalProcessSummaryRequest bulkInternalProcessSummaryRequest, Pageable pageable) {
    int page = pageable.getPageNumber();
    int size = pageable.getPageSize();
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<BulkInternalProcess> criteriaQuery = criteriaBuilder.createQuery(BulkInternalProcess.class);
    CriteriaQuery<Long> countCriteriaQuery = criteriaBuilder.createQuery(Long.class);
    Root<BulkInternalProcess> bulkInternalProcessRoot = criteriaQuery.from(BulkInternalProcess.class);
    Root<BulkInternalProcess> countRoot = countCriteriaQuery.from(BulkInternalProcess.class);
    List<Order> orderArrayList = new ArrayList<>();
    Predicate predicate =
        getPredicateByRequest(storeId, bulkInternalProcessSummaryRequest, criteriaBuilder, bulkInternalProcessRoot);
    Predicate countPredicate =
        getPredicateByRequest(storeId, bulkInternalProcessSummaryRequest, criteriaBuilder, countRoot);
    CriteriaQuery<BulkInternalProcess> resultCriteriaQuery =
        criteriaQuery.select(bulkInternalProcessRoot).where(predicate);
    countCriteriaQuery.select(criteriaBuilder.count(countRoot))
        .where(countPredicate);
    if (Objects.nonNull(bulkInternalProcessSummaryRequest.getSortColumn())) {
      if (ORDER_BY_UPLOAD_DATE.equalsIgnoreCase(bulkInternalProcessSummaryRequest.getSortColumn())) {
        if (ORDER_BY_ASC.equalsIgnoreCase(bulkInternalProcessSummaryRequest.getSortOrder())) {
          orderArrayList.add(criteriaBuilder.asc(bulkInternalProcessRoot.get(CREATED_DATE)));
        } else if (ORDER_BY_DESC.equalsIgnoreCase(bulkInternalProcessSummaryRequest.getSortOrder()) || Objects
            .isNull(bulkInternalProcessSummaryRequest.getSortOrder())) {
          orderArrayList.add(criteriaBuilder.desc(bulkInternalProcessRoot.get(CREATED_DATE)));
        }
      }
    }
    resultCriteriaQuery.orderBy(orderArrayList);
    List<BulkInternalProcess> pagingContent =
        entityManager.createQuery(resultCriteriaQuery).setFirstResult(page * size).setMaxResults(size).getResultList();
    Long totalRecords = entityManager.createQuery(countCriteriaQuery).getSingleResult();
    return new PageImpl<>(pagingContent,  PageRequest.of(page, size), totalRecords);
  }

  private Predicate getPredicateByRequest(String storeId,
      BulkInternalProcessSummaryRequest bulkInternalProcessSummaryRequest, CriteriaBuilder criteriaBuilder,
      Root<BulkInternalProcess> bulkInternalProcessRoot) {
    Predicate predicate = criteriaBuilder.equal((bulkInternalProcessRoot.get(STORE_ID)), storeId);
    if (Objects.nonNull(bulkInternalProcessSummaryRequest.getEndDate()) && Objects
        .nonNull(bulkInternalProcessSummaryRequest.getStartDate())) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder
          .between(bulkInternalProcessRoot.get(CREATED_DATE), bulkInternalProcessSummaryRequest.getStartDate(),
              bulkInternalProcessSummaryRequest.getEndDate()));
    }
    if (StringUtils.isNotEmpty(bulkInternalProcessSummaryRequest.getStatus()) && !STATUS_ALL
        .equalsIgnoreCase(bulkInternalProcessSummaryRequest.getStatus())) {
      if (IN_PROGRESS.equalsIgnoreCase(bulkInternalProcessSummaryRequest.getStatus())) {
        predicate = criteriaBuilder.and(predicate, criteriaBuilder
            .or(criteriaBuilder.equal(criteriaBuilder.upper(bulkInternalProcessRoot.get(STATUS)), IN_PROGRESS),
                criteriaBuilder.equal(criteriaBuilder.upper(bulkInternalProcessRoot.get(STATUS)), PUBLISHED)));
      } else {
        predicate = criteriaBuilder.and(predicate, criteriaBuilder
            .equal(criteriaBuilder.upper(bulkInternalProcessRoot.get(STATUS)),
                bulkInternalProcessSummaryRequest.getStatus().toUpperCase()));
      }
    }
    if (StringUtils.isNotEmpty(bulkInternalProcessSummaryRequest.getKeyword())) {
      if (BulkInternalProcessType.SALES_CATEGORY_UPDATE.name()
          .equals(bulkInternalProcessSummaryRequest.getProcessType())) {
        predicate = criteriaBuilder.and(predicate, criteriaBuilder.or(criteriaBuilder
            .equal(criteriaBuilder.upper(bulkInternalProcessRoot.get(CREATED_BY)),
                bulkInternalProcessSummaryRequest.getKeyword().toUpperCase()), criteriaBuilder
            .equal(criteriaBuilder.upper(bulkInternalProcessRoot.get(INTERNAL_PROCESS_REQUEST_CODE)),
                bulkInternalProcessSummaryRequest.getKeyword().toUpperCase())));
      } else {
        predicate = criteriaBuilder.and(predicate, criteriaBuilder.or(criteriaBuilder
            .equal(criteriaBuilder.upper(bulkInternalProcessRoot.get(SELLER_CODE)),
                bulkInternalProcessSummaryRequest.getKeyword().toUpperCase()), criteriaBuilder
            .equal(criteriaBuilder.upper(bulkInternalProcessRoot.get(INTERNAL_PROCESS_REQUEST_CODE)),
                bulkInternalProcessSummaryRequest.getKeyword().toUpperCase()), criteriaBuilder
            .equal(criteriaBuilder.upper(bulkInternalProcessRoot.get(SELLER_NAME)),
                bulkInternalProcessSummaryRequest.getKeyword().toUpperCase())));
      }
    }
    if (StringUtils.isNotEmpty(bulkInternalProcessSummaryRequest.getProcessType())) {
      predicate = criteriaBuilder.and(predicate, criteriaBuilder.equal(bulkInternalProcessRoot.get(PROCESS_TYPE),
          bulkInternalProcessSummaryRequest.getProcessType().toUpperCase()));
    }
    predicate =
        criteriaBuilder.and(predicate, criteriaBuilder.equal(bulkInternalProcessRoot.get(MARK_FOR_DELETE), false));
    return predicate;
  }
}
