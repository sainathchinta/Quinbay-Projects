package com.gdn.mta.bulk.repository;

import java.util.List;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

import org.springframework.stereotype.Component;

import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.constants.BulkProcessConstant;

@Component
public class BulkProcessCustomRepositoryImpl implements BulkProcessCustomRepository {

  @PersistenceContext
  private EntityManager entityManager;

  @Override
  public List<BulkProcess> findBlpToProcess(String storeId, boolean orderByRow, int limit, String status, String bulkProcessType) {
    CriteriaBuilder builder = entityManager.getCriteriaBuilder();
    CriteriaQuery<BulkProcess> criteriaQuery = builder.createQuery(BulkProcess.class);
    Root<BulkProcess> root = criteriaQuery.from(BulkProcess.class);
    Predicate predicate = getFilterQuery(builder, root, storeId, status, bulkProcessType);
    CriteriaQuery<BulkProcess> resultCriteriaQuery = criteriaQuery.select(root).where(predicate);
    if (orderByRow) {
      resultCriteriaQuery.orderBy(builder.desc(root.get(BulkProcessConstant.FIELD_TOTAL_COUNT)));
    } else {
      resultCriteriaQuery.orderBy(builder.asc(root.get(BulkProcessConstant.FIELD_CREATED_DATE)));
    }
    List<BulkProcess> resultList = entityManager.createQuery(resultCriteriaQuery).setMaxResults(limit).getResultList();
    return resultList;
  }

  private Predicate getFilterQuery(CriteriaBuilder builder, Root<BulkProcess> root, String storeId, String status, String bulkProcessType) {
    Predicate predicate = builder.equal(root.get(BulkProcessConstant.FIELD_STORE_ID), storeId);
    predicate = builder.and(predicate, builder.equal(root.get(BulkProcessConstant.FIELD_BULK_PROCESS_TYPE), bulkProcessType));
    predicate = builder.and(predicate, builder.equal(root.get(BulkProcessConstant.FIELD_STATUS), status));
    predicate = builder.and(predicate, builder.equal(root.get(BulkProcessConstant.FIELD_MARK_FOR_DELETE), false));
    return predicate;
  }
}
