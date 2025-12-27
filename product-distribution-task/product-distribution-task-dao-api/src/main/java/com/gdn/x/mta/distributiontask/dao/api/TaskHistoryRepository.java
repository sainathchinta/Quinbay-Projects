package com.gdn.x.mta.distributiontask.dao.api;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.x.mta.distributiontask.model.TaskHistory;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

/**
 * Created by virajjasani on 25/09/16.
 */
public interface TaskHistoryRepository extends JpaRepository<TaskHistory, String> {

  String QUERY_FIND_LATEST_TASK_HISTORY_BY_STORE_ID_PRODUCT_CODE_WORKFLOW_STATE_MARK_FOR_DELETE_FALSE =
      "SELECT dth.* FROM PDT_DISTRIBUTION_TASK_HISTORY dth WHERE dth.store_id = :storeId "
          + "AND dth.product_code = :productCode AND dth.state = :workflowState AND dth.mark_for_delete IS FALSE "
          + "ORDER BY dth.created_date DESC LIMIT 1";

  String DELETE_QUERY_USING_PRODUCT_CODES = "DELETE FROM PDT_DISTRIBUTION_TASK_HISTORY dth WHERE dth.store_id = :storeId and dth.product_code IN (:productCodeList)";


  String DELETE_QUERY_USING_STORE_ID_AND_PRODUCT_CODE =
      "DELETE FROM PDT_DISTRIBUTION_TASK_HISTORY dth WHERE dth.store_id = :storeId AND dth"
          + ".product_code = :productCode";

  @Query(
      value = TaskHistoryRepository.QUERY_FIND_LATEST_TASK_HISTORY_BY_STORE_ID_PRODUCT_CODE_WORKFLOW_STATE_MARK_FOR_DELETE_FALSE,
      nativeQuery = true)
  TaskHistory findLatestTaskHistoryByStoreIdAndProductCodeAndWorkflowStateAndMarkForDeleteFalse(
      @Param("storeId") String storeId, @Param("productCode") String productCode,
      @Param("workflowState") WorkflowState workflowState) throws Exception;

  Page<TaskHistory> findAllByStoreIdAndProductCodeAndMarkForDeleteFalse(
      String storeId, String productCode, Pageable page);
  
  Page<TaskHistory> findAllByStoreIdAndTaskCodeAndMarkForDeleteFalse(
      String storeId, String taskCode, Pageable page);

  @Modifying
  @Query(value = TaskHistoryRepository.DELETE_QUERY_USING_PRODUCT_CODES, nativeQuery = true)
  void deletebyProductCodes(@Param("storeId") String storeId, @Param("productCodeList") List<String> productCodeList);


  @Modifying
  @Query(value = TaskHistoryRepository.DELETE_QUERY_USING_STORE_ID_AND_PRODUCT_CODE,
         nativeQuery = true)
  void deleteByProductCode(@Param("storeId") String storeId,
      @Param("productCode") String productCode);

}
