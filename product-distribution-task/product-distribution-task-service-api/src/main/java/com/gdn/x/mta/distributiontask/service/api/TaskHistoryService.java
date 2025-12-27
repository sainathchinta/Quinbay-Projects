package com.gdn.x.mta.distributiontask.service.api;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.mta.distributiontask.domain.event.model.PDTHistoryEventModel;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.TaskHistory;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

/**
 * Created by virajjasani on 25/09/16.
 */
public interface TaskHistoryService {


  /**
   * create Task History
   *
   * @param product
   * @param vendor
   * @param reason
   * @param state
   * @throws Exception
   */
  void createTaskHistory(String storeId, String executor, Product product, Vendor vendor,
      String reason, WorkflowState state, String taskCode) throws Exception;

  /**
   * List of product history
   *
   * @param productCode
   * @return
   * @throws Exception
   */
  Page<TaskHistory> getHistoryFromProductCode(String storeId, String productCode, Pageable page)
      throws Exception;
  
  /**
   * List of task history
   * 
   * @param storeId
   * @param taskCode
   * @param page
   * @return
   * @throws Exception
   */
  Page<TaskHistory> getHistoryFromTaskCode(String storeId, String taskCode, Pageable page)
      throws Exception;

  /**
   *
   * @param storeId
   * @param executor
   * @param product
   * @param vendor
   * @param reason
   * @param state
   * @param taskCode
   * @return PDTHistoryEventModel
   */
  PDTHistoryEventModel generatePDTHistoryEventModel(String storeId, String executor, Product product, Vendor vendor,
      String reason, WorkflowState state, String taskCode);
}
