package com.gdn.x.mta.distributiontask.service.api;

import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;

import java.util.Date;
import java.util.List;

/**
 * Created by Alok on 9/20/16.
 */
public interface ProductDistributionTaskService {

  /**
   * Fetch productDistribution task by id and MarkForDelete is False and in review
   *
   * @param id
   * @return
   */
  ProductDistributionTask findByIdMarkForDeleteFalseAndStillInReview(String id);

  /**
   * Clearing the products from pdt table and product table assigned to vendor before deleting the vendor
   *
   * @param vendor
   * @throws Exception
   */
  void movingProductsbackToProductDistribution(Vendor vendor) throws Exception;

  /**
   * Fetch productDistribution task by productId and MarkForDelete is False
   *
   * @param productId
   * @return
   */
  ProductDistributionTask findByProductId(String productId);

  /**
   * List all sla Date exceeded productDistributionTask
   *
   * @param currentDate
   * @param isSlaDateExceed
   * @return
   * @throws Exception
   */
  List<ProductDistributionTask> getListOfProductDistributionTaskSLA(Date currentDate,
      boolean isSlaDateExceed) throws Exception;

  /**
   * get workflow state after approval of content/image
   *
   * @param productId
   * @param vendorId
   * @return
   * @throws Exception
   */
  WorkflowState getWorkflowStatePostApproval(String productId, String vendorId)
      throws Exception;

  /**
   * remove Product Distribution Task based on productId
   *
   * @param productId
   * @param status
   * @throws Exception
   */
  void removeProductDistributionTask(String productId, String status) throws Exception;


  /**
   * save Product Distribution Task record
   *
   * @param productDistributionTask
   */
  void saveProductDistributionTask(ProductDistributionTask productDistributionTask);

  /**
   * Save productTaskDistribution List
   *
    * @param productDistributionTaskList
   */
  void saveProductDistributionTaskList(List<ProductDistributionTask> productDistributionTaskList);

  /**
   * update Sla Exceed flag
   * 
   * @param productDistributionTask
   * @throws Exception
   */
  void slaExceedUpdate(ProductDistributionTask productDistributionTask) throws Exception;

  /**
   * Update product distribution task
   *
   * @param productDistributionTask
   * @return
   */
  ProductDistributionTask update(ProductDistributionTask productDistributionTask);
  
  /**
   * update state for task
   * 
   * @param productDistributionTask
   * @param state
   * @return
   */
  ProductDistributionTask updateState(ProductDistributionTask productDistributionTask, WorkflowState state);
  
  /**
   * <p>Find last task by product code</p>
   * 
   * @param productCode
   * @return
   */
  ProductDistributionTask findTopByProductProductCodeOrderByUpdatedDateDesc(String productCode);
  
  /**
   * <p>Find product distribution task by product code</p>
   * 
   * @param productCode
   * @return
   */
  ProductDistributionTask findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(String productCode);

  /**
   * Remove existing tasks in distribution task and create new ones
   *
   * @param product
   * @throws Exception
   */
  void clearPresentDistributionTaskAndCreateNewTask(Product product) throws Exception;

  /**
   * Find by product productId in all products
   *
   *
   * @param storeId
   * @param productId
   * @return
   */
  List<ProductDistributionTask> findStoreIdAndProductIdAndMarkForDeleteFalse(String storeId, String productId);
}
