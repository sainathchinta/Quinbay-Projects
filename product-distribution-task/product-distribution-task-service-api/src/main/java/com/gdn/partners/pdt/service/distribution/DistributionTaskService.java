package com.gdn.partners.pdt.service.distribution;

import java.util.List;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

public interface DistributionTaskService {

  /**
   * 
   * @param product
   * @param isForReview
   * @throws Exception
   */
  Product autoDistribute(Product product, boolean isForReview) throws Exception;

  /**
   * Publish vendor approval event
   *
   * @param product
   * @param reviewPending
   * @throws Exception
   */
  void publishVendorApprovedEvent(Product product, boolean reviewPending) throws Exception;

  void assignee(String vendorCode, List<String> productCodes) throws Exception;

  /**
   * clear existing distribution tasks and create new one
   *
   * @param storeId
   * @param product
   * @throws Exception
   */
  void clearPresentDistributionTaskAndCreateNewTask(String storeId, Product product) throws Exception;

  ProductDetailResponse getProductDetailByProductCode(
      String username, String productCode) throws Exception;

  /**
   * Get image qc response by product code
   *
   * @param productCode
   * @return
   */
  ImageQcProcessedAndBrandResponse getImageQcResponseByProductCode(String productCode);

  /**
   * Generate distribution task for a product
   *
   * @param storeId
   * @param vendor
   * @param products
   * @param workflowState
   * @return
   */
  List<ProductDistributionTask> generateDistributionTaskForProduct(String storeId, Vendor vendor,
      List<Product> products, WorkflowState workflowState) throws Exception;
}
