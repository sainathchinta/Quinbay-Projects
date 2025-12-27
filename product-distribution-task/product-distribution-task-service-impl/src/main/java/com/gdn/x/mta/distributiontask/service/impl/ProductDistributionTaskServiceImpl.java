package com.gdn.x.mta.distributiontask.service.impl;

import java.util.Date;
import java.util.List;
import java.util.Objects;

import com.gdn.x.mta.distributiontask.service.api.ErrorMessages;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.pdt.service.distribution.DistributionTaskService;
import com.gdn.x.mta.distributiontask.dao.api.ProductDistributionTaskRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductRepository;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.service.api.ProductDistributionTaskService;
import lombok.extern.slf4j.Slf4j;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

/**
 * Created by Alok on 9/20/16.
 */
@Service
@Slf4j
public class ProductDistributionTaskServiceImpl implements ProductDistributionTaskService {

  @Autowired
  ProductDistributionTaskRepository productDistributionTaskRepository;

  @Autowired
  ProductRepository productRepository;

  @Autowired
  private DistributionTaskService distributionTaskService;

  @Override
  @Transactional
  public void movingProductsbackToProductDistribution(Vendor vendor)
      throws Exception {
    this.productDistributionTaskRepository.clearProductsInProductDistributionTask(vendor.getId());
    productRepository.clearVendorForProduct(vendor.getId());

  }

  @Override
  public ProductDistributionTask findByIdMarkForDeleteFalseAndStillInReview(String id) {
    return this.productDistributionTaskRepository
        .getProductDistributionTaskByIdAndStillInReview(id);
  }

  @Override
  public ProductDistributionTask findByProductId(String productId) {
    return this.productDistributionTaskRepository.getProductDistributionTaskByProduct(productId);
  }

  @Override
  public List<ProductDistributionTask> getListOfProductDistributionTaskSLA(Date currentDate,
      boolean isSlaDateExceed) throws Exception {
    return this.productDistributionTaskRepository.getListOfProductDistributionTaskSLA(currentDate,
        isSlaDateExceed);
  }

  private ProductDistributionTask getProductDistributionTaskByProductId(String productId,
      String vendorId) throws Exception {
    ProductDistributionTask productDistributionTask =
        this.productDistributionTaskRepository.getProductDistributionTaskByProduct(productId);
    GdnPreconditions.checkArgument(productDistributionTask != null,
      "ProductDistributionTask record not present for productId: " + productId);
    GdnPreconditions.checkArgument(vendorId.equals(productDistributionTask.getVendorId()),
      "Product is not assigned to current Vendor. Assigned VendorId: "
        + productDistributionTask.getVendorId());
    return productDistributionTask;
  }

  @Override
  public WorkflowState getWorkflowStatePostApproval(String productId, String vendorId) throws Exception {
    try {
      ProductDistributionTask productDistributionTask = getProductDistributionTaskByProductId(productId, vendorId);
      productDistributionTask.setState(WorkflowState.PASSED);
      saveProductDistributionTask(productDistributionTask);
      return WorkflowState.PASSED;
    } catch (Exception e) {
      log.error("error while updating ProductDistributionTask for product content approval. productId: " + "{}",
          productId, e);
      throw e;
    }
  }

  @Override
  @Transactional
  public void removeProductDistributionTask(String productId, String status) throws Exception {
    try {
      ProductDistributionTask productDistributionTask =
          this.productDistributionTaskRepository.getProductDistributionTaskByProduct(productId);
      if (productDistributionTask == null) {
        throw new IllegalStateException(
            "Record not found in ProductDistributionTask for productId: " + productId);
      }
      productDistributionTask.setMarkForDelete(true);
      productDistributionTask.setState(Enum.valueOf(WorkflowState.class, status));
      this.productDistributionTaskRepository.save(productDistributionTask);
    } catch (Exception e) {
      log.error("error while updating ProductDistributionTask. product Id: {}", productId);
      throw e;
    }
  }

  @Override
  @Transactional
  public void saveProductDistributionTask(ProductDistributionTask productDistributionTask) {
    this.productDistributionTaskRepository.save(productDistributionTask);
  }

  @Override
  @Transactional
  public void saveProductDistributionTaskList(List<ProductDistributionTask> productDistributionTaskList) {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productDistributionTaskList),
      ErrorMessages.PRODUCT_DISTRIBUTION_TASK_NOT_EMPTY);
    this.productDistributionTaskRepository.saveAll(productDistributionTaskList);
  }

  @Override
  @Transactional
  public void slaExceedUpdate(ProductDistributionTask productDistributionTask) throws Exception {
    productDistributionTask.setSlaDateExceed(true);
    this.update(productDistributionTask);
  }

  @Override
  @Transactional
  public ProductDistributionTask update(ProductDistributionTask productDistributionTask) {
    if ((productDistributionTask.getId() == null)
        || (this.productDistributionTaskRepository.findById(productDistributionTask.getId()).get() == null)) {
      throw new IllegalArgumentException(
          "can not update un existence data with id : " + productDistributionTask.getId());
    }
    return this.productDistributionTaskRepository.saveAndFlush(productDistributionTask);
  }

  @Override
  @Transactional
  public ProductDistributionTask updateState(ProductDistributionTask productDistributionTask,
      WorkflowState state) {
    if(Objects.isNull(productDistributionTask)){
      return null;
    }
    productDistributionTask.setState(state);
    return this.update(productDistributionTask);
  }

  @Override
  public ProductDistributionTask findTopByProductProductCodeOrderByUpdatedDateDesc(
      String productCode) {
    return this.productDistributionTaskRepository.findTopByProductProductCodeOrderByUpdatedDateDesc(productCode);
  }


  @Override
  public ProductDistributionTask findProductDistributionTaskByProductCodeAndMarkForDeleteFalse(
      String productCode) {
    return this.productDistributionTaskRepository
        .findProductDistributionTaskByProductProductCodeAndMarkForDeleteFalse(productCode);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void clearPresentDistributionTaskAndCreateNewTask(Product product) throws Exception {
    distributionTaskService
        .clearPresentDistributionTaskAndCreateNewTask(GdnMandatoryRequestParameterUtil.getStoreId(), product);
  }

  @Override
  public List<ProductDistributionTask> findStoreIdAndProductIdAndMarkForDeleteFalse(String storeId, String productId) {
    checkArgument(StringUtils.isNotEmpty(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotEmpty(productId), ErrorMessages.PRODUCT_ID_MUST_NOT_BE_EMPTY);
    return this.productDistributionTaskRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(
      storeId, productId);
  }
}
