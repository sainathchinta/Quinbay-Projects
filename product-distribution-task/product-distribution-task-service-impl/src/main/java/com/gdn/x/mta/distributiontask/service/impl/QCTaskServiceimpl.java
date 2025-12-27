package com.gdn.x.mta.distributiontask.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.dao.api.VendorQuotaCounterRepository;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductDistributionTask;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.service.api.ProductDistributionTaskService;
import com.gdn.x.mta.distributiontask.service.api.ProductReviewerService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.QCTaskService;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import com.gdn.x.mta.distributiontask.service.api.TaskHistoryService;
import com.gdn.x.mta.distributiontask.service.api.publisher.ApprovedProductPublisherService;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Created by Poornima on 9/25/16.
 */

@Service
@Slf4j
public class QCTaskServiceimpl implements QCTaskService {

  private static final String QC_PASSED = "qc passed";

  @Autowired
  private ProductServiceRepository productServiceRepository;

  @Autowired
  private ProductService productService;

  @Autowired
  private TaskHistoryService taskHistoryService;

  @Autowired
  private ProductDistributionTaskService productDistributionTaskService;

  @Autowired
  private ApprovedProductPublisherService approvedProductPublisherService;

  @Autowired
  private VendorQuotaCounterRepository vendorQuotaCounterRepository;

  @Autowired
  private SolrVendorCollectionService solrVendorCollectionService;

  @Autowired
  private ProductReviewerService productReviewerService;

  @Value("${qc.products.retry.count}")
  private int qcProductsRetryCount;

  @Value("${qc.products.delta.hours}")
  private int qcProductsDeltaHours;

  @Value("${qc.products.batch.size}")
  private int qcProductsBatchSize;

  @Value("${qc.products.skip.states}")
  private String qcProductSkipStates;

  @Transactional(rollbackFor = Exception.class)
  @Override
  public void qcRejectProduct(Product product, boolean isAssignedToVendor, String reason,
      WorkflowState state) throws Exception {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    String username = GdnMandatoryRequestParameterUtil.getUsername();
    ProductDistributionTask productDistributionTask =
        productDistributionTaskService.findByProductId(product.getId());
    Integer productRejectedCount = productDistributionTask.getProduct().getRejectedCount() + 1;
    Integer productDistributionTaskRejectedCount = productDistributionTask.getRejectedCount() + 1;
    product.setRejectedCount(productRejectedCount);
    productDistributionTask.setRejectedCount(productDistributionTaskRejectedCount);
    log.info("Going to reject product {} {} ", product, productDistributionTask);
    if (isAssignedToVendor) {
      this.productService.updateStateAndRemoveAssigneeDetails(product, state);
      this.productReviewerService.clearAllReviewerDetails(product.getProductCode());
      this.productDistributionTaskService.updateState(productDistributionTask, state);
    } else {
      product.setCurrentVendor(null);
      product.setVendorId(null);
      WorkflowState stateForProduct = WorkflowState.UNASSIGNED;
      this.productService.updateState(product, stateForProduct);
      productDistributionTask.setMarkForDelete(true);
      this.productDistributionTaskService.updateState(productDistributionTask, stateForProduct);
      this.productServiceRepository.republishToPDT(storeId, username, product.getProductCode());
    }
    this.taskHistoryService.createTaskHistory(storeId, username,
        productDistributionTask.getProduct(), productDistributionTask.getVendor(), reason, state,
        productDistributionTask.getTaskCode());
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public void approveProductByQC(ProductDistributionTask productDistributionTask)
      throws Exception {
    String username = GdnMandatoryRequestParameterUtil.getUsername();
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    Product product = productService.findByProductId(productDistributionTask.getProductId());
    product.setMarkForDelete(true);
    productDistributionTask.setMarkForDelete(true);
    this.productDistributionTaskService.updateState(productDistributionTask, WorkflowState.PASSED);
    this.productService.updateState(product, WorkflowState.PASSED);
    this.approvedProductPublisherService.publishVendorApprovedEvent(product, false);
    this.vendorQuotaCounterRepository.decrementInProgressQuota(productDistributionTask.getVendor(), 1);
    this.taskHistoryService.createTaskHistory(storeId, username, product,
        productDistributionTask.getVendor(), QC_PASSED, WorkflowState.PASSED,
        productDistributionTask.getTaskCode());
  }

  @Override
  @Transactional(readOnly = false)
  public void moveFailedProductToQC(ProductDistributionTask productDistributionTask)
    throws Exception {
    String username = GdnMandatoryRequestParameterUtil.getUsername();
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    if (!qcProductSkipStates.contains(productDistributionTask.getState().name())
      && (productDistributionTask.isMarkForDelete())) {
      productDistributionTask.setMarkForDelete(false);
      productDistributionTask.getProduct().setMarkForDelete(false);
      this.productService.updateState(productDistributionTask.getProduct(), WorkflowState.PASSED);
      this.productDistributionTaskService.updateState(productDistributionTask,
        WorkflowState.PASSED);
      this.taskHistoryService.createTaskHistory(storeId, username,
        productDistributionTask.getProduct(), productDistributionTask.getVendor(),
        "Return to QC because failed to process", WorkflowState.PASSED,
        productDistributionTask.getTaskCode());

    }
  }

  @Async
  @Override
  public void retryQcProducts(int qcRetryCount, int deltaHours, int batchSize) {
    List<Product> updatedProducts = Optional.ofNullable(
            productService.republishFinalQcProductsForApproval(qcRetryCount <= 0 ? qcProductsRetryCount : qcRetryCount,
                deltaHours <= 0 ? qcProductsDeltaHours : deltaHours, batchSize <= 0 ? qcProductsBatchSize : batchSize))
        .orElse(new ArrayList<>());
    if (CollectionUtils.isNotEmpty(updatedProducts)) {
      updatedProducts.forEach(updatedProduct -> {
        try {
          this.solrVendorCollectionService.deleteProductFromSolr(updatedProduct.getProductCode());
        } catch (Exception e) {
          log.error("error while updating product in SolrVendorCollection where productCode: {}",
              updatedProduct.getProductCode(), e);
        }
      });
    }
  }

  @Override
  public Page<ProductAndReviewerDetailsDTO> getFilterProductSummaryFromSolr(String storeId, String status,
      ProductListRequest productListRequest, Pageable pageable) throws Exception {
    Page<ProductAndReviewerDetailsDTO> productAndReviewerDetailsDTOPage =
      solrVendorCollectionService.getFilterProductSummary(storeId, productListRequest,
        Arrays.asList(WorkflowState.PASSED), pageable);
    productService.setVendorForProductList(productAndReviewerDetailsDTOPage.getContent().stream()
      .map(ProductAndReviewerDetailsDTO::getProduct).collect(Collectors.toList()));
    return productAndReviewerDetailsDTOPage;
  }

}
