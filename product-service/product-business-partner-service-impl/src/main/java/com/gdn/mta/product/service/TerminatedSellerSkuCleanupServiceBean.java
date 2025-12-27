package com.gdn.mta.product.service;

import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.ProductBusinessPartnerAttributeRepository;
import com.gdn.mta.product.repository.ProductSuspensionHistoryRepository;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.workflow.WorkflowProcessor;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class TerminatedSellerSkuCleanupServiceBean implements TerminatedSellerSkuCleanupService{

  @Autowired
  private ProductService productService;

  @Autowired
  private ProductAutoApprovalCriteriaService productAutoApprovalCriteriaService;

  @Autowired
  private ProductImageQcProcessingResponseService productImageQcProcessingResponseService;


  @Autowired
  private ProductWorkflowService productWorkflowService;

  @Autowired
  private WorkflowProcessor workflowProcessor;

  @Autowired
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Autowired
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Autowired
  private ProductSuspensionHistoryRepository productSuspensionHistoryRepository;

  @Autowired
  private ProductStockAlertService productStockAlertService;

  @Autowired
  private ProductBusinessPartnerAttributeRepository productBusinessPartnerAttributeRepository;

  @Autowired
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public boolean terminatedSellerSkuCleanup(ProductCollection productCollection) throws Exception {
    String storeId = Optional.ofNullable(productCollection.getStoreId()).orElse(Constants.DEFAULT_STORE_ID);
    try {
      deleteIndependentTables(storeId, productCollection.getProductCode());
      deleteProductCollectionDependentTables(storeId, productCollection.getProductId());
      productService.deleteProductCollectionByStoreIdAndProductCode(storeId, productCollection.getProductCode());
      return true;
    } catch (Exception e) {
      log.error("Error while deleting terminated seller product from DB where productCode : {} ",
          productCollection.getProductCode(), e);
      return false;
    }
  }

  private void deleteIndependentTables(String storeId, String productCode) {
    productAutoApprovalCriteriaService.deleteProductAutoApprovalCriteriaByStoreIdAndProductCode(storeId, productCode);
    productImageQcProcessingResponseService.deleteProductImageQcProcessingResponseByStoreIdAndProductCode(storeId,
        productCode);
    productWorkflowService.deleteProductWorkflowByStoreIdAndProductCode(storeId, productCode);
    workflowProcessor.deleteProductWfHistoryByStoreIdAndProductCode(storeId, productCode);
  }

  private void deleteProductCollectionDependentTables(String storeId, String productId){
    productLevel1HistoryService.deleteProductHistoryByStoreIdAndProductId(storeId, productId);
    productSuspensionHistoryRepository.deleteByStoreIdAndProductId(storeId, productId);
    updatedProductHistoryService.deleteUpdateProductHistoryByStoreIdAndProductId(storeId, productId);
    productStockAlertService.deleteProductStockAlertByStoreIdAndProductId(storeId, productId);
    productBusinessPartnerAttributeRepository.deleteByStoreIdAndProductId(storeId, productId);
    productItemBusinessPartnerService.deleteProductItemBusinessPartnerByStoreIdAndProductId(storeId, productId);
    productBusinessPartnerService.deleteProductBusinessPartnerByStoreIdAndProductId(storeId, productId);
  }
}
