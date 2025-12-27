package com.gdn.x.mta.distributiontask.service.impl;

import com.gdn.x.mta.distributiontask.dao.api.ProductActionRetryRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductAttributeRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductAutoApprovalRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductDistributionTaskRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductImageQcFeedbackRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductImageRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductItemAttributeRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductItemImageRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductItemRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductRepository;
import com.gdn.x.mta.distributiontask.dao.api.ProductReviewerRepository;
import com.gdn.x.mta.distributiontask.dao.api.TaskHistoryRepository;
import com.gdn.x.mta.distributiontask.service.api.ProductsPermanentDeleteService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;

@Service
@Slf4j
public class ProductsPermanentDeleteServiceImpl implements ProductsPermanentDeleteService {

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductItemRepository productItemRepository;

  @Autowired
  private ProductItemImageRepository productItemImageRepository;

  @Autowired
  private ProductItemAttributeRepository productItemAttributeRepository;

  @Autowired
  private ProductReviewerRepository productReviewerRepository;

  @Autowired
  private ProductImageQcFeedbackRepository productImageQcFeedbackRepository;

  @Autowired
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Autowired
  private ProductAutoApprovalRepository productAutoApprovalRepository;

  @Autowired
  private ProductActionRetryRepository productActionRetryRepository;

  @Autowired
  private TaskHistoryRepository taskHistoryRepository;

  @Autowired
  private ProductImageRepository productImageRepository;

  @Autowired
  private ProductAttributeRepository productAttributeRepository;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void deleteProducts(String productCode, String productId, String storeId) {
    List<String> productItemIds = productItemRepository.findIdByProduct(productId);
    if (CollectionUtils.isNotEmpty(productItemIds)) {
      log.info("Deleting products permanently for productItemIds : {}", productItemIds);
      deleteItemData(productItemIds);
    }
    deleteIndependentProductData(productCode, storeId);
    deleteProductData(productId);
    productRepository.deleteById(Collections.singletonList(productId));
  }

  private void deleteItemData(List<String> productItemIds) {
    productItemImageRepository.deleteByProductItemIds(productItemIds);
    productItemAttributeRepository.deleteByProductItemIds(productItemIds);
  }

  private void deleteProductData(String productId) {
    productDistributionTaskRepository.deleteByProductIds(Collections.singletonList(productId));
    productItemRepository.deleteByProductIds(Collections.singletonList(productId));
    productImageRepository.deleteByProductIds(Collections.singletonList(productId));
    productAttributeRepository.deleteByProductIds(Collections.singletonList(productId));
  }

  private void deleteIndependentProductData(String productCode, String storeId) {
    log.info("Deleting products permanently for product code : {}", productCode);
    productReviewerRepository.deleteByProductCodeIn(Collections.singletonList(productCode));
    productImageQcFeedbackRepository.deleteByStoreIdAndProductCode(storeId, productCode);
    productAutoApprovalRepository.deleteByProductCode(productCode);
    productActionRetryRepository.deleteByStoreIdAndProductCode(storeId, productCode);
    taskHistoryRepository.deleteByProductCode(storeId, productCode);
  }
}
