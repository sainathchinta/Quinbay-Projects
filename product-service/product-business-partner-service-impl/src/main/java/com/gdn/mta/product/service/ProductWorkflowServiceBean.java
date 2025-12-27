package com.gdn.mta.product.service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductWorkflow;
import com.gdn.mta.product.repository.ProductHistoryRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.repository.ProductWorkflowRepository;
import com.gdn.mta.product.util.ProductWorkflowLookup;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.entity.workflow.product.ProductWf;
import com.gdn.partners.pbp.repository.workflow.ProductWfRepository;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Service
@Transactional(readOnly = true)
@Slf4j
public class ProductWorkflowServiceBean implements ProductWorkflowService {

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductWorkflowRepository productWorkflowRepository;

  @Autowired
  private ProductHistoryRepository productHistoryRepository;

  @Autowired
  private ProductWfRepository productWfRepository;

  private List<Integer> generateWorkflowStates(List<ProductWorkflow> productWorkflows) {
    List<Integer> workflowStates = new ArrayList<Integer>();
    for (ProductWorkflow productWorkflow : productWorkflows) {
      workflowStates.add(productWorkflow.getState());
    }
    return workflowStates;
  }

  private ProductWorkflow getProductWorkflowByState(List<ProductWorkflow> productWorkflows,
      Integer state) throws Exception {
    ProductWorkflow correctProductWorkflow = null;
    for (ProductWorkflow productWorkflow : productWorkflows) {
      if (state.equals(productWorkflow.getState())) {
        correctProductWorkflow = productWorkflow;
      }
    }
    return correctProductWorkflow;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void processImage(String storeId, String productCode, boolean retryProcessImage) throws Exception {
    ProductDetailResponse productData =
        this.productRepository.findProductDetailByProductCode(productCode);
    List<ProductWorkflow> productWorkflows =
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId,
            productData.getId());
    List<Integer> workflowStates = generateWorkflowStates(productWorkflows);
    if (!retryProcessImage && (workflowStates.isEmpty()
        || !workflowStates.contains(ProductWorkflowLookup.STATE_REVIEW_IMAGE)
        || workflowStates.contains(ProductWorkflowLookup.STATE_PROCESS_IMAGE))) {
      throw new ApplicationException(
          ErrorCategory.INVALID_STATE,
          "Product Code : "
              + productCode
              + ". Error Code : state of product workflow is invalid. Please refresh the page and try again");
    }
    ProductWorkflow addedProductWorkflow =
        new ProductWorkflow(productData.getId(), ProductWorkflowLookup.STATE_PROCESS_IMAGE,
            ProductWorkflowLookup.STATE_PROCESS_IMAGE_DESCRIPTION, null, null, storeId);
    ProductHistory addedProductHistory =
        new ProductHistory(productData.getId(), ProductWorkflowLookup.STATE_PROCESS_IMAGE,
            ProductWorkflowLookup.STATE_PROCESS_IMAGE_DESCRIPTION, null, null, storeId);
    this.productWorkflowRepository.saveAndFlush(addedProductWorkflow);
    this.productHistoryRepository.saveAndFlush(addedProductHistory);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void rejectProcessImage(String storeId, String productCode) throws Exception {
    ProductDetailResponse productData =
        this.productRepository.findProductDetailByProductCode(productCode);
    List<ProductWorkflow> productWorkflows =
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId,
            productData.getId());
    List<Integer> workflowStates = generateWorkflowStates(productWorkflows);
    if (workflowStates.isEmpty()
        || !workflowStates.contains(ProductWorkflowLookup.STATE_REVIEW_IMAGE)
        || !workflowStates.contains(ProductWorkflowLookup.STATE_PROCESS_IMAGE)) {
      throw new ApplicationException(
          ErrorCategory.INVALID_STATE,
          "Product Code : "
              + productCode
              + ". Error Code : state of product workflow is invalid. Please refresh the page and try again");
    }
    ProductWorkflow deletedProductWorkflow =
        getProductWorkflowByState(productWorkflows, ProductWorkflowLookup.STATE_PROCESS_IMAGE);
    ProductHistory addedProductHistory =
        new ProductHistory(productData.getId(), ProductWorkflowLookup.STATE_PROCESS_IMAGE,
            ProductWorkflowLookup.STATE_PROCESS_IMAGE_DESCRIPTION,
            ProductWorkflowLookup.STATE_REJECT_PROCESS_IMAGE_NOTES, null, null, storeId);
    this.productWorkflowRepository.delete(deletedProductWorkflow);
    this.productHistoryRepository.saveAndFlush(addedProductHistory);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public boolean approveImage(String storeId, String productCode, boolean retryApproveImage)
      throws Exception {
    ProductDetailResponse productData =
        this.productRepository.findProductDetailByProductCode(productCode);
    List<ProductWorkflow> productWorkflows =
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId,
            productData.getId());
    List<Integer> workflowStates = generateWorkflowStates(productWorkflows);
    if (!retryApproveImage && (workflowStates.isEmpty()
        || !workflowStates.contains(ProductWorkflowLookup.STATE_REVIEW_IMAGE)
        || !workflowStates.contains(ProductWorkflowLookup.STATE_PROCESS_IMAGE))) {
      throw new ApplicationException(
          ErrorCategory.INVALID_STATE,
          "Product Code : "
              + productCode
              + ". Error Code : state of product workflow is invalid. Please refresh the page and try again");
    }
    List<ProductWorkflow> deletedProductWorkflows = new ArrayList<ProductWorkflow>();
    deletedProductWorkflows.add(getProductWorkflowByState(productWorkflows,
        ProductWorkflowLookup.STATE_REVIEW_IMAGE));
    deletedProductWorkflows.add(getProductWorkflowByState(productWorkflows,
        ProductWorkflowLookup.STATE_PROCESS_IMAGE));
    List<ProductHistory> addedProductHistories = new ArrayList<ProductHistory>();
    addedProductHistories.add(new ProductHistory(productData.getId(),
        ProductWorkflowLookup.STATE_REVIEW_IMAGE,
        ProductWorkflowLookup.STATE_APPROVED_IMAGE_DESCRIPTION,
        ProductWorkflowLookup.STATE_APPROVED_IMAGE_NOTES, null, null, storeId));
    addedProductHistories.add(new ProductHistory(productData.getId(),
        ProductWorkflowLookup.STATE_PROCESS_IMAGE,
        ProductWorkflowLookup.STATE_PROCESS_IMAGE_DESCRIPTION,
        ProductWorkflowLookup.STATE_APPROVE_PROCESS_IMAGE_NOTES, null, null, storeId));
    boolean isActive = false;
    if (!workflowStates.contains(ProductWorkflowLookup.STATE_REVIEW_CONTENT)) {
      ProductWorkflow addedProductWorkflow =
          new ProductWorkflow(productData.getId(), ProductWorkflowLookup.STATE_ACTIVE,
              ProductWorkflowLookup.STATE_ACTIVE_DESCRIPTION, null, null, storeId);
      addedProductHistories.add(new ProductHistory(productData.getId(),
          ProductWorkflowLookup.STATE_ACTIVE, ProductWorkflowLookup.STATE_ACTIVE_DESCRIPTION,
          ProductWorkflowLookup.STATE_ACTIVE_NOTES, null, null, storeId));
      this.productWorkflowRepository.saveAndFlush(addedProductWorkflow);
      isActive = true;
    }
    if (deletedProductWorkflows.size() > 0) {
      this.productWorkflowRepository.deleteAll(deletedProductWorkflows);
    }
    this.productHistoryRepository.saveAll(addedProductHistories);
    return isActive;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public boolean approveContent(String storeId, String productCode, boolean retryApproveContent)
      throws Exception {
    ProductDetailResponse productData =
        this.productRepository.findProductDetailByProductCode(productCode);
    List<ProductWorkflow> productWorkflows =
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId,
            productData.getId());
    List<Integer> workflowStates = generateWorkflowStates(productWorkflows);
    if (!retryApproveContent && (workflowStates.isEmpty()
        || !workflowStates.contains(ProductWorkflowLookup.STATE_REVIEW_CONTENT))) {
      throw new ApplicationException(
          ErrorCategory.INVALID_STATE,
          "Product Code : "
              + productCode
              + ". Error Code : state of product workflow is invalid. Please refresh the page and try again");
    }
    ProductWorkflow deletedProductWorkflow =
        getProductWorkflowByState(productWorkflows, ProductWorkflowLookup.STATE_REVIEW_CONTENT);
    List<ProductHistory> addedProductHistories = new ArrayList<ProductHistory>();
    addedProductHistories.add(new ProductHistory(productData.getId(),
        ProductWorkflowLookup.STATE_REVIEW_CONTENT,
        ProductWorkflowLookup.STATE_APPROVED_CONTENT_DESCRIPTION,
        ProductWorkflowLookup.STATE_APPROVED_CONTENT_NOTES, null, null, storeId));
    boolean isActive = false;
    if (!workflowStates.contains(ProductWorkflowLookup.STATE_REVIEW_IMAGE)) {
      ProductWorkflow addedProductWorkflow =
          new ProductWorkflow(productData.getId(), ProductWorkflowLookup.STATE_ACTIVE,
              ProductWorkflowLookup.STATE_ACTIVE_DESCRIPTION, null, null, storeId);
      addedProductHistories.add(new ProductHistory(productData.getId(),
          ProductWorkflowLookup.STATE_ACTIVE, ProductWorkflowLookup.STATE_ACTIVE_DESCRIPTION,
          ProductWorkflowLookup.STATE_ACTIVE_NOTES, null, null, storeId));
      this.productWorkflowRepository.saveAndFlush(addedProductWorkflow);
      isActive = true;
    }
    if (deletedProductWorkflow != null) {
      this.productWorkflowRepository.delete(deletedProductWorkflow);
    }
    this.productHistoryRepository.saveAll(addedProductHistories);
    return isActive;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void create(String storeId, String productCode) throws ApplicationException, ApplicationRuntimeException {
    ProductDetailResponse productData =
        this.productRepository.findProductDetailByProductCode(productCode);
    List<ProductWorkflow> productWorkflows =
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId,
            productData.getId());
    List<Integer> workflowStates = generateWorkflowStates(productWorkflows);
    if (!workflowStates.isEmpty()) {
      throw new ApplicationException(
          ErrorCategory.INVALID_STATE,
          "Product Code : "
              + productCode
              + ". Error Code : state of product workflow is invalid. Please refresh the page and try again");
    }
    ProductWorkflow addedProductWorkflow =
        new ProductWorkflow(productData.getId(), ProductWorkflowLookup.STATE_DRAFT,
            ProductWorkflowLookup.STATE_DRAFT_DESCRIPTION, null, null, storeId);
    ProductHistory addedProductHistory =
        new ProductHistory(productData.getId(), ProductWorkflowLookup.STATE_DRAFT,
            ProductWorkflowLookup.STATE_DRAFT_DESCRIPTION,
            ProductWorkflowLookup.STATE_CREATE_TO_DRAFT_NOTES.replace("\\{name\\}",
                productData.getName()), null, null, storeId);
    this.productWorkflowRepository.saveAndFlush(addedProductWorkflow);
    this.productHistoryRepository.saveAndFlush(addedProductHistory);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void submit(String storeId, String productCode) throws Exception {
    ProductDetailResponse productData =
        this.productRepository.findProductDetailByProductCode(productCode);
    List<ProductWorkflow> productWorkflows =
        this.productWorkflowRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId,
            productData.getId());
    List<Integer> workflowStates = generateWorkflowStates(productWorkflows);
    if (workflowStates.isEmpty() || !workflowStates.contains(ProductWorkflowLookup.STATE_DRAFT)) {
      throw new ApplicationException(
          ErrorCategory.INVALID_STATE,
          "Product Code : "
              + productCode
              + ". Error Code : state of product workflow is invalid. Please refresh the page and try again");
    }
    ProductWorkflow deletedProductWorkflow =
        getProductWorkflowByState(productWorkflows, ProductWorkflowLookup.STATE_DRAFT);
    List<ProductWorkflow> addedProductWorkflows = new ArrayList<ProductWorkflow>();
    addedProductWorkflows.add(new ProductWorkflow(productData.getId(),
        ProductWorkflowLookup.STATE_REVIEW_CONTENT,
        ProductWorkflowLookup.STATE_REVIEW_CONTENT_DESCRIPTION, null, null, storeId));
    addedProductWorkflows.add(new ProductWorkflow(productData.getId(),
        ProductWorkflowLookup.STATE_REVIEW_IMAGE,
        ProductWorkflowLookup.STATE_REVIEW_IMAGE_DESCRIPTION, null, null, storeId));
    List<ProductHistory> addedProductHistories = new ArrayList<ProductHistory>();
    addedProductHistories.add(new ProductHistory(productData.getId(),
        ProductWorkflowLookup.STATE_REVIEW_CONTENT,
        ProductWorkflowLookup.STATE_REVIEW_CONTENT_DESCRIPTION,
        ProductWorkflowLookup.STATE_DRAFT_TO_REVIEW_CONTENT_NOTES, null, null, storeId));
    addedProductHistories.add(new ProductHistory(productData.getId(),
        ProductWorkflowLookup.STATE_REVIEW_IMAGE,
        ProductWorkflowLookup.STATE_REVIEW_IMAGE_DESCRIPTION,
        ProductWorkflowLookup.STATE_DRAFT_TO_REVIEW_IMAGE_NOTES, null, null, storeId));
    this.productWorkflowRepository.delete(deletedProductWorkflow);
    this.productWorkflowRepository.saveAll(addedProductWorkflows);
    this.productHistoryRepository.saveAll(addedProductHistories);
  }

  @Override
  public Map<String, List<ProductWf>> getProductWfByProductCodes(List<String> productCodes) {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    List<ProductWf> productWorkflows = new ArrayList<>();
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productCodes),
        Constants.REQUIRED_PRODUCT_CODES);
    productWorkflows =
        this.productWfRepository.findAllByStoreIdAndProductCodeInAndMarkForDeleteFalse(storeId,
            productCodes);
    return productWorkflows.stream()
        .collect(Collectors.groupingBy(productWorkflow -> productWorkflow.getProductCode(),
            Collectors.toList()));

  }

  @Override
  public Map<String, String> getRejectedNotesByProductIds(List<String> productIds) {
    List<Object[]> resultList = productWorkflowRepository.findByStoreIdAndProductIdIn(productIds);
    if (CollectionUtils.isEmpty(resultList)) {
      return new HashMap<>();
    }
    Map<String, String> productIdNotesMap = new HashMap<>();
    for (Object[] object : resultList) {
      productIdNotesMap.put((String) object[0], (String) object[1]);
    }
    return productIdNotesMap;
  }

  @Override
  public void deleteProductWorkflowByStoreIdAndProductCode(String storeId, String productCode){
    productWfRepository.deleteByStoreIdAndProductCode(storeId, productCode);
  }
}
