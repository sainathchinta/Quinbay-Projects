package com.gdn.partners.pbp.service.productlevel1;

import java.util.List;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.NeedRevisionNotes;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductFieldHistory;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductHistoryRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.util.ProductResubmitChangeUtil;
import com.gdn.mta.product.util.ProductWorkflowLookup;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.dto.workflow.product.ProductResubmitRequest;
import com.gdn.partners.pbp.workflow.WorkflowProcessCode;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductLevel1HistoryServiceBean implements ProductLevel1HistoryService {
  private static final String NOTES_PREFIX = "Product ";
  private static final String BULK = "bulk ";

  @Autowired
  private ProductHistoryRepository productLevel1HistoryRepository;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private ProductStatusPublisherService productStatusPublisherService;

  @Autowired
  private ProductRepository productRepository;

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Trace(dispatcher = true)
  @Async
  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void createForNeedRevision(String productCode, String processCode, String notes,
      NeedRevisionNotes revisionNotes, String username) throws Exception {
    ProductHistory productLevel1History = this.getProductHistory(productCode, processCode, notes, username);
    log.info("history creation for need revision {}, {}", productLevel1History, revisionNotes);
    if (Objects.nonNull(revisionNotes)) {
      productLevel1History.setCreatedBy(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
      productLevel1History.setNeedCorrectionNotes(objectMapper.writeValueAsString(revisionNotes));
    }
    if (Objects.nonNull(productLevel1History.getProductId())) {
      this.productLevel1HistoryRepository.save(productLevel1History);
    }
  }

  @Trace(dispatcher = true)
  @Async
  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void create(String productCode, String processCode, String notes) {
    ProductHistory productLevel1History = this.getProductHistory(productCode, processCode, notes,
        MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER));
    if (Objects.nonNull(productLevel1History.getProductId())) {
      this.productLevel1HistoryRepository.save(productLevel1History);
    }
  }

  private ProductHistory getProductHistory(String productCode, String processCode, String notes, String username) {
    String storeId = mandatoryParameterHelper.getStoreId();
    if (StringUtils.isEmpty(storeId)) {
      log.warn("StoreId is empty, for approving image of productCode : {}", productCode);
      storeId = Constants.DEFAULT_STORE_ID;
    }
    log.info("Saving product level history for productCode : {} with notes : {} and activity : {}", productCode, notes,
        processCode);
    if (StringUtils.isEmpty(username)) {
      log.warn("Username is empty, for activate the product of productCode : {}", productCode);
      username = Constants.DEFAULT_USERNAME;
      MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    }
    ProductHistory productLevel1History = new ProductHistory();
    if (StringUtils.isNoneBlank(username)) {
      ProductCollection productCollection =
          this.productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
      if (productCollection.isSkipReview()) {
        username = Constants.DEFAULT_USERNAME;
        MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
      }
      MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
      productLevel1History.setStoreId(storeId);
      productLevel1History.setProductId(productCollection.getProductId());
      productLevel1History.setState(-1);
      productLevel1History
          .setDescription(processCode == null ? processCode : WorkflowProcessCode.valueOf(processCode).getDesc());
      productLevel1History.setNotes(notes);
    }
    return productLevel1History;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void create(ProductCollection productCollection, String processCode, String notes) {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    String username = GdnMandatoryRequestParameterUtil.getUsername();
    if (StringUtils.isNoneBlank(username)) {
      ProductHistory productLevel1History = new ProductHistory();
      productLevel1History.setStoreId(storeId);
      productLevel1History.setProductId(productCollection.getProductId());
      productLevel1History.setState(-1);
      productLevel1History.setDescription(
          processCode == null ? processCode : WorkflowProcessCode.valueOf(processCode).getDesc());
      productLevel1History.setNotes(notes);
      this.productLevel1HistoryRepository.save(productLevel1History);
    }
  }

  @Override
  public List<ProductHistory> findByStoreIdAndProductIdAndMarkForDeleteFalse(String storeId, String productId) {
    return productLevel1HistoryRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId, productId);
  }

  @Trace(dispatcher=true)
  @Async
  @Override
  @Transactional(readOnly = false)
  public void addProductHistoryForProductAssignment(List<String> productCodes, String assignedTo, String assignedBy) {
    String notesPrefix = NOTES_PREFIX;
    if (productCodes.size() > 1) {
      notesPrefix = notesPrefix.concat(BULK);
    }
    for (String productCode : productCodes) {
      if (Constants.DEFAULT_ASSIGNEE.equals(assignedTo) || StringUtils.isBlank(assignedTo)) {
        saveProductHistory(productCode, assignedBy, SaveHistoryConstants.PRODUCT_UNASSIGNED,
             notesPrefix + "unassigned by : " + assignedBy);
      } else {
        saveProductHistory(productCode, assignedBy, SaveHistoryConstants.PRODUCT_ASSIGNED,
            notesPrefix + "assigned to : " + assignedTo + ", and was assigned by : " + assignedBy);
      }
    }
  }

  @Trace(dispatcher=true)
  @Async
  @Override
  @Transactional(readOnly = false)
  public void saveProductHistory(String productCode, String updatedBy, String description, String notes) {
    log.info("Saving product history for productCode : {} ", productCode);
    productService.saveProductHistory(Constants.DEFAULT_STORE_ID, productCode, updatedBy, description, notes);
  }

  @Trace(dispatcher=true)
  @Async
  @Override
  @Transactional(readOnly = false)
  public void addHistoryForProductResubmitted(ProductCreationRequest productCreationRequest) throws Exception {
    String changedFields = getChangedFields(productCreationRequest);
    saveProductHistory(productCreationRequest.getProductCode(), productCreationRequest.getUpdatedBy(),
        SaveHistoryConstants.PRODUCT_RESUBMITTED, changedFields);
  }

  @Trace(dispatcher=true)
  @Async
  @Override
  @Transactional(readOnly = false)
  public void addHistoryForProductResubmissionDueToContentOrImageRejection(ProductResubmitRequest productResubmitRequest)
      throws Exception {
    String changedFields = getChangedFields(productResubmitRequest);
    saveProductHistory(productResubmitRequest.getProductRequest().getProductCode(),
        productResubmitRequest.getProductRequest().getUpdatedBy(), SaveHistoryConstants.PRODUCT_RESUBMITTED,
        changedFields);
  }

  @Trace(dispatcher=true)
  @Override
  @Async
  @Transactional(readOnly = false)
  public void updateProductIdForRevisedProducts(String oldProductId, String newProductId) {
    int rowsEffected = this.productLevel1HistoryRepository
        .updateProductForResubmittedProducts(oldProductId, newProductId,
            WorkflowProcessCode.RETURN_FOR_CORRECTION.getDesc());
    if (rowsEffected <= 0) {
      log.error("Error while updating product histories of revised products, oldProductId:{}, newProductId:{}",
          oldProductId, newProductId);
    }
  }

  /**
   *
   * Get fields which got changed
   *
   * @param productCreationRequest
   * @return
   * @throws Exception
   */
  private String getChangedFields(ProductCreationRequest productCreationRequest) throws Exception {
    ProductDetailResponse savedProduct =
        this.productRepository.findProductDetailByProductCode(productCreationRequest.getProductCode());
    if (Objects.isNull(savedProduct)) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "Product not found for productCode : " + productCreationRequest.getProductCode());
    }
    List<ProductFieldHistory> changedFieldList =
        ProductResubmitChangeUtil.getProductDiff(productCreationRequest, savedProduct);
      String changedFields = changedFieldList.toString();
      changedFields = ProductWorkflowLookup.STATE_EDIT_DESCRIPTION + " : " + changedFields;
    return StringUtils.abbreviate(changedFields, 2000);
  }

  /**
   *
   * Get fields which got changed on resubmission
   *
   * @param productResubmitRequest
   * @return
   * @throws Exception
   */
  private String getChangedFields(ProductResubmitRequest productResubmitRequest) throws Exception {
    ProductDetailResponse savedProduct =
        this.productRepository.findProductDetailByProductCode(productResubmitRequest.getProductRequest().getProductCode());
    if (Objects.isNull(savedProduct)) {
      throw new ApplicationException(ErrorCategory.DATA_NOT_FOUND,
          "Product not found for productCode : " + productResubmitRequest.getProductRequest().getProductCode());
    }
    List<ProductFieldHistory> changedFieldList =
        ProductResubmitChangeUtil.getProductDiff(productResubmitRequest, savedProduct);
    String changedFields = changedFieldList.toString();
    changedFields = ProductWorkflowLookup.STATE_EDIT_DESCRIPTION + " : " + changedFields;
    return StringUtils.abbreviate(changedFields, 2000);
  }

  @Override
  public void deleteProductHistoryByStoreIdAndProductId(String storeId, String productId){
    productLevel1HistoryRepository.deleteByStoreIdAndProductId(storeId, productId);
  }
}
