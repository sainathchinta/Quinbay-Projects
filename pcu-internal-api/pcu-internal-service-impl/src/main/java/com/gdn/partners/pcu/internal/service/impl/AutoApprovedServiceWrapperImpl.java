package com.gdn.partners.pcu.internal.service.impl;

import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.internal.service.impl.event.model.BulkReviewUploadModel;
import com.gdn.partners.pcu.internal.service.impl.exception.ConstraintViolationException;
import com.gdn.partners.pcu.internal.service.model.BulkInternalProcessType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.AutoApprovedAllProductsDownloadRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.AutoApprovedSelectedProductsDownloadRequest;
import com.gdn.partners.pcu.internal.streaming.model.bulk.FileType;
import com.gdn.partners.pcu.internal.web.model.request.AutoApprovedProductsDownloadWebRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.internal.client.feign.ProductAnalyticsFeign;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedSelectedDownloadRequest;
import com.gdn.partners.pcu.internal.client.model.response.AutoApprovedListWebResponse;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.web.model.response.ProductDetailWebResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.properties.KafkaTopicProperties;
import com.gdn.partners.pcu.internal.service.PBPOutboundService;
import com.gdn.partners.pcu.internal.service.AutoApprovedServiceWrapper;
import com.gdn.partners.pcu.internal.service.ProductService;
import com.gdn.partners.pcu.internal.service.ProductServiceWrapper;
import com.gdn.partners.pcu.internal.service.impl.event.model.DeleteAutoApprovedProductsEventModel;
import com.gdn.partners.pcu.internal.service.impl.event.model.InternalHistoryEventModel;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.util.ConverterUtil;
import com.gdn.partners.pcu.internal.web.model.enums.AutoApprovedActionsEnum;
import com.gdn.partners.pcu.internal.web.model.request.AutoApprovedProductsActionWebRequest;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.Collections;

@Service
@Slf4j
public class AutoApprovedServiceWrapperImpl implements AutoApprovedServiceWrapper {

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Autowired
  private PBPOutboundService pbpOutboundService;

  @Autowired
  private ProductService productService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private ProductAnalyticsFeign productAnalyticsFeign;

  @Autowired
  private FileStorageService fileStorageService;

  @Value("${product.name.trim}")
  private boolean productNameTrim;

  @Override
  public void performActionOnAutoApprovedProducts(String productCode,
      AutoApprovedProductsActionWebRequest autoApprovedProductsActionWebRequest) throws Exception {
    RequestHelper.checkParameter(AutoApprovedActionsEnum.validAction(autoApprovedProductsActionWebRequest.getAction()),
        ErrorMessages.INVALID_ACTION_FOR_AUTO_APPROVED_PRODUCTS);
    String internalHistoryActivity;
    String errorMessage = null;
    if (AutoApprovedActionsEnum.SUSPEND.name().equals(autoApprovedProductsActionWebRequest.getAction())) {
      RequestHelper.checkParameter(StringUtils.isNotBlank(autoApprovedProductsActionWebRequest.getReason()),
          ErrorMessages.REASON_CANNOT_BE_EMPTY_FOR_SUSPENSION_OF_AUTO_APPROVED_PRODUCT);
      RequestHelper.checkParameter(StringUtils.isNotBlank(autoApprovedProductsActionWebRequest.getNotes()),
          ErrorMessages.NOTES_CANNOT_BE_EMPTY_FOR_SUSPENSION_OF_AUTO_APPROVED_PRODUCT);
      internalHistoryActivity = Constants.SUSPENDING_AUTO_APPROVED_PRODUCTS;
      errorMessage = validateAndSuspendProduct(productCode, autoApprovedProductsActionWebRequest, errorMessage);
    } else {
      internalHistoryActivity = Constants.ACCEPTING_AUTO_APPROVED_PRODUCTS;
      if (autoApprovedProductsActionWebRequest.isContentUpdated()) {
        this.productServiceWrapper.updateProduct(clientParameterHelper.getRequestId(),
            clientParameterHelper.getUserType(),
            ConverterUtil.toProductRequest(autoApprovedProductsActionWebRequest.getProductWebRequest(),
                clientParameterHelper, productNameTrim, false), false, true);
      }
    }
    kafkaProducer.send(kafkaTopicProperties.getDeleteAutoApprovedProductEventName(), productCode,
        DeleteAutoApprovedProductsEventModel.builder().productCode(productCode)
            .action(autoApprovedProductsActionWebRequest.getAction()).timestamp(System.currentTimeMillis()).build());
    if (StringUtils.isNotBlank(errorMessage)) {
      throw new ConstraintViolationException(errorMessage);
    }
    kafkaProducer.send(kafkaTopicProperties.getInternalHistoryEventName(), productCode,
        InternalHistoryEventModel.builder().productCode(productCode).activity(internalHistoryActivity)
            .storeId(clientParameterHelper.getStoreId()).notes(Constants.AUTO_APPROVED_PRODUCT_REVIEWED)
            .username(clientParameterHelper.getUsername()).timestamp(System.currentTimeMillis()).build());
  }

  private String validateAndSuspendProduct(String productCode,
      AutoApprovedProductsActionWebRequest autoApprovedProductsActionWebRequest, String errorMessage) {
    String productSku = pbpOutboundService.getProductSkuByProductCode(productCode);
    if (StringUtils.isBlank(productSku)) {
      errorMessage = ErrorMessages.ERR_PRODUCT_SKU_EMPTY;
      return errorMessage;
    }
    ProductBasicResponse productBasicResponse =
        productService.getProductBasicDetails(List.of(productSku)).stream().findFirst().orElse(null);
    if (Objects.isNull(productBasicResponse) || !productBasicResponse.isProductExists() || (
        productBasicResponse.isMarkForDelete() && !productBasicResponse.isSuspended())) {
      errorMessage = ErrorMessages.PRODUCT_STATE_INVALID;
    } else {
      productService.doSuspensionAction(
          RequestHelper.toSuspensionProductBulkActionsWebRequest(productSku, autoApprovedProductsActionWebRequest));
    }
    return errorMessage;
  }

  @Override
  public ProductDetailWebResponse fetchAutoApprovedProductDetail(String productCode,
      String clientId) {
    ProductDetailWebResponse productDetailWebResponse = new ProductDetailWebResponse();
    try {
      productDetailWebResponse = productService.getProductDetail(productCode, false, clientId, false);
    } catch (Exception e) {
      log.error("Failed to fetch product details for productCode = {} error - ", productCode, e);
      productDetailWebResponse.setProductCode(productCode);
      productDetailWebResponse.setMarkForDelete(true);
    }
    if (productDetailWebResponse.isMarkForDelete()) {
      kafkaProducer.send(kafkaTopicProperties.getDeleteAutoApprovedProductEventName(), productCode,
          DeleteAutoApprovedProductsEventModel.builder().productCode(productCode)
              .action(AutoApprovedActionsEnum.AUTO_HEAL.name()).build());
    } else {
      AutoApprovedSelectedDownloadRequest request = AutoApprovedSelectedDownloadRequest.builder()
          .productCodes(Collections.singletonList(productCode)).build();
      GdnRestListResponse<AutoApprovedListWebResponse> response =
          productAnalyticsFeign.fetchAutoApprovedSelectedProductsList(request);
      ResponseHelper.validateMasterSkuResponse(response);
      if (CollectionUtils.isNotEmpty(response.getContent())) {
        AutoApprovedListWebResponse content = response.getContent().stream().findFirst().get();
        productDetailWebResponse.setAssignedTo(content.getAssignedTo());
        productDetailWebResponse.setOfficialSeller(content.getSeller().isOfficial());
        productDetailWebResponse.setBusinessPartnerName(content.getSeller().getSellerName());
        productDetailWebResponse.setSourceEn(content.getSourceEn());
        productDetailWebResponse.setSourceId(content.getSourceId());
        productDetailWebResponse.setReason(content.getReason());
      }
    }
    return productDetailWebResponse;
  }

  @Override
  public void downloadItemsForAutoApprovedProducts(String username,
      AutoApprovedProductsDownloadWebRequest request) {
    String requestId = UUID.randomUUID().toString();
    log.info("Invoking auto approved products download with requestId: {},", requestId);
    Boolean b2bActivated = null;
    if (request.isB2bActivated() && !request.isB2cActivated()) {
      b2bActivated = true;
    } else if (!request.isB2bActivated() && request.isB2cActivated()) {
      b2bActivated = false;
    }
    String fileName = requestId + Constants.DOT + FileType.XLSX.name().toLowerCase();
    if (CollectionUtils.isEmpty(request.getProductCodeList())) {
      AutoApprovedAllProductsDownloadRequest autoApprovedAllProductsDownloadRequest =
          RequestHelper.toAutoApprovedProductsAllDownloadRequest(username, request, fileName,
              requestId);
      autoApprovedAllProductsDownloadRequest.setB2bActivated(b2bActivated);
      this.kafkaProducer.send(DomainEventName.BULK_DOWNLOAD_ALL_EVENT, username,
          autoApprovedAllProductsDownloadRequest);
    } else {
      AutoApprovedSelectedProductsDownloadRequest autoApprovedSelectedProductsDownloadRequest =
          RequestHelper.toAutoApprovedProductsSelectedDownloadRequest(username, request, fileName,
              requestId);
      this.kafkaProducer.send(DomainEventName.BULK_DOWNLOAD_ALL_EVENT, username,
          autoApprovedSelectedProductsDownloadRequest);
    }
  }

  @Override
  public void uploadBulkAssignFile(MultipartFile file, String requestId, String storeId,
      String username, String vendorCode) throws Exception {
    String baseDirPath = fileStorageService.uploadFilePath(file, requestId,
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.getValue());
    BulkReviewUploadModel bulkReviewUploadModel = RequestHelper.toBulkReviewUploadModel(storeId,
        new StringBuilder(baseDirPath).append(file.getOriginalFilename()).toString(),
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.getValue(), requestId,
      username, vendorCode);
    log.info("Publishing event {} for bulkProcessCode = {} bulkReviewUploadModel = {} ",
        DomainEventName.BULK_REVIEW_UPLOAD_EVENT, bulkReviewUploadModel.getBulkProcessCode(),
        bulkReviewUploadModel);
    kafkaProducer.send(DomainEventName.BULK_REVIEW_UPLOAD_EVENT,
        bulkReviewUploadModel.getBulkProcessCode(), bulkReviewUploadModel);
  }
}
