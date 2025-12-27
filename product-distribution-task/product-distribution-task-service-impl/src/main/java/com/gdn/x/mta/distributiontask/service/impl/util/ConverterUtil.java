package com.gdn.x.mta.distributiontask.service.impl.util;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gda.mta.product.dto.ProductDataAutoFixHistoryDto;
import com.gda.mta.product.dto.ProductLevel3Request;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gda.mta.product.dto.response.ProductDataAutoFixHistoryListRequest;
import com.gdn.x.message.mq.model.MessageEmailRequest;
import com.gdn.x.mta.distributiontask.domain.event.InternalHistoryEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.AddProductToIprSolrEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.IPRHistoryEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductEmailEventModel;
import com.gdn.x.mta.distributiontask.model.IPRHistory;
import com.gdn.x.mta.distributiontask.model.ProductIPR;
import com.gdn.x.mta.distributiontask.model.dto.IprActionRequest;
import com.gdn.x.mta.distributiontask.model.dto.SellerAnalyticsResponse;
import com.gdn.x.mta.distributiontask.model.dto.StuckProductsDTO;
import com.gdn.x.mta.distributiontask.model.dto.SummaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.enums.ProductSourceIPR;
import com.gdn.x.mta.distributiontask.model.enums.ProductStateIPR;
import com.gdn.x.mta.distributiontask.model.solr.IPRProductSolr;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import com.gdn.x.mta.distributiontask.rest.model.request.BoostedProductFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.BrandReportDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.EvidenceRequestedDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.EvidenceSubmittedDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.HistoricalSellerDataResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.IprProductDetailsResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.IprProductListResponse;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AutoQcConfigChangeRequest;
import com.gda.mta.product.dto.ChangedFieldDto;
import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.ProductHistoryRequest;
import com.gdn.mta.domain.event.modal.AddEditedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.AddRevisedProductToPDTEvent;
import com.gdn.mta.domain.event.modal.ScreeningProductApprovalEvent;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTAutoApprovalEventModel;
import com.gdn.x.mta.distributiontask.inbound.util.ProductDomainEventModelConverterUtils;
import com.gdn.x.mta.distributiontask.model.AutoQcConfigChange;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAutoApproval;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.ReportProduct;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.mta.distributiontask.model.enums.AutoApprovalStatus;
import com.gdn.x.mta.distributiontask.response.StuckProductsResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.ReportProductRequest;
import com.gdn.x.mta.distributiontask.util.NotesAndRejectReason;
import com.gdn.x.mta.distributiontask.util.RejectReasonRequest;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import lombok.extern.slf4j.Slf4j;


@Slf4j
public class ConverterUtil {

  private static ObjectMapper objectMapper = new ObjectMapper();

  public static ReportProduct convertToReportProduct(String storeId, ReportProductRequest reportProductRequest) {
    ReportProduct reportProduct = ReportProduct.builder().memberId(reportProductRequest.getMemberId())
        .itemSku(reportProductRequest.getItemSku()).reason(reportProductRequest.getReason())
        .notes(reportProductRequest.getNotes()).build();
    reportProduct.setStoreId(storeId);
    reportProduct.setMarkForDelete(false);
    return reportProduct;
  }

  public static Product convertProductDetailResponseToProduct(ProductDetailResponse productDetailResponse,
      AddEditedProductToPDTEvent addEditedProductToPDTEvent, ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse)
      throws JsonProcessingException {
    ScreeningProductApprovalEvent merchantDetails = new ScreeningProductApprovalEvent();
    merchantDetails.setMerchantName(addEditedProductToPDTEvent.getMerchantName());
    merchantDetails.setMerchantCode(addEditedProductToPDTEvent.getMerchantCode());
    merchantDetails.setPostLive(addEditedProductToPDTEvent.isPostLive());
    merchantDetails.setRestrictedKeywordsPresent(addEditedProductToPDTEvent.isRestrictedKeywordsPresent());
    merchantDetails.setRestrictedKeywordsDetected(addEditedProductToPDTEvent.getRestrictedKeywordsDetected());
    merchantDetails.setTrustedSeller(addEditedProductToPDTEvent.isTrustedSeller());
    merchantDetails.setB2bActivated(addEditedProductToPDTEvent.isB2bActivated());
    merchantDetails.setB2cActivated(addEditedProductToPDTEvent.isB2cActivated());
    merchantDetails.setPriceInfo(addEditedProductToPDTEvent.getPriceInfo());
    merchantDetails.setDistributionMappingStatus(
        addEditedProductToPDTEvent.getDistributionMappingStatus());
    merchantDetails.setProductCreationType(addEditedProductToPDTEvent.getProductCreationType());
    return ProductDomainEventModelConverterUtils
        .convertProductDomainEventModelToProduct(productDetailResponse, merchantDetails, false,
          imageQcProcessedAndBrandResponse);
  }

  public static Product convertProductDetailResponseToProduct(ProductDetailResponse productDetailResponse,
      AddRevisedProductToPDTEvent addRevisedProductToPDTEvent, ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse)
      throws JsonProcessingException {
    ScreeningProductApprovalEvent merchantDetails = new ScreeningProductApprovalEvent();
    merchantDetails.setMerchantName(addRevisedProductToPDTEvent.getMerchantName());
    merchantDetails.setMerchantCode(addRevisedProductToPDTEvent.getMerchantCode());
    merchantDetails.setPostLive(addRevisedProductToPDTEvent.isPostLive());
    merchantDetails.setTrustedSeller(addRevisedProductToPDTEvent.isTrustedSeller());
    merchantDetails.setRestrictedKeywordsPresent(addRevisedProductToPDTEvent.isRestrictedKeywordsPresent());
    merchantDetails.setRestrictedKeywordsDetected(addRevisedProductToPDTEvent.getRestrictedKeywordsDetected());
    merchantDetails.setB2cActivated(addRevisedProductToPDTEvent.isB2cActivated());
    merchantDetails.setB2bActivated(addRevisedProductToPDTEvent.isB2bActivated());
    merchantDetails.setPriceInfo(addRevisedProductToPDTEvent.getPriceInfo());
    merchantDetails.setDistributionMappingStatus(
        addRevisedProductToPDTEvent.getDistributionMappingStatus());
    merchantDetails.setProductCreationType(addRevisedProductToPDTEvent.getProductCreationType());
    return ProductDomainEventModelConverterUtils
        .convertProductDomainEventModelToProduct(productDetailResponse, merchantDetails, false,
          imageQcProcessedAndBrandResponse);
  }

  public static ProductHistoryRequest getProductHistoryRequestForAutoApproval(String notes, String activity,
      Product product) {
    Date date = new Date();
    ProductHistoryRequest productHistoryRequest = new ProductHistoryRequest();
    productHistoryRequest.setProductCode(product.getProductCode());
    productHistoryRequest.setDescription(activity);
    productHistoryRequest.setNotes(notes);
    productHistoryRequest.setState(Constants.INTEGER_FIVE);
    productHistoryRequest.setStoreId(product.getStoreId());
    productHistoryRequest.setCreatedDate(date);
    productHistoryRequest.setUpdatedDate(date);
    productHistoryRequest.setCreatedBy(Constants.SYSTEM);
    productHistoryRequest.setUpdatedBy(Constants.SYSTEM);
    return productHistoryRequest;
  }

  public static PDTAutoApprovalEventModel toPDTAutoApprovalEventModel(String productCode) {
    PDTAutoApprovalEventModel pdtAutoApprovalEventModel = new PDTAutoApprovalEventModel(productCode);
    return pdtAutoApprovalEventModel;
  }

  public static void setReviewAutoApprovalDetails(Date date, ProductReviewer productReviewer){
    productReviewer.setApproverAssignee(Constants.AUTO_APPROVED);
    productReviewer.setAssignedDate(date);
    productReviewer.setApprovedDate(date);
  }

  public static NotesAndRejectReason getNotesAndRejectReason(RejectProductDTO rejectProductDTO) {
    NotesAndRejectReason notesAndRejectReason = new NotesAndRejectReason();
    notesAndRejectReason.setNotes(rejectProductDTO.getNotes());
    RejectReasonRequest rejectReasonRequest = new RejectReasonRequest();
    BeanUtils.copyProperties(rejectProductDTO.getRejectReasonDto(), rejectReasonRequest);
    notesAndRejectReason.setRejectReasonRequest(rejectReasonRequest);
    return notesAndRejectReason;
  }

  public static ProductAutoApproval toProductAutoApproval(Product product) {
    ProductAutoApproval productAutoApproval = ProductAutoApproval.builder().productCode(product.getProductCode())
        .autoApprovalStatus(AutoApprovalStatus.PENDING).retryCount(0).build();
    productAutoApproval.setStoreId(product.getStoreId());
    return productAutoApproval;
  }

  public static AutoQcConfigChangeRequest toAutoQcConfigChangeRequest(AutoQcConfigChange autoQcConfigChange)
      throws IOException {
    AutoQcConfigChangeRequest autoQcConfigChangeRequest = new AutoQcConfigChangeRequest();
    autoQcConfigChangeRequest.setSellerCode(autoQcConfigChange.getSellerCode());
    autoQcConfigChangeRequest.setCategoryCode(autoQcConfigChange.getC1CategoryCode());
    autoQcConfigChangeRequest.setNewSeller(autoQcConfigChange.isNewSeller());
    autoQcConfigChangeRequest.setChangedFields(objectMapper.readValue(autoQcConfigChange.getChangedFields(),
        new TypeReference<HashMap<String, ChangedFieldDto>>() {
        }));
    return autoQcConfigChangeRequest;
  }

  public static Product setProductRetryStatus(Product product,
    ProductRetryStatusUpdate productRetryStatusUpdate) {
    if (StringUtils.isNotEmpty(productRetryStatusUpdate.getState())) {
      product.setState(WorkflowState.valueOf(productRetryStatusUpdate.getState()));
    }
    if (Objects.nonNull(productRetryStatusUpdate.getEdited())) {
      product.setEdited(productRetryStatusUpdate.getEdited());
    }
    if (Objects.nonNull(productRetryStatusUpdate.getRevised())) {
      product.setRevised(productRetryStatusUpdate.getRevised());
    }
    if (Objects.nonNull(productRetryStatusUpdate.getMarkForDelete())) {
      product.setMarkForDelete(productRetryStatusUpdate.getMarkForDelete());
    }
    if (StringUtils.isNotEmpty(productRetryStatusUpdate.getReviewType())) {
      product.setReviewType(ReviewType.valueOf(productRetryStatusUpdate.getReviewType()));
    }
    return product;
  }

  public static MessageEmailRequest getStuckProductMessageEmailRequest(
      int batchSize, List<StuckProductsDTO> productsAboveQcRetryCount, String sendStuckProductSummaryEmailAddress,
      String sendStuckProductSummaryEmailAddressCc) {
    List<StuckProductsResponse> stuckProductsResponsesList = productsAboveQcRetryCount.stream()
        .map(p -> new StuckProductsResponse(p.getProductCode(), p.getStateAsString(),
            p.getCreatedDateAsString().split(Constants.DOT)[0], p.getUpdatedDateAsString().split(Constants.DOT)[0]))
        .collect(Collectors.toList());
    Map<String, Object> mailObjectWrapper = new HashMap<>();
    Map<String, Object> emailObject = new HashMap<>();
    mailObjectWrapper.put(Constants.OBJECT, stuckProductsResponsesList);
    mailObjectWrapper.put(Constants.TOTAL, productsAboveQcRetryCount.size());
    mailObjectWrapper.put(Constants.BATCH_SIZE, batchSize);
    emailObject.put(Constants.EMAIL_OBJECT, mailObjectWrapper);
    MessageEmailRequest email = new MessageEmailRequest();
    email.setMessageId(Constants.RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID);
    email.setMessageTo(sendStuckProductSummaryEmailAddress);
    email.setMessageCc(sendStuckProductSummaryEmailAddressCc);
    email.setMessageIdentifierKey(Constants.RETRY_PRODUCT_ACTIVATION_TEMPLATE_ID);
    email.setMessageIdentifierValue(UUID.randomUUID().toString());
    email.setVariables(emailObject);
    return email;
  }

  public static MessageEmailRequest getResetDeltaReindexMessageEmailRequest(String resetLastUpdatedDeltaReindexEmailAddress,
          String resetLastUpdatedDeltaReindexEmailAddressCc, String message) {
    Map<String, Object> emailObject = new HashMap<>();
    emailObject.put(Constants.EMAIL_OBJECT, message);
    MessageEmailRequest email = new MessageEmailRequest();
    email.setMessageId(Constants.RESET_LAST_UPDATED_DATE_DELTA_REINDEX_TEMPLATE_ID);
    email.setMessageTo(resetLastUpdatedDeltaReindexEmailAddress);
    email.setMessageCc(resetLastUpdatedDeltaReindexEmailAddressCc);
    email.setMessageIdentifierKey(Constants.RESET_LAST_UPDATED_DATE_DELTA_REINDEX_TEMPLATE_ID);
    email.setMessageIdentifierValue(UUID.randomUUID().toString());
    email.setVariables(emailObject);
    return email;
  }

  public static ProductDataAutoFixHistoryDto setProductDataAutoFixHistoryDto(String productCode, String type,
      String additionalInfo) {
    ProductDataAutoFixHistoryDto productDataAutoFixHistoryDto = new ProductDataAutoFixHistoryDto();
    productDataAutoFixHistoryDto.setType(type);
    productDataAutoFixHistoryDto.setProductCode(productCode);
    productDataAutoFixHistoryDto.setAdditionalInfo(additionalInfo);
    return productDataAutoFixHistoryDto;
  }

  public static ProductDataAutoFixHistoryListRequest convertToProductDataAutoFixHistoryListRequest(ProductDataAutoFixHistoryDto productDataAutoFixHistoryDto) {
    List<ProductDataAutoFixHistoryDto> productDataAutoFixHistoryDtoList = new ArrayList<>();
    productDataAutoFixHistoryDtoList.add(productDataAutoFixHistoryDto);
    return new ProductDataAutoFixHistoryListRequest(productDataAutoFixHistoryDtoList);
  }

  public static SummaryFilterDTO convertBoostedFilterRequestToPrimaryFilterDTO(BoostedProductFilterRequest boostedProductFilterRequest) {
      SummaryFilterDTO summaryFilterDTO = new SummaryFilterDTO();
      BeanUtils.copyProperties(boostedProductFilterRequest, summaryFilterDTO, "keyword",
        "assignment");
      summaryFilterDTO.setTimeFilterType(TimeFilterType.getTimeFilterTypeByValue(
        boostedProductFilterRequest.getTimeFilterWebType()));
      if(StringUtils.isNotEmpty(boostedProductFilterRequest.getFaultyType())) {
        summaryFilterDTO.setFaultyImageType(boostedProductFilterRequest.getFaultyType());
      }
      return summaryFilterDTO;
    }

  public static boolean isCategoryDataToBeUpdated(String productCode, AutoApprovalTypeResponse autoApprovalTypeResponse,
      Product product) {
    boolean categoryDataToBeUpdated = false;
    if (Objects.nonNull(autoApprovalTypeResponse) && StringUtils.isNotEmpty(autoApprovalTypeResponse.getCategoryCode())
        && StringUtils.isNotEmpty(autoApprovalTypeResponse.getCategoryName())
        && !autoApprovalTypeResponse.getCategoryCode().equals(product.getCategoryCode())) {
      log.info("Category updated for product : {} to response {} ", productCode, autoApprovalTypeResponse);
      categoryDataToBeUpdated = true;
      product.setCategoryCode(autoApprovalTypeResponse.getCategoryCode());
      product.setCategoryName(autoApprovalTypeResponse.getCategoryName());
    }
    return categoryDataToBeUpdated;
  }

  public static InternalHistoryEventModel convertToInternalHistoryModel(String storeId,
    String productCode, String userName, String activity, String notes) {
    InternalHistoryEventModel historyEventModel = new InternalHistoryEventModel();
    historyEventModel.setStoreId(storeId);
    historyEventModel.setProductCode(productCode);
    historyEventModel.setUsername(userName);
    historyEventModel.setActivity(activity);
    historyEventModel.setNotes(notes);
    return historyEventModel;
  }

  public static List<IprProductListResponse> toIprProductListResponses(
    List<IPRProductSolr> iprProductSolr) {
    return iprProductSolr.stream().map(ConverterUtil::toIprProductListResponse)
      .collect(Collectors.toList());
  }

  public static IprProductListResponse toIprProductListResponse(IPRProductSolr iprProductSolr) {
    IprProductListResponse iprProductListResponse = new IprProductListResponse();
    BeanUtils.copyProperties(iprProductSolr, iprProductListResponse);
    return iprProductListResponse;
  }

  public static IPRProductSolr convertToIPRProductSolr(ProductIPR productIPR) {
    IPRProductSolr iprProductSolr = new IPRProductSolr();
    BeanUtils.copyProperties(productIPR, iprProductSolr);
    return iprProductSolr;
  }

  public static List<HistoricalSellerDataResponse> constructEmptyHistoricalDataResponse() {
    List<HistoricalSellerDataResponse> historicalSellerDataResponses = new ArrayList<>();
    for (String key : Constants.DEFAULT_KEYS_FOR_SELLER_ANALYTICS) {
      HistoricalSellerDataResponse response = new HistoricalSellerDataResponse(key, 0, null);
      historicalSellerDataResponses.add(response);
    }
    return historicalSellerDataResponses;
  }

  public static List<HistoricalSellerDataResponse> constructHistoricalDataResponse(
      SellerAnalyticsResponse sellerAnalyticsResponse) {
    List<HistoricalSellerDataResponse> historicalDataList = new ArrayList<>();
    addHistoricalData(historicalDataList, Constants.PATENT_KEY,
        sellerAnalyticsResponse.getPatentProductCount(),
        sellerAnalyticsResponse.getLatestFraudDetectedDate());
    addHistoricalData(historicalDataList, Constants.PIRATED_PRODUCT_KEY,
        sellerAnalyticsResponse.getPiratedProductCount(),
        sellerAnalyticsResponse.getLatestFraudDetectedDate());
    addHistoricalData(historicalDataList, Constants.REPACKAGE_PRODUCT_KEY,
        sellerAnalyticsResponse.getRepackageProductCount(),
        sellerAnalyticsResponse.getLatestFraudDetectedDate());
    addHistoricalData(historicalDataList, Constants.ILLEGAL_PRODUCT_KEY,
        sellerAnalyticsResponse.getIllegalProductCount(),
        sellerAnalyticsResponse.getLatestFraudDetectedDate());
    addHistoricalData(historicalDataList, Constants.FAKE_PRODUCT_KEY,
        sellerAnalyticsResponse.getCounterfeitProductCount(),
        sellerAnalyticsResponse.getLatestFraudDetectedDate());
    addHistoricalData(historicalDataList, Constants.UNAUTHORIZED_SELLER_KEY,
        sellerAnalyticsResponse.getUnauthorizedSellerProductCount(),
        sellerAnalyticsResponse.getLatestFraudDetectedDate());
    addHistoricalData(historicalDataList, Constants.COPYRIGHT_KEY,
        sellerAnalyticsResponse.getCopyRightInfringementProductCount(),
        sellerAnalyticsResponse.getLatestFraudDetectedDate());
    addHistoricalData(historicalDataList, Constants.TRADEMARK_KEY,
        sellerAnalyticsResponse.getTrademarkInfringementProductCount(),
        sellerAnalyticsResponse.getLatestFraudDetectedDate());
    addHistoricalData(historicalDataList, Constants.DESIGN_RIGHT_KEY,
        sellerAnalyticsResponse.getDesignRightInfringementProductCount(),
        sellerAnalyticsResponse.getLatestFraudDetectedDate());

    if (sellerAnalyticsResponse.getUnauthorizedResellerProductCount() > 0) {
      addHistoricalData(historicalDataList, Constants.UNAUTHORIZED_RESELLER_KEY,
          sellerAnalyticsResponse.getUnauthorizedResellerProductCount(),
          sellerAnalyticsResponse.getLatestFraudDetectedDate());
    }
    if (sellerAnalyticsResponse.getSuspiciousSkuProductCount() > 0) {
      addHistoricalData(historicalDataList, Constants.SUSPICIOUS_SKU_KEY,
          sellerAnalyticsResponse.getSuspiciousSkuProductCount(),
          sellerAnalyticsResponse.getLatestFraudDetectedDate());
    }
    if (sellerAnalyticsResponse.getRejectedProductCount() > 0) {
      addHistoricalData(historicalDataList, Constants.REJECTED_PRODUCT_KEY,
          sellerAnalyticsResponse.getRejectedProductCount(),
          sellerAnalyticsResponse.getLatestFraudDetectedDate());
    }
    if (sellerAnalyticsResponse.getExpiredProductCount() > 0) {
      addHistoricalData(historicalDataList, Constants.EXPIRED_PRODUCT_KEY,
          sellerAnalyticsResponse.getExpiredProductCount(),
          sellerAnalyticsResponse.getLatestFraudDetectedDate());
    }
    if (sellerAnalyticsResponse.getProhibitedProductCount() > 0) {
      addHistoricalData(historicalDataList, Constants.PROHIBITED_PRODUCT_KEY,
          sellerAnalyticsResponse.getProhibitedProductCount(),
          sellerAnalyticsResponse.getLatestFraudDetectedDate());
    }
    return historicalDataList;
  }


  private static void addHistoricalData(List<HistoricalSellerDataResponse> list, String key,
      int count, Date lastDetectedDate) {
    HistoricalSellerDataResponse response = new HistoricalSellerDataResponse();
    response.setKey(key);
    response.setValue(count);
    response.setLastDetectedDate(count > 0 ? lastDetectedDate : null);
    list.add(response);
  }

  public static EvidenceRequestedDetailResponse constructEvidenceRequestedDetailResponse(
      ProductIPR productIPR) {
    return EvidenceRequestedDetailResponse.builder()
        .evidenceRequestedBy(productIPR.getEvidenceRequestedBy())
        .evidenceRequestedDate(productIPR.getEvidenceRequestedDate())
        .violationType(productIPR.getViolationType())
        .evidenceRequestedNotes(productIPR.getSellerNotes())
        .reviewerNotes(productIPR.getReviewerNotes())
        .evidenceRequestedReasons(productIPR.getReasons()).build();
  }

  public static EvidenceSubmittedDetailResponse constructEvidenceSubmittedDetailResponse(
    ProductIPR productIPR) {
    return EvidenceSubmittedDetailResponse.builder()
      .evidenceSubmittedBy(productIPR.getEvidenceSubmittedBy())
      .evidenceSubmittedNotes(productIPR.getEvidenceSubmittedNotes())
      .evidenceFilePath(splitByComma(productIPR.getEvidenceFilePath()))
      .evidenceUrl(splitByComma(productIPR.getEvidenceUrl())).build();
  }

  private static List<String> splitByComma(String inputValue) {
    return Optional.ofNullable(inputValue).filter(value -> !value.trim().isEmpty())
      .map(value -> Arrays.asList(value.split(Constants.COMMA))).orElseGet(Collections::emptyList);
  }


  public static IprProductDetailsResponse constructIPRProductDetailResponseWithEmptySellerHistoricalData(
      ProductIPR existingProductIPR) {
    BrandReportDetailResponse brandReportDetailResponse = new BrandReportDetailResponse();
    if (ConverterUtil.convertSourceToSet(existingProductIPR.getSource())
      .contains(ProductSourceIPR.BRAND_REPORT.getValue())) {
      brandReportDetailResponse.setBrandName(existingProductIPR.getBrandName());
      brandReportDetailResponse.setReporterEmail(existingProductIPR.getReporterEmail());
      brandReportDetailResponse.setReportDate(existingProductIPR.getReportDate());
      brandReportDetailResponse.setReporterName(existingProductIPR.getReporterName());
      brandReportDetailResponse.setReporterReason(existingProductIPR.getReporterReason());
    }
    return IprProductDetailsResponse.builder().productName(existingProductIPR.getProductName())
        .productCode(existingProductIPR.getProductCode())
        .productSku(existingProductIPR.getProductSku())
        .businessPartnerCode(existingProductIPR.getBusinessPartnerCode())
        .businessPartnerName(existingProductIPR.getBusinessPartnerName())
        .assignedTo(existingProductIPR.getAssignedTo())
        .imageUrl(existingProductIPR.getImageUrl()).state(existingProductIPR.getState())
        .evidenceRequestedDetailResponse(
            constructEvidenceRequestedDetailResponse(existingProductIPR))
        .evidenceSubmittedDetailResponse(
            constructEvidenceSubmittedDetailResponse(existingProductIPR))
        .historicalSellerData(constructEmptyHistoricalDataResponse())
        .brandReportDetailResponse(brandReportDetailResponse)
        .source(existingProductIPR.getSource()).build();
  }

  public static IprProductDetailsResponse constructIPRProductDetailResponse(
      ProductIPR existingProductIPR, SellerAnalyticsResponse response) {
    BrandReportDetailResponse brandReportDetailResponse = new BrandReportDetailResponse();
    if (ConverterUtil.convertSourceToSet(existingProductIPR.getSource())
      .contains(ProductSourceIPR.BRAND_REPORT.getValue())) {
      brandReportDetailResponse.setBrandName(existingProductIPR.getBrandName());
      brandReportDetailResponse.setReporterEmail(existingProductIPR.getReporterEmail());
      brandReportDetailResponse.setReportDate(existingProductIPR.getReportDate());
      brandReportDetailResponse.setReporterName(existingProductIPR.getReporterName());
      brandReportDetailResponse.setReporterReason(existingProductIPR.getReporterReason());
    }
    return IprProductDetailsResponse.builder().productName(existingProductIPR.getProductName())
        .productSku(existingProductIPR.getProductSku())
        .productCode(existingProductIPR.getProductCode()).state(existingProductIPR.getState())
        .evidenceRequestedDetailResponse(
            constructEvidenceRequestedDetailResponse(existingProductIPR))
        .evidenceSubmittedDetailResponse(
            constructEvidenceSubmittedDetailResponse(existingProductIPR))
        .businessPartnerCode(existingProductIPR.getBusinessPartnerCode())
        .businessPartnerName(existingProductIPR.getBusinessPartnerName())
        .assignedTo(existingProductIPR.getAssignedTo())
        .sellerBadge(response.getSellerBadge()).officialSeller(response.isOfficialSeller())
        .imageUrl(existingProductIPR.getImageUrl())
        .historicalSellerData(constructHistoricalDataResponse(response))
        .source(existingProductIPR.getSource())
        .brandReportDetailResponse(brandReportDetailResponse)
        .build();
  }

  public static SuspensionProductRequest convertToSuspensionProductRequest(ProductIPR productIPR,
      IprActionRequest request) {
    ProductLevel3Request products = new ProductLevel3Request();
    products.setProductCode(productIPR.getProductCode());
    products.setProductName(productIPR.getProductName());
    products.setProductSku(productIPR.getProductSku());
    products.setBusinessPartnerCode(productIPR.getBusinessPartnerCode());
    return SuspensionProductRequest.builder().products(Collections.singletonList(products))
        .action(Constants.SUSPEND).reason(request.getReasons()).notes(request.getSellerNotes()).build();
  }

  public static IPRHistory toIprHistory(String storeId, IPRHistoryEventModel iprHistoryEventModel) {
    IPRHistory iprHistory = new IPRHistory();
    BeanUtils.copyProperties(iprHistoryEventModel, iprHistory);
    iprHistory.setStoreId(storeId);
    iprHistory.setUpdatedBy(iprHistoryEventModel.getUpdatedBy());
    iprHistory.setUpdatedDate(iprHistoryEventModel.getUpdatedDate());
    iprHistory.setCreatedBy(iprHistoryEventModel.getUpdatedBy());
    iprHistory.setCreatedDate(iprHistoryEventModel.getUpdatedDate());
    return iprHistory;
  }

  public static List<IPRProductSolr> populateAssigneeToIprFromDb(List<IPRProductSolr> iprProductSolrList,
    Map<String, String> productSkuAssigneeMap) {
    iprProductSolrList.forEach(ipr -> ipr.setAssignedTo(
      productSkuAssigneeMap.getOrDefault(ipr.getProductSku(), ipr.getAssignedTo())));
    return iprProductSolrList;
  }

  public static AddProductToIprSolrEventModel toAddProductToIprSolrEventModel(
    ProductIPR productIPR) {
    IPRProductSolr iprProductSolr = new IPRProductSolr();
    if (ProductStateIPR.WAITING_TO_GET_ACTIVATED.name().equals(productIPR.getState())) {
      return AddProductToIprSolrEventModel.builder().productSku(productIPR.getProductSku())
        .deleteSolrDocument(true).build();
    } else {
      BeanUtils.copyProperties(productIPR, iprProductSolr);
    }
    return AddProductToIprSolrEventModel.builder().iprProductSolr(iprProductSolr)
      .productSku(productIPR.getProductSku()).build();
  }

  public static ProductEmailEventModel processEventForProductEmail(String existingState,
    String newState, ProductIPR productIPR) {
    //If changing evidence requested product then reset
    if (ProductStateIPR.EVIDENCE_REQUESTED.name().equalsIgnoreCase(existingState)
      && !ProductStateIPR.EVIDENCE_REQUESTED.name().equalsIgnoreCase(newState)) {
      return ProductEmailEventModel.builder().productSku(productIPR.getProductSku())
        .businessPartnerCode(productIPR.getBusinessPartnerCode()).resetStatus(true).build();
    } else if (!ProductStateIPR.EVIDENCE_REQUESTED.name().equalsIgnoreCase(existingState)
      && ProductStateIPR.EVIDENCE_REQUESTED.name().equalsIgnoreCase(newState)) {
      return ConverterUtil.toProductEmailEventModel(productIPR);
    }
    return null;
  }

  public static ProductEmailEventModel toProductEmailEventModel(ProductIPR productIPR) {
    ProductEmailEventModel productEmailEventModel = new ProductEmailEventModel();
    BeanUtils.copyProperties(productIPR, productEmailEventModel);
    productEmailEventModel.setNotes(productIPR.getSellerNotes());
    productEmailEventModel.setProductEmailType(Constants.EVIDENCE_REQUESTED_MAIL_TYPE);
    productEmailEventModel.setStoreId(productIPR.getStoreId());
    return productEmailEventModel;
  }

  public static Set<String> convertSourceToSet(String sources) {
    return Optional.ofNullable(sources).map(source -> source.split(Constants.COMMA))
      .map(Arrays::stream).map(stream -> stream.collect(Collectors.toSet())).orElse(Set.of());
  }
}
