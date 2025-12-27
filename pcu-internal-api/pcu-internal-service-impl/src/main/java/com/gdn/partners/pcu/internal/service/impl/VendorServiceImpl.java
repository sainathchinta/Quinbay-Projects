package com.gdn.partners.pcu.internal.service.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.stream.Collectors;

import com.gdn.mta.bulk.dto.InternalProcessPendingFilesResponse;
import com.gdn.partners.pcu.internal.client.model.request.VendorAutoAssignmentRequest;
import com.gdn.partners.pcu.internal.client.model.response.DistributionProductResponse;
import com.gdn.partners.pcu.internal.client.model.response.ProductL3BasicResponse;
import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.internal.service.impl.event.model.BulkReviewUploadModel;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import com.gdn.partners.pcu.internal.service.impl.util.BeanUtils;
import com.gdn.partners.pcu.internal.service.impl.util.ConverterUtil;
import com.gdn.partners.pcu.internal.web.model.request.BulkDeleteProductRequest;
import com.gdn.partners.pcu.internal.web.model.request.RejectProductRequest;
import com.gdn.partners.pcu.internal.web.model.response.RejectProductResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import com.gdn.x.mta.distributiontask.response.VendorDefaultFilterResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.PrimaryFilterRequest;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.ProductHistoryResponse;
import com.gda.mta.product.dto.response.PredictionTypeResponse;
import com.gda.mta.product.dto.response.SequenceResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.dto.BulkVendorProductAssignRequest;
import com.gdn.mta.bulk.dto.product.UploadProcessCount;
import com.gdn.mta.bulk.dto.product.constant.DomainEventName;
import com.gdn.mta.product.util.SingleValueResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.client.feign.PDTFeign;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import com.gdn.partners.pcu.internal.service.BPService;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.PartnersEngineService;
import com.gdn.partners.pcu.internal.service.ProductMTAWrapper;
import com.gdn.partners.pcu.internal.service.ProductService;
import com.gdn.partners.pcu.internal.service.VendorService;
import com.gdn.partners.pcu.internal.service.XBulkOutboundService;
import com.gdn.partners.pcu.internal.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.service.model.BulkInternalProcessType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.internal.streaming.model.bulk.DownloadType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.FileType;
import com.gdn.partners.pcu.internal.streaming.model.bulk.VendorSummaryDownloadRequest;
import com.gdn.partners.pcu.internal.web.model.request.PrimaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImageQcWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ScreeningProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.VendorAutoConsignmentWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.VendorSummaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.AssigneeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.AttributeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BulkUpdatePendingWebResposne;
import com.gdn.partners.pcu.internal.web.model.response.DistributionProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ImageFaultyTypeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ImageWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.NeedRevisionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductAttributeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductBusinessPartnerMapperWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductImageQcWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductItemWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.VendorDetailWebResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.model.dto.VendorCapacityDTO;
import com.gdn.x.mta.distributiontask.request.DistributionTaskMultipleFilterRequest;
import com.gdn.x.mta.distributiontask.response.ProductImageQcFeedbackResponse;
import com.gdn.x.mta.distributiontask.response.VendorDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductImageQcFeedbackRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.NeedRevisionResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.TaskHistoryResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorAssigneeResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse;
import com.gdn.x.productcategorybase.dto.response.MasterAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

import lombok.extern.slf4j.Slf4j;

/**
 * Created by shivam on 26/06/2019 AD.
 */
@Service
@Slf4j
public class VendorServiceImpl implements VendorService {

  private static final String LANGUAGE = "en";
  private static final String DELIMETER = ".";
  private static final String DELIMITER_SLASH = "/";
  private static final String VENDORS = "vendors";

  @Autowired
  private PDTFeign pdtFeign;

  @Autowired
  private BPService bpService;

  @Autowired
  private PBPFeign pbpFeign;

  @Autowired
  private ProductService productService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private SystemParameterProperties systemParameterProperties;

  @Autowired
  private ProductMTAWrapper productMTAWrapper;

  @Autowired
  private PartnersEngineService partnersEngineService;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private XBulkOutboundService xBulkOutboundService;

  @Value("#{${brand.prediction.type.display.name}}")
  private Map<String, String> predictionTypeToDisplayName;

  @Value("#{${brand.prediction.type.confidence.threshold}}")
  private Map<String, Double> predictionTypeToConfidence;

  @Value("${gcs.pathPrefix}")
  private String pathPrefix;

  @Value("${gcs.domain.url.path}")
  private String gcsUrlPrefix;

  @Value("#{${prediction.type.label.colour}}")
  private Map<String, String> predictionTypeToLabelColourMap;

  @Value("${category.prediction.enabled}")
  private boolean categoryPredictionEnabled;

  @Value("${pdp.link.item.sku.prefix}")
  private String pdpLinkItemSkuPrefix;

  @Value("${pdp.link.product.code.prefix}")
  private String pdpLinkProductCodePrefix;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  @Override
  public List<ProductBusinessPartnerMapperWebResponse> getBusinessPartnerList(int page, int size,
      PrimaryFilterWebRequest request) throws Exception {
    PrimaryFilterRequest primaryFilterRequest = new PrimaryFilterRequest();
    BeanUtils.copyProperties(request, primaryFilterRequest, "timeFilterWebType");
    TimeFilterType timeFilterType =
        TimeFilterType.getTimeFilterTypeByValue(request.getTimeFilterWebType());
    primaryFilterRequest.setTimeFilterType(timeFilterType);
    GdnRestListResponse<ProductBusinessPartnerMapperResponse> response =
        this.pdtFeign.getBusinessPartnerList(page, size, primaryFilterRequest);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.convertProductBusinessPartnerMapperResponseToProductBusinessPartnerWebResponse(response);
  }

  @Override
  public List<AssigneeWebResponse> getAssigneeList(int page, int size,
      PrimaryFilterWebRequest request) throws Exception {
    PrimaryFilterRequest primaryFilterRequest = new PrimaryFilterRequest();
    BeanUtils.copyProperties(request, primaryFilterRequest, "timeFilterWebType");
    primaryFilterRequest
        .setTimeFilterType(TimeFilterType.getTimeFilterTypeByValue(request.getTimeFilterWebType()));
    GdnRestListResponse<VendorAssigneeResponse> response =
        this.pdtFeign.getAssigneeList(page, size, primaryFilterRequest);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.convertAssigneeResponseToAssigneeWebResponse(response);
  }

  @Override
  public Page<ProductHistoryWebResponse> getProductHistory(String productCode, int page, int size) {
    GdnRestListResponse<TaskHistoryResponse> response =
        pdtFeign.getProductHistory(productCode, page, size, false);
    ResponseHelper.validateResponse(response);
    List<ProductHistoryResponse> productHistoryResponseList =
        ResponseHelper.fromTaskHistoryResponseToProductHistoryResponse(response.getContent());
    return new PageImpl<>(ResponseHelper.toProductHistoryWebResponseListFromVendorHistoryResponse(
        productHistoryResponseList),  PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
  }

  @Override
  public boolean getEditedByMerchant(String productCode, int version) {
    GdnRestSimpleResponse<Boolean> response = pdtFeign.getEditedByMerchant(productCode, version);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public SingleBaseResponse<ProductDetailWebResponse> getProductDetailsByProductCode(String requestId,
            String username, String productCode) {
    String errorMessage = StringUtils.EMPTY;
    boolean isSuccess = false;
    ProductDetailWebResponse productDetailWebResponse = null;
    try {
      log.info("Getting product details by product code. requestId: {}, productCode: "
         + "{}", requestId, productCode);
      GdnRestSingleResponse<DistributionProductDetailResponse> response =
          pdtFeign.getProductDetails(requestId, username, productCode);
      if (!response.isSuccess()) {
        return new SingleBaseResponse<>(response.getErrorMessage(), ErrorCategory.INVALID_STATE.getCode(), false,
            requestId, null);
      }
      ResponseHelper.validateResponse(response);
      ProfileResponse profileResponse = null;
      if (!Constants.USER_TYPE_INTERNAL.equals(response.getValue().getBusinessPartnerCode())) {
        profileResponse =
            bpService.getProfileResponseByBusinessPartnerCode(response.getValue().getBusinessPartnerCode());
      }
      productDetailWebResponse = ResponseHelper.convertDistributionDetailResponseToProductDetailWebResponse(
          response.getValue(), profileResponse, pdpLinkItemSkuPrefix, pdpLinkProductCodePrefix);
      ProductDetailWebResponse pcbDetailWebResponse = setCategoryAndWebResponses(productCode,
          productDetailWebResponse, response.getValue().getCategoryCode());
      setAttributeWebResponse(productDetailWebResponse.getProductAttributeResponses());
      isSuccess = true;
      LinkedHashSet<ProductItemWebResponse> productItemWebResponses =
          productDetailWebResponse.getProductItemResponses().stream()
              .sorted(Comparator.comparing(ProductItemWebResponse::getGeneratedItemName))
              .collect(Collectors.toCollection(LinkedHashSet::new));
      setThumbnailImage(productDetailWebResponse, pcbDetailWebResponse);
      setCommonThumbnailImage(productDetailWebResponse, pcbDetailWebResponse);
      if (instoreNewFlowEnabled && Objects.nonNull(profileResponse) && profileResponse.getCompany()
          .isOfflineToOnlineFlag()) {
        ProductL3BasicResponse productL3BasicResponse =
            productService.getProductL3BasicDetails(productCode);
        if(Objects.isNull(productL3BasicResponse)) {
          productDetailWebResponse.setPureInstore(false);
        }
        else {
          productDetailWebResponse.setPureInstore(productL3BasicResponse.isPureInStoreProduct());
        }
      }
      productDetailWebResponse.setProductItemResponses(ResponseHelper.groupItemsByWarna(productItemWebResponses));
      if (Objects.nonNull(profileResponse)) {
        productDetailWebResponse.setOfficialSeller(profileResponse.isOfficial());
        ProductDetailWebResponse finalProductDetailWebResponse = productDetailWebResponse;
        Optional.ofNullable(profileResponse.getCompany()).ifPresent(company -> {
          Optional.ofNullable(company.getBusinessPartnerName())
                  .filter(StringUtils::isNotBlank)
                  .ifPresent(finalProductDetailWebResponse::setBusinessPartnerName);
          Optional.ofNullable(company.getOfficer())
                  .filter(StringUtils::isNotBlank)
                  .ifPresent(finalProductDetailWebResponse::setOfficer);
        });
      }
    } catch (Exception e) {
      log.error("Error listing product details for product by vendor. requestId: {}, productCode: {}, error - ",
         requestId, productCode, e);
    }
    return new SingleBaseResponse<>(errorMessage, null, isSuccess, requestId, productDetailWebResponse);
  }

  private ProductDetailWebResponse setCategoryAndWebResponses(String productCode,
      ProductDetailWebResponse response, String categoryCode) throws Exception {
    ProductDetailResponse productDetailResponse =
        this.productService
            .findDetailByProductCodeAndReplaceCategoryInfo(productCode, categoryCode);
    //Passing profile response as null because commission type and IS flag wont be populated here.
    ProductDetailWebResponse productDetailWebResponse =
        ResponseHelper.toProductDetailWebResponse(productDetailResponse, null);
    response.setProductCategoryResponses(
        new ArrayList<>(productDetailWebResponse.getProductCategoryResponses()));
    response.setCategories(new ArrayList<>(productDetailWebResponse.getCategories()));
    if (Objects.nonNull(productDetailWebResponse.getCategoriesEnglish())) {
      response.setCategoriesEnglish(productDetailWebResponse.getCategoriesEnglish());
    }
    return productDetailWebResponse;
  }

  private void setThumbnailImage(ProductDetailWebResponse response, ProductDetailWebResponse pcbProductDetailResponse) {
    Map<String, ImageWebResponse> itemSkuToThumbnailImage = new HashMap<>();
    for (ProductItemWebResponse itemWebResponse : pcbProductDetailResponse.getProductItemResponses()) {
      ImageWebResponse thumbnailImage = itemWebResponse.getImages().parallelStream().
          filter(image -> image.isMainImages()).
          filter(
              imageWebResponse -> Objects.isNull(imageWebResponse.getOriginalImage()) || imageWebResponse.isEdited() ?
              imageWebResponse.isActive() :
              !imageWebResponse.getOriginalImage()).findFirst().orElse(null);
      if (Objects.nonNull(thumbnailImage)) {
        itemSkuToThumbnailImage.put(itemWebResponse.getSkuCode(), thumbnailImage);
      }
    }
    for (ProductItemWebResponse productItemWebResponse : response.getProductItemResponses()) {
      if (itemSkuToThumbnailImage.containsKey(productItemWebResponse.getSkuCode())) {
        productItemWebResponse.setThumbnailPath(
            itemSkuToThumbnailImage.get(productItemWebResponse.getSkuCode()).getLocationPath());
        productItemWebResponse.setThumbnailActive(
            itemSkuToThumbnailImage.get(productItemWebResponse.getSkuCode()).isActive());
      }
    }
  }

  private void setCommonThumbnailImage(ProductDetailWebResponse response,
      ProductDetailWebResponse pcbProductDetailResponse) {
    ImageWebResponse imageWebResponse = pcbProductDetailResponse.getImages().stream().filter(
        imageWebResponseFilter -> imageWebResponseFilter.isMainImages() && Optional.ofNullable(
            imageWebResponseFilter.getOriginalImage()).orElse(true)
            && imageWebResponseFilter.isCommonImage()).findFirst().orElse(null);
    if(Objects.nonNull(imageWebResponse)) {
    response.setCommonImageThumbnailPath(imageWebResponse.getLocationPath());
    response.setCommonImageThumbnailActive(imageWebResponse.isActive());
    }
  }

  private void setAttributeWebResponse(List<ProductAttributeWebResponse> productAttributeResponses) throws Exception{
    for (ProductAttributeWebResponse productAttributeWebResponse : productAttributeResponses) {
      AttributeWebResponse attributeWebResponse = productAttributeWebResponse.getAttribute();
      MasterAttributeResponse response =
          productService.getAttributeInfoByAttributeCode(attributeWebResponse.getAttributeCode());
      BeanUtils
          .copyProperties(response, attributeWebResponse, "allowedAttributeValues", "predefinedAllowedAttributeValues", "extractedValue");
      attributeWebResponse.setExtractedValue(productAttributeWebResponse.getAttribute().isExtractedValue());
      productAttributeWebResponse.setAttribute(attributeWebResponse);
    }
  }

  @Override
  public NeedRevisionWebResponse doProductNeedCorrection(String vendorCode, ScreeningProductBulkActionsWebRequest request) {
    GdnRestSingleResponse<NeedRevisionResponse> response =
        pdtFeign.doProductNeedCorrection(vendorCode, RequestHelper.toNeedRevisionRequest(request));
    ResponseHelper.validateResponse(response);
    return new NeedRevisionWebResponse(response.getValue().isContentNeedCorrection(),
        response.getValue().isImageNeedCorrection(), response.getValue().isSuccess());
  }

  @Override
  public void updateProduct(String type, String vendorCode,
      DistributionProductDetailRequest distributionProductRequest) {
    GdnBaseRestResponse response = Constants.VENDOR_TYPE_CONTENT.equals(type) ?
        pdtFeign.updateProductContent(vendorCode, distributionProductRequest) :
        pdtFeign.updateProductImage(vendorCode, distributionProductRequest);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public Page<DistributionProductWebResponse> getVendorProductList(int page, int size,
      VendorSummaryFilterWebRequest vendorSummaryFilterWebRequest, String vendorCode) {
    FilterSummaryRequest summaryFilterRequest =
        RequestHelper.toFilterSummaryRequest(vendorSummaryFilterWebRequest, vendorCode);
    GdnRestListResponse<DistributionProductResponse> response =
        this.pdtFeign.getProductList(page, size, summaryFilterRequest);
    ResponseHelper.validateResponse(response);
    List<String> businessPartnerCodes = response.getContent().stream()
        .filter(distributionProductResponse -> Objects.nonNull(distributionProductResponse.getBusinessPartnerCode()))
        .map(DistributionProductResponse::getBusinessPartnerCode).distinct().collect(Collectors.toList());
    Map<String, ProfileResponse> profileResponseMap = bpService.getProfileResponseMap(businessPartnerCodes);
    List<DistributionProductWebResponse> responseList =
        ResponseHelper.fromDistributionProductResponseToDistributionProductWebResponse(response.getContent(), profileResponseMap);
    return new PageImpl<>(responseList,  PageRequest.of(page, size), response.getPageMetaData().getTotalRecords());
  }

  @Override
  public void vendorProductActions(String action, ScreeningProductBulkActionsWebRequest request) {
    GdnBaseRestResponse response =
        pdtFeign.doVendorProductActions(action, RequestHelper.toScreeningProductBulkActionsRequest(request));
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void approveVendorProduct(String vendorCode, DistributionProductDetailRequest distributionProductRequest) {
    GdnBaseRestResponse response = pdtFeign.approveVendorProduct(vendorCode, distributionProductRequest);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public List<RejectProductResponse> rejectProductByVendor(String vendorCode, BulkDeleteProductRequest request) throws Exception {
    List<RejectProductResponse> rejectProductResponses = new ArrayList<>();
    Map<String, String> productCodeToMerchantCommissionTypeMap = request.getCodes().stream().collect(
        Collectors.toMap(RejectProductRequest::getProductCode,
            rejectProductRequest -> Optional.ofNullable(rejectProductRequest.getMerchantCommissionType())
                .orElse(StringUtils.EMPTY)));
    for (String productCode : productCodeToMerchantCommissionTypeMap.keySet()) {
      RejectProductVendorRequest rejectProductVendorRequest =
          new RejectProductVendorRequest(productCodeToMerchantCommissionTypeMap.get(productCode),
              ConverterUtil.toRejectReason(request.getRejectReason()), request.getNotes(),
              productCode);
      try {
        GdnBaseRestResponse response = this.pdtFeign.rejectProduct(vendorCode, rejectProductVendorRequest);
        ResponseHelper.validateResponse(response);
      } catch (ClientException e) {
        RejectProductResponse rejectProductResponse = new RejectProductResponse();
        rejectProductResponse.setProductCode(productCode);
        rejectProductResponse.setErrorMessage(e.getMessage());
        rejectProductResponse.setErrorCode(e.getErrorCode());
        rejectProductResponses.add(rejectProductResponse);
      }
    }
    return rejectProductResponses;
  }

  @Override
  public MapResponse getFilterCounts(String vendorCode, Boolean postLive, Boolean edited,
    Boolean revised) {
    GdnRestSingleResponse<MapResponse> productFilterInReview =
        pdtFeign.getProductFilterInReview(vendorCode, postLive, edited, revised);
    ResponseHelper.validateResponse(productFilterInReview);
    return productFilterInReview.getValue();
  }

  @Override
  public MapResponse getReviewConfigProductCounts(String vendorCode) {
    GdnRestSingleResponse<MapResponse> reviewConfigCounts = pdtFeign.getProductReviewConfigCounts(vendorCode);
    ResponseHelper.validateResponse(reviewConfigCounts);
    return reviewConfigCounts.getValue();
  }

  @Override
  public void bulkDownloadFilteredVendorProducts(String username, String vendorCode,
      VendorSummaryFilterWebRequest vendorSummaryFilterWebRequest) {
    String requestId = UUID.randomUUID().toString();
    log.info("invoking of Bulk vendor Products Download. requestId: {},", requestId);
    String fileName =
        new StringBuilder().append(requestId).append(DELIMETER).append(FileType.XLSX.name().toLowerCase()).toString();
    VendorSummaryDownloadRequest vendorSummaryDownloadRequest =
        VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder()
            .keyword(vendorSummaryFilterWebRequest.getKeyword())
            .timeFilterType(vendorSummaryFilterWebRequest.getTimeFilterWebType())
            .contentPending(vendorSummaryFilterWebRequest.getContentPending())
            .imagePending(vendorSummaryFilterWebRequest.getImagePending())
            .assignment(vendorSummaryFilterWebRequest.getAssignment())
            .categoryCode(vendorSummaryFilterWebRequest.getCategoryCode())
            .isCnCategory((vendorSummaryFilterWebRequest.getIsCnCategory()))
            .businessPartnerCode(vendorSummaryFilterWebRequest.getBusinessPartnerCode())
            .assigneeEmailId(vendorSummaryFilterWebRequest.getAssigneeEmailId())
            .vendorCode(vendorCode)
            .sortOrderByCreatedDate(vendorSummaryFilterWebRequest.getSortOrder()).postLive(
            Optional.of(vendorSummaryFilterWebRequest).map(VendorSummaryFilterWebRequest::getPostLive)
              .orElse(null)).faultyImageType(vendorSummaryFilterWebRequest.getFaultyType())
          .brandPending(vendorSummaryFilterWebRequest.getBrandPending()).edited(
            Optional.of(vendorSummaryFilterWebRequest).map(VendorSummaryFilterWebRequest::getEdited)
              .orElse(null)).revised(
            Optional.of(vendorSummaryFilterWebRequest).map(VendorSummaryFilterWebRequest::getRevised)
              .orElse(null)).restrictedKeyword(vendorSummaryFilterWebRequest.getRestrictedKeyword())
            .downloadType(DownloadType.ALL).fileType(FileType.XLSX).bulkProcessEntity(BulkProcessEntity.VENDOR_FILTERED_PRODUCT)
            .directDownload(false).filename(fileName).emailTo(username).username(username)
            .language(LANGUAGE).requestId(requestId).build();
    vendorSummaryDownloadRequest.setB2bActivated(vendorSummaryFilterWebRequest.getB2bActivated());
    vendorSummaryDownloadRequest.setB2cActivated(vendorSummaryFilterWebRequest.getB2cActivated());
    vendorSummaryDownloadRequest.setAppealedProduct(vendorSummaryFilterWebRequest.getAppealedProduct());
    this.kafkaPublisher.send(DomainEventName.BULK_DOWNLOAD_ALL_EVENT, username,vendorSummaryDownloadRequest);
  }

  @Override
  public String getProductScreeningNotes(String productCode) {
    GdnRestSingleResponse<SingleValueResponse> response = pbpFeign.getProductScreeningNotes(productCode);
    ResponseHelper.validateResponse(response);
    return response.getValue().getValue();
  }

  @Override
  public void sendProductBackToVendor(String productCode) {
    GdnBaseRestResponse response = pdtFeign.sendProductBackToVendor(productCode);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public void saveBulkAssignFile(String storeId, String vendorCode, String userName, String requestId,
      MultipartFile multipartFile) throws Exception {
    GdnRestSingleResponse<SequenceResponse> sequenceResponse =
        this.pbpFeign.findCounterByKey(Constants.BULK_INTERNAL_PROCESS_CODE_KEY);
    ResponseHelper.validateResponse(sequenceResponse);
    String bipRequestCode =
        new StringBuilder().append(Constants.BULK_INTERNAL_PROCESS_CODE_KEY).append(Constants.HYPHEN)
            .append(StringUtils
                .leftPad(String.valueOf(sequenceResponse.getValue().getCounter()), Constants.PADDING_COUNT, Constants.PADDING_CONSTANT))
            .toString();
    String baseDirPath = fileStorageService.uploadFilePath(multipartFile, requestId,
        BulkInternalProcessType.INTERNAL_VENDOR_BULK.getValue());
    Map<String, List<String>> reviewers = partnersEngineService.getReviewers();
    BulkVendorProductAssignRequest bulkVendorProductAssignRequest =
        RequestHelper.toBulkVendorProductAssignRequest(storeId,
            new StringBuilder(baseDirPath).append(multipartFile.getOriginalFilename()).toString(), vendorCode,
            requestId, userName, reviewers, bipRequestCode);
    kafkaPublisher.send(DomainEventName.BULK_ASSIGN_VENDOR_PRODUCT, userName, bulkVendorProductAssignRequest);
  }

  @Override
  public List<VendorDetailWebResponse> getVendorList() {
    GdnRestSingleResponse<MapResponse> mapResponse =
        pdtFeign.countDistributionSummaryByFilter(false, true, new DistributionTaskMultipleFilterRequest());
    ResponseHelper.validateResponse(mapResponse);
    List<VendorCapacityDTO> vendors = new ObjectMapper()
        .convertValue(mapResponse.getValue().getMap().get(VENDORS), new TypeReference<List<VendorCapacityDTO>>() {
        });
    if (CollectionUtils.isNotEmpty(vendors)) {
      return ResponseHelper.toVendorDetailWebResponseList(vendors);
    } else {
      return new ArrayList<>();
    }
  }

  @Override
  public ProductImageQcWebResponse getProductImageQcFeedback(String productCode) throws IOException {
    GdnRestSimpleResponse<ProductImageQcFeedbackResponse> response = pdtFeign.getProductImageFeedback(productCode);
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toProductImageQcWebResponse(response.getValue(), predictionTypeToDisplayName,
        predictionTypeToConfidence, gcsUrlPrefix, categoryPredictionEnabled);
  }

  @Override
  public void updateProductImageQcFeedback(ProductImageQcWebRequest productImageQcWebRequest)
      throws JsonProcessingException {
    ProductImageQcFeedbackRequest request = RequestHelper.toProductImageQcFeedbackRequest(productImageQcWebRequest, pathPrefix, gcsUrlPrefix);
    GdnBaseRestResponse response = pdtFeign.updateProductImageFeedback(request);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public List<ImageFaultyTypeWebResponse> getDifferentFaultyType() {
    GdnRestListResponse<PredictionTypeResponse> response = pbpFeign.getDifferentPredictionType();
    ResponseHelper.validateResponse(response);
    return ResponseHelper
        .toFaultyTypeListWebResponse(response.getContent(), systemParameterProperties.getIgnoreForImageQc(),
            systemParameterProperties.getNotIgnoreForImageQc(), predictionTypeToLabelColourMap,
          categoryPredictionEnabled);
  }

  @Override
  public boolean reindexProductToSolr(String productCode) {
    GdnBaseRestResponse response = pdtFeign.deltaReindexPDTProductSolr(productCode);
    ResponseHelper.validateResponse(response);
    return true;
  }

  @Override
  public BulkUpdatePendingWebResposne checkCountOfUploads(String bulkProcessType, String status) {
    UploadProcessCount uploadProcessCount = xBulkOutboundService.getPendingProcessCount(bulkProcessType, status);
    if (uploadProcessCount.getCount() > 0) {
      return new BulkUpdatePendingWebResposne(false, uploadProcessCount.getCount());
    }
    return new BulkUpdatePendingWebResposne(true, uploadProcessCount.getCount());
  }

  @Override
  public MapResponse getReviewProductCountsForConfig(String vendorCode, boolean postLive) {
    GdnRestSingleResponse<MapResponse> reviewConfigCounts = pdtFeign.getReviewConfigCount(vendorCode, postLive);
    ResponseHelper.validateResponse(reviewConfigCounts);
    return reviewConfigCounts.getValue();
  }

  @Override
  public void republishEditedProduct(String productCode) {
    GdnBaseRestResponse response = pdtFeign.republishEditedProduct(productCode);
    ResponseHelper.validateResponse(response);
  }

  @Override
  public VendorDetailResponse getVendorDetail(String vendorCode) {
    GdnRestSingleResponse<VendorDetailResponse> vendorDetailResponseGdnRestSingleResponse =
        pdtFeign.getVendorByCode(vendorCode);
    ResponseHelper.validateResponse(vendorDetailResponseGdnRestSingleResponse);
    return vendorDetailResponseGdnRestSingleResponse.getValue();
  }

  @Override
  public VendorQuickApprovalResponse vendorProductQuickApproval(VendorQuickApprovalRequest vendorQuickApprovalRequest) {
    GdnRestSingleResponse<VendorQuickApprovalResponse> response =
        pdtFeign.quickApproveProduct(vendorQuickApprovalRequest);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public List<String> getProductReviewers() throws Exception {
    Map<String, List<String>> reviewers = partnersEngineService.getReviewers();
    return reviewers.get(Constants.REVIEWERS);
  }

  @Override
  public void autoAssignProducts(VendorAutoConsignmentWebRequest vendorAutoConsignmentWebRequest, String storeId,
    String vendorCode) {
    if (vendorAutoConsignmentWebRequest.isDefaultSettingsEnabled()) {
      GdnBaseRestResponse response = pdtFeign.saveDefaultSetting(RequestHelper.toVendorDefaultFilterRequest(vendorAutoConsignmentWebRequest));
      ResponseHelper.validateResponse(response);
    }
    GdnRestSingleResponse<SequenceResponse> sequenceResponse =
        this.pbpFeign.findCounterByKey(Constants.BULK_INTERNAL_PROCESS_CODE_KEY);
    ResponseHelper.validateResponse(sequenceResponse);
    VendorAutoAssignmentRequest vendorAutoAssignmentRequest =
        RequestHelper.toVendorAutoAssignmentRequest(vendorAutoConsignmentWebRequest, storeId,
            sequenceResponse.getValue(), vendorCode);
    kafkaPublisher.send(DomainEventName.VENDOR_AUTO_ASSIGNMENT_EVENT, vendorAutoAssignmentRequest.getVendorEmail(),
        vendorAutoAssignmentRequest);
    log.info("event = {} got published having vendorEmail = {} vendorAutoAssignmentRequest = {} ",
        DomainEventName.VENDOR_AUTO_ASSIGNMENT_EVENT, vendorAutoAssignmentRequest.getVendorEmail(),
        vendorAutoAssignmentRequest);
  }

  @Override
  public VendorDefaultFilterResponse getDefaultSetting(String vendorEmail) {
    GdnRestSingleResponse<VendorDefaultFilterResponse> response = pdtFeign.getDefaultSettingFilter(vendorEmail);
    ResponseHelper.validateResponse(response);
    return response.getValue();
  }

  @Override
  public InternalProcessPendingFilesResponse checkPendingAssignments(String storeId, String username, String processType) {
    return xBulkOutboundService.checkPendingFilesForAutoAssignment(storeId, username, processType);
  }

  @Override
  public void saveBulkReviewFile(MultipartFile request, String processType, String requestId,
    String storeId, String username) throws Exception {
    String baseDirPath = fileStorageService.uploadFilePath(request, requestId, processType);
    BulkReviewUploadModel bulkReviewUploadModel = RequestHelper.toBulkReviewUploadModel(storeId,
      new StringBuilder(baseDirPath).append(request.getOriginalFilename()).toString(), processType,
      requestId, username, null);
    kafkaPublisher.send(DomainEventName.BULK_REVIEW_UPLOAD_EVENT,
      bulkReviewUploadModel.getBulkProcessCode(), bulkReviewUploadModel);
    log.info("event = {} got published having bulkProcessCode = {} bulkReviewUploadModel = {} ",
      DomainEventName.BULK_REVIEW_UPLOAD_EVENT, bulkReviewUploadModel.getBulkProcessCode(),
      bulkReviewUploadModel);
  }

}
