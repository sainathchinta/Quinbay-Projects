package com.gdn.x.mta.distributiontask.controller;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import com.gdn.x.mta.distributiontask.request.AppealProductRequest;
import com.gdn.x.mta.distributiontask.response.ProductCodeResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.BoostedProductFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.AiGeneratedFieldsResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.AppealProductResponse;
import com.gdn.x.mta.distributiontask.service.impl.util.ImageUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Validate;
import org.apache.commons.lang3.reflect.FieldUtils;
import org.joda.time.format.DateTimeFormat;
import org.joda.time.format.DateTimeFormatter;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.http.MediaType;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.mta.distributiontask.controller.util.ProductConverterUtil;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAttribute;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemAttribute;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.dto.DistributionTaskFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.DistributionTaskMultipleFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.PrimaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductBusinessPartnerMapper;
import com.gdn.x.mta.distributiontask.model.dto.RejectProductDTO;
import com.gdn.x.mta.distributiontask.model.dto.SummaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.VendorProductStatusDTO;
import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.request.DistributionTaskFilterRequest;
import com.gdn.x.mta.distributiontask.request.DistributionTaskMultipleFilterRequest;
import com.gdn.x.mta.distributiontask.request.ProductRetryStatusUpdate;
import com.gdn.x.mta.distributiontask.response.VendorDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.constant.ExceptionMsg;
import com.gdn.x.mta.distributiontask.rest.model.constant.WorkflowWebState;
import com.gdn.x.mta.distributiontask.rest.model.request.AutoNeedRevisionRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ChangeBrandRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.PrimaryFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductCodeListRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductAttributeResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductImageResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductItemResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.ItemNotesResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.PDTProductDomainEventModelResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.ProductListResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.ProductNotesResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.RestrictedKeywordsByFieldVendor;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorAssigneeResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorProductStatusResponse;
import com.gdn.x.mta.distributiontask.service.api.ProductImageQcFeedbackService;
import com.gdn.x.mta.distributiontask.service.api.ProductReviewerService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import com.gdn.x.mta.distributiontask.service.api.VendorService;
import com.gdn.x.mta.distributiontask.service.api.publisher.ApprovedProductPublisherService;
import com.gdn.x.mta.distributiontask.service.impl.PDTExceptions.ExceptionUtil;
import com.gdn.x.mta.distributiontask.util.GdnRestSimpleResponse;
import com.google.common.collect.ImmutableMap;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

/**
 * Created by virajjasani on 14/09/16.
 */
@RestController
@RequestMapping(value = ProductDistributionController.BASE_PATH)
@Tag(name = "Product Distribution Controller", description = "Product Distribution Controller")
@Slf4j
public class ProductDistributionController {
  public static final String BASE_PATH = "/product";
  public static final String FILTER_BUSINESS_PARTNER = "filter/businessPartner";
  private static final String ROOT = "/";
  public static final String GET_PRODUCT_DOMAIN_MODEL_RESPONSE =
      ROOT + "get-product-domain-response-by-code";
  private static final String GET_PRODUCT_STATUS_IN_REVIEW = "/productFilterCountInReview";
  private static final String GET_PRODUCT_REVIEW_CONFIG_COUNTS = "/productReviewConfigCounts";
  public static final String GET_PRODUCT_DISTRIBUTION_SUMMARY_BY_MULTIPLE_FILTER =
            ROOT + "getDistributionSummaryByMultipleFilter";
  private static final String REPUBLISH_EDITED_PRODUCTS = "/republish-edited-product/{productCode}";
  public static final String COUNT_PRODUCT_DISTRIBUTION_SUMMARY_BY_FILTER =
      ROOT + "countDistributionSummaryByFilter";
  public static final String RETRIEVE_PRODUCT_DETAIL = "details";
  public static final String GET_PRODUCT_DETAIL_ALL = "details-for-all-product-type";
  private static final String COUNT_PRODUCT_STATUS_FOR_VENDOR = "/vendor/countProductStatus";
  private static final String IS_PRODUCT_EXIST = "/product-existence";
  public static final String GET_BUSINESS_PARTNER_LIST_FOR_VENDOR = "/getBusinessPartnerListForVendor";
  public static final String GET_PRODUCT_CODES = "/get-product-codes";
  public static final String REJECT_PRODUCT = "/reject-product";
  public static final String FILTER_BUSINESS_PARTNERS_BY_PRODUCT_LIST = "/getBusinessPartnerList";
  public static final String FILTER_ASSIGNEES_BY_PRODUCT_LIST = "/getAssigneeList";
  public static final String FILTER_SUMMARY = "/filter/summary";
  public static final String SEND_PRODUCT_BACK_TO_VENDOR = "/sendProductBackToVendor";
  public static final String DETECT_EDIT_BY_MERCHANT = "/{productCode}/detect-edit-by-merchant";
  public static final String GET_REVIEW_CONFIG_COUNT = "/getReviewConfigCount";
  public static final String SEND_PRODUCT_TO_AUTO_NEED_REVISION = "/sendProductToAutoNeedRevision";
  public static final String PRODUCT_RETRY_STATUS_UPDATE = "/{productCode}/product-retry-status-update";
  public static final String UPDATE_PRODUCT_BRAND = "/updateProductBrand";
  public static final String APPEAL_PRODUCT = "/appeal-product";

  public static final String FETCH_PRODUCTS_FOR_AUTO_ASSIGNMENT = "/fetchProductsForAutoAssignment";

  private static final DateTimeFormatter DATE_TIME_FORMATTER =
      DateTimeFormat.forPattern("yyyy-MM-dd HH:mm");
  private static final String CHAR_ENCODING = "UTF-8";
  private static final String CREATED_DATE = "createdDate";
  private static final String ASC = "Asc";
  private static final String DESC = "Desc";

  private static final Map<WorkflowState, String> WORKFLOW_STATE_STATUS_MAP = ImmutableMap.<WorkflowState, String>builder()
      .put(WorkflowState.IN_REVIEW, "inReview")
      .put(WorkflowState.PASSED, "passed")
      .build();

  @Autowired
  private ProductService productService;

  @Autowired
  private ProductConverterUtil productConverterUtil;

  @Autowired
  private VendorService vendorService;

  @Autowired
  private ApprovedProductPublisherService productPublisherService;

  @Autowired
  private ProductWrapperService productWrapperService;

  @Autowired
  private ProductImageQcFeedbackService productImageQcFeedbackService;

  @Autowired
  private ObjectMapper mapper;

  @Autowired
  private ProductReviewerService productReviewerService;

  @Value("${vendor.product.labels.ordered}")
  private String vendorProductLabelsOrdered;

  @Value("${override.main.image.if.missing.enabled}")
  private boolean overrideMainImageFlag;

  @Value("${auto.heal.enabled.get.product.domain.model.response.api}")
  private boolean autoHealEnabledForGetProductDomainModelResponseApi;

  private DistributionProductDetailResponse getDistributionProductDetailResponse(Product product,
      DistributionProductDetailResponse response) throws IOException {
    if (product != null) {
      response = new DistributionProductDetailResponse();
      BeanUtils.copyProperties(product, response);
      if (StringUtils.isNotEmpty(product.getRestrictedKeywordsDetected())) {
        response.setRestrictedKeywordsDetected(mapper.readValue(product.getRestrictedKeywordsDetected(),
          new TypeReference<List<RestrictedKeywordsByFieldVendor>>() {
          }));
      }
      if (StringUtils.isNotEmpty(product.getProductNotes())) {
        ProductNotesResponse productNotesResponse =
            mapper.readValue(product.getProductNotes(), ProductNotesResponse.class);
        if (Constants.REVISED.equals(productNotesResponse.getLastModified())) {
          response.setRevisedFields(productNotesResponse.getModifiedFields());
        } else {
          response.setEditedFields(productNotesResponse.getModifiedFields());
        }
      }
      if (Objects.nonNull(product.getReviewType())) {
        response.setReviewType(product.getReviewType().name());
      }
      if (Objects.nonNull(product.getState())) {
        response.setState(WorkflowWebState.valueOf(product.getState().name()));
        response.setProductApproved(WorkflowState.PASSED.equals(product.getState()));
      }

      List<DistributionProductItemResponse> distributionProductItemResponses =
          new ArrayList<DistributionProductItemResponse>();
      for (ProductItem productItem : product.getProductItems()) {
        DistributionProductItemResponse productItemResponse = new DistributionProductItemResponse();
        productItemResponse.setProductItemImages(new ArrayList<DistributionProductImageResponse>());
        productItemResponse
            .setProductItemAttributes(new ArrayList<DistributionProductAttributeResponse>());
        BeanUtils.copyProperties(productItem, productItemResponse,
            new String[] {"productItemImages", "productItemAttributes"});

        for (ProductItemImage productImage : productItem.getProductItemImages()) {
          if (Objects.nonNull(productImage.getOriginalImage())) {
            DistributionProductImageResponse imageResponse = new DistributionProductImageResponse();
            BeanUtils.copyProperties(productImage, imageResponse, "mainImage");
            if (productImage.getMainImage() != null) {
              imageResponse.setMainImage(productImage.getMainImage());
            }
            productItemResponse.getProductItemImages().add(imageResponse);
          } else {
            DistributionProductImageResponse imageResponseTrue = getDistributionProductImageResponse(productImage);
            imageResponseTrue.setOriginalImage(true);
            productItemResponse.getProductItemImages().add(imageResponseTrue);
            DistributionProductImageResponse imageResponseFalse = getDistributionProductImageResponse(productImage);
            imageResponseFalse.setOriginalImage(false);
            productItemResponse.getProductItemImages().add(imageResponseFalse);
          }
        }
        for (ProductItemAttribute itemAttribute : productItem.getProductItemAttributes()) {
          DistributionProductAttributeResponse attributeResponse =
              new DistributionProductAttributeResponse();
          BeanUtils.copyProperties(itemAttribute, attributeResponse);
          productItemResponse.getProductItemAttributes().add(attributeResponse);
        }
        if (StringUtils.isNotEmpty(productItem.getItemNotes())) {
          ItemNotesResponse itemNotesResponse =
              mapper.readValue(productItem.getItemNotes(), ItemNotesResponse.class);
          productItemResponse.setItemNotes(itemNotesResponse);
        }
        distributionProductItemResponses.add(productItemResponse);
      }
      response.setProductItems(distributionProductItemResponses);
      List<DistributionProductAttributeResponse> distributionProductAttributeResponses =
          new ArrayList<DistributionProductAttributeResponse>();
      for (ProductAttribute attribute : product.getProductAttributes()) {
        DistributionProductAttributeResponse attributeResponse =
            new DistributionProductAttributeResponse();
        BeanUtils.copyProperties(attribute, attributeResponse);
        distributionProductAttributeResponses.add(attributeResponse);
      }
      response.setProductAttributes(distributionProductAttributeResponses);

      List<DistributionProductImageResponse> distributionProductImageResponses =
          new ArrayList<DistributionProductImageResponse>();
      for (ProductImage image : product.getProductImages()) {
        if (Objects.nonNull(image.getOriginalImage())) {
          DistributionProductImageResponse imageResponse = new DistributionProductImageResponse();
          BeanUtils.copyProperties(image, imageResponse);
          distributionProductImageResponses.add(imageResponse);
        } else {
          DistributionProductImageResponse imageResponseTrue = new DistributionProductImageResponse();
          BeanUtils.copyProperties(image, imageResponseTrue);
          imageResponseTrue.setOriginalImage(true);
          DistributionProductImageResponse imageResponseFalse = new DistributionProductImageResponse();
          BeanUtils.copyProperties(image, imageResponseFalse);
          imageResponseFalse.setOriginalImage(false);
          distributionProductImageResponses
              .addAll(new ArrayList<>(Arrays.asList(imageResponseFalse, imageResponseTrue)));
        }
      }
      response.setProductImages(distributionProductImageResponses);
    }
    if (product.getCurrentVendor() != null) {
      VendorDetailResponse vendorDetailResponse = new VendorDetailResponse();
      BeanUtils.copyProperties(product.getCurrentVendor(), vendorDetailResponse);
      response.setCurrentVendor(vendorDetailResponse);
      response.setVendorName(product.getCurrentVendor().getName());
    }

    if (Objects.isNull(product.getImageViolations()) && Objects.isNull(productImageQcFeedbackService
        .findProductQcFeedbackResponseByProductCode(product.getStoreId(), product.getProductCode()))) {
      response.setEnableImageFeedback(false);
    } else {
      response.setEnableImageFeedback(true);
    }
    if (StringUtils.isNotEmpty(product.getProductNotes())) {
      ProductNotesResponse productNotesResponse =
          mapper.readValue(product.getProductNotes(), ProductNotesResponse.class);
      response.setProductNotes(productNotesResponse);
    }
    response.setRevised(product.isRevised());
    response.setRestrictedKeywordsPresent(product.isRestrictedKeywordsPresent());
    response.setAutoNeedRevision(product.isAutoNeedRevision());
    response.setProductType(product.getProductType());
    response.setReviewType(Objects.nonNull(product.getReviewType()) ?
      product.getReviewType().name() : null);
    response.setImageViolations(productConverterUtil.getOrderedTextAndImageViolations(product, vendorProductLabelsOrdered));
    response.setShowProductUrl(product.isPostLive() && !product.isForceReview());
    if(StringUtils.isNotBlank(product.getAiGeneratedFields())) {
      response.setAiGeneratedFieldsResponse(
          mapper.readValue(product.getAiGeneratedFields(), AiGeneratedFieldsResponse.class));
    }
    List<DistributionProductImageResponse> images = response.getProductImages();
    if (ImageUtils.overrideMainImageIfNotPresent(overrideMainImageFlag, images)) {
      images.stream().filter(DistributionProductImageResponse::isCommonImage)
        .filter(img -> Boolean.FALSE.equals(img.getOriginalImage())).findFirst()
        .ifPresent(img -> img.setMainImage(true));
    }
    return response;
  }

  private DistributionProductImageResponse getDistributionProductImageResponse(ProductItemImage productImage) {
    DistributionProductImageResponse imageResponse = new DistributionProductImageResponse();
    BeanUtils.copyProperties(productImage, imageResponse, "mainImage");
    if (Objects.nonNull(productImage.getMainImage())) {
      imageResponse.setMainImage(productImage.getMainImage());
    }
    return imageResponse;
  }

  public static DistributionTaskMultipleFilterDTO getDistributionTaskMultipleFilterRequest(String storeId,
      DistributionTaskMultipleFilterRequest request) throws ParseException, UnsupportedEncodingException {
    if (Objects.isNull(request)) {
      return new DistributionTaskMultipleFilterDTO();
    }
    DistributionTaskMultipleFilterDTO distributionTaskmultipleFilterDTO = new DistributionTaskMultipleFilterDTO();
    BeanUtils.copyProperties(request, distributionTaskmultipleFilterDTO, "statusList", "vendorCodes", "rejectedList",
        "timeFilterType");
    distributionTaskmultipleFilterDTO.setStoreId(storeId);
    if (!CollectionUtils.isEmpty(request.getStatusList())) {
      List<String> statusList = request.getStatusList();
      List<WorkflowState> workflowStates = new ArrayList<>();
      for (String status : statusList) {
        WorkflowState statusFormat = WorkflowState.valueOf(status.trim().toUpperCase());
        workflowStates.add(statusFormat);

      }
      distributionTaskmultipleFilterDTO.setStatusList(workflowStates);
    }
    if (!CollectionUtils.isEmpty(request.getRejectedList())) {

      distributionTaskmultipleFilterDTO.setRejectedList(request.getRejectedList());
    }
    if (!CollectionUtils.isEmpty(request.getVendorCodes())) {

      distributionTaskmultipleFilterDTO.setVendorCodes(request.getVendorCodes());
    }
    if (StringUtils.isNotEmpty(request.getTimeFilterType())) {
      TimeFilterType timeFilterType = TimeFilterType.getTimeFilterTypeByValue(request.getTimeFilterType());
      distributionTaskmultipleFilterDTO.setTimeFilterType(timeFilterType);
    }
    if (StringUtils.isNotEmpty(request.getStartDate())) {
      distributionTaskmultipleFilterDTO.setStartDate(
          DATE_TIME_FORMATTER.parseDateTime(URLDecoder.decode(request.getStartDate(), CHAR_ENCODING)).toDate());
    }
    if (StringUtils.isNotEmpty(request.getEndDate())) {
      distributionTaskmultipleFilterDTO.setEndDate(
          DATE_TIME_FORMATTER.parseDateTime(URLDecoder.decode(request.getEndDate(), CHAR_ENCODING)).toDate());
    }
    return distributionTaskmultipleFilterDTO;
  }


  private DistributionTaskFilterDTO getDistributionTaskFilterRequest(String storeId,
      DistributionTaskFilterRequest request) throws ParseException, UnsupportedEncodingException {
    if (request == null) {
      return new DistributionTaskFilterDTO();
    } else {
      DistributionTaskFilterDTO distributionTaskFilterDTO = new DistributionTaskFilterDTO();
      BeanUtils.copyProperties(request, distributionTaskFilterDTO, "status");
      distributionTaskFilterDTO.setStoreId(storeId);
      if (!StringUtils.isEmpty(request.getStatus())) {
        distributionTaskFilterDTO
            .setStatus(WorkflowState.valueOf(request.getStatus().trim().toUpperCase()));
      }
      return distributionTaskFilterDTO;
    }
  }

  @RequestMapping(value = DETECT_EDIT_BY_MERCHANT, method = RequestMethod.GET, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "To detect edit by merchant while vendor review", description = "To detect edit by merchant while vendor review")
  @ResponseBody
  public GdnRestSimpleResponse<Boolean> detectEditByMerchant(@RequestParam String requestId,
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String username, @PathVariable("productCode") String productCode,
      @RequestParam long version)
      throws Exception {
    String errorMessage = StringUtils.EMPTY;
    boolean isSuccess = false;
    Boolean edited = true;
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId);
    try {
      log.info(
          "API to find whether its modified during vendor verification or not. productCode: {}",
          productCode);
      edited = productService.getEditedByMerchant(productCode, version);
      isSuccess = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error("Error while checking edit by merchant api. productCode: {}", productCode, e);
    }
    return new GdnRestSimpleResponse<>(errorMessage, null, isSuccess, requestId, edited);
  }

  @RequestMapping(value = COUNT_PRODUCT_STATUS_FOR_VENDOR, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "count products status for vendor", description = "count products status for vendor")
  @Deprecated(since = Constants.DEPRECATED_VERSION, forRemoval = true)
  public GdnRestSingleResponse<VendorProductStatusResponse> countProductStatusForVendor(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String vendorCode)
      throws Exception {
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId);
    try {
      log.debug("Api to countProductStatusForVendor {} ",
          new Object[] {vendorCode, mandatoryRequestParam});
      Vendor vendor = vendorService.findByVendorCode(vendorCode);
      Validate.isTrue(vendor != null, "VendorCode is invalid", vendorCode);
      List<VendorProductStatusDTO> vendorProductStatus = productService.findProductStatusForVendor(vendor, storeId);
      VendorProductStatusResponse response = getProductStatusResponse(vendorProductStatus);
      response.setCapacity(vendor.getQuota());
      return new GdnRestSingleResponse<VendorProductStatusResponse>(null, null, true,
          response, requestId);
    } catch(Exception e){
      log.error("Error getting countProductStatusForVendor ", e);
      return new GdnRestSingleResponse<VendorProductStatusResponse>(e.getMessage(), null,
          false, null, requestId);
    }
  }

  private VendorProductStatusResponse getProductStatusResponse(
      List<VendorProductStatusDTO> vendorProductStatus) throws IllegalAccessException {
    VendorProductStatusResponse response = new VendorProductStatusResponse();
    for (VendorProductStatusDTO vendorProductState : vendorProductStatus) {
      String fieldToWrite = WORKFLOW_STATE_STATUS_MAP.get(vendorProductState.getState());
      if (StringUtils.isNotBlank(fieldToWrite)) {
        FieldUtils.writeDeclaredField(response, fieldToWrite, vendorProductState.getCount(), true);
      }
    }
    return response;
  }

  @RequestMapping(value = RETRIEVE_PRODUCT_DETAIL, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "get product details", description = "get product details")
  public GdnRestSingleResponse<DistributionProductDetailResponse> getProductDetails(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String productCode)
      throws Exception {
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId);
    try {
      log.debug(
          "Api to fetch product detail for controller. productCode: {}, mandatoryRequestParam: {}",
          new Object[] {productCode, mandatoryRequestParam});
      Product product = productWrapperService.getAllProductDetailsByCodeAndMarkForDeleteFalse(productCode);
      ProductReviewer productReviewer =
        this.productReviewerService.findProductReviewerByProductCode(productCode);
      DistributionProductDetailResponse response = null;
      response = getDistributionProductDetailResponse(product, response);
      this.productConverterUtil.setReviewerDetails(productReviewer, response);
      return new GdnRestSingleResponse<>(null, null, true,
          response, requestId);
    } catch (Exception e) {
      log.error(
          "Error retrieving product detail for controller Product Code : {}. "
              + "MandatoryRequestParam: {}",
          productCode, mandatoryRequestParam, e);
      return new GdnRestSingleResponse<>(e.getMessage(), null,
          false, null, requestId);
    }
  }

  @RequestMapping(value = GET_BUSINESS_PARTNER_LIST_FOR_VENDOR, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE}) @ResponseBody
  @Operation(summary = "get business partner for vendor", description = "get business partner for vendor")
  @Deprecated(since = Constants.DEPRECATED_VERSION, forRemoval = true)
  public GdnRestListResponse<ProductBusinessPartnerMapperResponse> getBusinessPartnerForVendor(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String vendorCode,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    MandatoryRequestParam mandatoryRequestParam = null;
    Pageable pageable = PageRequest.of(page, size);

    GdnRestListResponse<ProductBusinessPartnerMapperResponse> response = null;
    try {
      mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId);
      log.debug(
          "Api to fetch list of business partner of vendor details for all types of product. "
              + "vendorCode: {}, " + "mandatoryRequestParam: {}", vendorCode,
          mandatoryRequestParam);
      String vendorId = productService.getvendorIdByVendorCode(vendorCode);
      ExceptionUtil.checkConditions(!StringUtils.isEmpty(vendorId),
          ExceptionMsg.EXCEPTION_VENDOR_CODE_INVALID_OR_NULL.getValue());
      List<ProductBusinessPartnerMapperResponse> businessPartnerList =
          new ArrayList<ProductBusinessPartnerMapperResponse>();

      Page<ProductBusinessPartnerMapper> businessPartners =
          productService.getBusinessPartnerForVendor(vendorId, pageable);
      for (ProductBusinessPartnerMapper businessPartner : businessPartners.getContent()) {
        ProductBusinessPartnerMapperResponse businessPartnerMapperResponse =
            new ProductBusinessPartnerMapperResponse();
        String businessPartnerCode = businessPartner.getBusinessPartnerCode();
        String businessPartnerName = businessPartner.getBusinessPartnerName();

        if (!StringUtils.isEmpty(businessPartnerCode) && !StringUtils
            .isEmpty(businessPartnerName)) {
          businessPartnerMapperResponse.setBusinessPartnerCode(businessPartnerCode);
          businessPartnerMapperResponse.setBusinessPartnerName(businessPartnerName);
          businessPartnerList.add(businessPartnerMapperResponse);
        }
      }
      response = new GdnRestListResponse<ProductBusinessPartnerMapperResponse>(businessPartnerList,
          new PageMetaData(page, size, businessPartners.getTotalElements()), requestId);

    }
    catch (Exception e) {
      log.error("Error to filter business partner for specific vendor.  request id : {}, "
          + " mandatoryRequestParam: {}", requestId, mandatoryRequestParam, e);
      response =
          new GdnRestListResponse<ProductBusinessPartnerMapperResponse>(e.getMessage(), null, false,
              requestId);

    }
    return  response;
  }

  @RequestMapping(value = GET_PRODUCT_DETAIL_ALL, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "getProductDetailsForAllProductTypes", description = "getProductDetailsForAllProductTypes")
  public GdnRestSingleResponse<DistributionProductDetailResponse>
  getProductDetailsForAllProductTypes(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam String productCode) throws Exception {
    String errorMessage = StringUtils.EMPTY;
    boolean isSuccess = false;
    MandatoryRequestParam mandatoryRequestParam = null;
    DistributionProductDetailResponse response = new DistributionProductDetailResponse();
    try {
      mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId);
      ExceptionUtil.checkConditions(productCode != null,
          ExceptionMsg.EXCEPTION_PRODUCT_CODE_NULL.getValue());
      log.debug(
          "Api to fetch product details for all types of product. productCode: {}, "
              + "mandatoryRequestParam: {}", productCode, mandatoryRequestParam);
      Product product = productService.getDetailsForAnyProductTypeByCode(productCode);
      response = getDistributionProductDetailResponse(product, response);
      isSuccess = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error("error retrieving product details for all types of product. productCode: {}, "
          + "mandatoryRequestParam: {} ", productCode, mandatoryRequestParam, e);
    }
    return new GdnRestSingleResponse<>(errorMessage, null, isSuccess, response, requestId);
  }

  @RequestMapping(value = GET_PRODUCT_STATUS_IN_REVIEW, method = RequestMethod.GET, produces = MediaType
      .APPLICATION_JSON_VALUE)
  @Operation(summary = "get product status count in vendor", description = "get product status count in vendor")
  @ResponseBody
  public GdnRestSingleResponse<MapResponse> getProductInReviewStatus(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username, @RequestParam String vendorCode,
    @RequestParam(required = false) Boolean postLive,
    @RequestParam(required = false) Boolean edited,
    @RequestParam(required = false) Boolean revised)
      throws Exception {
    log.info("Get vendor primary filter values for vendorCode : {}, postlive : {}. edited : {}, "
        + "revised : {} for Request : {} ", vendorCode, postLive, edited, revised, requestId);
    Map<String, Object> response = productService.getProductStatusByVendor(storeId, vendorCode,
      postLive, edited, revised);
    return new GdnRestSingleResponse<>(null, null, true, new MapResponse(response), requestId);
  }

  @RequestMapping(value = GET_PRODUCT_REVIEW_CONFIG_COUNTS, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product count by review config", description = "pre live post live review counts")
  @ResponseBody
  @Deprecated(since = Constants.DEPRECATED_VERSION, forRemoval = true)
  public GdnRestSingleResponse<MapResponse> getProductReviewConfigCounts(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String vendorCode) throws Exception {
    log.info("Get vendor review config product counts : {}", vendorCode);
    try {
      Map<String, Object> response = productService.getReviewConfigProductCountByVendor(storeId, vendorCode);
      return new GdnRestSingleResponse<>(null, null, true, new MapResponse(response), requestId);
    } catch (Exception e) {
      log.error("Error fetching product review config for products ", e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }

  private DistributionProductResponse getProductListResponse(
      Map<String, List<WorkflowState>> workflowStateMap, Product product) {
    DistributionProductResponse distributionProductResponse = new DistributionProductResponse();
    BeanUtils.copyProperties(product, distributionProductResponse);
    if (product.getCurrentVendor() != null) {
      VendorDetailResponse vendorDetailResponse = new VendorDetailResponse();
      BeanUtils.copyProperties(product.getCurrentVendor(), vendorDetailResponse);
      distributionProductResponse.setCurrentVendor(vendorDetailResponse);
      distributionProductResponse.setVendorCode(product.getCurrentVendor().getVendorCode());
      distributionProductResponse.setVendorId(product.getCurrentVendor().getId());
      distributionProductResponse.setVendorName(product.getCurrentVendor().getName());
    }
    distributionProductResponse.setCreatedDate(product.getCreatedDate());
    distributionProductResponse.setProductId(product.getId());
    if (workflowStateMap.containsKey(product.getId())) {
      setProductListingFlags(workflowStateMap, product, distributionProductResponse);
    }
    return distributionProductResponse;
  }


  private DistributionProductResponse getProductListResponse(String vendorCode, Product product) {
    DistributionProductResponse distributionProductResponse = new DistributionProductResponse();
    BeanUtils.copyProperties(product, distributionProductResponse);
    if (product.getState() != null) {
      distributionProductResponse.setState(
          WorkflowWebState
              .valueOf(product.getState().toString()));
    }
    distributionProductResponse.setVendorCode(vendorCode);
    distributionProductResponse.setProductId(product.getId());
    distributionProductResponse.setCreatedDate(product.getProductCreatedDate());
    WorkflowState workflowState = product.getState();
    if (workflowState != null) {
      setApprovalFlagsForProductResponse(distributionProductResponse, workflowState);
    }
    return distributionProductResponse;
  }

  private void setApprovalFlagsForProductResponse(
      DistributionProductResponse distributionProductResponse, WorkflowState workflowState) {
    if (WorkflowState.PASSED.equals(workflowState)) {
      distributionProductResponse.setProductApproved(true);
    }
  }


  @RequestMapping(value = GET_PRODUCT_DISTRIBUTION_SUMMARY_BY_MULTIPLE_FILTER, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get products listing for applied multiple filter", description = "get products listing for applied multiple filter")
  @ResponseBody
  public GdnRestListResponse<DistributionProductResponse> getSummaryByMultipleFilter(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam(required = false) String sortBy,
      @RequestBody(required = false) DistributionTaskMultipleFilterRequest request) throws Exception {
    String errorMessage = StringUtils.EMPTY;
    boolean isSuccess = false;
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    List<DistributionProductResponse> response = new ArrayList<>();
    long totalRecords = 0;
    try {
      log.info("Api call to get product listing for applied multiple filters. MandatoryRequestParam: {}",
          mandatoryRequestParam);
      DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO =
          getDistributionTaskMultipleFilterRequest(storeId, request);
      Page<Product> productList = null;
      if (Direction.ASC.toString().equalsIgnoreCase(sortBy)) {
        distributionTaskMultipleFilterDTO.setSortOrderByCreatedDate(ASC);
        productList = this.productService.getAllProductDetailsWithMultipleFilter(
            distributionTaskMultipleFilterDTO,
            PageRequest.of(page, size, Sort.by(Direction.ASC, CREATED_DATE)), storeId);
      } else {
        distributionTaskMultipleFilterDTO.setSortOrderByCreatedDate(DESC);
        productList = this.productService.getAllProductDetailsWithMultipleFilter(
            distributionTaskMultipleFilterDTO,
            PageRequest.of(page, size, Sort.by(Direction.DESC, CREATED_DATE)), storeId);
      }
      productService.setVendorForProductList(productList.getContent());
      totalRecords = productList.getTotalElements();
      if (!CollectionUtils.isEmpty(productList.getContent())) {
        for (Product product : productList.getContent()) {
          DistributionProductResponse distributionProductResponse =
              productConverterUtil.getProductListResponseWithoutProductId(
              (Objects.nonNull(product.getCurrentVendor()) ?
                  product.getCurrentVendor().getVendorCode() :
                  StringUtils.EMPTY), product);
          response.add(distributionProductResponse);
        }
      }
      isSuccess = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error("Error retrieving product listing for given vendor. MandatoryRequestParam: {} error {}",
          mandatoryRequestParam, e.getMessage(), e);
    }
    return new GdnRestListResponse<>(errorMessage, null, isSuccess, response,
        new PageMetaData(size, page, totalRecords), requestId);
  }

  @RequestMapping(value = COUNT_PRODUCT_DISTRIBUTION_SUMMARY_BY_FILTER, method = RequestMethod.POST,
      produces = {MediaType.APPLICATION_JSON_VALUE},
      consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "count products listing for given filter",
      description = "count products listing for given filter")
  @ResponseBody
  public GdnRestSingleResponse<MapResponse> countDistributionSummaryByFilter(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(required = false, defaultValue = "true") Boolean includeStatus,
      @RequestParam(required = false, defaultValue = "true") Boolean includeVendors,
      @RequestBody(required = false) DistributionTaskMultipleFilterRequest request) throws Exception {
    String errorMessage = StringUtils.EMPTY;
    Map<String, Object> response = new HashMap<>();
    MapResponse mapResponse = new MapResponse();
    boolean isSuccess = false;
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    try {
      log.info("Api call to get product listing for given filter. MandatoryRequestParam: {}",
          mandatoryRequestParam);
      DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO =
          getDistributionTaskMultipleFilterRequest(storeId, request);
      response = this.productService.countAllProductDetailsWithMultipleFilter(includeStatus, includeVendors,
          distributionTaskMultipleFilterDTO);
      mapResponse.setMap(response);
      isSuccess = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error(
          "Error retrieving product listing for given vendor. MandatoryRequestParam: {} error {}",
          mandatoryRequestParam, e.getMessage(), e);
    }
    return new GdnRestSingleResponse<MapResponse>(errorMessage, null, isSuccess, mapResponse,
        requestId);
  }

  private void setProductListingFlags(Map<String, List<WorkflowState>> workflowStateMap,
      Product product, DistributionProductResponse distributionProductResponse) {
    for (WorkflowState workflowState : workflowStateMap.get(product.getId())) {
      setApprovalFlagsForProductResponse(distributionProductResponse, workflowState);
    }
  }

  @RequestMapping(value = ROOT + FILTER_BUSINESS_PARTNER, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get unique business partner by workFlowState ",
      description = "get unique business partner  by workFlowState")
  @ResponseBody
  public GdnRestListResponse<ProductBusinessPartnerMapperResponse>
  filterProductBusinessPartnerMapperByWorkFlowState(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam boolean isSearch,
      @RequestParam(required = false) String searchCriteria, @RequestParam String workflowState)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    try {
      MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      log.info(
          "API to filter business partner based on workFlowState products. Mandatory Request "
              + "Parameter : {}, Pageable : {}, searchCriteria : {}, workflowState : {}",
          new Object[] {mandatoryRequestParam, pageable, searchCriteria, workflowState});
      List<ProductBusinessPartnerMapperResponse> response =
          new ArrayList<ProductBusinessPartnerMapperResponse>();
      WorkflowState state = WorkflowState.valueOf(workflowState.toUpperCase());
      Page<ProductBusinessPartnerMapper> businessPartners = productService
          .findProductBusinessPartnerMapper(state, searchCriteria, pageable, isSearch, storeId);
      for (ProductBusinessPartnerMapper businessPartner : businessPartners.getContent()) {
        ProductBusinessPartnerMapperResponse wrapper = new ProductBusinessPartnerMapperResponse();
        if (StringUtils.isNotBlank(businessPartner.getBusinessPartnerCode()) && StringUtils
            .isNotBlank(businessPartner.getBusinessPartnerName())) {
          wrapper.setBusinessPartnerCode(businessPartner.getBusinessPartnerCode());
          wrapper.setBusinessPartnerName(businessPartner.getBusinessPartnerName());
          response.add(wrapper);
        }
      }
      return new GdnRestListResponse<ProductBusinessPartnerMapperResponse>(null, null, true,
          response, new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
          businessPartners.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error(
          "Error to filter business partner based on workFlowState products.  Pageable : {},"
              + "searchCriteria : {}, workflowState : {} , request id : {} ",
          pageable, searchCriteria, workflowState, requestId, e);
      return new GdnRestListResponse<ProductBusinessPartnerMapperResponse>(e.getMessage(), null,
          false, null, new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), 0),
          requestId);
    }
  }

  @RequestMapping(value = GET_PRODUCT_DOMAIN_MODEL_RESPONSE, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get PDTProductDomainEventModelResponse by productCode",
      description = "get PDTProductDomainEventModelResponse by productCode")
  @ResponseBody
  public GdnRestSingleResponse<PDTProductDomainEventModelResponse> getProductDomainEventModelResponse(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam String productCode)
      throws Exception {
    String errorMessage = StringUtils.EMPTY;
    boolean isSuccess = false;
    MandatoryRequestParam mandatoryRequestParam = null;
    PDTProductDomainEventModelResponse response = new PDTProductDomainEventModelResponse();
    try {
      mandatoryRequestParam=MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      log.info(
          "api to get PDTProductDomainEventModelResponse from productCode. mandatoryRequestParam:"
              + " {}, productCode: {}", mandatoryRequestParam, productCode);
      Product product = productService.getDetailsForAnyProductTypeByCode(productCode);
      if (autoHealEnabledForGetProductDomainModelResponseApi) {
        product = productService.autoHealProductData(product, Constants.AUTOHEAL_GET_PRODUCT_MODEL_API);
      }
      PDTProductDomainEventModel pdtProductDomainEventModel = productPublisherService
          .convertProductToProductDomainEventModel(product,
              !(product.isMarkForDelete() || WorkflowState.PASSED.equals(product.getState())));
      response.setPdtProductDomainEventModel(pdtProductDomainEventModel);
      isSuccess=true;
    } catch (Exception e) {
      errorMessage=e.getMessage();
      log.error(
          "error retrieving PDTProductDomainEventModelResponse from productCode. " +
              "mandatoryRequestParam: {}, productCode: {}", mandatoryRequestParam, productCode, e);
    }
    return new GdnRestSingleResponse<>(errorMessage, null, isSuccess, response, requestId);
  }

  @RequestMapping(value = IS_PRODUCT_EXIST, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "api to check product existence", description = "api to check product existence")
  @ResponseBody
  public GdnRestSimpleResponse<Boolean> isProductExists(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String productCode,
      @RequestParam(defaultValue = "false") boolean allProducts) throws Exception {
    String errorMessage = StringUtils.EMPTY;
    boolean isSuccess = false;
    MandatoryRequestParam mandatoryRequestParam = null;
    Boolean productExists = false;
    try {
      mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      log.info("api to get product existence detail. mandatoryRequestParam: {}, productCode: {}",
          mandatoryRequestParam, productCode);
      if (allProducts) {
        productExists = Objects.nonNull(productService.getProductByCode(productCode));
      } else {
        productExists = Objects.nonNull(productService.getProductByProductCodeAndMarkForDeleteFalse(productCode));
      }
      isSuccess = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error(
          "error while retrieving product existence detail. mandatoryRequestParam: {}, "
              + "productCode: {} ", mandatoryRequestParam, productCode, e);
    }
    return new GdnRestSimpleResponse<>(errorMessage, null, isSuccess, requestId, productExists);
  }

  @RequestMapping(value = REPUBLISH_EDITED_PRODUCTS, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "api to check product existence", description = "api to check product existence")
  @ResponseBody
  public GdnBaseRestResponse republishEditedProducts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable(value = "productCode") String productCode) throws Exception {
    try {
      log.info("Republish edited product productCode : {}", productCode);
      productService.republishEditedProduct(productCode);
    } catch (Exception e) {
      log.error(
          "Republish edited product failed productCode : {}", productCode, e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
    return new GdnBaseRestResponse(null, null, true, requestId);
  }


  @RequestMapping(value = GET_PRODUCT_CODES, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product codes from PDT", description = "get products from PDT")
  @ResponseBody
  @Deprecated(since = Constants.DEPRECATED_VERSION, forRemoval = true)
  public GdnRestSingleResponse<ProductListResponse> getProductCodeList(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductCodeListRequest request) throws Exception {
    List<String> productList = new ArrayList<>();
    Boolean isSuccess = Boolean.FALSE;
    String errorMsg = StringUtils.EMPTY;
    try {
      MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      log.info("Api call to get product codes from PDT. MandatoryRequestParam: {}",
          mandatoryRequestParam);
      productList = productService.getProductCodeList(request.getProductList());
      isSuccess = Boolean.TRUE;
    } catch (Exception e) {
      errorMsg = e.getMessage();
      log.error(e.getMessage(), e);
    }
    return new GdnRestSingleResponse<ProductListResponse>(errorMsg, null, isSuccess,
        new ProductListResponse(productList), requestId);
  }

  @RequestMapping(value = ProductDistributionController.REJECT_PRODUCT, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Api to reject product", description = "Api to reject product")
  @ResponseBody
  public GdnBaseRestResponse rejectProduct(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody RejectProductListRequest request) throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    try {
      GdnPreconditions.checkArgument(request != null, "request must not be null");
      GdnPreconditions.checkArgument(!CollectionUtils.isEmpty(request.getProducts()), "products must not be empty");
      for (RejectProductVendorRequest rejectProductRequest : request.getProducts()) {
        RejectProductDTO rejectProductDTO = new RejectProductDTO();
        BeanUtils.copyProperties(rejectProductRequest, rejectProductDTO);
        this.productService.rejectProduct(rejectProductDTO);
      }
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("error on invoking reject product, requestId: {}", requestId, e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }

  @RequestMapping(value = ProductDistributionController.FILTER_BUSINESS_PARTNERS_BY_PRODUCT_LIST, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Api to get businessPartners based on selected primary filters")
  @ResponseBody
  @Deprecated(since = Constants.DEPRECATED_VERSION, forRemoval = true)
  public GdnRestListResponse<ProductBusinessPartnerMapperResponse> getBusinessPartnerList(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "30") int size, @RequestBody PrimaryFilterRequest requestFilter) {
    try {
      log.info("Fetching business partner list for currently selected primary filters : {}", requestFilter);
      PrimaryFilterDTO primaryFilterDTO = new PrimaryFilterDTO();
      BeanUtils.copyProperties(requestFilter, primaryFilterDTO);
      return this.productService.getBusinessPartnerList(storeId, requestId, primaryFilterDTO, page, size);
    } catch (Exception e) {
      return new GdnRestListResponse<>(e.getMessage(), null, false, null, null, requestId);
    }
  }

  @RequestMapping(value = ProductDistributionController.FILTER_ASSIGNEES_BY_PRODUCT_LIST, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Api to get assignee based on selected primary filters")
  @ResponseBody
  @Deprecated(since = Constants.DEPRECATED_VERSION, forRemoval = true)
  public GdnRestListResponse<VendorAssigneeResponse> getAssigneeList(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "30") int size, @RequestBody PrimaryFilterRequest requestFilter) {
    try {
      log.info("Fetching assignee list for currently selected primary filters : {}", requestFilter);
      PrimaryFilterDTO primaryFilterDTO = new PrimaryFilterDTO();
      BeanUtils.copyProperties(requestFilter, primaryFilterDTO);
      List<String> assigneeEmailIds  = this.productService.getAssigneeList(storeId, requestId, primaryFilterDTO);
      VendorAssigneeResponse vendorAssigneeResponse;
      List<VendorAssigneeResponse> vendorAssigneeResponseList = new ArrayList<>();
      for(String assigneeEmailId : assigneeEmailIds) {
        vendorAssigneeResponse = new VendorAssigneeResponse(assigneeEmailId);
        vendorAssigneeResponseList.add(vendorAssigneeResponse);
      }
      return new GdnRestListResponse<>(null, null, true, vendorAssigneeResponseList,
          new PageMetaData(page, size, vendorAssigneeResponseList.size()), requestId);
    } catch (Exception e) {
      return new GdnRestListResponse<>(e.getMessage(), null, false, null, null, requestId);
    }
  }

  @RequestMapping(value = ProductDistributionController.FILTER_SUMMARY, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch product listing based on primary and secondary filters")
  @ResponseBody
  public GdnRestListResponse<DistributionProductResponse> getProductList(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "30") int size, @RequestBody FilterSummaryRequest filterSummaryRequest) {
    log.info("Fetching product listing for filterSummaryRequest: {}, page: {}, size {}",
      filterSummaryRequest, page, size);
    try {
      SummaryFilterDTO summaryFilterDTO = new SummaryFilterDTO();
      BeanUtils.copyProperties(filterSummaryRequest, summaryFilterDTO);
      Page<ProductAndReviewerDetailsDTO> productPage =
          this.productService.getProductList(storeId, requestId, username, summaryFilterDTO, page, size);
      List<DistributionProductResponse> distributionProductResponseList =
          productConverterUtil.convertToDistributionProductResponse(productPage, vendorProductLabelsOrdered);
      return new GdnRestListResponse<>(distributionProductResponseList,
          new PageMetaData(page, size, productPage.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error fetching product list for vendor : ", e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, null, null, requestId);
    }
  }

  @RequestMapping(value = SEND_PRODUCT_BACK_TO_VENDOR, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "send product back for vendor approval", description = "send product back for vendor approval")
  @ResponseBody
  public GdnBaseRestResponse sendProductBackToVendor(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam String productCode) {
    try {
      log.info("APi to return product back to vendor , productCode : {}", productCode);
      productWrapperService.sendProductBackToVendorAndReindexSolr(productCode);
      return new GdnBaseRestResponse(null, null, true, null);
    } catch (Exception e) {
      log.error("error sending product back to vendor, productCode: {}", productCode, e);
    }
    return new GdnBaseRestResponse(null, null, false, null);
  }

  @RequestMapping(value = GET_REVIEW_CONFIG_COUNT, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get count of newAdded/edited products for review config",
      description = "get count of newAdded/edited products for review config")
  @ResponseBody
  public GdnRestSingleResponse<MapResponse> getReviewConfigCount(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String vendorCode, @RequestParam boolean postLive) throws Exception {
    log.info("Get vendor review config product counts for vendor: {} and postLive : {} ", vendorCode, postLive);
    try {
      Map<String, Object> response =
          productService.getReviewConfigProductCountByVendorAndConfig(storeId, vendorCode, postLive);
      return new GdnRestSingleResponse<>(null, null, true, new MapResponse(response), requestId);
    } catch (Exception e) {
      log.error("Error fetching product review config for products ", e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }

  @RequestMapping(value = SEND_PRODUCT_TO_AUTO_NEED_REVISION, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "send product to auto need revision", description = "send product to auto need revision")
  @ResponseBody
  public GdnBaseRestResponse sendProductToAutoNeedRevision(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "true") boolean validateAssignment,
      @RequestBody AutoNeedRevisionRequest autoNeedRevisionRequest) {
    try {
      log.info("APi to send product to auto need revision , productCode : {}", autoNeedRevisionRequest.getProductCode());
      productWrapperService.updateProductToAutoNeedRevision(storeId, autoNeedRevisionRequest, validateAssignment);
      return new GdnBaseRestResponse(null, null, true, null);
    } catch (Exception e) {
      log.error("error while sending product to auto need revision, productCode: {} ", autoNeedRevisionRequest.getProductCode(), e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, null);
    }
  }

  @RequestMapping(value = PRODUCT_RETRY_STATUS_UPDATE, method = RequestMethod.POST,
    produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Product Retry status update", description = "Product Retry status update")
  public GdnBaseRestResponse productRetryStatusUpdate(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username, @PathVariable String productCode,
    @RequestBody ProductRetryStatusUpdate productRetryStatusUpdate) {
    log.info("Update status for product : {}, request : {}", productCode, productRetryStatusUpdate);
    try {
      this.productService.updateProductRetryStatus(storeId, productCode, productRetryStatusUpdate);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Exception on product status update for : {}, request : {}, error - ",
        productCode, productRetryStatusUpdate, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
        requestId);
    }
  }

  @RequestMapping(value = FETCH_PRODUCTS_FOR_AUTO_ASSIGNMENT, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch Products For Auto Assignment with Boost and Filter", description = "Fetch Filtered Boosted Products")
  public GdnRestListResponse<ProductCodeResponse> fetchProductsForAutoAssignment(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam String username,
    @RequestParam(defaultValue = "0") Integer page,
    @RequestParam(defaultValue = "1000") Integer size,
    @RequestBody BoostedProductFilterRequest boostedProductFilterRequest) {
    try {
      Page<ProductCodeResponse> productPage =
        this.productService.filterProductWithBoostForAutoAssignment(storeId, requestId, username,
          boostedProductFilterRequest, page, size);
      return new GdnRestListResponse<>(productPage.getContent(),
        new PageMetaData(page, size, productPage.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error fetching product list for vendor : ", e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, null, null, requestId);
    }
  }

  @RequestMapping(value = UPDATE_PRODUCT_BRAND, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update Product Brand", description = "Update Product Brand")
  public GdnBaseRestResponse updateProductBrand(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody ChangeBrandRequest changeBrandRequest) {
    log.info("Update brand for product : {}, request : {} ", changeBrandRequest.getProductCode(), changeBrandRequest);
    try {
      productWrapperService.updateBrandInProductAndProductItems(changeBrandRequest);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Exception on product brand update for : {}, request : {}, error - ",
          changeBrandRequest.getProductCode(), changeBrandRequest, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @PostMapping(value = APPEAL_PRODUCT, consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {
    MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Update appealed products", description = "Update appealed products")
  public GdnRestSingleResponse<AppealProductResponse> updateAppealProduct(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestBody AppealProductRequest appealProductRequest) throws Exception {
    AppealProductResponse appealProductResponse = null;
    try {
      log.info("Update appeal for product code : {}, request : {}",
        appealProductRequest.getProductCode(), appealProductRequest);
      appealProductResponse = productService.updateAppealProduct(appealProductRequest, storeId);
      return new GdnRestSingleResponse<>(appealProductResponse.getErrorMessage(),
        appealProductResponse.getErrorCode(), true, appealProductResponse, requestId);
    } catch (Exception e) {
      log.error("Error on saving appealed product for request : {} with exception : ",
        appealProductRequest, e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, appealProductResponse,
        requestId);
    }
  }
}
