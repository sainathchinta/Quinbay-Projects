package com.gdn.mta.product.controller;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import com.gda.mta.product.dto.ProductL3RetryListRequest;
import com.gdn.mta.product.service.ProductLevel3Wrapper;
import com.gdn.partners.pbp.model.productlevel3.CreateProductLevel3Response;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gda.mta.product.dto.ProductBusinessPartnerAttributeRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerAttributeResponse;
import com.gda.mta.product.dto.ProductBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerResponse;
import com.gda.mta.product.dto.ProductCopyRequest;
import com.gda.mta.product.dto.ProductItemBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductItemBusinessPartnerResponse;
import com.gda.mta.product.dto.RejectedSkuProductResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.RejectedSkuProductCollection;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.util.WholesaleValidationUtil;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.mta.product.util.ProductCreationValidation;
import com.gdn.mta.product.util.SingleValueResponse;
import com.gdn.mta.product.web.model.ProductBusinessPartnerControllerErrorMessage;
import com.gdn.mta.product.web.model.ProductBusinessPartnerControllerPath;
import com.gdn.partner.pbp.annotations.AuditLog;
import com.gdn.partner.pbp.logger.standar.LoggerAspect;
import com.gdn.partner.pbp.logger.standar.LoggerAttributeModel;
import com.gdn.partner.pbp.logger.standar.LoggerParam;
import com.gdn.partner.pbp.logger.standar.LoggerStandard;
import com.gdn.partners.pbp.commons.constants.Constants;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

@Slf4j
@RestController
@RequestMapping(value = ProductBusinessPartnerControllerPath.BASE_PATH)
@Tag(name = "ProductBusinessPartnerController", description = "Product Business Partner Service API")
public class ProductBusinessPartnerController {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductBusinessPartnerController.class);

  @Value("${max.wholesale.price.requests}")
  private int maxWholesalePriceRequests;

  @Lazy
  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Autowired
  private WholesaleValidationUtil wholesaleValidationUtil;

  @Autowired
  private ProductLevel3Wrapper productLevel3Wrapper;

  private void convertProductBusinessPartnerRequestToProductBusinessPartner(ProductBusinessPartnerRequest request,
      ProductBusinessPartner productBusinessPartner) throws Exception {
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : request.getProductItemBusinessPartners()) {
      if (StringUtils.isEmpty(productItemBusinessPartnerRequest.getId())
          || productItemBusinessPartnerRequest.getId() == null) {
        ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
        BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
        productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
        productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
      } else {
        for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner
            .getProductItemBusinessPartners()) {
          if (productItemBusinessPartnerRequest.getId().equals(productItemBusinessPartner.getId())) {
            BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
            break;
          }
        }
      }
    }
    for (ProductBusinessPartnerAttributeRequest productBusinessPartnerAttributeRequest : request
        .getProductBusinessPartnerAttributes()) {
      if (StringUtils.isEmpty(productBusinessPartnerAttributeRequest.getId())
          || productBusinessPartnerAttributeRequest.getId() == null) {
        ProductBusinessPartnerAttribute productBusinessPartnerAttribute = new ProductBusinessPartnerAttribute();
        BeanUtils.copyProperties(productBusinessPartnerAttributeRequest, productBusinessPartnerAttribute);
        productBusinessPartnerAttribute.setProductBusinessPartner(productBusinessPartner);
        productBusinessPartner.getProductBusinessPartnerAttributes().add(productBusinessPartnerAttribute);
      } else {
        for (ProductBusinessPartnerAttribute productBusinessPartnerAttribute : productBusinessPartner
            .getProductBusinessPartnerAttributes()) {
          if (productBusinessPartnerAttributeRequest.getId().equals(productBusinessPartnerAttribute.getId())) {
            BeanUtils.copyProperties(productBusinessPartnerAttributeRequest, productBusinessPartnerAttribute);
            break;
          }
        }
      }
    }
  }

  private void convertProductBusinessPartnerToProductBusinessPartnerResponse(
      ProductBusinessPartner productBusinessPartner, ProductBusinessPartnerResponse response) throws Exception {
    for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartnerResponse productItemBusinessPartnerResponse = new ProductItemBusinessPartnerResponse();
      BeanUtils.copyProperties(productItemBusinessPartner, productItemBusinessPartnerResponse);
      response.getProductItemBusinessPartners().add(productItemBusinessPartnerResponse);
    }
    for (ProductBusinessPartnerAttribute productBusinessPartnerAttribute : productBusinessPartner
        .getProductBusinessPartnerAttributes()) {
      ProductBusinessPartnerAttributeResponse productBusinessPartnerAttributeResponse =
          new ProductBusinessPartnerAttributeResponse();
      BeanUtils.copyProperties(productBusinessPartnerAttribute, productBusinessPartnerAttributeResponse);
      response.getProductBusinessPartnerAttributes().add(productBusinessPartnerAttributeResponse);
    }
  }

  @RequestMapping(value = ProductBusinessPartnerControllerPath.VALIDATE_PRODUCT_SKU,
      method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  private GdnRestSingleResponse<SingleValueResponse> validateProductSku(
      @RequestParam String productSku, @RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username) {
    try {
      LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "validateProductSku", null, 
          username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_FETCH, 
          null, null);
      
      MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
      LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
      SingleValueResponse singleValueResponse =
          new SingleValueResponse(productBusinessPartnerService.validateProductSku(productSku));
      
      return new GdnRestSingleResponse(null, null, true, singleValueResponse, requestId);
    } catch (Exception e) {
      LOGGER.error(LoggerStandard.appendLogErrorTemplate(e.getMessage(), e));
      return new GdnRestSingleResponse("Validate product SKU error: " + e.getMessage(), null, false,
          null, requestId);
    }
  }

  private ProductBusinessPartner generateProductBusinessPartner(
      ProductBusinessPartnerRequest request) throws Exception {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(request, productBusinessPartner, "productItemBusinessPartners",
        "productBusinessPartnerAttributes");
    productBusinessPartner
        .setProductItemBusinessPartners(new ArrayList<ProductItemBusinessPartner>());
    productBusinessPartner
        .setProductBusinessPartnerAttributes(new ArrayList<ProductBusinessPartnerAttribute>());
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : request
        .getProductItemBusinessPartners()) {
      ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
      BeanUtils.copyProperties(productItemBusinessPartnerRequest, productItemBusinessPartner);
      productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
      productBusinessPartner.getProductItemBusinessPartners().add(productItemBusinessPartner);
    }
    for (ProductBusinessPartnerAttributeRequest productBusinessPartnerAttributeRequest : request
        .getProductBusinessPartnerAttributes()) {
      ProductBusinessPartnerAttribute productBusinessPartnerAttribute = new ProductBusinessPartnerAttribute();
      BeanUtils.copyProperties(productBusinessPartnerAttributeRequest, productBusinessPartnerAttribute);
      if (!productBusinessPartnerAttributeRequest.isMandatory() && StringUtils
          .isBlank(productBusinessPartnerAttributeRequest.getValue())) {
        productBusinessPartnerAttribute.setValue(Constants.DELIMITER_DASH);
      }
      productBusinessPartnerAttribute.setProductBusinessPartner(productBusinessPartner);
      productBusinessPartner.getProductBusinessPartnerAttributes().add(productBusinessPartnerAttribute);
    }
    return productBusinessPartner;
  }
  
  @AuditLog
  @RequestMapping(value = ProductBusinessPartnerControllerPath.DELETE, method = RequestMethod.POST
      , produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "delete product business partner", description = "delete product business partner")
  @ResponseBody
  public GdnBaseRestResponse deleteProductBusinessPartner(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody ProductBusinessPartnerRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "deleteProductBusinessPartner", request.getBusinessPartnerId(), 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_DELETE, 
        null, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(request.getUpdatedBy()) || request.getUpdatedDate() == null),
        ProductBusinessPartnerControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE);
    validateCreateOrUpdateProductBusinessPartner(request, storeId);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartnerService().findById(request.getId());
    BeanUtils.copyProperties(request, productBusinessPartner, "id", "storeId", "createdDate",
        "createdBy", "markForDelete", "productItemBusinessPartners",
        "productBusinessPartnerAttributes");
    convertProductBusinessPartnerRequestToProductBusinessPartner(request, productBusinessPartner);
    productBusinessPartner.setMarkForDelete(true);
    for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner
        .getProductItemBusinessPartners()) {
      productItemBusinessPartner.setMarkForDelete(true);
    }
    for (ProductBusinessPartnerAttribute productBusinessPartnerAttribute : productBusinessPartner
        .getProductBusinessPartnerAttributes()) {
      productBusinessPartnerAttribute.setMarkForDelete(true);
    }
    getProductBusinessPartnerService().update(productBusinessPartner);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductBusinessPartnerControllerPath.ACTIVATED,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product business partner summary by activated true",
  description = "get product business partner summary by storeId and activated true")
  @ResponseBody
  public GdnRestListResponse<ProductBusinessPartnerResponse> filterProductBusinessPartnerSummaryByActivatedTrue(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductBusinessPartnerSummaryByActivatedTrue", null, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_FETCH, 
        page + ":" + size, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    Pageable pageable = PageRequest.of(page, size);
    Page<ProductBusinessPartner> productBusinessPartners =
        getProductBusinessPartnerService().findByActivatedTrue(storeId, pageable);
    List<ProductBusinessPartnerResponse> wrapper = new ArrayList<ProductBusinessPartnerResponse>();
    for (ProductBusinessPartner productBusinessPartner : productBusinessPartners.getContent()) {
      ProductBusinessPartnerResponse wrapperElement = new ProductBusinessPartnerResponse();
      BeanUtils.copyProperties(productBusinessPartner, wrapperElement, "productItemBusinessPartners",

          "productBusinessPartnerAttributes");
      convertProductBusinessPartnerToProductBusinessPartnerResponse(productBusinessPartner, wrapperElement);
      wrapper.add(wrapperElement);
    }
    return new GdnRestListResponse<ProductBusinessPartnerResponse>(null, null, true, wrapper, new PageMetaData(
        pageable.getPageSize(), pageable.getPageNumber(), productBusinessPartners.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductBusinessPartnerControllerPath.FILTER_BUSINESS_PARTNER,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product business partner summary by businessPartnerId",
      description = "get product business partner summary by storeId and businessPartnerId and pageable")
  @ResponseBody
  public GdnRestListResponse<ProductBusinessPartnerResponse> filterProductBusinessPartnerSummaryByBusinessPartnerId(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @PathVariable("id") String businessPartnerId) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductBusinessPartnerSummaryByBusinessPartnerId", null, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_FETCH, 
        page + ":" + size + ":" + businessPartnerId, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    Pageable pageable = PageRequest.of(page, size);
    Page<ProductBusinessPartner> productBusinessPartners =
        getProductBusinessPartnerService().findByBusinessPartnerId(storeId, businessPartnerId, pageable);
    List<ProductBusinessPartnerResponse> wrapper = new ArrayList<ProductBusinessPartnerResponse>();
    for (ProductBusinessPartner productBusinessPartner : productBusinessPartners.getContent()) {
      ProductBusinessPartnerResponse wrapperElement = new ProductBusinessPartnerResponse();
      BeanUtils.copyProperties(productBusinessPartner, wrapperElement, "productItemBusinessPartners",

          "productBusinessPartnerAttributes");
      convertProductBusinessPartnerToProductBusinessPartnerResponse(productBusinessPartner, wrapperElement);
      wrapper.add(wrapperElement);
    }
    return new GdnRestListResponse<ProductBusinessPartnerResponse>(null, null, true, wrapper, new PageMetaData(
        pageable.getPageSize(), pageable.getPageNumber(), productBusinessPartners.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductBusinessPartnerControllerPath.FILTER_REJECTED_SKU, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product business partner summary by businessPartnerId",
      description = "get product business partner summary by storeId and businessPartnerId and pageable")
  @ResponseBody
  public GdnRestListResponse<RejectedSkuProductResponse> filterProductBusinessPartnerSummaryByBusinessPartnerId1(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String businessPartnerId,
      @RequestParam(required = false) String searchCriteria, @RequestParam(required = false,
      defaultValue = "updatedDate") String orderBy, @RequestParam(required = false,
      defaultValue = "desc") String sortBy) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductBusinessPartnerSummaryByBusinessPartnerId1", null, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_FETCH, 
        page + ":" + size + ":" + businessPartnerId, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    Pageable pageable = PageRequest.of(page, size);
    Page<RejectedSkuProductCollection> rejectedSkuProductCollection =
        getProductBusinessPartnerService()
            .findRejectedProductsByBusinessPartnerIdWithOrderByAndSortBy(storeId, businessPartnerId, pageable,
                searchCriteria, orderBy, sortBy);
    List<RejectedSkuProductResponse> response = new ArrayList<RejectedSkuProductResponse>();
    for (RejectedSkuProductCollection rejectedSku : rejectedSkuProductCollection.getContent()) {
      RejectedSkuProductResponse wrapper = new RejectedSkuProductResponse();
      BeanUtils.copyProperties(rejectedSku, wrapper);
      response.add(wrapper);
    }
    return new GdnRestListResponse<RejectedSkuProductResponse>(null, null, true, response,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            rejectedSkuProductCollection.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductBusinessPartnerControllerPath.FILTER_REJECTED_SKU_BY_MERCHANT_SKU
      , method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product business partner summary by businessPartnerId and merchantSku")
  @ResponseBody
  public GdnRestListResponse<RejectedSkuProductResponse> filterRejectedSkuByBusinessPartnerIdAndMerchantSku(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String businessPartnerId,
      @RequestParam String merchantSku) throws Exception {
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductBusinessPartnerSummaryByBusinessPartnerId1", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_FETCH,
        page + ":" + size + ":" + businessPartnerId, null);
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    try {
      Pageable pageable = PageRequest.of(page, size);
      Page<RejectedSkuProductCollection> response = getProductBusinessPartnerService()
          .findRejectedProductsByBusinessPartnerIdAndMerchantSku(storeId, businessPartnerId, pageable, merchantSku);
      List<RejectedSkuProductResponse> rejectedSkuProductResponses = ConverterUtil.convertRejectedSkuProductCollectionToRejectedSkuProductResponse(response.getContent());
      return new GdnRestListResponse<>(null, null, true, rejectedSkuProductResponses,
          new PageMetaData(size, page, rejectedSkuProductResponses.size()), requestId);
    } catch (Exception e) {
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getMessage(), false, null,
          new PageMetaData(), requestId);
    }
  }

  @RequestMapping(value = ProductBusinessPartnerControllerPath.FILTER_PICKUP_POINT,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product business partner summary by pickupPointCode",
      description = "get product business partner summary by storeId and businessPartnerId and pageable")
  @ResponseBody
  public GdnRestListResponse<ProductBusinessPartnerResponse> filterProductItemBusinessPartnerSummaryByPickupPointId(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @PathVariable("pickupPointId") String pickupPointId) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductItemBusinessPartnerSummaryByPickupPointId", null, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_FETCH, 
        page + ":" + size + ":" + pickupPointId, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    Pageable pageable = PageRequest.of(page, size);
    Page<ProductBusinessPartner> productBusinessPartners =
        getProductBusinessPartnerService().findByPickupPointId(storeId, pickupPointId, pageable);
    List<ProductBusinessPartnerResponse> wrapper = new ArrayList<ProductBusinessPartnerResponse>();
    for (ProductBusinessPartner productBusinessPartner : productBusinessPartners.getContent()) {
      ProductBusinessPartnerResponse wrapperElement = new ProductBusinessPartnerResponse();
      BeanUtils.copyProperties(productBusinessPartner, wrapperElement, "productItemBusinessPartners",

          "productBusinessPartnerAttributes");
      convertProductBusinessPartnerToProductBusinessPartnerResponse(productBusinessPartner, wrapperElement);
      wrapper.add(wrapperElement);
    }
    return new GdnRestListResponse<ProductBusinessPartnerResponse>(null, null, true, wrapper, new PageMetaData(
        pageable.getPageSize(), pageable.getPageNumber(), productBusinessPartners.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductBusinessPartnerControllerPath.DETAIL, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product business partner by id",
      description = "get product business partner by id")
  @ResponseBody
  public GdnRestSingleResponse<ProductBusinessPartnerResponse> getProductBusinessPartner(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("id") String id) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "getProductBusinessPartner", null, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_FETCH, 
        id, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    ProductBusinessPartnerResponse wrapper = null;
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartnerService().findById(id);
    if (productBusinessPartner != null) {
      wrapper = new ProductBusinessPartnerResponse();
      BeanUtils.copyProperties(productBusinessPartner, wrapper, "productItemBusinessPartners",
          "productBusinessPartnerAttributes");
      convertProductBusinessPartnerToProductBusinessPartnerResponse(productBusinessPartner, wrapper);
    }
    return new GdnRestSingleResponse<ProductBusinessPartnerResponse>(null, null, true, wrapper, requestId);
  }

  public ProductBusinessPartnerService getProductBusinessPartnerService() {
    return productBusinessPartnerService;
  }

  @RequestMapping(value = ProductBusinessPartnerControllerPath.ROOT, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product business partner summary",
      description = "get product business partner summary by storeId and pageable")
  @ResponseBody
  public GdnRestListResponse<ProductBusinessPartnerResponse> getProductBusinessPartnerSummary(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "getProductBusinessPartnerSummary", null, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_FETCH, 
        page + ":" + size, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    Pageable pageable = PageRequest.of(page, size);
    Page<ProductBusinessPartner> productBusinessPartners =
        getProductBusinessPartnerService().findByStoreId(storeId, pageable);
    List<ProductBusinessPartnerResponse> wrapper = new ArrayList<ProductBusinessPartnerResponse>();
    for (ProductBusinessPartner productBusinessPartner : productBusinessPartners.getContent()) {
      ProductBusinessPartnerResponse wrapperElement = new ProductBusinessPartnerResponse();
      BeanUtils.copyProperties(productBusinessPartner, wrapperElement, "productItemBusinessPartners",

          "productBusinessPartnerAttributes");
      convertProductBusinessPartnerToProductBusinessPartnerResponse(productBusinessPartner, wrapperElement);
      wrapper.add(wrapperElement);
    }
    return new GdnRestListResponse<ProductBusinessPartnerResponse>(null, null, true, wrapper, new PageMetaData(
        pageable.getPageSize(), pageable.getPageNumber(), productBusinessPartners.getTotalElements()), requestId);
  }
  
  @AuditLog
  @RequestMapping(value = ProductBusinessPartnerControllerPath.RETRY_CREATE, method = RequestMethod.POST
      , produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "retry save product business partner",
      description = "retry save product business partner")
  @ResponseBody
  public GdnBaseRestResponse retrySaveProductBusinessPartner(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductBusinessPartnerRequest request) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "retrySaveProductBusinessPartner", request.getBusinessPartnerId(), 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_RETRY_CREATE, 
        request.getProductId(), request.toString());
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(request.getUpdatedBy()) || request.getUpdatedDate() == null),
        ProductBusinessPartnerControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getId()),
        ProductBusinessPartnerControllerErrorMessage.PRODUCT_BUSINESS_PARTNER_ID_MUST_NOT_BE_BLANK);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(request, productBusinessPartner, "productItemBusinessPartners",
        "productBusinessPartnerAttributes");
    productBusinessPartner.setStoreId(storeId);
    getProductBusinessPartnerService().retrySave(productBusinessPartner);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductBusinessPartnerControllerPath.CREATE, method = RequestMethod.POST,
      produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {
          MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "save product business partner", description = "save product business partner")
  @ResponseBody
  public GdnBaseRestResponse saveProductBusinessPartner(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody ProductBusinessPartnerRequest request) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "saveProductBusinessPartner", request.getBusinessPartnerId(), 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_CREATE, 
        request.getProductId(), request.toString());
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(request.getCreatedBy()) || request.getCreatedDate() == null),
        ProductBusinessPartnerControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE);
    validateCreateOrUpdateProductBusinessPartner(request, storeId);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(request, productBusinessPartner, "productItemBusinessPartners",
        "productBusinessPartnerAttributes");
    convertProductBusinessPartnerRequestToProductBusinessPartner(request, productBusinessPartner);
    productBusinessPartner.setActivated(true);
    getProductBusinessPartnerService().save(productBusinessPartner);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }
  
  private String saveWithActivatedFalse(ProductBusinessPartnerRequest request) throws Exception{
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(request.getCreatedBy()) || request.getCreatedDate() == null),
        ProductBusinessPartnerControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE);
    validateCreateOrUpdateProductBusinessPartner(request, request.getStoreId());
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    BeanUtils.copyProperties(request, productBusinessPartner, "productItemBusinessPartners",
        "productBusinessPartnerAttributes");
    convertProductBusinessPartnerRequestToProductBusinessPartner(request, productBusinessPartner);
    productBusinessPartner.setActivated(false);
    String id = getProductBusinessPartnerService().saveWithActivatedFalse(productBusinessPartner);
    return id;
  }

  @AuditLog
  @RequestMapping(value = ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "save product business partner with activated false",
      description = "save product business partner with activated false")
  @ResponseBody
  public GdnBaseRestResponse saveProductBusinessPartnerWithActivatedFalse(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductBusinessPartnerRequest request) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "saveProductBusinessPartnerWithActivatedFalse", request.getBusinessPartnerId(), 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_ACTIVATE_FALSE, 
        request.getProductId(), request.toString());
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    saveWithActivatedFalse(request);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }
  
  @AuditLog
  @RequestMapping(value = ProductBusinessPartnerControllerPath.CREATE_ACTIVATED_FALSE_RETURN_ID, method = RequestMethod.POST
      , produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "save product business partner with activated false",
      description = "save product business partner with activated false")
  @ResponseBody
  public GdnRestSimpleResponse<String> saveProductBusinessPartnerWithActivatedFalseReturnId(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductBusinessPartnerRequest request) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "saveProductBusinessPartnerWithActivatedFalse", request.getBusinessPartnerId(), 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_ACTIVATE_FALSE, 
        request.getProductId(), request.toString());
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.info(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    String id = saveWithActivatedFalse(request);
    return new GdnRestSimpleResponse<String>(null, null, true, requestId, id);
  }

  public void setProductBusinessPartnerService(ProductBusinessPartnerService productBusinessPartnerService) {
    this.productBusinessPartnerService = productBusinessPartnerService;
  }

  @AuditLog
  @RequestMapping(value = ProductBusinessPartnerControllerPath.UPDATE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update product business partner",
      description = "update product business partner")
  @ResponseBody
  public GdnBaseRestResponse updateProductBusinessPartner(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody ProductBusinessPartnerRequest request) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "updateProductBusinessPartner", request.getBusinessPartnerId(), 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_UPDATE, 
        request.getProductId(), request.toString());
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(request.getUpdatedBy()) || request.getUpdatedDate() == null),
        ProductBusinessPartnerControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE);
    validateCreateOrUpdateProductBusinessPartner(request, storeId);
    ProductBusinessPartner productBusinessPartner = getProductBusinessPartnerService().findById(request.getId());
    BeanUtils.copyProperties(request, productBusinessPartner, "id", "storeId", "createdDate",
        "createdBy", "markForDelete", "productItemBusinessPartners",
        "productBusinessPartnerAttributes");
    convertProductBusinessPartnerRequestToProductBusinessPartner(request, productBusinessPartner);
    getProductBusinessPartnerService().update(productBusinessPartner);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductBusinessPartnerControllerPath.CREATE_PBP, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "create product business partner at controller",
      description = "create product business partner at controller")
  @ResponseBody
  public GdnBaseRestResponse create(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody ProductBusinessPartnerRequest request) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "create", request.getBusinessPartnerId(), 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_CREATE, 
        request.getProductId(), request.toString());

    ApiErrorCode apiErrorCode = null;
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    Integer minimumPrice = validateCreateOrUpdateProductBusinessPartner(request, storeId);
    setMdcParameters(storeId, channelId, clientId, requestId, username);
    ProductCreationValidation
        .validateWholeSalePriceSettingOnFlow2(request.getProductItemBusinessPartners(), minimumPrice,
            maxWholesalePriceRequests);
    if (request.getProductItemBusinessPartners().stream().anyMatch(productItemBusinessPartnerRequest -> CollectionUtils
        .isNotEmpty(productItemBusinessPartnerRequest.getProductItemWholesalePriceRequests()))) {
      apiErrorCode = wholesaleValidationUtil
          .validateWholesaleConfigOnFlow2(request.getCategoryCode(), request.getProductItemBusinessPartners());
    }
    ProductBusinessPartner productBusinessPartner = generateProductBusinessPartner(request);
    CreateProductLevel3Response createProductLevel3Response = this.productBusinessPartnerService.create(storeId,
        productBusinessPartner, false, request.getProductItemBusinessPartners());
    if (!createProductLevel3Response.isLogisticsSaveSuccess()) {
      return new GdnBaseRestResponse(ApiErrorCode.LOGISTICS_DATA_NOT_SAVED.getDesc(),
          ApiErrorCode.LOGISTICS_DATA_NOT_SAVED.getCode(), true, requestId);
    }
    return new GdnBaseRestResponse(Objects.nonNull(apiErrorCode) ? apiErrorCode.getDesc() : null,
        Objects.nonNull(apiErrorCode) ? apiErrorCode.getCode() : null, true, requestId);
  }
  
  @AuditLog
  @RequestMapping(value = ProductBusinessPartnerControllerPath.RETRY_CREATE_PBP,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "retry create product business partner at controller",
      description = "retry create product business partner at controller")
  @ResponseBody
  public GdnBaseRestResponse retryCreate(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String productBusinessPartnerId) throws Exception {
    
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "retryCreate", productBusinessPartnerId, 
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_RETRY_CREATE, 
        null, null);
    
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(productBusinessPartnerId),
        ProductBusinessPartnerControllerErrorMessage.PRODUCT_BUSINESS_PARTNER_ID_MUST_NOT_BE_BLANK);
    this.productBusinessPartnerService.retryCreate(storeId, productBusinessPartnerId, null);
    return new GdnBaseRestResponse(requestId);
  }

  @AuditLog
  @ResponseBody
  @RequestMapping(value = ProductBusinessPartnerControllerPath.COPY_PRODUCTS, method = RequestMethod.POST,
    produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "copy product item", description = "copy product item from associated business partner")
  public GdnBaseRestResponse copyPartnerProducts(@RequestParam String storeId, @RequestParam String channelId,
    @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
    @RequestParam(defaultValue = "false") boolean isRetryAttempt, @RequestBody ProductCopyRequest request) {
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "copyPartnerProducts",
      request.getBusinessPartnerCode(), username, requestId, storeId, channelId, clientId,
      LoggerAspect.PRODUCT_BP_COPY, "", request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));

    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    this.validateProductItemCopyRequest(request);
    try {
      productBusinessPartnerService
        .copy(storeId, username, request.getBusinessPartnerCode(), request.getPickupPointCode(), isRetryAttempt,
          UUID.randomUUID().toString(), request.getGdnItemSkus(), request.getSourceBusinessPartnerCode());
    } catch(ApplicationRuntimeException e) {
      if (e.getErrorMessage().contains(ProductBusinessPartnerService.ERR_CODE_NO_AVAILABLE_VALID_ITEMS)) {
        return new GdnBaseRestResponse(null, ProductBusinessPartnerService.ERR_CODE_NO_AVAILABLE_VALID_ITEMS, true, requestId);
      }
      throw e;
    }
    return new GdnBaseRestResponse(requestId);
  }

  @AuditLog
  @ResponseBody
  @RequestMapping(value = ProductBusinessPartnerControllerPath.COPY_ALL_PRODUCTS, method = RequestMethod.POST,
    produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "copy all product items", description = "copy all product items from associated business partner")
  public GdnBaseRestResponse copyPartnerAllProducts(@RequestParam String storeId, @RequestParam String channelId,
    @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
    @RequestBody ProductCopyRequest request) throws Exception {
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "copyPartnerAllProducts",
      request.getBusinessPartnerCode(), username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_COPY,
      request.getSourceBusinessPartnerCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));

    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    this.validateProductItemCopyRequest(request);
    try {
      productBusinessPartnerService
        .copyAllProducts(storeId, username, request.getSourceBusinessPartnerCode(), request.getBusinessPartnerCode(),
          request.getPickupPointCode());
    } catch(ApplicationRuntimeException e) {
      if (e.getErrorMessage().contains(ProductBusinessPartnerService.ERR_CODE_NO_AVAILABLE_VALID_ITEMS)) {
        return new GdnBaseRestResponse(null, ProductBusinessPartnerService.ERR_CODE_NO_AVAILABLE_VALID_ITEMS, true, requestId);
      }
      throw e;
    }
    return new GdnBaseRestResponse(requestId);
  }

  @AuditLog
  @ResponseBody
  @RequestMapping(value = ProductBusinessPartnerControllerPath.COPY_PRODUCTS_NOTIFY, method = RequestMethod.POST,
    produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "notification for copy partner products", description = "send notification for all the finished copy processes")
  public GdnBaseRestResponse NotificationForCopyPartnerProducts(@RequestParam String storeId, @RequestParam String channelId,
    @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username) {
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "NotificationForCopyPartnerProducts",
      null, username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_COPY_NOTIFICATION,
      null, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));

    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    productBusinessPartnerService.notifyForProductCopyingProcess(storeId);

    return new GdnBaseRestResponse(requestId);
  }

  @AuditLog
  @ResponseBody
  @RequestMapping(value = ProductBusinessPartnerControllerPath.UPDATE_SYNC_RETRY, method = RequestMethod.POST,
    produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update sync status from IN_PROGRESS to FAIL", description = "update sync status for inProgress recored before sync retry duration")
  public GdnBaseRestResponse resetProductItemSyncStatusForRetry(@RequestParam String storeId, @RequestParam String channelId,
    @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username){
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "resetProductItemSyncStatusForRetry",
      null, username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_BP_COPY_UPDATE_RETRY,
      null, null);
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));

    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    productBusinessPartnerService.resetProductItemSyncStatus(storeId);
    return new GdnBaseRestResponse(requestId);
  }

  @AuditLog
  @ResponseBody
  @RequestMapping(value = ProductBusinessPartnerControllerPath.IS_PRODUCT_MAPPED_TO_MERCHANT, method = RequestMethod
      .GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Check if any product is mapped to merchant", description = "Check if any product is mapped to "
      + "merchant")
  public GdnRestSimpleResponse<Boolean> isProductsMappedToBusinessPartnerCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("merchantCode") String merchantCode) {
    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "isProductMappedToBusinessPartnerCode", merchantCode, username, requestId,
            storeId, channelId, clientId, LoggerAspect.PRODUCT_MAPPED_TO_MERCHANT_CODE, null, null);
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));

    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    boolean isProductsMapped = productBusinessPartnerService.isProductMappedToMerchant(storeId, merchantCode);
    return new GdnRestSimpleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId, isProductsMapped);
  }

  private void validateProductItemCopyRequest(ProductCopyRequest request) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getBusinessPartnerCode()),
      ProductBusinessPartnerControllerErrorMessage.BUSINESS_PARTNER_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getSourceBusinessPartnerCode()),
      ProductBusinessPartnerControllerErrorMessage.MISSING_SOURCE_BUSINESS_PARTNER_ID);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(request.getPickupPointCode()),
      ProductBusinessPartnerControllerErrorMessage.PICKUP_POINT_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(Objects.nonNull(request.getGdnItemSkus()),
      ProductBusinessPartnerControllerErrorMessage.MISSING_PRODUCT_ITEMS);

    Optional<String> invalidSku = request.getGdnItemSkus().entrySet().stream()
      .flatMap(stringListEntry -> stringListEntry.getValue().stream())
      .filter(itemSku -> !itemSku.trim().startsWith(request.getSourceBusinessPartnerCode()))
      .findAny();

    if (invalidSku.isPresent()) {
      LOGGER.warn("ItemSku {} doesn't belong to linked partner {}", invalidSku, request.getSourceBusinessPartnerCode());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, "invalid item sku found");
    }
  }

  private Integer validateCreateOrUpdateProductBusinessPartner(ProductBusinessPartnerRequest request,
      String storeId) throws Exception {
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(request.getBusinessPartnerId())),
        ProductBusinessPartnerControllerErrorMessage.BUSINESS_PARTNER_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(request.getProductId())),
        ProductBusinessPartnerControllerErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getProductName()),
        ProductBusinessPartnerControllerErrorMessage.PRODUCT_NAME_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getCategoryName()),
        ProductBusinessPartnerControllerErrorMessage.CATEGORY_NAME_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getBrand()),
        ProductBusinessPartnerControllerErrorMessage.BRAND_MUST_NOT_BE_BLANK);
    GdnPreconditions
        .checkArgument(
            !(request.getProductItemBusinessPartners().isEmpty()),
            ProductBusinessPartnerControllerErrorMessage.PRODUCT_ITEM_BUSINESS_PARTNERS_MUST_NOT_BE_BLANK);
    Integer minimumPrice = productBusinessPartnerService.getMinimumPrice(storeId);
    for (ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest : request
        .getProductItemBusinessPartners()) {
      GdnPreconditions.checkArgument(
          !(StringUtils.isEmpty(productItemBusinessPartnerRequest.getProductItemId())),
          ProductBusinessPartnerControllerErrorMessage.PRODUCT_ITEM_ID_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(Objects.nonNull(productItemBusinessPartnerRequest.getProductType()),
          ProductBusinessPartnerControllerErrorMessage.PRODUCT_TYPE_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(Objects.nonNull(productItemBusinessPartnerRequest.getPrice()),
          ProductBusinessPartnerControllerErrorMessage.PRICE_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(Objects.nonNull(productItemBusinessPartnerRequest.getSalePrice()),
          ProductBusinessPartnerControllerErrorMessage.SALE_PRICE_MUST_NOT_BE_BLANK);
      GdnPreconditions
          .checkArgument(
              productItemBusinessPartnerRequest.getPrice() >= minimumPrice
                  && productItemBusinessPartnerRequest.getSalePrice() >= minimumPrice,
              ProductBusinessPartnerControllerErrorMessage.MINIMUM_PRICE_VALUE_INVALID + minimumPrice);
      GdnPreconditions.checkArgument(Objects.nonNull(productItemBusinessPartnerRequest.getStock()),
          ProductBusinessPartnerControllerErrorMessage.STOCK_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(Objects.nonNull(productItemBusinessPartnerRequest.getMinimumStock()),
          ProductBusinessPartnerControllerErrorMessage.MINIMUM_STOCK_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(
          !(StringUtils.isEmpty(productItemBusinessPartnerRequest.getPickupPointId())),
          ProductBusinessPartnerControllerErrorMessage.PICKUP_POINT_ID_MUST_NOT_BE_BLANK);
    }
    if (CollectionUtils.isNotEmpty(request.getProductBusinessPartnerAttributes())) {
      for (ProductBusinessPartnerAttributeRequest partnerAttributeRequest : request
          .getProductBusinessPartnerAttributes()) {
        if (partnerAttributeRequest.isMandatory()) {
          GdnPreconditions.checkArgument(!StringUtils.isBlank(partnerAttributeRequest.getValue()),
              ProductBusinessPartnerControllerErrorMessage.MISSING_MANDATORY_VALUE);
        }
      }
    }
    return minimumPrice;
  }

  private void setMdcParameters(String storeId, String channelId, String clientId,
      String requestId, String username) {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, channelId);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
  }

  @RequestMapping(value = ProductBusinessPartnerControllerPath.RETRY_L3_CREATE_JOB,
    method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  private GdnBaseRestResponse retryL3CreateJob(@RequestParam String storeId, @RequestParam String channelId,
    @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username) {
    log.info("Started scheduler for retry of L3 creation");
    productLevel3Wrapper.retryL3CreationJob(storeId, requestId, username, clientId, channelId);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductBusinessPartnerControllerPath.OVERRIDE_L3_RETRY_ENTRY,
    method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  private GdnBaseRestResponse overrideL3RetryEntry(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username,
    @RequestBody ProductL3RetryListRequest productL3RetryListRequest) {
    log.info("Started scheduler for retry of L3 creation");
    productLevel3Wrapper.overrideL3Retry(storeId, requestId, username, productL3RetryListRequest);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }
}
