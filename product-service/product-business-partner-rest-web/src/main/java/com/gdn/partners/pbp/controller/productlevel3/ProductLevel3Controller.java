package com.gdn.partners.pbp.controller.productlevel3;

import java.util.ArrayList;
import java.util.List;

import com.gda.mta.product.dto.response.AvailableToCopyProductDetailsResponse;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gda.mta.product.dto.ProductLevel3ImageResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryMinifiedRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryMinifiedResponse;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.converter.ProductLevel3RequestConverter;
import com.gdn.mta.product.converter.ProductLevel3ResponseConverter;
import com.gdn.mta.product.entity.ProductLevel3Image;
import com.gdn.mta.product.entity.ProductLevel3ImageBundle;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductLevel3SummaryMinified;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.mta.product.valueobject.ProductLevel3SummaryFilter;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partner.pbp.annotations.AuditLog;
import com.gdn.partner.pbp.logger.standar.LoggerAspect;
import com.gdn.partner.pbp.logger.standar.LoggerAttributeModel;
import com.gdn.partner.pbp.logger.standar.LoggerParam;
import com.gdn.partner.pbp.logger.standar.LoggerStandard;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3CountOOSResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3GdnSkuListRequest;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3ImageBundleResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3ItemSearchRequest;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3ItemSearchResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3UpdateItemSyncStockRequest;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3UpdateSyncStockBusinessPartnerRequest;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Item;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3ItemSearch;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3Service;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryCountWebItemSkuOOSResponseDTO;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

@RestController
@RequestMapping(value = ProductLevel3ControllerPath.BASE_PATH)
@Tag(name = "ProductLevel3Controller", description = "Product Level 3 Service API")
public class ProductLevel3Controller {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductLevel3Controller.class);

  @Autowired
  private ProductLevel3Service productLevel3Service;

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private ProductLevel3RequestConverter productLevel3RequestConverter;

  @Autowired
  private ProductLevel3ResponseConverter productLevel3ResponseConverter;

  @Deprecated
  @RequestMapping(value = {ProductLevel3ControllerPath.COUNT_ITEM_OOS},
      method = {RequestMethod.GET}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Get total item oos by business partner code", description = "Get total item oos")
  @ResponseBody
  public GdnRestSingleResponse<ProductLevel3CountOOSResponse> countItemOOS(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String businessPartnerCode) throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId,
            requestId, username, null);
    ProductLevel3Controller.LOGGER.info(
        "Invoke countItemOOS with mandatoryParameter= {} and BusinessPartnerCode= {}",
        mandatoryRequestParam, businessPartnerCode);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    ProductLevel3CountOOSResponse response = new ProductLevel3CountOOSResponse();
    return new GdnRestSingleResponse<>(response, requestId);
  }

  @RequestMapping(value = {ProductLevel3ControllerPath.ITEM_IMAGE_BUNDLE}, method = {RequestMethod.POST}, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Get item image bundle by list of gdn sku", description = "Get item image bundle")
  @ResponseBody
  public GdnRestListResponse<ProductLevel3ImageBundleResponse> getImageBundle(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody ProductLevel3GdnSkuListRequest request) throws Exception {
    MandatoryRequestParam mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    ProductLevel3Controller.LOGGER.info(
        "Invoke getImageBundle with mandatoryParameter= {} and request= {}", mandatoryRequestParam,
        request);
    GdnPreconditions.checkArgument(!CollectionUtils.isEmpty(request.getGdnSkus()),
        ProductLevel3ControllerErrorMessage.REQUEST_MUST_NOT_BE_EMPTY);
    ItemSummaryRequest filter = new ItemSummaryRequest();
    filter.setItemSkus(request.getGdnSkus());
    Pageable pageRequest = PageRequest.of(0, request.getGdnSkus().size());
    SortOrder sort = new SortOrder();
    Page<ProductLevel3ImageBundle> imageBundlePage =
        this.productLevel3Service.findImageBundleByFilter(filter, pageRequest, sort);
    List<ProductLevel3ImageBundleResponse> imageBundleResponses = new ArrayList<>();
    for (ProductLevel3ImageBundle imageBundle : imageBundlePage.getContent()) {
      ProductLevel3ImageBundleResponse imageBundleResponse = new ProductLevel3ImageBundleResponse();
      imageBundleResponse.setGdnSku(imageBundle.getGdnSku());
      imageBundleResponse.setImages(new ArrayList<>());
      for (ProductLevel3Image image : imageBundle.getImages()) {
        imageBundleResponse.getImages().add(new ProductLevel3ImageResponse(image.getMainImage(),
            image.getSequence(), image.getLocationPath()));
      }
      imageBundleResponses.add(imageBundleResponse);
    }
    return new GdnRestListResponse<>(imageBundleResponses,
        new PageMetaData(imageBundleResponses.size(), 0, imageBundleResponses.size()), requestId);
  }

  @RequestMapping(value = {ProductLevel3ControllerPath.UPDATE_ITEM_SYNC_STOCK_BUSINESS_PARTNER},
      method = {RequestMethod.POST}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Update synchronize stock business partner",
      description = "Update synchronize stock")
  @ResponseBody
  public GdnBaseRestResponse updateSyncStockBusinessPartner(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ProductLevel3UpdateSyncStockBusinessPartnerRequest request) throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId,
            requestId, username, null);
    ProductLevel3Controller.LOGGER.info(
        "Invoke updateSyncStockBusinessPartner with mandatoryParameter= {}, and request= {}",
        mandatoryRequestParam, request);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getBusinessPartnerCode()),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(request.getSyncStock() != null,
        ProductLevel3ControllerErrorMessage.SYNC_STOCK_MUST_NOT_BE_BLANK);
    try {
      this.productLevel3Service.updateSyncStockByBusinessPartnerCode(
          request.getBusinessPartnerCode(), request.getSyncStock());
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      ProductLevel3Controller.LOGGER.error("Error updateSyncStockByMerchantCode with request= {}",
          request);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }


  @RequestMapping(value = {ProductLevel3ControllerPath.UPDATE_ITEM_SYNC_STOCK},
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update Item synchronize stock ", description = "Update item synchronize stock")
  @ResponseBody
  public GdnBaseRestResponse updateProductSyncStock(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody ProductLevel3UpdateItemSyncStockRequest request) throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId,
            requestId, username, null);
    ProductLevel3Controller.LOGGER.info(
        "Invoke update Item SyncStock with mandatoryParameter= {}, and request= {}",
        mandatoryRequestParam, request);
    try {
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getBusinessPartnerCode()),
          ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(request.getSyncStock() != null,
          ProductLevel3ControllerErrorMessage.SYNC_STOCK_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getGdnSKU()),
          ProductLevel3ControllerErrorMessage.GDN_SKU_MUST_NOT_BE_BLANK);
      this.productLevel3Service.updateSyncStockByBusinessPartnerCodeAndGdnSku(
          request.getBusinessPartnerCode(), request.getGdnSKU(), request.getSyncStock());
      return new GdnBaseRestResponse(requestId);
    } catch (Exception e) {
      ProductLevel3Controller.LOGGER.error("Error updateSyncStockByMerchantCode with request= {}",
          request);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
  }

  @RequestMapping(value = ProductLevel3ControllerPath.FILTER_SUMMARY, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter product level 3 summary by filter at controller",
      description = "filter product level 3 summary by filter at controller")
  @ResponseBody
  public GdnRestListResponse<ProductLevel3SummaryResponse> filterSummary(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String businessPartnerCode, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam(required = false,
          defaultValue = "createdDate") String orderBy, @RequestParam(required = false,
          defaultValue = "desc") String sortBy, @RequestBody ProductLevel3SummaryRequest request)
      throws Exception {

    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "filterSummary", businessPartnerCode, username, requestId,
            storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_FETCH, page + ":" + size,
            request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(),
        LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    SortOrder sort = new SortOrder(orderBy, sortBy);
    PageRequest pageRequest = PageRequest.of(page, size);
    ProductLevel3SummaryFilter filter =
        productLevel3RequestConverter
            .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(request);
    filter.setBusinessPartnerCode(businessPartnerCode);
    Page<ProductLevel3Summary> pageOfProductLevel3Summary =
        productLevel3Service.findSummaryByFilter(filter, pageRequest, sort);
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    for (ProductLevel3Summary productLevel3Summary : pageOfProductLevel3Summary.getContent()) {
      ProductLevel3SummaryResponse response =
          productLevel3ResponseConverter
              .convertProductLevel3SummaryToProductLevel3SummaryResponse(productLevel3Summary);
      productLevel3SummaryResponses.add(response);
    }
    return new GdnRestListResponse<>(productLevel3SummaryResponses, new PageMetaData(
        pageRequest.getPageSize(), pageRequest.getPageNumber(),
        pageOfProductLevel3Summary.getTotalElements()), requestId);
  }

  @Deprecated
  @RequestMapping(value = ProductLevel3ControllerPath.FILTER_SUMMARY_MINIFIED, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "filter product level 3 summary minified at controller",
      description = "filter product level 3 summary minified at controller")
  @ResponseBody
  public GdnRestListResponse<ProductLevel3SummaryMinifiedResponse> filterSummaryMinified(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String businessPartnerCode, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam(required = false,
          defaultValue = "createdDate") String orderBy, @RequestParam(required = false,
          defaultValue = "desc") String sortBy,
      @RequestBody ProductLevel3SummaryMinifiedRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "filterSummaryMinified", businessPartnerCode, username,
            requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_FETCH, page + ":"
                + size, request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(),
        LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    SortOrder sort = new SortOrder(orderBy, sortBy);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);

    ProductLevel3SummaryFilter filterVO =
        productLevel3RequestConverter
            .convertProductLevel3SummaryMinifiedRequestToProductLevel3SummaryFilter(request);
    filterVO.setBusinessPartnerCode(businessPartnerCode);
    Page<ProductLevel3SummaryMinified> productLevel3Minified =
        productLevel3Service.findSummaryMinifiedByFilter(filterVO, pageable, sort);
    List<ProductLevel3SummaryMinifiedResponse> productResponses = new ArrayList<>();
    for (ProductLevel3SummaryMinified product : productLevel3Minified.getContent()) {
      ProductLevel3SummaryMinifiedResponse productResponse =
          productLevel3ResponseConverter
              .convertProductLevel3SummaryToProductLevel3SummaryMinifiedResponse(product);
      productResponses.add(productResponse);
    }
    return new GdnRestListResponse<>(null, null, true, productResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            productLevel3Minified.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductLevel3ControllerPath.UPDATE_RESIGN_BUSINESS_PARTNER_ITEMS, method = RequestMethod.POST
      , produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update item price", description = "update resign business partner items")
  @ResponseBody
  public GdnBaseRestResponse updateResignBusinessPartnerItems(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String businessPartnerCode) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "updateResignBusinessPartnerItems", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_UPDATE, null, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(businessPartnerCode)),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    this.productLevel3Service.updateResignBusinessPartnerItems(storeId, businessPartnerCode);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductLevel3ControllerPath.SEARCH_ITEM, method = RequestMethod.POST, consumes = {
      MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "search product level 3 item at controller",
      description = "search product level 3 item at controller")
  @ResponseBody
  public GdnRestListResponse<ProductLevel3ItemSearchResponse> searchItem(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String businessPartnerCode, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size,
      @RequestParam(required = false, defaultValue = "itemSku") String orderBy,
      @RequestParam(required = false, defaultValue = "asc") String sortBy,
      @RequestBody ProductLevel3ItemSearchRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "searchItem",
        businessPartnerCode, username, requestId, storeId, channelId, clientId,
        LoggerAspect.PRODUCT_LV3_FETCH, page + ":" + size, request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(),
        LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductLevel3Controller.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);

    SortOrder sort = new SortOrder(orderBy, sortBy);
    PageRequest pageable = PageRequest.of(page, size);
    ProductLevel3ItemSearch search = this.productLevel3RequestConverter
        .convertProductLevel3ItemSearchRequestToProductLevel3ItemSearch(request);
    search.setBusinessPartnerCode(businessPartnerCode);

    Page<ProductLevel3Item> pageOfItem =
        this.productLevel3Service.findItemBySearch(search, pageable, sort);
    Page<ProductLevel3ItemSearchResponse> pageOfSearchResponse =
        pageOfItem.map(item -> this.productLevel3ResponseConverter
            .convertProductLevel3ItemToProductLevel3ItemSearchResponse(item));
    return new GdnRestListResponse<>(pageOfSearchResponse.getContent(),
        new PageMetaData(pageOfSearchResponse.getSize(), pageOfSearchResponse.getNumber(),
            pageOfSearchResponse.getTotalElements()),
        requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductLevel3ControllerPath.UPDATE_PRODUCT_ASSIGNMENT, method = RequestMethod.PUT, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Assign/ unassign reviewer ", description = "Assign/ unassign reviewer ")
  @ResponseBody
  public GdnBaseRestResponse updateProductAssignment(@RequestParam String storeId, @RequestParam String requestId
      , @RequestParam(defaultValue = "NA") String assignedTo,
      @RequestParam String assignedBy, @RequestParam String productCode) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(productCode),
        ProductLevel3ControllerErrorMessage.ERROR_PRODUCT_CODE_EMPTY);
    GdnPreconditions
        .checkArgument(StringUtils.isNotEmpty(storeId), ProductLevel3ControllerErrorMessage.ERROR_STORE_ID_EMPTY);
    GdnPreconditions
        .checkArgument(StringUtils.isNotEmpty(assignedBy), ProductLevel3ControllerErrorMessage.ERROR_ASSIGNED_BY_EMPTY);
    GdnPreconditions
        .checkArgument(StringUtils.isNotEmpty(assignedTo), ProductLevel3ControllerErrorMessage.ERROR_ASSIGNED_TO_EMPTY);
    ProductLevel3Controller.LOGGER
        .info("Assign/unassign api for assignedBy : {} , assignedTo : {}, for productCode : {}", assignedBy, assignedTo,
            productCode);
    try {
      this.productServiceWrapper.updateProductAssignmentStatus(storeId, productCode, assignedTo, assignedBy);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOGGER.error("Error in updating product assignment, productCode:{}", productCode, e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), true,
          requestId);
    }
  }

  @RequestMapping(value = ProductLevel3ControllerPath.CHECK_AVAILABLE_STOCK, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "check available stock by business partner code", description = "check available stock by business partner code")
  @ResponseBody
  public GdnRestSimpleResponse<Boolean> checkAvailableStock(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String businessPartnerCode) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
        ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    Boolean response = this.productLevel3Service.checkAvailableStock(businessPartnerCode);
    return new GdnRestSimpleResponse<>(null, null, true, requestId, response);
  }

  @ResponseBody
  @RequestMapping(value = ProductLevel3ControllerPath.COPY_PRODUCTS_FILTER_SUMMARY, method = RequestMethod.POST,
    consumes = {MediaType.APPLICATION_JSON_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "product level 3 summary by filter for products available to copy",
      description = "filter product level 3 summary for products available to be copy")
  public GdnRestListResponse<AvailableToCopyProductDetailsResponse> productsAvailableToCopy(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username, @RequestParam String businessPartnerCode, @RequestParam String linkedPartnerCode,
    @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "10") Integer size,
    @RequestBody ProductLevel3SummaryRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute =
      new LoggerAttributeModel(this, "productCopyFilterSummary", businessPartnerCode, username, requestId,
        storeId, channelId, clientId, LoggerAspect.PRODUCT_LV3_FETCH, page + ":" + size, request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));

    LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(businessPartnerCode),
      ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(linkedPartnerCode),
      ProductLevel3ControllerErrorMessage.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);

    PageRequest pageRequest = PageRequest.of(page, size);

    ProductLevel3SummaryFilter filter = productLevel3RequestConverter
      .convertProductLevel3SummaryRequestToProductLevel3SummaryFilter(request);

    filter.setStoreId(storeId);
    filter.setBusinessPartnerCode(linkedPartnerCode);

    Page<AvailableToCopyProductDetailsResponse> productsAvailableToBeCopied = productLevel3Service
      .productsAvailableToBeCopied(filter, businessPartnerCode, pageRequest);

    return new GdnRestListResponse<>(productsAvailableToBeCopied.getContent(), new PageMetaData(pageRequest.getPageSize(),
      pageRequest.getPageNumber(), productsAvailableToBeCopied.getTotalElements()), requestId);
  }

}
