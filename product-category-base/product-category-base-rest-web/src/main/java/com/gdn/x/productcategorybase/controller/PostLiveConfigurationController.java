package com.gdn.x.productcategorybase.controller;

import java.util.Date;
import java.util.List;

import org.apache.commons.lang.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.MerchantSearchResponse;
import com.gdn.x.productcategorybase.dto.PostLiveConfigurationControllerPath;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryConfigurationRequestList;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.request.MerchantConfigurationRequestList;
import com.gdn.x.productcategorybase.dto.response.BulkCategoryConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;
import com.gdn.x.productcategorybase.dto.response.BulkMerchantConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.ConfigurationCountResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.service.PostLiveConfigurationService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@RequestMapping(value = PostLiveConfigurationControllerPath.BASE_PATH)
@Tag(name = "PostLiveConfigurationController", description = "PostLiveConfigurationService")
public class PostLiveConfigurationController {

  private static String MERCHANT = "merchant";
  private static String CATEGORY = "category";

  @Autowired
  private PostLiveConfigurationService postLiveConfigurationService;

  @RequestMapping(value = PostLiveConfigurationControllerPath.FETCH_MERCHANT_CONFIGURATION, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  
  public GdnRestListResponse<MerchantSearchResponse> fetchMerchantSearchResult(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestBody List<MerchantConfigurationRequest> merchantConfigurationRequestList) {
    try {
      List<MerchantSearchResponse> merchantSearchResponseList =
          this.postLiveConfigurationService.fetchMerchantConfiguration(merchantConfigurationRequestList);
      return new GdnRestListResponse<>(null, null, true, merchantSearchResponseList, new PageMetaData(), requestId);
    } catch (Exception e) {
      log.error("Error while fetching configuration for given merchant details : {}", merchantConfigurationRequestList,
          e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    }
  }

  @RequestMapping(value = PostLiveConfigurationControllerPath.FILTER_CATEGORY_CONFIGURATION,
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  
  public GdnRestListResponse<CategoryConfigurationFilterResponse> getCategoryConfigurationList(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ConfigurationFilterRequest configurationFilterRequest,
      @RequestParam(required = false, defaultValue = "0") int page,
      @RequestParam(required = false, defaultValue = "25") int size) {
    log.info("Fetching category configuration listing according to following request : {}", configurationFilterRequest);
    try {
      Page<CategoryConfigurationFilterResponse> categoryConfigurationFilterResponsePage =
          postLiveConfigurationService.getCategoryConfigurationList(storeId, configurationFilterRequest, page, size);
      return new GdnRestListResponse<>(null, null, true, categoryConfigurationFilterResponsePage.getContent(),
          new PageMetaData(size, page, categoryConfigurationFilterResponsePage.getTotalElements()), requestId);
    } catch (Exception e){
      log.error("Error while fetching category configuration for request : {}", configurationFilterRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, null, null, requestId);
    }
  }

  @RequestMapping(value = PostLiveConfigurationControllerPath.ADD_CATEGORY_CONFIGURATION, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "add category configuration", description = "add the configuration by storeId and categoryCode")
  
  public GdnBaseRestResponse addCategoryConfiguration(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody List<CategoryConfigurationRequest> requests) throws Exception {
    try {
      MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, username);
      this.postLiveConfigurationService.addCategoryConfiguration(storeId, requests);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("error while adding the configuration for request : {}", requests, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = PostLiveConfigurationControllerPath.UPDATE_CATEGORY_CONFIGURATION, method = RequestMethod.PUT, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update category configuration", description = "update the configuration by storeId and categoryCode")
  
  public GdnBaseRestResponse updateCategoryConfiguration(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody CategoryConfigurationRequest request) throws Exception {
    try {
      MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, username);
      this.postLiveConfigurationService.updateCategoryConfiguration(storeId, request);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("error while updating the configuration for categoryCode : {}", request.getCategoryCode(), e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = PostLiveConfigurationControllerPath.DELETE_CATEGORY_CONFIGURATION, method = RequestMethod.DELETE, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "delete category configuration", description = "delete the configuration by storeId and categoryCode")
  
  public GdnBaseRestResponse deleteCategoryConfiguration(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("categoryCode") String categoryCode) throws Exception {
    try {
      MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, username);
      this.postLiveConfigurationService.deleteCategoryConfiguration(storeId, categoryCode);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("error while deleting the configuration for categoryCode : {}", categoryCode, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }


  @RequestMapping(value = PostLiveConfigurationControllerPath.ADD_MERCHANT_CONFIGURATION, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "add merchant configuration", description = "add the configuration by storeId and merchantCode")
  
  public GdnBaseRestResponse addMerchantConfiguration(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody List<MerchantConfigurationRequest> requests)
      throws Exception {
    try {
      MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, username);
      this.postLiveConfigurationService.addMerchantConfiguration(storeId, requests);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("error while adding the configuration for request : {}", requests, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = PostLiveConfigurationControllerPath.UPDATE_MERCHANT_CONFIGURATION, method = RequestMethod.PUT, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update merchant configuration", description = "update the configuration by storeId and merchantCode")
  
  public GdnBaseRestResponse updateMerchantConfiguration(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody MerchantConfigurationRequest request) throws Exception {
    try {
      MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, username);
      this.postLiveConfigurationService.updateMerchantConfiguration(storeId, request);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("error while updating the configuration for merchantCode : {}", request.getBusinessPartnerCode(), e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = PostLiveConfigurationControllerPath.DELETE_MERCHANT_CONFIGURATION, method = RequestMethod.DELETE, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "delete merchant configuration", description = "delete the configuration by storeId and merchantCode")
  
  public GdnBaseRestResponse deleteMerchantConfiguration(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("merchantCode") String merchantCode) throws Exception {
    try {
      MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, username);
      this.postLiveConfigurationService.deleteMerchantConfiguration(storeId, merchantCode);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("error while deleting the configuration for merchantCode : {}", merchantCode, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = PostLiveConfigurationControllerPath.GET_CONFIGURATION_STATUS, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get the configuration status", description = "get the configuration by merchant and category code")
  
  public GdnRestListResponse<ConfigurationStatusResponse> getconfigurationstatus(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody List<ConfigurationStatusRequest> requests) {
    try {
      log.info("getting configurations status for : {}", requests);
      List<ConfigurationStatusResponse> response = this.postLiveConfigurationService.getConfigurations(storeId, requests);
      return new GdnRestListResponse<>(null, null, true, response, new PageMetaData(), requestId);
    } catch (Exception e) {
      log.error("Error while getting configurations status for : {}", requests, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    }
  }

  @RequestMapping(value = PostLiveConfigurationControllerPath.FETCH_CONFIGURATION_COUNT, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  
  public GdnRestSingleResponse<ConfigurationCountResponse> fetchConfigurationCounts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username) {
    try{
      log.info("Fetching configuration counts for merchant and category");
      return new GdnRestSingleResponse<>(null, null, true,
          this.postLiveConfigurationService.fetchConfigurationCounts(storeId), requestId);
    } catch (Exception e) {
      log.error("Error while fetching configuration counts for merchant and category", e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = PostLiveConfigurationControllerPath.BULK_CONFIG_MERCHANT, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Bulk Merchant Config Upload", description = "Bulk Merchant Config Upload")
  
  public GdnRestListResponse<BulkMerchantConfigUploadResponse> bulkMerchantConfigUpload(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody MerchantConfigurationRequestList merchantConfigurationRequestList)
      throws Exception {
    log.info("Bulk upload for merchant configuration for request : {} ",
        merchantConfigurationRequestList.getMerchantConfigurationRequestList());
    List<BulkMerchantConfigUploadResponse> response;
    try {
      MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, username);
      response = this.postLiveConfigurationService
          .bulkMerchantConfigUpload(storeId, merchantConfigurationRequestList.getMerchantConfigurationRequestList());
    } catch (Exception e) {
      log.error("Error while bulk updating the merchant configuration", e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null,
          new PageMetaData(), requestId);
    }
    return new GdnRestListResponse<>(response, new PageMetaData(), requestId);
  }

  @RequestMapping(value = PostLiveConfigurationControllerPath.BULK_CONFIG_CATEGORY, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Bulk Category Config Upload", description = "Bulk Category Config Upload")
  
  public GdnRestListResponse<BulkCategoryConfigUploadResponse> bulkCategoryConfigUpload(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody CategoryConfigurationRequestList categoryConfigurationRequestList)
      throws Exception {
    log.info("Bulk upload for category configuration for request : {} ",
        categoryConfigurationRequestList.getCategoryConfigurationRequestList());
    List<BulkCategoryConfigUploadResponse> response;
    try {
      MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, username);
      response = this.postLiveConfigurationService
          .bulkCategoryConfigUpload(storeId, categoryConfigurationRequestList.getCategoryConfigurationRequestList());
    } catch (Exception e) {
      log.error("Error while bulk updating the category configuration", e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null,
          new PageMetaData(), requestId);
    }
    return new GdnRestListResponse<>(response, new PageMetaData(), requestId);
  }

  @RequestMapping(value = PostLiveConfigurationControllerPath.GET_CONFIGURATION_CHANGES,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get the configuration status", description = "get the configuration by merchant and category code")
  
  public GdnRestListResponse<ConfigurationStatusResponse> getConfigurationChanges(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam long fromDateInMillis) {
    try {
      log.info("getting configurations changes after date: {}", fromDateInMillis);
      List<ConfigurationStatusResponse> response = postLiveConfigurationService.getConfigurationChangesByDate(
          storeId, new Date(fromDateInMillis));
      return new GdnRestListResponse<>(null, null, true, response,
          new PageMetaData(response.size(), 0, response.size()), requestId);
    } catch (Exception e) {
      log.error("Error while getting configurations changes", e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    }
  }

  @RequestMapping(value = PostLiveConfigurationControllerPath.FILTER_MERCHANT_CONFIGURATION,
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  
  public GdnRestListResponse<MerchantConfigurationFilterResponse> getMerchantConfigurationList(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ConfigurationFilterRequest configurationFilterRequest,
      @RequestParam(required = false, defaultValue = "0") int page,
      @RequestParam(required = false, defaultValue = "25") int size) {
    log.info("Fetching merchant configuration listing according to following request : {}", configurationFilterRequest);
    try {
      Page<MerchantConfigurationFilterResponse> merchantConfigurationFilterResponsePage =
          postLiveConfigurationService.getMerchantConfigurationList(storeId, configurationFilterRequest, page, size);
      return new GdnRestListResponse<>(null, null, true, merchantConfigurationFilterResponsePage.getContent(),
          new PageMetaData(size, page, merchantConfigurationFilterResponsePage.getTotalElements()), requestId);
    } catch (Exception e){
      log.error("Error while fetching category configuration for request : {}", configurationFilterRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, null, null, requestId);
    }
  }

  @RequestMapping(value = PostLiveConfigurationControllerPath.CATEGORY_CONFIGURATION_HISTORY,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  
  public GdnRestListResponse<CategoryConfigurationHistoryResponse> getCategoryConfigurationHistory(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("categoryCode") String categoryCode, @RequestParam(required = false, defaultValue = "0") int page,
      @RequestParam(required = false, defaultValue = "25") int size) {
    GdnPreconditions
        .checkArgument(StringUtils.isNotEmpty(categoryCode), ErrorMessage.CATEGORY_CODE_MUST_NOT_BE_BLANK.getMessage());
    try{
      Page<CategoryConfigurationHistoryResponse> categoryConfigurationHistoryResponsePage =
          this.postLiveConfigurationService.getCategoryConfigurationHistory(storeId, categoryCode, page, size);
      return new GdnRestListResponse<>(null, null, true, categoryConfigurationHistoryResponsePage.getContent(),
          new PageMetaData(size, page, categoryConfigurationHistoryResponsePage.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error while fetching category configuration history for category code : {}", categoryCode, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    }

  }

  @RequestMapping(value = PostLiveConfigurationControllerPath.MERCHANT_CONFIGURATION_HISTORY, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch merchant configuration history")
  
  public GdnRestListResponse<MerchantConfigurationHistoryResponse> getMerchantConfigurationHistory(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @PathVariable("merchantCode") String merchantCode,
      @RequestParam(required = false, defaultValue = "0") int page,
      @RequestParam(required = false, defaultValue = "25") int size) {
      log.info("Fetching merchant configuration for : {}", merchantCode);
      try {
        Page<MerchantConfigurationHistoryResponse> merchantConfigurationHistoryResponsePage =
            this.postLiveConfigurationService.getMerchantConfigurationHistory(storeId, merchantCode, page, size);
        return new GdnRestListResponse<>(null, null, true, merchantConfigurationHistoryResponsePage.getContent(),
            new PageMetaData(size, page, merchantConfigurationHistoryResponsePage.getTotalElements()), requestId);
      } catch (Exception e) {
        log.error("Error while fetching merchant configuration history for : {}", merchantCode, e);
        return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
            requestId);
      }
  }

  @RequestMapping(value = PostLiveConfigurationControllerPath.FETCH_DETAILS_BY_CODE, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch config details by codes", description = "Fetch config details by codes")
  
  public GdnRestListResponse<BulkConfigDataResponse> fetchConfigDetailsByCodes(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam("configType") String configType,
      @RequestBody AttributeCodesRequest attributeCodesRequest) throws Exception {
    GdnPreconditions.checkArgument(MERCHANT.equalsIgnoreCase(configType) || CATEGORY.equalsIgnoreCase(configType),
        ErrorMessage.ERROR_CONFIG_TYPE.getMessage());
    log.info("Fetching the configuration details with configType : {} for codes : {}", configType,
        attributeCodesRequest);
    List<BulkConfigDataResponse> result;
    try {
      result = postLiveConfigurationService
          .fetchConfigDetailsByConfigTypeForCodes(storeId, configType, attributeCodesRequest.getAttributeCodes());
    } catch (Exception e) {
      log.error("Error while fetching the configuration details with configType : {} for codes : {}", configType,
          attributeCodesRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, null, null, requestId);
    }
    return new GdnRestListResponse<>(null, null, true, result, null, requestId);
  }
}
