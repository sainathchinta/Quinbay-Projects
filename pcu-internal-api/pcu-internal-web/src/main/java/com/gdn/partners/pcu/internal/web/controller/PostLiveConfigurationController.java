package com.gdn.partners.pcu.internal.web.controller;

import java.io.IOException;
import java.util.List;

import jakarta.validation.Valid;

import org.hibernate.validator.constraints.NotBlank;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.model.PostLiveConfigurationControllerPath;
import com.gdn.partners.pcu.internal.service.PostLiveConfigurationService;
import com.gdn.partners.pcu.internal.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.internal.service.impl.util.ConverterUtil;
import com.gdn.partners.pcu.internal.web.model.request.CategoryConfigurationWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ConfigurationFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ConfigurationWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.MerchantConfigurationWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.CategoryConfigurationFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.CategoryConfigurationHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ConfigurationCountWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ConfigurationsStatusWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantConfigurationFilterWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantConfigurationHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.MerchantWebSearchResponse;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "PostLive Configuration API")
@RestController
@RequestMapping(value = PostLiveConfigurationControllerPath.BASE_PATH)
@Validated
public class PostLiveConfigurationController {

  private final String MERCHANT = "Merchant";
  private final String CATEGORY = "Category";

  @Autowired
  private PostLiveConfigurationService postLiveConfigurationService;

  @Autowired
  private ClientParameterHelper clientParameterHelper;

  @Operation(summary = "Returns results of merchant search with configuration")
  @GetMapping(value = PostLiveConfigurationControllerPath.MERCHANT_SEARCH, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<MerchantWebSearchResponse> getMerchantsBySearchKeyword(
      @PathVariable("searchKeyword") @Valid @NotBlank(message = ErrorMessages.EMPTY_MERCHANT_SEARCH_KEYWORD)
          String searchKeyword, @RequestParam(defaultValue = "0", required = false) int page,
      @RequestParam(defaultValue = "30", required = false) int size) {
    log.info("Fetching business partners for keyword : {}", searchKeyword);
    Page<MerchantWebSearchResponse> merchantWebSearchResponsePage =
        postLiveConfigurationService.getMerchantsBySearchKeyword(searchKeyword, page, size);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        merchantWebSearchResponsePage.getContent(),
        new Metadata(page, size, merchantWebSearchResponsePage.getTotalElements()));
  }

  @Operation(summary = "add configuration")
  @PostMapping(value = PostLiveConfigurationControllerPath.ADD_CATEGORY_CONFIGURATION, consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse addCategoryConfiguration(@RequestBody List<CategoryConfigurationWebRequest> request) {
    log.info("update configurations for categories : {}", request);
    postLiveConfigurationService.addCategoryConfiguration(ConverterUtil.toCategoryConfigurationListRequest(request));
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        null);
  }

  @Operation(summary = "update category configuration by status")
  @PutMapping(value = PostLiveConfigurationControllerPath.UPDATE_CATEGORY_CONFIGURATION, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse updateCategoryConfiguration(@PathVariable String categoryCode,
      @RequestBody CategoryConfigurationWebRequest request) throws Exception {
    log.info("update configuration for categoryCode : {}", categoryCode);
    postLiveConfigurationService.updateCategoryConfiguration(categoryCode, request.getReviewConfig());
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        null);
  }

  @Operation(summary = "delete category configuration by status")
  @DeleteMapping(value = PostLiveConfigurationControllerPath.DELETE_CATEGORY_CONFIGURATION, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse deleteCategoryConfiguration(@PathVariable String categoryCode) throws Exception {
    log.info("delete configuration for categoryCode : {}", categoryCode);
    postLiveConfigurationService.deleteCategoryConfiguration(categoryCode);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        null);
  }

  @Operation(summary = "add merchant configuration")
  @PostMapping(value = PostLiveConfigurationControllerPath.ADD_MERCHANT_CONFIGURATION, consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse addMerchantConfiguration(@RequestBody List<MerchantConfigurationWebRequest> request)
      throws Exception {
    log.info("update configurations for merchants : {}", request);
    postLiveConfigurationService.addMerchantConfiguration(ConverterUtil.toMerchantConfigurationListRequest(request));
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        null);
  }

  @Operation(summary = "update merchant configuration by status")
  @PutMapping(value = PostLiveConfigurationControllerPath.UPDATE_MERCHANT_CONFIGURATION, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse updateMerchantConfiguration(@PathVariable String merchantCode,
      @RequestBody MerchantConfigurationWebRequest request) throws Exception {
    log.info("update configuration for merchantCode : {}", merchantCode);
    postLiveConfigurationService.updateMerchantConfiguration(merchantCode, request.getReviewConfig());
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        null);
  }

  @Operation(summary = "delete merchant configuration by status")
  @DeleteMapping(value = PostLiveConfigurationControllerPath.DELETE_MERCHANT_CONFIGURATION, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse deleteMerchantConfiguration(@PathVariable String merchantCode) throws Exception {
    log.info("delete configuration for merchantCode : {}", merchantCode);
    postLiveConfigurationService.deleteMerchantConfiguration(merchantCode);
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        null);
  }

  @Operation(summary = "Bulk Configuration Upload")
  @PostMapping(value = PostLiveConfigurationControllerPath.
      BULK_CONFIGURATION_UPLOAD, produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse upLoadBulkConfiguration(
      @RequestParam("type") @Valid @NotBlank(message = ErrorMessages.CONFIGURATION_TYPE_EMPTY) String type,
      @RequestParam MultipartFile request) throws Exception {
    String requestId = this.clientParameterHelper.getRequestId();
    GdnPreconditions.checkArgument(MERCHANT.equalsIgnoreCase(type) || CATEGORY.equalsIgnoreCase(type),
        ErrorMessages.CONFIGURATION_TYPE_INVALID);
    log.info("Bulk update configuration for type {} and requestId {}", type, requestId);
    try {
      postLiveConfigurationService
          .uploadBulkConfiguration(request, type, requestId, clientParameterHelper.getStoreId(),
              this.clientParameterHelper.getUsername());
    } catch (IOException e) {
      return new BaseResponse("Error when transferring file " + request.getOriginalFilename() + e.getMessage(), null,
          false, requestId);
    }
    return new BaseResponse(null, null, true, requestId);
  }

  @Operation(summary = "Get the configuration status by merchantCode and categoryCode")
  @PostMapping(value = PostLiveConfigurationControllerPath.CONFIGURATIONS_STATUS, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<ConfigurationsStatusWebResponse> getConfigurationsStatus(
      @RequestBody List<ConfigurationWebRequest> request) throws Exception {
    log.info("Fetching configurations status for : {}", request);
    List<ConfigurationsStatusWebResponse> configurationsStatusWebResponses =
        postLiveConfigurationService.getConfigurationsStatus(request);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        configurationsStatusWebResponses, null);
  }

  @Operation(summary = "Returns count of configurations recorded for merchant and category")
  @GetMapping(value = PostLiveConfigurationControllerPath.CONFIGURATION_COUNT,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<ConfigurationCountWebResponse> getConfigurationCounts() {
    log.info("Retrieving counts for category and merchant configuration");
    return new SingleBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        ResponseHelper.toConfigurationCountWebResponse(postLiveConfigurationService.getConfigurationCounts()));
  }

  @Operation(summary = "Returns filtered category configuration listing")
  @PostMapping(value = PostLiveConfigurationControllerPath.CATEGORY_LISTING,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<CategoryConfigurationFilterWebResponse> filterCategoryConfiguration(@RequestBody
      ConfigurationFilterWebRequest configurationFilterWebRequest,
      @RequestParam(required = false, defaultValue = "0") int page,
      @RequestParam(required = false, defaultValue = "25") int size) {
      log.info("Fetching category configuration listing for filters : {}", configurationFilterWebRequest);
    Page<CategoryConfigurationFilterWebResponse> categoryConfigurationFilterWebResponsePage =
        this.postLiveConfigurationService.filterCategoryConfiguration(configurationFilterWebRequest, page, size);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        categoryConfigurationFilterWebResponsePage.getContent(),
        new Metadata(page, size, categoryConfigurationFilterWebResponsePage.getTotalElements()));
  }

  @Operation(summary = "Returns filtered merchant configuration listing")
  @PostMapping(value = PostLiveConfigurationControllerPath.MERCHANT_LISTING,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<MerchantConfigurationFilterWebResponse> filterMerchantConfiguration(@RequestBody
      ConfigurationFilterWebRequest configurationFilterWebRequest,
      @RequestParam(required = false, defaultValue = "0") int page,
      @RequestParam(required = false, defaultValue = "25") int size) {
    log.info("Fetching merchant configuration listing for filters : {}", configurationFilterWebRequest);
    Page<MerchantConfigurationFilterWebResponse> merchantConfigurationFilterWebResponsePage =
        this.postLiveConfigurationService.filterMerchantConfiguration(configurationFilterWebRequest, page, size);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        merchantConfigurationFilterWebResponsePage.getContent(),
        new Metadata(page, size, merchantConfigurationFilterWebResponsePage.getTotalElements()));
  }

  @Operation(summary = "Return category configuration history by category code")
  @GetMapping(value = PostLiveConfigurationControllerPath.CATEGORY_CONFIGURATION_HISTORY,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<CategoryConfigurationHistoryWebResponse> fetchCategoryConfigurationHistory(
      @PathVariable("category-code") @Valid @NotBlank(message = ErrorMessages.EMPTY_CATEGORY_CODE) String categoryCode,
      @RequestParam(required = false, defaultValue = "0") int page,
      @RequestParam(required = false, defaultValue = "25") int size) {
    log.info("Fetching category configuration history for category code : {}", categoryCode);
    Page<CategoryConfigurationHistoryWebResponse> categoryConfigurationHistoryWebResponsePage =
        this.postLiveConfigurationService.getCategoryConfigurationHistory(categoryCode, page, size);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        categoryConfigurationHistoryWebResponsePage.getContent(),
        new Metadata(page, size, categoryConfigurationHistoryWebResponsePage.getTotalElements()));
  }

  @Operation(summary = "Returns history of merchant configurations")
  @GetMapping(value = PostLiveConfigurationControllerPath.MERCHANT_HISTORY, produces = MediaType.APPLICATION_JSON_VALUE)
  public ListBaseResponse<MerchantConfigurationHistoryWebResponse> getMerchantConfigurationHistory(
      @PathVariable("merchant-code") @Valid @NotBlank(message = ErrorMessages.EMPTY_MERCHANT_CODE) String merchantCode,
      @RequestParam(required = false, defaultValue = "0") int page,
      @RequestParam(required = false, defaultValue = "25") int size) {
    log.info("Retrieving merchant configuration history for merchant code : {}", merchantCode);
    Page<MerchantConfigurationHistoryWebResponse> merchantConfigurationHistoryWebResponsePage =
        this.postLiveConfigurationService.getMerchantConfigurationHistory(merchantCode, page, size);
    return new ListBaseResponse<>(null, null, true, clientParameterHelper.getRequestId(),
        merchantConfigurationHistoryWebResponsePage.getContent(),
        new Metadata(page, size, merchantConfigurationHistoryWebResponsePage.getTotalElements()));

  }

  @Operation(summary = "Bulk Configuration Download")
  @PostMapping(value = PostLiveConfigurationControllerPath.
      BULK_CONFIGURATION_DOWNLOAD, consumes = MediaType.APPLICATION_JSON_VALUE,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public BaseResponse downloadBulkConfiguration(
      @RequestParam("type") @Valid @NotBlank(message = ErrorMessages.CONFIGURATION_TYPE_EMPTY) String type,
      @RequestBody ConfigurationFilterWebRequest configurationFilterWebRequest) throws Exception {
    GdnPreconditions.checkArgument(MERCHANT.equalsIgnoreCase(type) || CATEGORY.equalsIgnoreCase(type),
        ErrorMessages.CONFIGURATION_TYPE_INVALID);
    String requestId = this.clientParameterHelper.getRequestId();
    log.info("Bulk download configuration for type {} and requestId {}", type, requestId);
    this.postLiveConfigurationService.downloadBulkConfiguration(configurationFilterWebRequest, type,
        this.clientParameterHelper.getStoreId(),
        this.clientParameterHelper.getUsername());
    return new BaseResponse(null, null, true, requestId);
  }
}
