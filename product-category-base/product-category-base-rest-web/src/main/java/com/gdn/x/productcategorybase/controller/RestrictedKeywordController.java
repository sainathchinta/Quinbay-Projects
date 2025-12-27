package com.gdn.x.productcategorybase.controller;

import java.util.List;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.productcategorybase.controller.util.ConverterUtil;
import com.gdn.x.productcategorybase.dto.RestrictedKeywordsUpdateDTO;
import com.gdn.x.productcategorybase.dto.request.RestrictedKeywordsSearchRequest;
import com.gdn.x.productcategorybase.dto.request.RestrictedKeywordsUpdateRequest;
import com.gdn.x.productcategorybase.service.RestrictedKeywordServiceWrapper;
import io.swagger.v3.oas.annotations.Operation;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;

import com.gdn.x.productcategorybase.RestrictedKeywordApiPath;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsListingResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.dto.response.UiValidationRestrictedKeywordsResponse;
import com.gdn.x.productcategorybase.helper.RestrictedKeywordServiceHelper;
import com.gdn.x.productcategorybase.service.RestrictedKeywordHistoryService;
import com.gdn.x.productcategorybase.service.RestrictedKeywordService;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Set;


import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.RequestBody;

@Slf4j
@RestController
@RequestMapping(value = RestrictedKeywordApiPath.BASE_PATH)
@Tag(name = "RestrictedKeywordController", description = "Restricted Keyword Service API")
public class RestrictedKeywordController {

    private static final String RESTRICTED_KEYWORDS_UPDATE_ERROR_MESSAGE = "Error while updating restricted keywords";

    @Value("${default.internal.activation.period}")
    private int defaultInternalActivationPeriod;

    @Value("${product.attribute.configuration}")
    private Set<String> attributeConfigurationSet;

    @Autowired
    private RestrictedKeywordServiceWrapper restrictedKeywordServiceWrapper;
    @Autowired
    private RestrictedKeywordServiceHelper restrictedKeywordServiceHelper;
    @Autowired
    private RestrictedKeywordHistoryService restrictedKeywordHistoryService;
    @Autowired
    private RestrictedKeywordService restrictedKeywordService;

  @RequestMapping(value = RestrictedKeywordApiPath.HISTORY, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get history by keywordId", description = "get history by store id and "
      + "brand and pageable")
  public GdnRestListResponse<RestrictedKeywordHistoryResponse> getRestrictedKeywordHistory(
      @RequestParam String storeId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable("keywordId") String keywordId,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    log.info("Getting restricted keyword history. storeId : {}, requestId : {}, keywordId : {} ",
        storeId, requestId, keywordId);
        try {
            Page<RestrictedKeywordHistoryResponse> restrictedKeywordHistoryResponses =
                restrictedKeywordHistoryService.getRestrictedKeywordHistory(storeId, keywordId,
                    PageRequest.of(page, size));
            return new GdnRestListResponse<>(null, null, true, restrictedKeywordHistoryResponses.getContent(),
                new PageMetaData(size, page, restrictedKeywordHistoryResponses.getTotalElements()), requestId);
        } catch (ApplicationRuntimeException e) {
            log.error("Error while getting restricted keyword history. storeId : {}, requestId : {}, keywordId : {} ",
                storeId, requestId, keywordId);
            return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCodes().getMessage(), false, null, null,
                requestId);
        } catch (Exception e) {
            log.error("Error while getting restricted keyword history. storeId : {}, requestId : {}, keywordId : {} ",
                storeId, requestId, keywordId);
            return new GdnRestListResponse<>(e.getMessage(), null, false, null, null, requestId);
        }
    }

  @RequestMapping(value = RestrictedKeywordApiPath.SEARCH_RESTRICTED_KEYWORDS, method =
      RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get restricted keywords", description = "get restricted keywords")
  public GdnRestListResponse<RestrictedKeywordsResponse> getRestrictedKeywords(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size,
      @RequestBody RestrictedKeywordsSearchRequest request) throws Exception {
    try {
      Page<RestrictedKeywordsResponse> restrictedKeywordsResponsePage =
        restrictedKeywordServiceHelper.getRestrictedKeywordsResponses(storeId, request.getKeyword(),
          PageRequest.of(page, size));
      return new GdnRestListResponse<>(null, null, true,
        restrictedKeywordsResponsePage.getContent(),
        new PageMetaData(size, page, restrictedKeywordsResponsePage.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error occurred while fetching keywords : {} ", request.getKeyword(), e);
      return new GdnRestListResponse<>(e.getMessage(), null, false, requestId);
    }
  }

  @RequestMapping(value = RestrictedKeywordApiPath.UPDATE_RESTRICTED_KEYWORDS, method =
      RequestMethod.PUT, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Update the restricted keywords", description = "Update the restricted "
      + "keywords")
  public GdnBaseRestResponse updateRestrictedKeywords(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody RestrictedKeywordsUpdateRequest restrictedKeywordUpdateRequest)
      throws Exception {
    try {
      log.info("Update restricted keywords with request : {}", restrictedKeywordUpdateRequest);
            MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER, username);
            RestrictedKeywordsUpdateDTO restrictedKeywordsUpdateDTO =
                ConverterUtil.toRestrictedKeywordsUpdateDTO(restrictedKeywordUpdateRequest);
            restrictedKeywordServiceWrapper.updateRestrictedKeyword(restrictedKeywordsUpdateDTO, storeId);
            return new GdnBaseRestResponse(null, null, true, requestId);
        } catch (ApplicationRuntimeException e) {
            log.error("Runtime Exception occurred while updating restricted keywords", e);
            return new GdnBaseRestResponse(e.getErrorMessage(), e.getErrorCodes().getCode(), Boolean.FALSE, requestId);
        } catch (Exception e) {
            log.error("Exception occurred while updating restricted keywords", e);
            return new GdnBaseRestResponse(RESTRICTED_KEYWORDS_UPDATE_ERROR_MESSAGE,
                ErrorCategory.UNSPECIFIED.getCode(), Boolean.FALSE, requestId);
        }
    }

  @RequestMapping(value = RestrictedKeywordApiPath.LISTING, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get restricted keyword for listing", description = "get restricted "
      + "keyword for listing")
  public GdnRestListResponse<RestrictedKeywordsListingResponse> getRestrictedKeywordForListing(
      @RequestParam String storeId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(required = false, defaultValue = "") String keyword,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) {
    log.info(
        "Fetching restricted keyword for listing. storeId : {} , requestId : {}, keyword : {} ",
        storeId, requestId, keyword);
        try {
            Page<RestrictedKeywordsListingResponse> response =
                restrictedKeywordService.getRestrictedKeywordForListing(storeId, keyword, PageRequest.of(page, size));
            return new GdnRestListResponse<>(null, null, true, response.getContent(),
                new PageMetaData(size, page, response.getTotalElements()), requestId);
        } catch (ApplicationRuntimeException e) {
            log.error("Error while fetching restricted keyword for listing. requestId : {}, keyword : {}, error - ",
                requestId, keyword);
            return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCodes().getMessage(), false, null, null,
                requestId);
        } catch (Exception e) {
            log.error("Error while fetching restricted keyword for listing. requestId : {}, keyword : {}, error - ",
                requestId, keyword);
            return new GdnRestListResponse<>(e.getMessage(), null, false, null, null, requestId);
        }
    }

  @RequestMapping(value = RestrictedKeywordApiPath.UI_VALIDATION_LIST, method = RequestMethod.GET
      , produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get restricted keyword for ui-validation listing", description = "get "
      + "restricted keyword for ui-validation listing")
  public GdnRestListResponse<UiValidationRestrictedKeywordsResponse> getRestrictedKeywordForUiValidationListing(
      @RequestParam String storeId, @RequestParam String requestId,
      @RequestParam(required = false) String username) {
    log.info("Fetching restricted keyword for listing. storeId : {} , requestId : {} ", storeId,
        requestId);
    try {
      List<UiValidationRestrictedKeywordsResponse> response =
                restrictedKeywordService.getListOfRestrictedKeywordsForUiValidation(storeId);
            return new GdnRestListResponse<>(null, null, true, response, new PageMetaData(0, 0, response.size()),
                requestId);
        } catch (ApplicationRuntimeException e) {
            log.error("Error while fetching restricted keyword for ui-validation listing. requestId : {}, error - ",
                requestId);
            return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCodes().getMessage(), false, null, null,
                requestId);
        }
    }
}
