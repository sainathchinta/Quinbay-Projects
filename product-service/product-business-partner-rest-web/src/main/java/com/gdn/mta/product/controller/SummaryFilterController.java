package com.gdn.mta.product.controller;

import java.io.IOException;
import java.util.List;

import com.gda.mta.product.dto.HistoryUpdateRequest;
import com.gda.mta.product.dto.response.HistoryUpdateResponse;
import org.apache.solr.client.solrj.SolrServerException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gda.mta.product.dto.HistoryRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.response.AssigneeResponse;
import com.gda.mta.product.dto.response.FilterCountResponse;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gda.mta.product.dto.response.HistoryResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.service.SummaryFilterService;
import com.gdn.mta.product.util.ControllerUtils;
import com.gdn.mta.product.valueobject.SummaryFilterServiceRequest;
import com.gdn.mta.product.web.model.SummaryFilterApiPath;

import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;
import lombok.extern.slf4j.Slf4j;

@RestController
@RequestMapping(value = SummaryFilterApiPath.BASE_PATH)
@Tag(name = "SummaryFilterController", description = "Filter controller api")
@Slf4j
public class SummaryFilterController {

  @Autowired
  private SummaryFilterService summaryFilterService;

  @RequestMapping(value = SummaryFilterApiPath.REVIEW_PRODUCTS_FILTER, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Review products by activated and viewable flag", description = "Review products by activated and "
      + "viewable flag")
  @ResponseBody
  public GdnRestListResponse<ReviewProductResponse> getReviewProductsByActivatedAndViewableFlag(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam boolean activated,
      @RequestParam boolean viewable, @RequestBody SummaryFilterRequest request,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "100") int size) {
    log.info("API method to get review products by activated : {}, viewable : {}, requestId : {}, request :{}",
        activated, viewable, requestId, request);
    try {
      Page<ReviewProductResponse> response = summaryFilterService
          .getReviewProductsByFilterRequestAndActivatedAndViewableFlag(storeId, request, activated, viewable, page,
              size);
      return new GdnRestListResponse<>(response.getContent(), new PageMetaData(page, size, response.getTotalElements()),
          requestId);
    } catch (SolrServerException | IOException e) {
      log.error("Exception while fetching review products from solr, requestId :{}", requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.DATA_NOT_FOUND.getMessage(), false, null, null,
          requestId);
    } catch (Exception e) {
      log.error("Exception while fetching review products from solr, requestId :{}", requestId, e);
      return new GdnRestListResponse<>(null, ErrorCategory.UNSPECIFIED.getMessage(), false, null, null, requestId);
    }
  }

  @RequestMapping(value = SummaryFilterApiPath.COUNTS_FILTER, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "count filters by activated and viewable flag", description = "count filters by activated and viewable flag")
  @ResponseBody
  public GdnRestSingleResponse<FilterCountResponse> getFilterCounts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam boolean activated, @RequestParam boolean viewable) {
    log.info("API to get filter counts by activated : {}, viewable : {}, requestId : {}", activated, viewable,
        requestId);
    try {
      FilterCountResponse response =
          summaryFilterService.getFilterCountsByStoreIdAndActivatedAndViewable(storeId, activated, viewable);
      return new GdnRestSingleResponse<>(response, requestId);
    } catch (SolrServerException | IOException e) {
      log.error("Exception caught while retrieving filter counts, requestId : {}", requestId, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.DATA_NOT_FOUND.getMessage(), false, null,
          requestId);
    } catch (Exception e) {
      log.error("Exception caught while retrieving filter counts, requestId : {}", requestId, e);
      return new GdnRestSingleResponse<>(null, ErrorCategory.UNSPECIFIED.getMessage(), false, null, requestId);
    }
  }

  @RequestMapping(value = SummaryFilterApiPath.BUSINESS_PARTNER_FILTER, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Business partners list by time filter, status filter, activated and viewable flag", description = "Business partners list by time filter, status filter, activated and viewable flag")
  @ResponseBody
  public GdnRestListResponse<ProductBusinessPartnerMapperResponse> getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestBody SummaryFilterRequest request,
      @RequestParam boolean activated, @RequestParam boolean viewable, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "30") int size) {
    log.info("API to get business partners list  by timeFilter : {}, statusFilter : {}, requestId : {}",
        request.getTimeFilter(), request.getStatusFilter(), requestId);
    try {
      SummaryFilterServiceRequest summaryFilterServiceRequest = ControllerUtils.getSummaryFilterServiceRequest(request);
      Page<ProductBusinessPartnerMapperResponse> response = summaryFilterService
          .getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(storeId, summaryFilterServiceRequest,
              activated, viewable, page, size);
      return new GdnRestListResponse<>(response.getContent(), new PageMetaData(page, size, response.getTotalElements()),
          requestId);
    } catch (SolrServerException | IOException e) {
      log.error(
          "Exception caught while getting business partners list by time and status filters, RequestId: {}, timeFilter:{}, statusFilter:{}",
          requestId, request.getTimeFilter(), request.getStatusFilter(), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.DATA_NOT_FOUND.getMessage(), false, null, null,
          requestId);
    } catch (Exception e) {
      log.error(
          "Exception caught while getting business partners list by time and status filters, RequestId: {}, timeFilter:{}, statusFilter:{}",
          requestId, request.getTimeFilter(), request.getStatusFilter(), e);
      return new GdnRestListResponse<>(null, ErrorCategory.UNSPECIFIED.getMessage(), false, null, null, requestId);
    }
  }

  @RequestMapping(value = SummaryFilterApiPath.ASSIGNEE_FILTER, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Assignee list by time filter, status filter, activated and viewable flag", description = "Assignee"
      + " list by time filter, status filter, activated and viewable flag")
  @ResponseBody
  public GdnRestListResponse<AssigneeResponse> getAssigneeListByFilterRequestAndActivatedAndViewableFlag(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestBody SummaryFilterRequest request,
      @RequestParam boolean activated, @RequestParam boolean viewable) {
    log.info("API to get assignee's list by timeFilter : {}, statusFilter : {}, requestId : {}",
        request.getTimeFilter(), request.getStatusFilter(), requestId);
    try {
      SummaryFilterServiceRequest serviceRequest = ControllerUtils.getSummaryFilterServiceRequest(request);
      List<AssigneeResponse> response = summaryFilterService
          .getAssigneeListByFilterRequestAndActivatedAndViewableFlag(storeId, serviceRequest, activated, viewable);
      return new GdnRestListResponse<>(response, new PageMetaData(), requestId);
    } catch (SolrServerException | IOException e) {
      log.error(
          "Exception caught while getting assignee list by time and status filters, RequestId: {}, " + "timeFilter:{}, "
              + "statusFilter:{}", requestId, request.getTimeFilter(), request.getStatusFilter(), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.DATA_NOT_FOUND.getMessage(), false, null, null, requestId);
    } catch (Exception e) {
      log.error(
          "Exception caught while getting assignee list by time and status filters, RequestId: {}, " + "timeFilter:{}, "
              + "statusFilter:{}", requestId, request.getTimeFilter(), request.getStatusFilter(), e);
      return new GdnRestListResponse<>(null, ErrorCategory.UNSPECIFIED.getMessage(), false, null, null, requestId);
    }
  }

  @RequestMapping(value = SummaryFilterApiPath.PRODUCT_EDIT_HISTORY, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get variant history summary", description = "Get variant history summary")
  @ResponseBody
  public GdnRestListResponse<HistoryResponse> getProductHistorySummary(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam int page, @RequestParam int size,
      @RequestBody HistoryRequest historyRequest) {
    log.info("API to get Variant edit history : request : {} , requestId : {} ", historyRequest, requestId);
    try {
      Page<HistoryResponse> variantHistoryResponses = summaryFilterService
          .getProductHistoryByProductSkuAndKeyword(storeId, historyRequest, page, size);
      return new GdnRestListResponse<>(variantHistoryResponses.getContent(),
          new PageMetaData(size, page, variantHistoryResponses.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Exception caught while getting history summary, RequestId: {},  HistoryRequest: {} ", requestId,
          historyRequest, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getMessage(), false, null, null, requestId);
    }
  }

  @RequestMapping(value = SummaryFilterApiPath.PRODUCT_UPDATE_HISTORY, method = RequestMethod.POST,
  produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get Product update history", description = "Get Product update history")
  public GdnRestListResponse<HistoryUpdateResponse> getProductUpdateHistory(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username, @RequestParam int page, @RequestParam int size,
    @RequestBody HistoryUpdateRequest historyUpdateRequest) {
    log.info("Fetching update-history for request : {}, page : {}, size : {}", historyUpdateRequest,
      page, size);
    try {
      Page<HistoryUpdateResponse> historyUpdateResponses =
        this.summaryFilterService.getProductUpdateHistoryByRequest(storeId, requestId,
          historyUpdateRequest, page, size);
      return new GdnRestListResponse<>(null, null, true, historyUpdateResponses.getContent(),
        new PageMetaData(size, page, historyUpdateResponses.getTotalElements()), requestId);
    } catch (Exception e) {
      log.error("Error on update-history for request : {}, page : {}, size : {}, error - ",
        historyUpdateRequest, page, size, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
        null, null, requestId);
    }
  }

}
