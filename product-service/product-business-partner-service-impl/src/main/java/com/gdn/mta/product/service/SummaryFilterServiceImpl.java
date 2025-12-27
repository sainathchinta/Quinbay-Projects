package com.gdn.mta.product.service;


import java.io.IOException;
import java.time.temporal.ChronoUnit;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.HistoryUpdateRequest;
import com.gda.mta.product.dto.response.HistoryUpdateResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.FacetField;
import org.apache.solr.client.solrj.response.IntervalFacet;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrException;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.HistoryRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.response.AssigneeResponse;
import com.gda.mta.product.dto.response.FilterCountResponse;
import com.gda.mta.product.dto.response.HistoryResponse;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gdn.mta.product.enums.StatusFilterType;
import com.gdn.mta.product.enums.TimeFilterType;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.SolrHistoryCollectionRepository;
import com.gdn.mta.product.repository.SolrReviewProductCollectionRepository;
import com.gdn.mta.product.util.SummaryFilterUtil;
import com.gdn.mta.product.valueobject.HistorySolr;
import com.gdn.mta.product.valueobject.SummaryFilterServiceRequest;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.util.CommonUtils;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;
import com.gdn.partners.pbp.model.productlevel3.ProductCollectionCountResponse;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class SummaryFilterServiceImpl implements SummaryFilterService {

  private static final String REGEX_META_CHARACTERS = "[`~!@#$%^&*()_+\\\\[\\\\]\\\\\\\\;\\',/{}|:\\\"<>?]";

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private SolrReviewProductCollectionRepository solrReviewProductCollectionRepository;

  @Autowired
  private SolrHistoryCollectionRepository solrHistoryCollectionRepository;

  @Autowired
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Override
  public FilterCountResponse getFilterCountsByStoreIdAndActivatedAndViewable(String storeId, boolean activated,
      boolean viewable) throws Exception {
    FilterCountResponse filterCountResponse = null;
    QueryResponse queryResponse = solrReviewProductCollectionRepository
        .getFilterCountByStoreIdAndActivatedAndViewable(storeId, activated, viewable);
    filterCountResponse = convertQueryResponseToFilterCountsResponse(queryResponse);
    return filterCountResponse;
  }

  @Override
  public Page<ReviewProductResponse> getReviewProductsByFilterRequestAndActivatedAndViewableFlag(String storeId,
      SummaryFilterRequest request, boolean activated, boolean viewable, int page, int size) throws Exception {
    SummaryFilterServiceRequest filterServiceRequest = getSummaryFilterServiceRequest(request);
    List<ReviewProductResponse> responseList;
    Page<ReviewProductResponse> responsePage = null;
    Pageable pageable = PageRequest.of(page, size);
    SolrDocumentList solrDocuments = solrReviewProductCollectionRepository
        .getReviewProductsByFilterRequestAndActivatedAndViewable(storeId, filterServiceRequest, activated, viewable,
            page, size);
    responseList = SummaryFilterUtil.toReviewProductResponseList(solrDocuments);
    responsePage = new PageImpl<>(responseList, pageable, solrDocuments.getNumFound());
    return responsePage;
  }

  @Override
  public Page<ProductBusinessPartnerMapperResponse> getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(
      String storeId, SummaryFilterServiceRequest request, boolean activated, boolean viewable, int page, int size)
      throws Exception {
    Page<ProductBusinessPartnerMapperResponse> businessPartnerResponsePage = null;
    Pageable pageable = PageRequest.of(page, size);
    SolrDocumentList solrDocuments = solrReviewProductCollectionRepository
        .getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(storeId, request, activated, viewable, page,
            size);
    List<ProductBusinessPartnerMapperResponse> businessPartnerResponses =
        SummaryFilterUtil.convertQueryResponseToProductBusinessPartnerMapperResponseList(solrDocuments);
    businessPartnerResponsePage = new PageImpl<>(businessPartnerResponses, pageable, solrDocuments.getNumFound());
    return businessPartnerResponsePage;
  }

  @Override
  public List<AssigneeResponse> getAssigneeListByFilterRequestAndActivatedAndViewableFlag(String storeId,
      SummaryFilterServiceRequest request, boolean activated, boolean viewable) throws Exception {
    List<AssigneeResponse> assigneeResponses = null;
    SolrDocumentList solrDocuments = solrReviewProductCollectionRepository
        .getAssigneeListByFilterRequestAndActivatedAndViewableFlag(storeId, request, activated, viewable);
    assigneeResponses = SummaryFilterUtil.toAssigneeResponseList(solrDocuments);
    return assigneeResponses;
  }

  @Override
  public Page<HistoryResponse> getProductHistoryByProductSkuAndKeyword(String storeId, HistoryRequest historyRequest,
      int page, int size) {
    if (!historyRequest.isBeforeThreeMonths()) {
      return getProductHistoryByProductSkuAndKeywordFromSolr(storeId, historyRequest, page, size);
    } else {
      return getProductHistoryByProductSkuAndKeywordFromDB(historyRequest, page, size);
    }
  }

  @Override
  public Page<HistoryUpdateResponse> getProductUpdateHistoryByRequest(String storeId,
    String requestId, HistoryUpdateRequest historyUpdateRequest, int page, int size) {
    GdnPreconditions.checkArgument(
      StringUtils.isNotEmpty(historyUpdateRequest.getProductSku()) || StringUtils.isNotEmpty(
        historyUpdateRequest.getPickupPointCode()),
      ErrorMessages.BOTH_PRODUCT_SKU_AND_PICKUP_POINT_CODE_NOT_EMPTY);
    historyUpdateRequest.setProductSku(Optional.ofNullable(historyUpdateRequest.getProductSku())
      .map(sku -> sku.replaceAll(REGEX_META_CHARACTERS, StringUtils.EMPTY))
      .filter(StringUtils::isNotEmpty).orElseThrow(
        () -> new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT,
          ErrorCategory.INVALID_FORMAT.getCode())));
    log.info("ProductSku in getProductUpdateHistoryByRequest is {}",
      historyUpdateRequest.getProductSku());
    Page<HistoryUpdateResponse> historyUpdateResponses = new PageImpl<>(Collections.emptyList());
    if (historyUpdateRequest.isBeforeOneMonths()) {
      historyUpdateResponses = getProductUpdateHistoryFromDB(historyUpdateRequest, page, size);
    } else {
      historyUpdateResponses =
        getProductUpdateHistoryFromSolr(storeId, historyUpdateRequest, page, size);
    }
    if (CollectionUtils.isEmpty(historyUpdateResponses.getContent())) {
      return historyUpdateResponses;
    }
    List<BusinessPartnerPickupPointResponse> pickupPointDetails =
      xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(
        historyUpdateResponses.getContent().stream().map(HistoryUpdateResponse::getPickupPointCode)
          .collect(Collectors.toList()));
    return ConverterUtil.setPickupPointName(historyUpdateResponses,
      pickupPointDetails.stream().collect(Collectors.toMap(BusinessPartnerPickupPointResponse::getCode,
        Function.identity())));
  }

  private Page<HistoryUpdateResponse> getProductUpdateHistoryFromDB(
    HistoryUpdateRequest historyUpdateRequest, int page, int size) {
    CommonUtils.validateHistoryDateFilterLastNMonths(historyUpdateRequest.getStartDate(),
      Constants.TWELVE_MONTH);
    CommonUtils.validateHistoryDateFilterLastNMonths(historyUpdateRequest.getEndDate(), Constants.TWELVE_MONTH);
    CommonUtils.validateStartDateIsBeforeEndDate(historyUpdateRequest.getStartDate(), historyUpdateRequest.getEndDate());
    CommonUtils.validateHistoryDateFilterAfterLastNMonths(historyUpdateRequest.getStartDate(),
      Constants.ONE_MONTH);
    CommonUtils.validateHistoryDateFilterAfterLastNMonths(historyUpdateRequest.getEndDate(),
      Constants.ONE_MONTH);
    log.info("Fetching history from DB for request : {}", historyUpdateRequest);
    Calendar calendar = null;
    if (Objects.isNull(historyUpdateRequest.getStartDate())) {
      calendar = Calendar.getInstance();
      calendar.add(Calendar.MONTH, -Constants.TWELVE_MONTH);
      historyUpdateRequest.setStartDate(calendar.getTime());
    }
    if (Objects.isNull(historyUpdateRequest.getEndDate())) {
      calendar = Calendar.getInstance();
      calendar.add(Calendar.MONTH, -Constants.THREE_MONTH);
      historyUpdateRequest.setEndDate(calendar.getTime());
    } else {
      historyUpdateRequest
        .setEndDate(Date.from(historyUpdateRequest.getEndDate().toInstant().plus(Constants.ONE_DAY, ChronoUnit.DAYS)));
    }
    return updatedProductHistoryService.findProductUpdateHistoryByProductSkuAndKeyword(
      historyUpdateRequest, page, size);
  }

  private Page<HistoryUpdateResponse> getProductUpdateHistoryFromSolr(String storeId,
    HistoryUpdateRequest historyUpdateRequest, int page, int size) {
    CommonUtils.validateHistoryDateFilterLastNDays(historyUpdateRequest.getStartDate(),
      Constants.DAYS_IN_MONTH);
    CommonUtils.validateHistoryDateFilterLastNMonths(historyUpdateRequest.getEndDate(),
      Constants.TWELVE_MONTH);
    CommonUtils.validateStartDateIsBeforeEndDate(historyUpdateRequest.getStartDate(),
      historyUpdateRequest.getEndDate());
    SolrDocumentList solrDocuments = new SolrDocumentList();
    try {
      solrDocuments = solrHistoryCollectionRepository.findProductUpdateHistoryByRequest(storeId,
        historyUpdateRequest, page, size);
    } catch (SolrServerException | IOException e) {
      log.error("Exception while fetching update history from solr : {} error - ",
        historyUpdateRequest, e);
    }
    List<HistorySolr> historySolrList = SummaryFilterUtil.convertToHistorySolrList(solrDocuments);
    return updatedProductHistoryService.getProductUpdateHistoryByAuditTrailId(
      historySolrList.stream().map(HistorySolr::getId).collect(Collectors.toList()), page,
      size, solrDocuments.getNumFound());
  }

  private Page<HistoryResponse> getProductHistoryByProductSkuAndKeywordFromSolr(String storeId,
      HistoryRequest historyRequest, int page, int size) {
    CommonUtils.validateHistoryDateFilterLastNMonths(historyRequest.getStartDate(), Constants.ONE_MONTH);
    CommonUtils.validateHistoryDateFilterLastNMonths(historyRequest.getEndDate(), Constants.ONE_MONTH);
    CommonUtils.validateStartDateIsBeforeEndDate(historyRequest.getStartDate(), historyRequest.getEndDate());
    SolrDocumentList solrDocuments = new SolrDocumentList();
    try {
      solrDocuments =
          solrHistoryCollectionRepository.findProductHistoryByProductSkuAndKeyword(storeId, historyRequest, page, size);
    } catch (IOException | SolrServerException | SolrException e) {
      log.error("Exception while fetching variant history from solr : {} ", e);
    }
    List<HistorySolr> historySolrList = SummaryFilterUtil.convertToHistorySolrList(solrDocuments);
    return updatedProductHistoryService.getProductEditHistoryByAuditTrailId(
        historySolrList.stream().map(HistorySolr::getId).collect(Collectors.toList()), page,
        size, solrDocuments.getNumFound());
  }

  private Page<HistoryResponse> getProductHistoryByProductSkuAndKeywordFromDB(HistoryRequest historyRequest, int page,
      int size) {
    CommonUtils.validateHistoryDateFilterLastNMonths(historyRequest.getStartDate(), Constants.TWELVE_MONTH);
    CommonUtils.validateHistoryDateFilterLastNMonths(historyRequest.getEndDate(), Constants.TWELVE_MONTH);
    CommonUtils.validateStartDateIsBeforeEndDate(historyRequest.getStartDate(), historyRequest.getEndDate());
    CommonUtils.validateHistoryDateFilterAfterLastNMonths(historyRequest.getStartDate(), Constants.ONE_MONTH);
    CommonUtils.validateHistoryDateFilterAfterLastNMonths(historyRequest.getEndDate(),Constants.ONE_MONTH);
    log.info("Fetching history from DB for request : {}", historyRequest);
    Calendar calendar = null;
    if (Objects.isNull(historyRequest.getStartDate())) {
      calendar = Calendar.getInstance();
      calendar.add(Calendar.MONTH, -Constants.TWELVE_MONTH);
      historyRequest.setStartDate(calendar.getTime());
    }
    if (Objects.isNull(historyRequest.getEndDate())) {
      calendar = Calendar.getInstance();
      calendar.add(Calendar.MONTH, -Constants.ONE_MONTH);
      historyRequest.setEndDate(calendar.getTime());
    } else {
      historyRequest
          .setEndDate(Date.from(historyRequest.getEndDate().toInstant().plus(Constants.ONE_DAY, ChronoUnit.DAYS)));
    }
    return updatedProductHistoryService.findProductHistoryByProductSkuAndKeyword(historyRequest, page, size);
  }

  private SummaryFilterServiceRequest getSummaryFilterServiceRequest(SummaryFilterRequest request) {
    SummaryFilterServiceRequest filterServiceRequest = new SummaryFilterServiceRequest();
    BeanUtils.copyProperties(request, filterServiceRequest, "statusFilter", "timeFilter");
    filterServiceRequest.setStatusFilter(StatusFilterType.getStatusFilterTypeByValue(request.getStatusFilter()));
    filterServiceRequest.setTimeFilter(TimeFilterType.getTimeFilterTypeByValue(request.getTimeFilter()));
    return filterServiceRequest;
  }

  private FilterCountResponse convertQueryResponseToFilterCountsResponse(QueryResponse queryResponse) {
    int numFound = Math.toIntExact(queryResponse.getResults().getNumFound());
    FilterCountResponse filterCountResponse = new FilterCountResponse();
    List<IntervalFacet> intervalFacets = queryResponse.getIntervalFacets();
    Map<String, Integer> facetCountMap = intervalFacets.get(0).getIntervals().stream()
        .collect(Collectors.toMap(IntervalFacet.Count::getKey, IntervalFacet.Count::getCount));
    filterCountResponse.setToday(facetCountMap.get(SolrConstants.TODAY_FACET_INTERVAL));
    filterCountResponse.setYesterday(facetCountMap.get(SolrConstants.YESTERDAY_FACET_INTERVAL));
    filterCountResponse.setTwoDaysAgo(facetCountMap.get(SolrConstants.TWO_DAYS_AGO_FACET_INTERVAL));
    filterCountResponse.setThreeToFiveDays(facetCountMap.get(SolrConstants.THREE_TO_FIVE_DAYS_AGO_FACET_INTERVAL));
    filterCountResponse.setMoreThanFiveDaysAgo(facetCountMap.get(SolrConstants.FIVE_DAYS_AGO_FACET_INTERVAL));
    Map<String, Long> assignedFacetMap = queryResponse.getFacetField(SolrFieldNames.ASSIGNED_TO).getValues().stream()
        .collect(Collectors.toMap(FacetField.Count::getName, FacetField.Count::getCount));
    if (assignedFacetMap.containsKey(SolrConstants.ASSIGNED_TO_PREFIX)) {
      filterCountResponse.setUnassigned(assignedFacetMap.get(SolrConstants.ASSIGNED_TO_PREFIX).intValue());
      filterCountResponse.setAssigned(numFound - assignedFacetMap.get(SolrConstants.ASSIGNED_TO_PREFIX).intValue());
    }
    Map<String, Long> resubmittedFacetMap = queryResponse.getFacetField(SolrFieldNames.RESUBMIT_COUNT).getValues().stream()
        .collect(Collectors.toMap(FacetField.Count::getName, FacetField.Count::getCount));
    if (resubmittedFacetMap.containsKey(SolrConstants.RESUBMITTED_COUNT_ZERO)) {
      filterCountResponse
          .setRevised(numFound - resubmittedFacetMap.get(SolrConstants.RESUBMITTED_COUNT_ZERO).intValue());
    }
    Map<String, Long> brandApprovedMap = queryResponse.getFacetField(SolrFieldNames.BRAND_APPROVED).getValues().stream()
        .collect(Collectors.toMap(FacetField.Count::getName, FacetField.Count::getCount));
    if (brandApprovedMap.containsKey(String.valueOf(Boolean.TRUE))) {
      filterCountResponse.setBrandApproved(brandApprovedMap.get(String.valueOf(Boolean.TRUE)).intValue());
      filterCountResponse.setBrandNotApproved(numFound - brandApprovedMap.get(String.valueOf(Boolean.TRUE)).intValue());
    }
    return filterCountResponse;
  }
}
