package com.gdn.mta.product.repository;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.EnumUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.util.ClientUtils;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.params.CommonParams;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;
import org.springframework.util.CollectionUtils;

import com.gda.mta.product.dto.DalamProductListRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductBusinessPartnerMapper;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.enums.StatusFilterType;
import com.gdn.mta.product.enums.TimeFilterType;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import com.gdn.mta.product.valueobject.SummaryFilterServiceRequest;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;
import lombok.extern.slf4j.Slf4j;

@Repository
@Slf4j
public class SolrReviewProductCollectionRepositoryBean implements SolrReviewProductCollectionRepository {

  private static final String REGEX_FOR_SPACE = "\\s+";
  private static final String REGEX_FOR_SPECIAL_CHARACTERS = "[^A-Za-z0-9]";
  private static final Pattern PATTERN_FOR_SPECIAL_CHARACTERS = Pattern.compile(REGEX_FOR_SPECIAL_CHARACTERS);
  private static final String DRAFT_STATE = "DRAFT";
  private static final String NOT_A_VALID_TYPE = "Time status filter is not valid";

  @Value("${review.product.collection.solr.event.enabled}")
  private boolean reviewProductCollectionSolrEventEnabled;

  @Autowired
  @Qualifier(value = "reviewProductCollectionClient")
  private CloudSolrClient reviewProductCollectionClient;

  @Override
  public QueryResponse getFilterCountByStoreIdAndActivatedAndViewable(String storeId, boolean activated,
      boolean viewable) throws IOException, SolrServerException {
    SolrQuery solrQuery = getSolrQueryForFilterCounts(storeId, activated, viewable);
    return reviewProductCollectionClient.query(solrQuery);
  }

  @Override
  public QueryResponse getInReviewProducts(String storeId, String keyword, String businessPartnerCode,
      String categoryCode) throws IOException, SolrServerException {
    SolrQuery solrQuery = getSolrQueryForReviewProducts(storeId, keyword, businessPartnerCode, categoryCode);
    return reviewProductCollectionClient.query(solrQuery);
  }

  @Override
  public SolrDocumentList getReviewProductsByFilterRequestAndActivatedAndViewable(String storeId,
      SummaryFilterServiceRequest request, boolean activated, boolean viewable, int page, int size)
      throws IOException, SolrServerException {
    SolrQuery solrQuery = getSolrQueryForReviewProductsList(request, activated, viewable, page, size);
    QueryResponse queryResponse = reviewProductCollectionClient.query(solrQuery);
    return queryResponse.getResults();
  }

  @Override
  public SolrDocumentList getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(String storeId,
      SummaryFilterServiceRequest request, boolean activated, boolean viewable, int page, int size)
      throws IOException, SolrServerException {
    SolrQuery solrQuery = getSolrQueryForBusinessPartnersFilter(storeId, request, activated, viewable, page, size);
    QueryResponse queryResponse = reviewProductCollectionClient.query(solrQuery);
    return queryResponse.getResults();
  }

  @Override
  public SolrDocumentList getAssigneeListByFilterRequestAndActivatedAndViewableFlag(String storeId,
      SummaryFilterServiceRequest request, boolean activated, boolean viewable)
      throws IOException, SolrServerException {
    SolrQuery solrQuery = getSolrQueryForAssigneeList(storeId, request, activated, viewable);
    QueryResponse queryResponse = reviewProductCollectionClient.query(solrQuery);
    return queryResponse.getResults();
  }

  @Override
  public void updateAssignedToInSolrCollection(String id, String assignedTo)
      throws CloudSolrClient.RouteException, IOException, SolrServerException {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(SolrFieldNames.ID, id);
    solrInputDocument.setField(SolrFieldNames.ASSIGNED_TO,
        Collections.singletonMap(SolrConstants.SET, assignedTo));
    this.reviewProductCollectionClient.add(solrInputDocument);
  }

  @Override
  public void updateProductToSolrCollection(SolrInputDocument solrInputDocument) throws Exception {
    this.reviewProductCollectionClient.add(solrInputDocument);
  }

  @Override
  public void deleteAllScreeningDocumentsFromSolr() throws IOException, SolrServerException {
    this.reviewProductCollectionClient.deleteByQuery(
        SolrFieldNames.STATE + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTES + WorkflowStates.DRAFT.getValue()
            + SolrConstants.DOUBLE_QUOTES);
  }

  @Override
  public void deleteAllInProgressDocumentsFromSolr() throws IOException, SolrServerException {
    this.reviewProductCollectionClient.deleteByQuery(
        SolrConstants.NOT + SolrFieldNames.STATE + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTES
            + WorkflowStates.DRAFT.getValue() + SolrConstants.DOUBLE_QUOTES);
  }

  private SolrQuery getSolrQueryForBusinessPartnersFilter(String storeId, SummaryFilterServiceRequest request,
      boolean activated, boolean viewable, int page, int size) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.addFilterQuery(SolrFieldNames.STORE_ID + SolrConstants.COLON + storeId);
    solrQuery.addFilterQuery(SolrFieldNames.ACTIVATED + SolrConstants.COLON + activated);
    solrQuery.addFilterQuery(SolrFieldNames.VIEWABLE + SolrConstants.COLON + viewable);
    solrQuery.addFilterQuery(SolrConstants.COLLAPSE_FQ_FOR_BP_CODE);
    solrQuery.setSort(SolrFieldNames.COPY_BUSINESS_PARTNER_NAME, SolrQuery.ORDER.asc);
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    StringBuilder query = new StringBuilder();
    setTimeAndStatusFilters(request, solrQuery);
    if (StringUtils.isNotEmpty(request.getSearchKeyword())) {
      String keyword = getSearchKeyword(request.getSearchKeyword());
      query.append(SolrFieldNames.BUSINESS_PARTNER_NAME).append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
          .append(keyword).append(SolrConstants.LIKE_QUERY).append(SolrConstants.CLOSE_BRACKET);
    } else {
      query.append(SolrConstants.DEFAULT_QUERY);
    }
    solrQuery.setQuery(query.toString());
    solrQuery.setStart(page * size);
    solrQuery.setRows(size);
    solrQuery.setFields(Arrays.asList(SolrFieldNames.BUSINESS_PARTNER_CODE, SolrFieldNames.BUSINESS_PARTNER_NAME)
            .toArray(String[]::new));
    return solrQuery;
  }

  private SolrQuery getSolrQueryForAssigneeList(String storeId, SummaryFilterServiceRequest request, boolean activated,
      boolean viewable) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrConstants.DEFAULT_QUERY);
    solrQuery.addFilterQuery(SolrFieldNames.ACTIVATED + SolrConstants.COLON + activated);
    solrQuery.addFilterQuery(SolrFieldNames.VIEWABLE + SolrConstants.COLON + viewable);
    solrQuery.addFilterQuery(SolrFieldNames.STORE_ID + SolrConstants.COLON + storeId);
    solrQuery.addFilterQuery(SolrConstants.COLLAPSE_FQ_FOR_ASSIGNED_TO);
    solrQuery.setSort(SolrFieldNames.ASSIGNED_TO, SolrQuery.ORDER.asc);
    solrQuery.setStart(0);
    solrQuery.setRows(Integer.MAX_VALUE);
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    solrQuery.setFields(SolrFieldNames.ASSIGNED_TO);
    setTimeAndStatusFilters(request, solrQuery);
    return solrQuery;
  }

  private void setTimeAndStatusFilters(SummaryFilterServiceRequest request, SolrQuery solrQuery) {
    if (!StatusFilterType.ALL.equals(request.getStatusFilter()) && EnumUtils
        .isValidEnum(StatusFilterType.class, request.getStatusFilter().name())) {
      solrQuery.addFilterQuery(getFilterQueryForStatusFilter(request.getStatusFilter()));
    }
    String timeFilterQuery = getTimeFilterQuery(request.getTimeFilter());
    if (!TimeFilterType.ALL.equals(request.getTimeFilter()) && EnumUtils
        .isValidEnum(TimeFilterType.class, request.getTimeFilter().name())) {
      solrQuery.addFilterQuery(SolrFieldNames.SUBMITTED_DATE + SolrConstants.COLON + timeFilterQuery);
    }
  }

  private String getSearchKeywordWithoutSpecialCharacters(StringBuilder searchKeyword) {
    Matcher matcher = PATTERN_FOR_SPECIAL_CHARACTERS.matcher(searchKeyword.toString());
    StringBuilder keyword = new StringBuilder(getSearchKeyword(matcher.replaceAll(StringUtils.SPACE)));
    return keyword.append(SolrConstants.LIKE_QUERY).toString();
  }

  private String getSearchKeyword(String searchKeyword) {
    return String
        .join(SolrConstants.AND_WITH_FUZZY_SEARCH, StringUtils.trimToEmpty(searchKeyword).split(REGEX_FOR_SPACE));
  }

  private SolrQuery getSolrQueryForReviewProductsList(SummaryFilterServiceRequest request, boolean activated,
      boolean viewable, int page, int size) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.addFilterQuery(SolrFieldNames.ACTIVATED + SolrConstants.COLON + activated);
    solrQuery.addFilterQuery(SolrFieldNames.VIEWABLE + SolrConstants.COLON + viewable);
    solrQuery.addFilterQuery(SolrFieldNames.STORE_ID + SolrConstants.COLON + Constants.DEFAULT_STORE_ID);
    solrQuery.setStart(page * size);
    solrQuery.setRows(size);
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    if (SolrConstants.ASC.equals(request.getSortOrder())) {
      solrQuery.setSort(request.getSortColumn(), SolrQuery.ORDER.asc);
    } else {
      solrQuery.setSort(request.getSortColumn(), SolrQuery.ORDER.desc);
    }
    StringBuilder query = getSearchQueryBySearchKeyword(request.getSearchKeyword());
    solrQuery.setQuery(SolrConstants.OPEN_BRACKET + query.toString() + SolrConstants.CLOSE_BRACKET);
    if (!SolrConstants.ALL.equals(request.getAssignedTo())) {
      solrQuery.addFilterQuery(SolrFieldNames.ASSIGNED_TO + SolrConstants.COLON + request.getAssignedTo());
    }
    if (!SolrConstants.ALL.equals(request.getBusinessPartnerCode())) {
      solrQuery.addFilterQuery(
          SolrFieldNames.BUSINESS_PARTNER_CODE + SolrConstants.COLON + request.getBusinessPartnerCode());
    }
    if (!SolrConstants.ALL.equals(request.getCategoryCode())) {
      solrQuery.addFilterQuery(SolrFieldNames.CATEGORY_CODES + SolrConstants.COLON + request.getCategoryCode());
    }
    solrQuery.setQuery(
        solrQuery.getQuery() + StringUtils.SPACE + SolrConstants.AND + StringUtils.SPACE + SolrFieldNames.STATE
            + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTES + DRAFT_STATE + SolrConstants.DOUBLE_QUOTES);
    setTimeAndStatusFilters(request, solrQuery);
    return solrQuery;
  }

  private StringBuilder getSearchQueryBySearchKeyword(String searchKeyword) {
    String keyword = getSearchKeyword(StringUtils.trimToEmpty(searchKeyword));
    String keywordWithoutSpecialChars = getSearchKeywordWithoutSpecialCharacters(
        new StringBuilder(StringUtils.trimToEmpty(searchKeyword)));
    StringBuilder query = new StringBuilder();
    if (StringUtils.isNotEmpty(searchKeyword)) {
      getKeywordQuery(keyword, searchKeyword, keywordWithoutSpecialChars, query);
    } else {
      query.append(SolrConstants.DEFAULT_QUERY);
    }
    return query;
  }

  private void getKeywordQuery(String keyword, String searchKeyword, String keywordWithoutSpecialChars,
      StringBuilder query) {
    query.append(SolrConstants.OPEN_BRACKET).append(SolrFieldNames.PRODUCT_NAME).append(SolrConstants.COLON)
        .append(SolrConstants.OPEN_BRACKET).append(keywordWithoutSpecialChars).append(SolrConstants.CLOSE_BRACKET)
        .append(StringUtils.SPACE).append(SolrConstants.OR).append(StringUtils.SPACE)
        .append(SolrFieldNames.PRODUCT_CODE).append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
        .append(SolrConstants.DOUBLE_QUOTES).append(StringUtils.trimToEmpty(keyword))
        .append(SolrConstants.DOUBLE_QUOTES).append(SolrConstants.CLOSE_BRACKET).append(StringUtils.SPACE)
        .append(SolrConstants.OR).append(StringUtils.SPACE).append(SolrFieldNames.CREATED_BY)
        .append(SolrConstants.COLON).append(SolrConstants.OPEN_BRACKET)
        .append(ClientUtils.escapeQueryChars(searchKeyword)).append(SolrConstants.LIKE_QUERY)
        .append(SolrConstants.CLOSE_BRACKET).append(SolrConstants.CLOSE_BRACKET);
  }

  private String getFilterQueryForStatusFilter(StatusFilterType statusFilter) {
    if (StatusFilterType.ASSIGNED.equals(statusFilter)) {
      return SolrConstants.NOT + SolrFieldNames.ASSIGNED_TO + SolrConstants.COLON + SolrConstants.ASSIGNED_TO_PREFIX;
    } else if (StatusFilterType.UNASSIGNED.equals(statusFilter)) {
      return SolrFieldNames.ASSIGNED_TO + SolrConstants.COLON + SolrConstants.ASSIGNED_TO_PREFIX;
    } else if (StatusFilterType.REVISED.equals(statusFilter)) {
      return SolrConstants.NOT + SolrFieldNames.RESUBMIT_COUNT + SolrConstants.COLON
          + SolrConstants.RESUBMITTED_COUNT_ZERO;
    } else if (StatusFilterType.BRAND_APPROVED.equals(statusFilter)) {
      return SolrFieldNames.BRAND_APPROVED + SolrConstants.COLON + Boolean.TRUE;
    } else if (StatusFilterType.BRAND_NOT_APPROVED.equals(statusFilter)){
      return SolrFieldNames.BRAND_APPROVED + SolrConstants.COLON + Boolean.FALSE;
    }
    return SolrConstants.ALL;
  }

  private String getTimeFilterQuery(TimeFilterType timeFilterType) {
    if (TimeFilterType.TODAY.equals(timeFilterType)) {
      return SolrConstants.TODAY_FACET_INTERVAL.replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO);
    } else if (TimeFilterType.YESTERDAY.equals(timeFilterType)) {
      return SolrConstants.YESTERDAY_FACET_INTERVAL.replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO);
    } else if (TimeFilterType.TWO_DAYS_AGO.equals(timeFilterType)) {
      return SolrConstants.TWO_DAYS_AGO_FACET_INTERVAL
          .replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO);
    } else if (TimeFilterType.THREE_TO_FIVE_DAYS_AGO.equals(timeFilterType)) {
      return SolrConstants.THREE_TO_FIVE_DAYS_AGO_FACET_INTERVAL
          .replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO);
    } else if (TimeFilterType.FIVE_DAYS_AGO.equals(timeFilterType)) {
      return SolrConstants.FIVE_DAYS_AGO_FACET_INTERVAL
          .replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO);
    } else {
      return SolrConstants.ALL;
    }
  }

  private SolrQuery getSolrQueryForFilterCounts(String storeId, boolean activated, boolean viewable) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrConstants.DEFAULT_QUERY);
    solrQuery.addFilterQuery(SolrFieldNames.ACTIVATED + SolrConstants.COLON + activated);
    solrQuery.addFilterQuery(SolrFieldNames.VIEWABLE + SolrConstants.COLON + viewable);
    solrQuery.addFilterQuery(SolrFieldNames.STORE_ID + SolrConstants.COLON + storeId);
    solrQuery.setQuery(
        solrQuery.getQuery() + StringUtils.SPACE + SolrConstants.AND + StringUtils.SPACE + SolrFieldNames.STATE
            + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTES + DRAFT_STATE + SolrConstants.DOUBLE_QUOTES);
    solrQuery.setStart(0);
    solrQuery.setRows(1);
    solrQuery.setFacet(Boolean.TRUE);
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    String[] facetFields = Arrays.asList(SolrFieldNames.ASSIGNED_TO, SolrFieldNames.RESUBMIT_COUNT,
        SolrFieldNames.BRAND_APPROVED).toArray(String[]::new);
    solrQuery.addFacetField(facetFields);
    solrQuery.addIntervalFacets(SolrFieldNames.SUBMITTED_DATE, Arrays
        .asList(SolrConstants.TODAY_FACET_INTERVAL, SolrConstants.YESTERDAY_FACET_INTERVAL,
            SolrConstants.TWO_DAYS_AGO_FACET_INTERVAL, SolrConstants.THREE_TO_FIVE_DAYS_AGO_FACET_INTERVAL,
            SolrConstants.FIVE_DAYS_AGO_FACET_INTERVAL).toArray(String[]::new));
    solrQuery.setFacetPrefix(SolrFieldNames.ASSIGNED_TO, SolrConstants.ASSIGNED_TO_PREFIX);
    return solrQuery;
  }

  private SolrQuery getSolrQueryForReviewProducts(String storeId, String searchKeyword, String businessPartnerCode,
      String categoryCode) {
    SolrQuery solrQuery = new SolrQuery();
    StringBuilder queryString = new StringBuilder();
    queryString.append(SolrConstants.NOT).append(SolrFieldNames.STATE).append(SolrConstants.COLON).append(DRAFT_STATE);
    addKeywordIfNotEmpty(searchKeyword, queryString);
    solrQuery.setQuery(queryString.toString());
    solrQuery.addFilterQuery(SolrFieldNames.STORE_ID + SolrConstants.COLON + storeId);
    addBusinessPartnerAndCategoryCodeToFilterQuery(businessPartnerCode, categoryCode, solrQuery);
    solrQuery.setStart(0);
    solrQuery.setRows(1);
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    solrQuery.addIntervalFacets(SolrFieldNames.SUBMITTED_DATE, Arrays
        .asList(SolrConstants.TODAY_FACET_INTERVAL, SolrConstants.YESTERDAY_FACET_INTERVAL,
            SolrConstants.TWO_DAYS_AGO_FACET_INTERVAL, SolrConstants.THREE_TO_FIVE_DAYS_AGO_FACET_INTERVAL,
            SolrConstants.FIVE_DAYS_AGO_FACET_INTERVAL).toArray(String[]::new));
    return solrQuery;
  }

  private void addBusinessPartnerAndCategoryCodeToFilterQuery(String businessPartnerCode, String categoryCode,
      SolrQuery solrQuery) {
    if (StringUtils.isNotEmpty(businessPartnerCode)) {
      if (SolrConstants.EXTERNAL.equalsIgnoreCase(businessPartnerCode)) {
        solrQuery.addFilterQuery(
            SolrConstants.NOT + SolrFieldNames.BUSINESS_PARTNER_CODE + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTES
                + SolrConstants.INTERNAL + SolrConstants.DOUBLE_QUOTES);
      } else {
        solrQuery.addFilterQuery(
            SolrFieldNames.BUSINESS_PARTNER_CODE + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTES
                + businessPartnerCode + SolrConstants.DOUBLE_QUOTES);
      }
    }
    if (StringUtils.isNotEmpty(categoryCode)) {
      solrQuery.addFilterQuery(
          SolrFieldNames.CATEGORY_CODES + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTES + categoryCode
              + SolrConstants.DOUBLE_QUOTES);
    }
  }

  private void addKeywordIfNotEmpty(String searchKeyword, StringBuilder queryString) {
    if (StringUtils.isNotEmpty(searchKeyword)) {
      queryString.append(StringUtils.SPACE).append(SolrConstants.AND).append(StringUtils.SPACE);
      String keyword = getSearchKeyword(StringUtils.trimToEmpty(searchKeyword));
      String keywordWithoutSpecialChars =
          getSearchKeywordWithoutSpecialCharacters(new StringBuilder(StringUtils.trimToEmpty(searchKeyword)));
      getKeywordQuery(keyword, searchKeyword, keywordWithoutSpecialChars, queryString);
    }
  }

  @Override
  public Page<SolrProductCollectionDTO> findDalamProductsList(DalamProductListRequest dalamProductListRequest,
      Pageable pageable) throws IOException, SolrServerException {
    List<SolrProductCollectionDTO> solrProductCollectionDTOList = new ArrayList<>();
    SolrQuery solrQuery = getSolrQueryForDalamProcessProductList(dalamProductListRequest);
    QueryResponse queryResponse = this.reviewProductCollectionClient.query(solrQuery);
    long totalRecords = queryResponse.getResults().getNumFound();
    queryResponse.getResults().forEach(solrDocument -> {
      SolrProductCollectionDTO solrProductCollectionDTO = getSolrProductCollectionDTO(solrDocument);
      solrProductCollectionDTOList.add(solrProductCollectionDTO);
    });
    return new PageImpl<>(solrProductCollectionDTOList, pageable, totalRecords);
  }

  private SolrQuery getSolrQueryForDalamProcessProductList(DalamProductListRequest dalamProductListRequest) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.STORE_ID + SolrConstants.COLON + Constants.DEFAULT_STORE_ID);
    solrQuery.setStart(dalamProductListRequest.getPage() * dalamProductListRequest.getSize());
    solrQuery.setRows(dalamProductListRequest.getSize());
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    if (StringUtils.isNotBlank(dalamProductListRequest.getKeyword())) {
      StringBuilder query = getSearchQueryBySearchKeyword(dalamProductListRequest.getKeyword());
      solrQuery.setQuery(
          solrQuery.getQuery() + StringUtils.SPACE + SolrConstants.AND + StringUtils.SPACE + SolrConstants.OPEN_BRACKET
              + query.toString() + SolrConstants.CLOSE_BRACKET);
    }
    if (StringUtils.isNotBlank(dalamProductListRequest.getBusinessPartnerCode())) {
      if (SolrConstants.EXTERNAL.equalsIgnoreCase(dalamProductListRequest.getBusinessPartnerCode())) {
        solrQuery.setQuery(
            solrQuery.getQuery() + StringUtils.SPACE + SolrConstants.AND + StringUtils.SPACE + SolrConstants.NOT
                + SolrFieldNames.BUSINESS_PARTNER_CODE + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTES
                + SolrConstants.INTERNAL + SolrConstants.DOUBLE_QUOTES);
      } else {
        solrQuery.setQuery(solrQuery.getQuery() + StringUtils.SPACE + SolrConstants.AND + StringUtils.SPACE
            + SolrFieldNames.BUSINESS_PARTNER_CODE + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTES
            + dalamProductListRequest.getBusinessPartnerCode() + SolrConstants.DOUBLE_QUOTES);
      }
    }
    if (StringUtils.isNotBlank(dalamProductListRequest.getCategoryCode())) {
      solrQuery.setQuery(solrQuery.getQuery() + StringUtils.SPACE + SolrConstants.AND + StringUtils.SPACE
          + SolrFieldNames.CATEGORY_CODES + SolrConstants.COLON + dalamProductListRequest.getCategoryCode());
    }
    solrQuery.setQuery(
        solrQuery.getQuery() + StringUtils.SPACE + SolrConstants.AND + StringUtils.SPACE + SolrConstants.NOT
            + SolrFieldNames.STATE + SolrConstants.COLON + DRAFT_STATE);
      solrQuery.setSort(SolrFieldNames.UPDATED_STEP_DATE, SolrQuery.ORDER.desc);
    setTimeAndStatusFiltersForDalamProcess(dalamProductListRequest, solrQuery);
    return solrQuery;
  }

  private void setTimeAndStatusFiltersForDalamProcess(DalamProductListRequest request, SolrQuery solrQuery) {
    if (StringUtils.isNotBlank(request.getTimeFilterType())) {
      setTimeAndStatusFiltersForDalamProcessUsingTimeFilterType(request, solrQuery);
    } else if (Objects.nonNull(request.getLessThanAge())) {
      solrQuery.addFilterQuery(SolrFieldNames.SUBMITTED_DATE + SolrConstants.COLON + SolrConstants.OPEN_SQUARE_BRACKET
          + SolrConstants.LIKE_QUERY + StringUtils.SPACE + SolrConstants.TO + StringUtils.SPACE
          + SolrConstants.DOUBLE_QUOTES + request.getLessThanAge() + SolrConstants.DOUBLE_QUOTES
          + SolrConstants.CLOSE_SQUARE_BRACKET);
    } else if (Objects.nonNull(request.getStartAge()) && Objects.nonNull(request.getEndAge())) {
      solrQuery.addFilterQuery(SolrFieldNames.SUBMITTED_DATE + SolrConstants.COLON + SolrConstants.OPEN_SQUARE_BRACKET
          + SolrConstants.DOUBLE_QUOTES + request.getStartAge() + SolrConstants.DOUBLE_QUOTES + StringUtils.SPACE
          + SolrConstants.TO + StringUtils.SPACE + SolrConstants.DOUBLE_QUOTES + request.getEndAge()
          + SolrConstants.DOUBLE_QUOTES + SolrConstants.CLOSE_SQUARE_BRACKET);
    }
  }

  private void setTimeAndStatusFiltersForDalamProcessUsingTimeFilterType(DalamProductListRequest request,
      SolrQuery solrQuery) {
    String timeFilterQuery = null;
    if (TimeFilterType.TODAY.getTimeFilterType().equalsIgnoreCase(request.getTimeFilterType())) {
      timeFilterQuery =
          SolrConstants.TODAY_FACET_INTERVAL.replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO);
    } else if (TimeFilterType.YESTERDAY.getTimeFilterType().equalsIgnoreCase(request.getTimeFilterType())) {
      timeFilterQuery =
          SolrConstants.YESTERDAY_FACET_INTERVAL.replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO);
    } else if (TimeFilterType.TWO_DAYS_AGO.getTimeFilterType().equalsIgnoreCase(request.getTimeFilterType())) {
      timeFilterQuery =
          SolrConstants.TWO_DAYS_AGO_FACET_INTERVAL.replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO);
    } else if (TimeFilterType.THREE_TO_FIVE_DAYS_AGO.getTimeFilterType().equalsIgnoreCase(request.getTimeFilterType())) {
      timeFilterQuery = SolrConstants.THREE_TO_FIVE_DAYS_AGO_FACET_INTERVAL
          .replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO);
    } else if (TimeFilterType.FIVE_DAYS_AGO.getTimeFilterType().equalsIgnoreCase(request.getTimeFilterType())) {
      timeFilterQuery =
          SolrConstants.FIVE_DAYS_AGO_FACET_INTERVAL.replace(SolrConstants.COMMA, StringUtils.SPACE + SolrConstants.TO);
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, NOT_A_VALID_TYPE);
    }
    solrQuery.addFilterQuery(SolrFieldNames.SUBMITTED_DATE + SolrConstants.COLON + timeFilterQuery);
  }

  private SolrProductCollectionDTO getSolrProductCollectionDTO(SolrDocument solrDocument) {
    SolrProductCollectionDTO solrProductCollectionDTO = new SolrProductCollectionDTO();
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.ID))) {
      solrProductCollectionDTO.setId(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.ID)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.STORE_ID))) {
      solrProductCollectionDTO.setStoreId(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.STORE_ID)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_ID))) {
      solrProductCollectionDTO.setProductId(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_ID)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_CODE))) {
      solrProductCollectionDTO.setProductCode(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_CODE)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME))) {
      solrProductCollectionDTO.setProductName(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.PRODUCT_NAME)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.BRAND))) {
      solrProductCollectionDTO.setBrand(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.BRAND)));
    }
    if (!CollectionUtils.isEmpty((List<String>) solrDocument.getFieldValue(SolrFieldNames.CATEGORY_CODES))) {
      solrProductCollectionDTO.setCategoryCode(
          String.valueOf(((List<String>) solrDocument.getFieldValue(SolrFieldNames.CATEGORY_CODES)).get(0)));
    }
    if (!CollectionUtils.isEmpty((List<String>) solrDocument.getFieldValue(SolrFieldNames.CATEGORY_NAMES))) {
      solrProductCollectionDTO.setCategoryName(
          String.valueOf(((List<String>) solrDocument.getFieldValue(SolrFieldNames.CATEGORY_NAMES)).get(0)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.BUSINESS_PARTNER_CODE))) {
      solrProductCollectionDTO
          .setBusinessPartnerCode(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.BUSINESS_PARTNER_CODE)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.BUSINESS_PARTNER_NAME))) {
      solrProductCollectionDTO
          .setBusinessPartnerName(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.BUSINESS_PARTNER_NAME)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.UPDATED_BY))) {
      solrProductCollectionDTO.setUpdatedBy(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.UPDATED_BY)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.CREATED_BY))) {
      solrProductCollectionDTO.setCreatedBy(String.valueOf(solrDocument.getFieldValue(SolrFieldNames.CREATED_BY)));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.UPDATED_STEP_DATE))) {
      solrProductCollectionDTO.setUpdatedStepDate((Date) solrDocument.getFieldValue(SolrFieldNames.UPDATED_STEP_DATE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.UPDATED_DATE))) {
      solrProductCollectionDTO.setUpdatedDate((Date) solrDocument.getFieldValue(SolrFieldNames.UPDATED_DATE));
    }
    if (Objects.nonNull(solrDocument.getFieldValue(SolrFieldNames.CREATED_DATE))) {
      solrProductCollectionDTO.setCreatedDate((Date) solrDocument.getFieldValue(SolrFieldNames.CREATED_DATE));
    }
    return solrProductCollectionDTO;
  }

  @Override
  public Page<ProductBusinessPartnerMapper> findProductBusinessPartnerMapper(String storeId, boolean activated,
      boolean viewable, boolean isSearch, String searchCriteria, Pageable pageable)
      throws IOException, SolrServerException {
    SolrQuery solrQuery =
        getSolrQueryForProductBusinessPartnerMapper(activated, viewable, isSearch, searchCriteria, pageable);
    List<SolrProductCollectionDTO> solrProductCollectionDTOList = new ArrayList<>();
    QueryResponse queryResponse = this.reviewProductCollectionClient.query(solrQuery);
    queryResponse.getResults().forEach(solrDocument -> {
      SolrProductCollectionDTO solrProductCollectionDTO = getSolrProductCollectionDTO(solrDocument);
      solrProductCollectionDTOList.add(solrProductCollectionDTO);
    });
    List<ProductBusinessPartnerMapper> partnerMappers = toProductBusinessPartnerMappers(solrProductCollectionDTOList);
    return new PageImpl<>(partnerMappers, pageable, partnerMappers.size());
  }

  private List<ProductBusinessPartnerMapper> toProductBusinessPartnerMappers(
      List<SolrProductCollectionDTO> solrProductCollectionDTOList) {
    List<ProductBusinessPartnerMapper> productBusinessPartnerMappers = new ArrayList<>();
    for (SolrProductCollectionDTO solrProductCollectionDTO : solrProductCollectionDTOList) {
      ProductBusinessPartnerMapper productBusinessPartnerMapper = new ProductBusinessPartnerMapper();
      if (!SolrConstants.INTERNAL.equals(solrProductCollectionDTO.getBusinessPartnerCode())) {
        BeanUtils.copyProperties(solrProductCollectionDTO, productBusinessPartnerMapper);
        productBusinessPartnerMappers.add(productBusinessPartnerMapper);
      }
    }
    return productBusinessPartnerMappers;
  }

  private SolrQuery getSolrQueryForProductBusinessPartnerMapper(boolean activated, boolean viewable, boolean isSearch,
      String searchCriteria, Pageable pageable) {
    SolrQuery solrQuery = new SolrQuery();
    solrQuery.setQuery(SolrFieldNames.STORE_ID + SolrConstants.COLON + Constants.DEFAULT_STORE_ID);
    solrQuery.setStart(pageable.getPageNumber() * pageable.getPageSize());
    solrQuery.setRows(pageable.getPageSize());
    solrQuery.addFilterQuery(SolrConstants.COLLAPSE_FQ_FOR_BP_CODE);
    solrQuery.setSort(SolrFieldNames.COPY_BUSINESS_PARTNER_NAME, SolrQuery.ORDER.asc);
    solrQuery.set(CommonParams.TZ, SolrConstants.JKT_TIME_ZONE);
    if (isSearch) {
      if (SolrConstants.EXTERNAL.equalsIgnoreCase(searchCriteria)) {
        solrQuery.setQuery(
            solrQuery.getQuery() + StringUtils.SPACE + SolrConstants.AND + StringUtils.SPACE + SolrConstants.NOT
                + SolrFieldNames.BUSINESS_PARTNER_CODE + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTES
                + SolrConstants.INTERNAL + SolrConstants.DOUBLE_QUOTES);
      } else {
        String searchKeyword = getSearchKeyword(searchCriteria);
        solrQuery.setQuery(solrQuery.getQuery() + StringUtils.SPACE + SolrConstants.AND + StringUtils.SPACE
            + SolrFieldNames.BUSINESS_PARTNER_NAME + SolrConstants.COLON + SolrConstants.OPEN_BRACKET + searchKeyword
            + SolrConstants.LIKE_QUERY + SolrConstants.CLOSE_BRACKET);
      }
    }
    solrQuery.setQuery(
        solrQuery.getQuery() + StringUtils.SPACE + SolrConstants.AND + StringUtils.SPACE + SolrConstants.NOT
            + SolrFieldNames.STATE + SolrConstants.COLON + DRAFT_STATE);
    return solrQuery;
  }

  @Override
  public void atomicUpdateToSolr(Map<String, Object> fieldsAndValues) {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    fieldsAndValues.forEach(solrInputDocument::setField);
    try {
      reviewProductCollectionClient.add(solrInputDocument);
      log.info("Document added to review product solr collection : {} ", solrInputDocument);
    } catch (SolrException | SolrServerException | IOException e) {
      log.error("Exception caught while doing atomic update on review product collection solr ", e);
    }
  }
}
