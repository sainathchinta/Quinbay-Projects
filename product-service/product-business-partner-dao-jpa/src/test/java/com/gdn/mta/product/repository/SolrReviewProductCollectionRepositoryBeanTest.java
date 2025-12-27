package com.gdn.mta.product.repository;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrQuery;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrInputDocument;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.DalamProductListRequest;
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

public class SolrReviewProductCollectionRepositoryBeanTest {

  @InjectMocks
  private SolrReviewProductCollectionRepositoryBean solrReviewProductCollectionRepositoryBean;

  @Mock
  @Qualifier(value = "reviewProductCollectionClient")
  private CloudSolrClient cloudSolrClient;

  private QueryResponse queryResponse;

  @Mock
  private QueryResponse reviewProductResponse;

  @Mock
  private QueryResponse businessPartnerResponse;

  @Mock
  private QueryResponse assigneeResponse;

  private SolrDocumentList solrDocumentList;
  private SolrDocumentList businessPartnersList;
  private DalamProductListRequest dalamProductListRequest;

  @Captor
  private ArgumentCaptor<SolrQuery> solrQuery;

  @Captor
  private ArgumentCaptor<SolrInputDocument> solrInputDocument;
  private SummaryFilterServiceRequest request;
  private static final int PAGE = 0;
  private static final int SIZE = 100;
  private static final String KEYWORD = "keyword";
  private static final String EXTERNAL = "EXTERNAL";
  private static final String SEARCH_KEYWORD_1 = "ApiAutomation-2020-05-28 10:53:13";
  private static final String STORE_ID = "store_id";
  private static final String CATEGORY_CODE = "category_code";
  private static final String CATEGORY_NAME = "category_name";
  private static final String CATEGORY_CODES = "category_codes";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_NAME = "productName";
  private static final String BUSINESS_PARTNER_CODE = "business_partner_code";
  private static final String EXTERNAL_BUSINESS_PARTNER_CODE = "EXTERNAL";
  private static final String BUSINESS_PARTNER_NAME = "business_partner_name";
  private static final String USER_NAME = "username";
  private static final String SEARCH_KEYWORD = "search_keyword";
  private static final String SEARCH_KEYWORD_WITH_SPECIAL_CHARACTERS = "Samsung Galaxy [32GB- 3GB]";
  private static final String PRODUCT_ID = "PRODUCT_ID";
  private static final String BRAND_NAME = "brand_name";
  private static final String DALAM_Q_QUERY = "-state:DRAFT AND (product_name:(keyword*) OR product_code:(\"keyword\") OR created_by:(keyword*))";
  private static final String DALAM_Q_QUERY_1 = "-state:DRAFT AND (product_name:(ApiAutomation~ AND 2020~ AND 05~ AND 28~ AND 10~ AND 53~ AND 13*) OR product_code:(\"ApiAutomation-2020-05-28~ AND 10:53:13\") OR created_by:(ApiAutomation\\-2020\\-05\\-28\\ 10\\:53\\:13*))";
  private SolrDocumentList assigneeList;
  private static final String DALAM_FQ_0 = "store_id:10001";
  private static final String DALAM_FQ_1  = "business_partner_code:\"business_partner_code\"";
  private static final String EXTERNAL_DALAM_FQ_1  = "-business_partner_code:\"INTERNAL\"";
  private static final String DALAM_FQ_2 = "category_codes:\"category_code\"";
  private static final String BRACKET_PRODUCT_NAME = "(product_name";
  private static final String BRACKET_BUSINESS_PARTNER_NAME = "business_partner_name:(";
  private static final String NOT_BUSINESS_PARTNER_CODE = "-business_partner_code:\"INTERNAL\"";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    queryResponse = new QueryResponse();
    request = new SummaryFilterServiceRequest();
    solrDocumentList = new SolrDocumentList();
    businessPartnersList = new SolrDocumentList();
    assigneeList = new SolrDocumentList();
    SolrDocument assignee = new SolrDocument();
    assignee.setField(SolrFieldNames.ASSIGNED_TO, SolrFieldNames.ASSIGNED_TO);
    assigneeList.add(assignee);
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.setField(SolrFieldNames.BUSINESS_PARTNER_NAME, SolrFieldNames.BUSINESS_PARTNER_NAME);
    solrDocument.setField(SolrFieldNames.BUSINESS_PARTNER_CODE, SolrFieldNames.BUSINESS_PARTNER_CODE);
    businessPartnersList.add(solrDocument);
    solrDocumentList.add(solrDocument);
    dalamProductListRequest = DalamProductListRequest.builder().storeId(STORE_ID).categoryCode(CATEGORY_CODE)
        .businessPartnerCode(BUSINESS_PARTNER_CODE).keyword(KEYWORD).page(PAGE).size(SIZE).viewable(Boolean.FALSE)
        .activated(Boolean.FALSE).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(cloudSolrClient, reviewProductResponse, businessPartnerResponse, assigneeResponse);
  }

  @Test
  public void getFilterCountsByStoreIdAndActivatedAndViewable() throws IOException, SolrServerException {
    when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    QueryResponse queryResponse = this.solrReviewProductCollectionRepositoryBean
        .getFilterCountByStoreIdAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, Boolean.FALSE, Boolean.FALSE);
    verify(this.cloudSolrClient).query(solrQuery.capture());
    Assertions.assertNotNull(queryResponse);
  }

  @Test
  public void getInReviewProductsTest() throws IOException, SolrServerException {
    when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    QueryResponse queryResponse = this.solrReviewProductCollectionRepositoryBean
        .getInReviewProducts(Constants.DEFAULT_STORE_ID, KEYWORD, BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    verify(this.cloudSolrClient).query(solrQuery.capture());
    Assertions.assertNotNull(queryResponse);
    Assertions.assertEquals(DALAM_Q_QUERY, solrQuery.getValue().getQuery());
    Assertions.assertTrue(Arrays.toString(solrQuery.getValue().getFilterQueries()).contains(DALAM_FQ_0));
    Assertions.assertTrue(Arrays.toString(solrQuery.getValue().getFilterQueries()).contains(DALAM_FQ_1));
    Assertions.assertTrue(Arrays.toString(solrQuery.getValue().getFilterQueries()).contains(DALAM_FQ_2));
  }

  @Test
  public void getInReviewProductsMultipleWordsKeywordTest() throws IOException, SolrServerException {
    when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    QueryResponse queryResponse = this.solrReviewProductCollectionRepositoryBean
        .getInReviewProducts(Constants.DEFAULT_STORE_ID, SEARCH_KEYWORD_1, BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    verify(this.cloudSolrClient).query(solrQuery.capture());
    Assertions.assertNotNull(queryResponse);
    Assertions.assertEquals(DALAM_Q_QUERY_1, solrQuery.getValue().getQuery());
    Assertions.assertTrue(Arrays.toString(solrQuery.getValue().getFilterQueries()).contains(DALAM_FQ_0));
    Assertions.assertTrue(Arrays.toString(solrQuery.getValue().getFilterQueries()).contains(DALAM_FQ_1));
    Assertions.assertTrue(Arrays.toString(solrQuery.getValue().getFilterQueries()).contains(DALAM_FQ_2));
  }

  @Test
  public void getInReviewProductsMultipleWordsKeywordExternalBusinessPartnerTest() throws IOException, SolrServerException {
    when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    QueryResponse queryResponse = this.solrReviewProductCollectionRepositoryBean
        .getInReviewProducts(Constants.DEFAULT_STORE_ID, SEARCH_KEYWORD_1, EXTERNAL_BUSINESS_PARTNER_CODE, CATEGORY_CODE);
    verify(this.cloudSolrClient).query(solrQuery.capture());
    Assertions.assertNotNull(queryResponse);
    Assertions.assertEquals(DALAM_Q_QUERY_1, solrQuery.getValue().getQuery());
    Assertions.assertTrue(Arrays.toString(solrQuery.getValue().getFilterQueries()).contains(DALAM_FQ_0));
    Assertions.assertTrue(Arrays.toString(solrQuery.getValue().getFilterQueries()).contains(EXTERNAL_DALAM_FQ_1));
    Assertions.assertTrue(Arrays.toString(solrQuery.getValue().getFilterQueries()).contains(DALAM_FQ_2));
  }

  @Test
  public void getReviewProductsByFilterRequestAndActivatedAndViewableTest() throws IOException, SolrServerException {
    when(this.cloudSolrClient.query(solrQuery.capture())).thenReturn(reviewProductResponse);
    when(this.reviewProductResponse.getResults()).thenReturn(solrDocumentList);
    request.setTimeFilter(TimeFilterType.TODAY);
    request.setStatusFilter(StatusFilterType.ASSIGNED);
    setSummaryFilterRequest();
    request.setSortOrder(SolrConstants.ASC);
    SolrDocumentList solrDocuments = this.solrReviewProductCollectionRepositoryBean
        .getReviewProductsByFilterRequestAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, request, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE); verify(this.cloudSolrClient).query(solrQuery.capture());
    verify(this.reviewProductResponse).getResults();
    Assertions.assertNotNull(solrDocuments);
    Assertions.assertTrue(solrDocuments.size() == 1);
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_NAME, solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_NAME));
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_CODE, solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_CODE));
    Assertions.assertTrue(solrQuery.getValue().getQuery().contains(BRACKET_PRODUCT_NAME));
    Assertions.assertTrue(Arrays.toString(solrQuery.getValue().getFilterQueries()).contains(DALAM_FQ_0));
  }

  @Test
  public void getReviewProductsByFilterRequestAndActivatedAndViewableWithSpecialCharsTest() throws IOException, SolrServerException {
    when(this.cloudSolrClient.query(solrQuery.capture())).thenReturn(reviewProductResponse);
    when(this.reviewProductResponse.getResults()).thenReturn(solrDocumentList);
    request.setTimeFilter(TimeFilterType.TODAY);
    request.setStatusFilter(StatusFilterType.ASSIGNED);
    request.setAssignedTo(SolrConstants.ASSIGNED_TO_PREFIX);
    request.setCategoryCode(CATEGORY_CODE);
    request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    request.setSearchKeyword(SEARCH_KEYWORD_WITH_SPECIAL_CHARACTERS);
    request.setSortColumn(SolrFieldNames.SUBMITTED_DATE);
    request.setSortOrder(SolrConstants.ASC);
    SolrDocumentList solrDocuments = this.solrReviewProductCollectionRepositoryBean
        .getReviewProductsByFilterRequestAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, request, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE); verify(this.cloudSolrClient).query(solrQuery.capture());
    verify(this.reviewProductResponse).getResults();
    Assertions.assertNotNull(solrDocuments);
    Assertions.assertEquals(1, solrDocuments.size());
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_NAME, solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_NAME));
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_CODE,
        solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_CODE));
  }

  @Test
  public void getReviewProductsByFilterRequestAndActivatedAndViewableWithYesterdayFilterTest()
      throws IOException, SolrServerException {
    when(this.cloudSolrClient.query(solrQuery.capture())).thenReturn(reviewProductResponse);
    when(this.reviewProductResponse.getResults()).thenReturn(solrDocumentList);
    request.setTimeFilter(TimeFilterType.YESTERDAY);
    request.setStatusFilter(StatusFilterType.UNASSIGNED);
    setSummaryFilterRequest();
    request.setSortOrder(SolrConstants.ASC);
    SolrDocumentList solrDocuments = this.solrReviewProductCollectionRepositoryBean
        .getReviewProductsByFilterRequestAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, request, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE); verify(this.cloudSolrClient).query(solrQuery.capture());
    verify(this.reviewProductResponse).getResults();
    Assertions.assertNotNull(solrDocuments);
    Assertions.assertTrue(solrDocuments.size() == 1);
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_NAME, solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_NAME));
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_CODE,
        solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_CODE));
  }

  @Test
  public void getReviewProductsByActivatedAndViewableTwoDaysAgoFilterTest() throws IOException, SolrServerException {
    when(this.cloudSolrClient.query(solrQuery.capture())).thenReturn(reviewProductResponse);
    when(this.reviewProductResponse.getResults()).thenReturn(solrDocumentList);
    request.setTimeFilter(TimeFilterType.TWO_DAYS_AGO);
    request.setStatusFilter(StatusFilterType.REVISED);
    setSummaryFilterRequest();
    request.setSortOrder(SolrConstants.ASC);
    SolrDocumentList solrDocuments = this.solrReviewProductCollectionRepositoryBean
        .getReviewProductsByFilterRequestAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, request, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE); verify(this.cloudSolrClient).query(solrQuery.capture());
    verify(this.reviewProductResponse).getResults();
    Assertions.assertNotNull(solrDocuments);
    Assertions.assertTrue(solrDocuments.size() == 1);
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_NAME, solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_NAME));
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_CODE,
        solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_CODE));
  }

  @Test
  public void getReviewProductsByActivatedAndViewableBrandApprovedFilterTest() throws IOException, SolrServerException {
    when(this.cloudSolrClient.query(solrQuery.capture())).thenReturn(reviewProductResponse);
    when(this.reviewProductResponse.getResults()).thenReturn(solrDocumentList);
    request.setTimeFilter(TimeFilterType.TWO_DAYS_AGO);
    request.setStatusFilter(StatusFilterType.BRAND_APPROVED);
    setSummaryFilterRequest();
    request.setSortOrder(SolrConstants.ASC);
    SolrDocumentList solrDocuments = this.solrReviewProductCollectionRepositoryBean
        .getReviewProductsByFilterRequestAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, request, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE); verify(this.cloudSolrClient).query(solrQuery.capture());
    verify(this.reviewProductResponse).getResults();
    Assertions.assertNotNull(solrDocuments);
    Assertions.assertEquals(1, solrDocuments.size());
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_NAME, solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_NAME));
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_CODE,
        solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_CODE));
  }

  @Test
  public void getReviewProductsByActivatedAndViewableBrandNotApprovedFilterTest() throws IOException, SolrServerException {
    when(this.cloudSolrClient.query(solrQuery.capture())).thenReturn(reviewProductResponse);
    when(this.reviewProductResponse.getResults()).thenReturn(solrDocumentList);
    request.setTimeFilter(TimeFilterType.TWO_DAYS_AGO);
    request.setStatusFilter(StatusFilterType.BRAND_NOT_APPROVED);
    setSummaryFilterRequest();
    request.setSortOrder(SolrConstants.ASC);
    SolrDocumentList solrDocuments = this.solrReviewProductCollectionRepositoryBean
        .getReviewProductsByFilterRequestAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, request, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE); verify(this.cloudSolrClient).query(solrQuery.capture());
    verify(this.reviewProductResponse).getResults();
    Assertions.assertNotNull(solrDocuments);
    Assertions.assertTrue(solrDocuments.size() == 1);
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_NAME,
        solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_NAME));
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_CODE,
        solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_CODE));
  }

  @Test
  public void getReviewProductsByActivatedAndViewableRevisedFilterTest() throws IOException, SolrServerException {
    when(this.cloudSolrClient.query(solrQuery.capture())).thenReturn(reviewProductResponse);
    when(this.reviewProductResponse.getResults()).thenReturn(solrDocumentList);
    request.setTimeFilter(TimeFilterType.TWO_DAYS_AGO);
    request.setStatusFilter(StatusFilterType.REVISED);
    setSummaryFilterRequest();
    request.setSortOrder(SolrConstants.ASC);
    SolrDocumentList solrDocuments = this.solrReviewProductCollectionRepositoryBean
        .getReviewProductsByFilterRequestAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, request, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE); verify(this.cloudSolrClient).query(solrQuery.capture());
    verify(this.reviewProductResponse).getResults();
    Assertions.assertNotNull(solrDocuments);
    Assertions.assertEquals(1, solrDocuments.size());
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_NAME, solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_NAME));
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_CODE,
        solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_CODE));
  }

  private void setSummaryFilterRequest() {
    request.setAssignedTo(SolrConstants.ASSIGNED_TO_PREFIX);
    request.setCategoryCode(CATEGORY_CODE);
    request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    request.setSearchKeyword(SEARCH_KEYWORD);
    request.setSortColumn(SolrFieldNames.SUBMITTED_DATE);
  }

  @Test
  public void getReviewProductsByActivatedAndViewableAndThreeToFiveDaysAgoFilterTest()
      throws IOException, SolrServerException {
    when(this.cloudSolrClient.query(solrQuery.capture())).thenReturn(reviewProductResponse);
    when(this.reviewProductResponse.getResults()).thenReturn(solrDocumentList);
    request.setTimeFilter(TimeFilterType.THREE_TO_FIVE_DAYS_AGO);
    request.setStatusFilter(StatusFilterType.ASSIGNED);
    setSummaryFilterRequest();
    request.setSortOrder(SolrConstants.ASC);
    SolrDocumentList solrDocuments = this.solrReviewProductCollectionRepositoryBean
        .getReviewProductsByFilterRequestAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, request, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE); verify(this.cloudSolrClient).query(solrQuery.capture());
    verify(this.reviewProductResponse).getResults();
    Assertions.assertNotNull(solrDocuments);
    Assertions.assertTrue(solrDocuments.size() == 1);
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_NAME, solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_NAME));
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_CODE,
        solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_CODE));
  }

  @Test
  public void getReviewProductsByActivatedAndViewableAndFiveDaysAgoTest() throws IOException, SolrServerException {
    when(this.cloudSolrClient.query(solrQuery.capture())).thenReturn(reviewProductResponse);
    when(this.reviewProductResponse.getResults()).thenReturn(solrDocumentList);
    request.setTimeFilter(TimeFilterType.FIVE_DAYS_AGO);
    request.setStatusFilter(StatusFilterType.ASSIGNED);
    request.setAssignedTo(SolrConstants.ASSIGNED_TO_PREFIX);
    request.setCategoryCode(CATEGORY_CODE);
    request.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    request.setSearchKeyword(StringUtils.EMPTY);
    request.setSortColumn(SolrFieldNames.SUBMITTED_DATE);
    request.setSortOrder(SolrConstants.DESC);
    SolrDocumentList solrDocuments = this.solrReviewProductCollectionRepositoryBean
        .getReviewProductsByFilterRequestAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, request, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE);
    verify(this.cloudSolrClient).query(solrQuery.capture());
    verify(this.reviewProductResponse).getResults();
    Assertions.assertNotNull(solrDocuments);
    Assertions.assertEquals(1, solrDocuments.size());
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_NAME, solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_NAME));
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_CODE,
        solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_CODE));
  }

  @Test
  public void getBusinessPartnersByTimeAndStatusFilterTest() throws IOException, SolrServerException {
    SummaryFilterServiceRequest summaryFilterServiceRequest =
        SummaryFilterServiceRequest.builder().statusFilter(StatusFilterType.ALL).timeFilter(TimeFilterType.ALL)
            .searchKeyword(StringUtils.EMPTY).build();
    when(this.cloudSolrClient.query(solrQuery.capture())).thenReturn(businessPartnerResponse);
    when(this.businessPartnerResponse.getResults()).thenReturn(businessPartnersList);
    SolrDocumentList solrDocuments = this.solrReviewProductCollectionRepositoryBean
        .getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, summaryFilterServiceRequest, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE);
    verify(this.cloudSolrClient).query(solrQuery.capture());
    verify(this.businessPartnerResponse).getResults();
    Assertions.assertNotNull(solrDocuments);
    Assertions.assertTrue(solrDocuments.size() == 1);
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_NAME, solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_NAME));
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_CODE,
        solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_CODE));
  }

  @Test
  public void getBusinessPartnersByTimeAndStatusFilterNonEmptySearchKeywordTest()
      throws IOException, SolrServerException {
    SummaryFilterServiceRequest summaryFilterServiceRequest =
        SummaryFilterServiceRequest.builder().statusFilter(StatusFilterType.ALL).timeFilter(TimeFilterType.ALL)
            .searchKeyword(KEYWORD).build();
    when(this.cloudSolrClient.query(solrQuery.capture())).thenReturn(businessPartnerResponse);
    when(this.businessPartnerResponse.getResults()).thenReturn(businessPartnersList);
    SolrDocumentList solrDocuments = this.solrReviewProductCollectionRepositoryBean
        .getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, summaryFilterServiceRequest, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE);
    verify(this.cloudSolrClient).query(solrQuery.capture());
    verify(this.businessPartnerResponse).getResults();
    Assertions.assertNotNull(solrDocuments);
    Assertions.assertTrue(solrDocuments.size() == 1);
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_NAME, solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_NAME));
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_CODE,
        solrDocuments.get(0).getFieldValue(SolrFieldNames.BUSINESS_PARTNER_CODE));
  }

  @Test
  public void getAssigneeListByTimeAndStatusFilterNonEmptySearchKeywordTest() throws IOException, SolrServerException {
    SummaryFilterServiceRequest serviceRequest = SummaryFilterServiceRequest.builder()
        .statusFilter(StatusFilterType.getStatusFilterTypeByValue(SolrConstants.ALL))
        .timeFilter(TimeFilterType.getTimeFilterTypeByValue(SolrConstants.ALL))
        .build();
    when(this.cloudSolrClient.query(solrQuery.capture())).thenReturn(assigneeResponse);
    when(this.assigneeResponse.getResults()).thenReturn(assigneeList);
    SolrDocumentList solrDocuments = this.solrReviewProductCollectionRepositoryBean
        .getAssigneeListByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, serviceRequest, Boolean.FALSE, Boolean.FALSE);
    verify(this.cloudSolrClient).query(solrQuery.capture());
    verify(this.assigneeResponse).getResults();
    Assertions.assertNotNull(solrDocuments);
    Assertions.assertEquals(1, solrDocuments.size());
    Assertions.assertEquals(SolrFieldNames.ASSIGNED_TO, solrDocuments.get(0).getFieldValue(SolrFieldNames.ASSIGNED_TO));
  }

  @Test
  public void updateAssignedToInReviewProductCollectionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    this.solrReviewProductCollectionRepositoryBean
        .updateAssignedToInSolrCollection(PRODUCT_ID, SolrFieldNames.ASSIGNED_TO);
    Mockito.verify(this.cloudSolrClient).add(solrInputDocument.capture());
  }

  @Test
  public void updateProductToReviewProductCollectionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.add(Mockito.any(SolrInputDocument.class))).thenReturn(new UpdateResponse());
    this.solrReviewProductCollectionRepositoryBean
        .updateProductToSolrCollection(new SolrInputDocument());
    Mockito.verify(this.cloudSolrClient).add(solrInputDocument.capture());
  }

  @Test
  public void deleteAllDocumentsScreeningFromSolr() throws Exception {
    this.solrReviewProductCollectionRepositoryBean.deleteAllScreeningDocumentsFromSolr();
    Mockito.verify(this.cloudSolrClient).deleteByQuery(
        SolrFieldNames.STATE + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTES + WorkflowStates.DRAFT.getValue()
            + SolrConstants.DOUBLE_QUOTES);
  }

  @Test
  public void deleteAllInProgressDocumentsFromSolr() throws Exception {
    this.solrReviewProductCollectionRepositoryBean.deleteAllInProgressDocumentsFromSolr();
    Mockito.verify(this.cloudSolrClient).deleteByQuery(
        SolrConstants.NOT + SolrFieldNames.STATE + SolrConstants.COLON + SolrConstants.DOUBLE_QUOTES + WorkflowStates.DRAFT.getValue()
            + SolrConstants.DOUBLE_QUOTES);
  }

  @Test
  public void findDalamProductsListTest() throws Exception {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils.setField(queryResponse, QueryResponse.class, "_results", solrDocumentList,
        SolrDocumentList.class);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<SolrProductCollectionDTO> solrProductCollectionDTOPage =
        solrReviewProductCollectionRepositoryBean.findDalamProductsList(
            dalamProductListRequest, PageRequest.of(0, 1));
    Mockito.verify(cloudSolrClient).query(Mockito.any());
    checkAssertionForProductCollectionList(date, docId, solrProductCollectionDTOPage);
  }

  private void checkAssertionForProductCollectionList(Date date, String docId,
      Page<SolrProductCollectionDTO> solrProductCollectionDTOPage) {
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().size(), 1);
    Assertions.assertEquals(100, solrProductCollectionDTOPage.getTotalElements());
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().get(0).getId(), docId);
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().get(0).getProductCode(), PRODUCT_CODE);
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().get(0).getProductId(), PRODUCT_ID);
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().get(0).getUpdatedStepDate(), date);
    Assertions.assertEquals(1, solrProductCollectionDTOPage.getContent().size());
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().get(0).getCreatedBy(), USER_NAME);
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().get(0).getBrand(), BRAND_NAME);
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().get(0).getBusinessPartnerCode(), BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().get(0).getBusinessPartnerName(), BUSINESS_PARTNER_NAME);
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().get(0).getCategoryCode(), CATEGORY_CODE);
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().get(0).getCategoryName(), CATEGORY_NAME);
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().get(0).getUpdatedBy(), USER_NAME);
    Assertions.assertEquals(solrProductCollectionDTOPage.getContent().get(0).getUpdatedDate(), date);
  }

  private SolrDocument getSolrDocument(Date date, String docId) {
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField("id", docId);
    solrDocument.addField("store_id", STORE_ID);
    solrDocument.addField("product_id", PRODUCT_ID);
    solrDocument.addField("product_code", PRODUCT_CODE);
    solrDocument.addField("product_name", PRODUCT_NAME);
    solrDocument.addField("brand", BRAND_NAME);
    solrDocument.addField("category_codes", Collections.singletonList(CATEGORY_CODE));
    solrDocument.addField("category_names", Collections.singletonList(CATEGORY_NAME));
    solrDocument.addField("business_partner_code", BUSINESS_PARTNER_CODE);
    solrDocument.addField("business_partner_name", BUSINESS_PARTNER_NAME);
    solrDocument.addField("activated", true);
    solrDocument.addField("viewable", true);
    solrDocument.addField("updated_step_date", date);
    solrDocument.addField("created_date", date);
    solrDocument.addField("updated_date", date);
    solrDocument.addField("created_by", USER_NAME);
    solrDocument.addField("updated_by", USER_NAME);
    return solrDocument;
  }

  @Test
  public void findProductBusinessPartnerMapperTest() throws IOException, SolrServerException {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<ProductBusinessPartnerMapper> result = solrReviewProductCollectionRepositoryBean
        .findProductBusinessPartnerMapper(STORE_ID, false, false, false, null, PageRequest.of(0, 1));
    Mockito.verify(cloudSolrClient).query(solrQuery.capture());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, result.getContent().get(0).getBusinessPartnerCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, result.getContent().get(0).getBusinessPartnerName());
  }

  @Test
  public void findProductBusinessPartnerMapperSearchTest() throws IOException, SolrServerException {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<ProductBusinessPartnerMapper> result = solrReviewProductCollectionRepositoryBean
        .findProductBusinessPartnerMapper(STORE_ID, false, false, true, KEYWORD, PageRequest.of(0, 1));
    Mockito.verify(cloudSolrClient).query(solrQuery.capture());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, result.getContent().get(0).getBusinessPartnerCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, result.getContent().get(0).getBusinessPartnerName());
    Assertions.assertTrue(solrQuery.getValue().getQuery().contains(BRACKET_BUSINESS_PARTNER_NAME));
  }

  @Test
  public void findDalamProductsListExternalMerchantTest() throws Exception {
    dalamProductListRequest.setBusinessPartnerCode(SolrConstants.EXTERNAL);
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils.setField(queryResponse, QueryResponse.class, "_results", solrDocumentList,
        SolrDocumentList.class);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<SolrProductCollectionDTO> solrProductCollectionDTOPage =
        solrReviewProductCollectionRepositoryBean.findDalamProductsList(
            dalamProductListRequest, PageRequest.of(0, 1));
    Mockito.verify(cloudSolrClient).query(Mockito.any());
    checkAssertionForProductCollectionList(date, docId, solrProductCollectionDTOPage);
  }

  @Test
  public void findDalamProductsListTimeFilterTest() throws Exception {
    dalamProductListRequest.setEndAge(Calendar.getInstance().getTime().toString());
    dalamProductListRequest.setStartAge(Calendar.getInstance().getTime().toString());
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils.setField(queryResponse, QueryResponse.class, "_results", solrDocumentList,
        SolrDocumentList.class);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<SolrProductCollectionDTO> solrProductCollectionDTOPage =
        solrReviewProductCollectionRepositoryBean.findDalamProductsList(
            dalamProductListRequest, PageRequest.of(0, 1));
    Mockito.verify(cloudSolrClient).query(solrQuery.capture());
    checkAssertionForProductCollectionList(date, docId, solrProductCollectionDTOPage);
    Assertions.assertTrue(solrQuery.getValue().getQuery().contains(BRACKET_PRODUCT_NAME));
    Assertions.assertTrue(solrQuery.getValue().getQuery().contains(DALAM_FQ_0));
  }

  @Test
  public void findDalamProductsListTimeFilterLessThenAgeTest() throws Exception {
    dalamProductListRequest.setLessThanAge(Calendar.getInstance().getTime().toString());
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils.setField(queryResponse, QueryResponse.class, "_results", solrDocumentList,
        SolrDocumentList.class);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<SolrProductCollectionDTO> solrProductCollectionDTOPage =
        solrReviewProductCollectionRepositoryBean.findDalamProductsList(
            dalamProductListRequest, PageRequest.of(0, 1));
    Mockito.verify(cloudSolrClient).query(solrQuery.capture());
    checkAssertionForProductCollectionList(date, docId, solrProductCollectionDTOPage);
    Assertions.assertTrue(solrQuery.getValue().getQuery().contains(BRACKET_PRODUCT_NAME));
    Assertions.assertTrue(solrQuery.getValue().getQuery().contains(DALAM_FQ_0));
  }

  @Test
  public void findDalamProductsListTimeFilterTypeTodayTest() throws Exception {
    dalamProductListRequest.setTimeFilterType(TimeFilterType.TODAY.getTimeFilterType());
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils.setField(queryResponse, QueryResponse.class, "_results", solrDocumentList,
        SolrDocumentList.class);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<SolrProductCollectionDTO> solrProductCollectionDTOPage =
        solrReviewProductCollectionRepositoryBean.findDalamProductsList(
            dalamProductListRequest, PageRequest.of(0, 1));
    Mockito.verify(cloudSolrClient).query(solrQuery.capture());
    checkAssertionForProductCollectionList(date, docId, solrProductCollectionDTOPage);
    Assertions.assertTrue(solrQuery.getValue().getQuery().contains(BRACKET_PRODUCT_NAME));
    Assertions.assertTrue(solrQuery.getValue().getQuery().contains(DALAM_FQ_0));
  }

  @Test
  public void findDalamProductsListTimeFilterTypeYesterdayTest() throws Exception {
    dalamProductListRequest.setTimeFilterType(TimeFilterType.YESTERDAY.getTimeFilterType());
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils.setField(queryResponse, QueryResponse.class, "_results", solrDocumentList,
        SolrDocumentList.class);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<SolrProductCollectionDTO> solrProductCollectionDTOPage =
        solrReviewProductCollectionRepositoryBean.findDalamProductsList(
            dalamProductListRequest, PageRequest.of(0, 1));
    Mockito.verify(cloudSolrClient).query(solrQuery.capture());
    checkAssertionForProductCollectionList(date, docId, solrProductCollectionDTOPage);
    Assertions.assertTrue(solrQuery.getValue().getQuery().contains(BRACKET_PRODUCT_NAME));
    Assertions.assertTrue(solrQuery.getValue().getQuery().contains(DALAM_FQ_0));
  }

  @Test
  public void findDalamProductsListTimeFilterTypeTwodaysAgoTest() throws Exception {
    dalamProductListRequest.setTimeFilterType(TimeFilterType.TWO_DAYS_AGO.getTimeFilterType());
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils.setField(queryResponse, QueryResponse.class, "_results", solrDocumentList,
        SolrDocumentList.class);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<SolrProductCollectionDTO> solrProductCollectionDTOPage =
        solrReviewProductCollectionRepositoryBean.findDalamProductsList(
            dalamProductListRequest, PageRequest.of(0, 1));
    Mockito.verify(cloudSolrClient).query(solrQuery.capture());
    checkAssertionForProductCollectionList(date, docId, solrProductCollectionDTOPage);
    Assertions.assertTrue(solrQuery.getValue().getQuery().contains(BRACKET_PRODUCT_NAME));
    Assertions.assertTrue(solrQuery.getValue().getQuery().contains(DALAM_FQ_0));
  }

  @Test
  public void findDalamProductsListTimeFilterType3to5DaysTest() throws Exception {
    dalamProductListRequest.setTimeFilterType(TimeFilterType.THREE_TO_FIVE_DAYS_AGO.getTimeFilterType());
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils.setField(queryResponse, QueryResponse.class, "_results", solrDocumentList,
        SolrDocumentList.class);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<SolrProductCollectionDTO> solrProductCollectionDTOPage =
        solrReviewProductCollectionRepositoryBean.findDalamProductsList(
            dalamProductListRequest, PageRequest.of(0, 1));
    Mockito.verify(cloudSolrClient).query(solrQuery.capture());
    checkAssertionForProductCollectionList(date, docId, solrProductCollectionDTOPage);
    Assertions.assertTrue(solrQuery.getValue().getQuery().contains(BRACKET_PRODUCT_NAME));
    Assertions.assertTrue(solrQuery.getValue().getQuery().contains(DALAM_FQ_0));
  }

  @Test
  public void findDalamProductsListTimeFilterTypeFiveDaysAgoTest() throws Exception {
    dalamProductListRequest.setTimeFilterType(TimeFilterType.FIVE_DAYS_AGO.getTimeFilterType());
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils.setField(queryResponse, QueryResponse.class, "_results", solrDocumentList,
        SolrDocumentList.class);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<SolrProductCollectionDTO> solrProductCollectionDTOPage =
        solrReviewProductCollectionRepositoryBean.findDalamProductsList(
            dalamProductListRequest, PageRequest.of(0, 1));
    Mockito.verify(cloudSolrClient).query(solrQuery.capture());
    checkAssertionForProductCollectionList(date, docId, solrProductCollectionDTOPage);
    Assertions.assertTrue(solrQuery.getValue().getQuery().contains(BRACKET_PRODUCT_NAME));
    Assertions.assertTrue(solrQuery.getValue().getQuery().contains(DALAM_FQ_0));
  }

  @Test
  public void findDalamProductsListTimeFilterTypeExceptionTest() throws Exception {
    dalamProductListRequest.setTimeFilterType(TimeFilterType.ALL.name());
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      solrReviewProductCollectionRepositoryBean
          .findDalamProductsList(dalamProductListRequest, PageRequest.of(0, 1));
    });
  }

  @Test
  public void findProductBusinessPartnerMapperExternalSearchTest() throws IOException, SolrServerException {
    Date date = Calendar.getInstance().getTime();
    String docId = UUID.randomUUID().toString();
    SolrDocument solrDocument = getSolrDocument(date, docId);
    SolrDocumentList solrDocumentList = new SolrDocumentList();
    solrDocumentList.add(solrDocument);
    QueryResponse queryResponse = new QueryResponse();
    ReflectionTestUtils
        .setField(queryResponse, QueryResponse.class, "_results", solrDocumentList, SolrDocumentList.class);
    queryResponse.getResults().setNumFound(100);
    Mockito.when(cloudSolrClient.query(Mockito.any())).thenReturn(queryResponse);
    Page<ProductBusinessPartnerMapper> result = solrReviewProductCollectionRepositoryBean
        .findProductBusinessPartnerMapper(STORE_ID, false, false, true, EXTERNAL,
            PageRequest.of(0, 1));
    Mockito.verify(cloudSolrClient).query(solrQuery.capture());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, result.getContent().get(0).getBusinessPartnerCode());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, result.getContent().get(0).getBusinessPartnerName());
    Assertions.assertTrue(solrQuery.getValue().getQuery().contains(NOT_BUSINESS_PARTNER_CODE));
  }

  @Test
  public void eventBasedAtomicUpdateToSolrTest() throws SolrServerException, IOException {
    Map<String ,Object> fieldValues = new HashMap<>();
    fieldValues.put("key","value");
    solrReviewProductCollectionRepositoryBean.atomicUpdateToSolr(fieldValues);
    verify(cloudSolrClient).add(solrInputDocument.capture());
    Assertions.assertEquals("value",solrInputDocument.getValue().getFieldValue("key"));
  }

  @Test
  public void eventBasedAtomicUpdateToSolrExceptionTest() throws SolrServerException, IOException {
    try {
      Map<String, Object> fieldValues = new HashMap<>();
      fieldValues.put("key", "value");
      when(cloudSolrClient.add(solrInputDocument.capture())).thenThrow(SolrServerException.class);
      solrReviewProductCollectionRepositoryBean.atomicUpdateToSolr(fieldValues);
    }
    finally {
      verify(cloudSolrClient).add(solrInputDocument.capture());
      Assertions.assertEquals("value",solrInputDocument.getValue().getFieldValue("key"));
    }
  }

}