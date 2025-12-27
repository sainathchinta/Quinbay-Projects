package com.gdn.mta.product.service;

import static com.gdn.partners.pbp.commons.constants.Constants.CATEGORY_CODE;
import static com.gdn.partners.pbp.commons.constants.Constants.DEFAULT_STORE_ID;
import static com.gdn.partners.pbp.commons.constants.Constants.PRODUCT_CODE;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.gda.mta.product.dto.HistoryUpdateRequest;
import com.gda.mta.product.dto.response.HistoryUpdateResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.FacetField;
import org.apache.solr.client.solrj.response.IntervalFacet;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import com.gda.mta.product.dto.HistoryRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.response.AssigneeResponse;
import com.gda.mta.product.dto.response.FilterCountResponse;
import com.gda.mta.product.dto.response.HistoryResponse;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.enums.StatusFilterType;
import com.gdn.mta.product.enums.TimeFilterType;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.SolrHistoryCollectionRepository;
import com.gdn.mta.product.repository.SolrReviewProductCollectionRepositoryBean;
import com.gdn.mta.product.valueobject.SummaryFilterServiceRequest;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;
import com.gdn.partners.pbp.model.productlevel3.ProductCollectionCountResponse;

public class SummaryFilterServiceImplTest {

  @InjectMocks
  private SummaryFilterServiceImpl summaryFilterService;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  private SolrReviewProductCollectionRepositoryBean solrReviewProductCollectionRepositoryBean;

  @Mock
  private QueryResponse queryResponse;

  @Mock
  private QueryResponse reviewProductsResponse;

  @Mock
  private IntervalFacet intervalFacet;

  @Mock
  private List<IntervalFacet> intervalFacets;
  @Mock
  private IntervalFacet.Count count1;
  @Mock
  private IntervalFacet.Count count2;
  @Mock
  private IntervalFacet.Count count3;
  @Mock
  private IntervalFacet.Count count4;
  @Mock
  private IntervalFacet.Count count5;
  @Mock
  private FacetField facetField1;
  @Mock
  private FacetField facetField2;
  @Mock
  private FacetField facetField3;
  @Mock
  private FacetField.Count count6;
  @Mock
  private FacetField.Count count7;
  @Mock
  private FacetField.Count count8;

  @Mock
  private SolrHistoryCollectionRepository solrHistoryCollectionRepository;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Captor
  private ArgumentCaptor<SummaryFilterServiceRequest> summaryFilterServiceRequestArgumentCaptor;

  private static final String STORE_ID = "store-id";
  private static final String REQUEST_ID = "request-id";
  private static final String PRODUCT_SKU = "Product-Sku";
  private static final String ITEM_SKU = "Item-Sku";
  private static final int FACET_COUNT = 10;
  private static final long COUNT = 10;
  private static final long TOTAL_NUM_FOUND = 100;
  private static final int PAGE = 0;
  private static final int SIZE = 100;
  private static final String ASSIGNED_TO = "assignedTo";
  public static final String AUDIT_TRAIL_ID = "1";
  public static final String AUDIT_TRAIL_ID1 = "2";
  private static final String KEYWORD = "keyword";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PICKUP_POINT_NAME = "pickupPointName";
  private static final String INVALID_PRODUCT_SKU = "SKU-1235-90/23-10/90$";
  private static final String VALID_PRODUCT_SKU = "SKU-1235-9023-1090";

  private SolrDocumentList solrDocumentList;
  private ProductCollectionCountResponse productCollectionCountResponse;
  private SummaryFilterRequest summaryFilterRequest;
  private SolrDocumentList reviewProductsList;
  private SolrDocumentList businessPartnersList;
  private SolrDocumentList assigneeList;
  private Page<ProductCollection> productCollectionPage;
  private HistoryRequest historyRequest;
  private HistoryUpdateRequest historyUpdateRequest = new HistoryUpdateRequest();
  private HistoryUpdateRequest historyBeforeOneMonthUpdateRequest = new HistoryUpdateRequest();
  private HistoryUpdateResponse historyUpdateResponse = new HistoryUpdateResponse();
  private BusinessPartnerPickupPointResponse businessPartnerPickupPointResponse =
    new BusinessPartnerPickupPointResponse();

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    solrDocumentList = new SolrDocumentList();
    solrDocumentList.setNumFound(TOTAL_NUM_FOUND);
    productCollectionCountResponse = new ProductCollectionCountResponse(COUNT, COUNT, COUNT, COUNT, COUNT);
    summaryFilterRequest = new SummaryFilterRequest();
    summaryFilterRequest.setStatusFilter(StatusFilterType.ASSIGNED.getStatusFilterType());
    reviewProductsList = new SolrDocumentList();
    SolrDocument document = new SolrDocument();
    document.addField(SolrFieldNames.PRODUCT_NAME, SolrFieldNames.PRODUCT_NAME);
    document.addField(SolrFieldNames.PRODUCT_ID, SolrFieldNames.PRODUCT_ID);
    document.addField(SolrFieldNames.PRODUCT_CODE, SolrFieldNames.PRODUCT_CODE);
    document.addField(SolrFieldNames.BUSINESS_PARTNER_NAME, SolrFieldNames.BUSINESS_PARTNER_NAME);
    document.addField(SolrFieldNames.BUSINESS_PARTNER_CODE, SolrFieldNames.BUSINESS_PARTNER_CODE);
    document.addField(SolrFieldNames.CATEGORY_CODES, Arrays.asList(SolrFieldNames.CATEGORY_CODES));
    document.addField(SolrFieldNames.CATEGORY_NAMES, Arrays.asList(SolrFieldNames.CATEGORY_NAMES));
    document.addField(SolrFieldNames.UPDATED_DATE, new Date());
    document.addField(SolrFieldNames.SUBMITTED_DATE, new Date());
    document.addField(SolrFieldNames.CREATED_DATE, new Date());
    document.addField(SolrFieldNames.CREATED_BY, SolrFieldNames.CREATED_BY);
    document.addField(SolrFieldNames.BRAND, SolrFieldNames.BRAND);
    document.addField(SolrFieldNames.ASSIGNED_TO, SolrFieldNames.ASSIGNED_TO);
    document.addField(SolrFieldNames.STATE, SolrFieldNames.STATE);
    document.addField(SolrFieldNames.ID, SolrFieldNames.ID);
    reviewProductsList.add(document);
    reviewProductsList.setNumFound(TOTAL_NUM_FOUND);
    businessPartnersList = new SolrDocumentList();
    businessPartnersList.setNumFound(TOTAL_NUM_FOUND);
    SolrDocument businessPartner = new SolrDocument();
    businessPartner.addField(SolrFieldNames.BUSINESS_PARTNER_NAME, SolrFieldNames.BUSINESS_PARTNER_NAME);
    businessPartner.addField(SolrFieldNames.BUSINESS_PARTNER_CODE, SolrFieldNames.BUSINESS_PARTNER_CODE);
    businessPartnersList.add(businessPartner);
    assigneeList = new SolrDocumentList();
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.addField(SolrFieldNames.ASSIGNED_TO, SolrConstants.ASSIGNED_TO_PREFIX);
    assigneeList.add(solrDocument);
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode(PRODUCT_CODE);
    productCollection.setStoreId(DEFAULT_STORE_ID);
    productCollection.setCategoryCode(CATEGORY_CODE);
    productCollection.setBusinessPartnerName(SolrFieldNames.BUSINESS_PARTNER_NAME);
    List<ProductCollection> productCollections = Arrays.asList(productCollection);
    productCollectionPage = new PageImpl<>(productCollections, PageRequest.of(PAGE, SIZE), TOTAL_NUM_FOUND);

    historyRequest = new HistoryRequest(PRODUCT_SKU, null, ITEM_SKU, new Date(), new Date(), false);

    historyUpdateRequest =
      HistoryUpdateRequest.builder().productSku(PRODUCT_SKU).beforeOneMonths(false)
        .startDate(new Date()).endDate(new Date()).keyword(KEYWORD)
        .pickupPointCode(PICKUP_POINT_CODE).build();

    historyBeforeOneMonthUpdateRequest =
      HistoryUpdateRequest.builder().productSku(PRODUCT_SKU).beforeOneMonths(true).keyword(KEYWORD)
      .pickupPointCode(PICKUP_POINT_CODE).build();

    Calendar startDate = Calendar.getInstance();
    startDate.add(Calendar.MONTH, -1);
    historyBeforeOneMonthUpdateRequest.setEndDate(startDate.getTime());
    startDate = Calendar.getInstance();
    startDate.add(Calendar.MONTH, -11);
    historyBeforeOneMonthUpdateRequest.setStartDate(startDate.getTime());
    historyUpdateResponse.setPickupPointCode(PICKUP_POINT_CODE);
    businessPartnerPickupPointResponse.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPointResponse.setName(PICKUP_POINT_NAME);

    Mockito.when(this.xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(
        Collections.singletonList(PICKUP_POINT_CODE)))
      .thenReturn(Collections.singletonList(businessPartnerPickupPointResponse));

    Mockito.when(this.productCollectionRepository.findByStoreIdAndProductCodeIn(eq(DEFAULT_STORE_ID),
        anyList())).thenReturn(productCollections);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(productCollectionRepository);
    verifyNoMoreInteractions(solrReviewProductCollectionRepositoryBean);
    verifyNoMoreInteractions(queryResponse, intervalFacets, intervalFacet, count1, count2, count3, count4, count5,
        count6, count7, facetField1, facetField2, reviewProductsResponse, facetField3, count8);
    verifyNoMoreInteractions(xProductOutbound);
  }

  @Test
  public void getFilterCountsByStoreIdAndActivatedAndViewableTest() throws IOException, SolrServerException, Exception {
    when(this.solrReviewProductCollectionRepositoryBean
        .getFilterCountByStoreIdAndActivatedAndViewable(DEFAULT_STORE_ID, Boolean.FALSE, Boolean.FALSE))
        .thenReturn(queryResponse);
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getIntervalFacets()).thenReturn(intervalFacets);
    when(this.queryResponse.getFacetField(SolrFieldNames.ASSIGNED_TO)).thenReturn(facetField1);
    when(this.queryResponse.getFacetField(SolrFieldNames.RESUBMIT_COUNT)).thenReturn(facetField2);
    when(this.queryResponse.getFacetField(SolrFieldNames.BRAND_APPROVED)).thenReturn(facetField3);
    when(this.intervalFacets.get(0)).thenReturn(intervalFacet);
    when(this.intervalFacet.getIntervals()).thenReturn(Arrays.asList(count1, count2, count3, count4, count5));
    when(this.count1.getCount()).thenReturn(FACET_COUNT);
    when(this.count2.getCount()).thenReturn(FACET_COUNT);
    when(this.count3.getCount()).thenReturn(FACET_COUNT);
    when(this.count4.getCount()).thenReturn(FACET_COUNT);
    when(this.count5.getCount()).thenReturn(FACET_COUNT);
    when(this.count1.getKey()).thenReturn(SolrConstants.TODAY_FACET_INTERVAL);
    when(this.count2.getKey()).thenReturn(SolrConstants.YESTERDAY_FACET_INTERVAL);
    when(this.count3.getKey()).thenReturn(SolrConstants.TWO_DAYS_AGO_FACET_INTERVAL);
    when(this.count4.getKey()).thenReturn(SolrConstants.THREE_TO_FIVE_DAYS_AGO_FACET_INTERVAL);
    when(this.count5.getKey()).thenReturn(SolrConstants.FIVE_DAYS_AGO_FACET_INTERVAL);
    when(this.facetField1.getName()).thenReturn(SolrFieldNames.ASSIGNED_TO);
    when(this.facetField2.getName()).thenReturn(SolrFieldNames.RESUBMIT_COUNT);
    when(this.facetField3.getName()).thenReturn(SolrFieldNames.BRAND_APPROVED);
    when(this.facetField1.getValues()).thenReturn(Arrays.asList(count6));
    when(this.facetField2.getValues()).thenReturn(Arrays.asList(count7));
    when(this.facetField3.getValues()).thenReturn(Arrays.asList(count8));
    when(this.count6.getName()).thenReturn(SolrConstants.ASSIGNED_TO_PREFIX);
    when(this.count7.getName()).thenReturn(SolrConstants.RESUBMITTED_COUNT_ZERO);
    when(this.count8.getName()).thenReturn(String.valueOf(Boolean.TRUE));
    when(this.count6.getCount()).thenReturn(COUNT);
    when(this.count7.getCount()).thenReturn(COUNT);
    when(this.count8.getCount()).thenReturn(COUNT);
    FilterCountResponse response = this.summaryFilterService
        .getFilterCountsByStoreIdAndActivatedAndViewable(DEFAULT_STORE_ID, Boolean.FALSE, Boolean.FALSE);
    verify(this.solrReviewProductCollectionRepositoryBean)
        .getFilterCountByStoreIdAndActivatedAndViewable(DEFAULT_STORE_ID, Boolean.FALSE, Boolean.FALSE);
    verify(this.queryResponse).getResults();
    verify(this.queryResponse).getIntervalFacets();
    verify(this.queryResponse).getFacetField(SolrFieldNames.ASSIGNED_TO);
    verify(this.queryResponse).getFacetField(SolrFieldNames.RESUBMIT_COUNT);
    verify(this.intervalFacets).get(0);
    verify(this.intervalFacet).getIntervals();
    verify(this.count1).getCount();
    verify(this.count2).getCount();
    verify(this.count3).getCount();
    verify(this.count4).getCount();
    verify(this.count5).getCount();
    verify(this.count1).getKey();
    verify(this.count2).getKey();
    verify(this.count3).getKey();
    verify(this.count4).getKey();
    verify(this.count5).getKey();
    verify(this.facetField1).getValues();
    verify(this.facetField2).getValues();
    verify(this.count6).getName();
    verify(this.count7).getName();
    verify(this.count6).getCount();
    verify(this.count7).getCount();
    verify(this.queryResponse).getFacetField(SolrFieldNames.BRAND_APPROVED);
    verify(this.facetField3).getValues();
    verify(this.count8).getCount();
    verify(this.count8).getName();
    Assertions.assertNotNull(response);
    Assertions.assertFalse(response.isSourceDb());
    Assertions.assertEquals(FACET_COUNT, response.getToday());
    Assertions.assertEquals(FACET_COUNT, response.getYesterday());
    Assertions.assertEquals(FACET_COUNT, response.getMoreThanFiveDaysAgo());
    Assertions.assertEquals(FACET_COUNT, response.getThreeToFiveDays());
    Assertions.assertEquals(FACET_COUNT, response.getTwoDaysAgo());
    Assertions.assertEquals(FACET_COUNT, response.getUnassigned());
    Assertions.assertEquals(90, response.getAssigned());
    Assertions.assertEquals(90, response.getRevised());
    Assertions.assertEquals(FACET_COUNT, response.getBrandApproved());
    Assertions.assertEquals(90, response.getBrandNotApproved());
  }

  @Test
  public void getReviewProductsByFilterRequestAndActivatedAndViewableFlagTest()
      throws IOException, SolrServerException, Exception {
    when(this.solrReviewProductCollectionRepositoryBean
        .getReviewProductsByFilterRequestAndActivatedAndViewable(eq(DEFAULT_STORE_ID),
            any(SummaryFilterServiceRequest.class), eq(Boolean.FALSE), eq(Boolean.FALSE), eq(PAGE), eq(SIZE)))
        .thenReturn(reviewProductsList);
    Page<ReviewProductResponse> responsePage = this.summaryFilterService
        .getReviewProductsByFilterRequestAndActivatedAndViewableFlag(DEFAULT_STORE_ID, summaryFilterRequest,
            Boolean.FALSE, Boolean.FALSE, PAGE, SIZE);
    verify(this.solrReviewProductCollectionRepositoryBean)
        .getReviewProductsByFilterRequestAndActivatedAndViewable(eq(DEFAULT_STORE_ID),
            summaryFilterServiceRequestArgumentCaptor.capture(), eq(Boolean.FALSE), eq(Boolean.FALSE), eq(PAGE),
            eq(SIZE));
    Assertions.assertNotNull(responsePage);
    Assertions.assertTrue(responsePage.getContent().size() == 1);
    Assertions.assertTrue(responsePage.getContent().get(0).getProductName().equals(SolrFieldNames.PRODUCT_NAME));
    Assertions.assertEquals(StatusFilterType.ASSIGNED,
        summaryFilterServiceRequestArgumentCaptor.getValue().getStatusFilter());
  }

  @Test
  public void getReviewProductsByFilterRequestAndActivatedAndViewableFlagExceptionTest()
      throws IOException, SolrServerException {
    when(this.solrReviewProductCollectionRepositoryBean
        .getReviewProductsByFilterRequestAndActivatedAndViewable(eq(DEFAULT_STORE_ID),
            any(SummaryFilterServiceRequest.class), eq(Boolean.FALSE), eq(Boolean.FALSE), eq(PAGE),
            eq(SIZE))).thenThrow(RuntimeException.class);
    Page<ReviewProductResponse> responsePage = null;
    try {
      responsePage = this.summaryFilterService
          .getReviewProductsByFilterRequestAndActivatedAndViewableFlag(DEFAULT_STORE_ID, summaryFilterRequest,
              Boolean.FALSE, Boolean.FALSE, PAGE, SIZE);
    } catch (Exception e) {
    } finally {
      verify(this.solrReviewProductCollectionRepositoryBean)
          .getReviewProductsByFilterRequestAndActivatedAndViewable(eq(DEFAULT_STORE_ID),
              summaryFilterServiceRequestArgumentCaptor.capture(), eq(Boolean.FALSE), eq(Boolean.FALSE), eq(PAGE)
              , eq(SIZE));
      Assertions.assertNull(responsePage);
      Assertions.assertEquals(StatusFilterType.ASSIGNED, summaryFilterServiceRequestArgumentCaptor.getValue().getStatusFilter());
    }
  }

  @Test
  public void getBusinessPartnersByTimeAndStatusFilterTest() throws IOException, SolrServerException,Exception {
    SummaryFilterServiceRequest summaryFilterServiceRequest =
        SummaryFilterServiceRequest.builder().statusFilter(StatusFilterType.ALL).timeFilter(TimeFilterType.ALL)
            .searchKeyword(StringUtils.EMPTY).build();
    when(this.solrReviewProductCollectionRepositoryBean
        .getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(eq(DEFAULT_STORE_ID),
            any(SummaryFilterServiceRequest.class), eq(Boolean.FALSE), eq(Boolean.FALSE), eq(PAGE), eq(SIZE)))
        .thenReturn(businessPartnersList);
    Page<ProductBusinessPartnerMapperResponse> responsePage = this.summaryFilterService
        .getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(DEFAULT_STORE_ID, summaryFilterServiceRequest, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE);
    verify(this.solrReviewProductCollectionRepositoryBean)
        .getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(eq(DEFAULT_STORE_ID),
            summaryFilterServiceRequestArgumentCaptor.capture(), eq(Boolean.FALSE), eq(Boolean.FALSE), eq(PAGE),
            eq(SIZE));
    Assertions.assertNotNull(responsePage);
    Assertions.assertTrue(responsePage.getContent().size() == 1);
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_CODE, responsePage.getContent().get(0).getBusinessPartnerCode());
    Assertions.assertEquals(SolrFieldNames.BUSINESS_PARTNER_NAME, responsePage.getContent().get(0).getBusinessPartnerName());
  }

  @Test
  public void getAssigneeByTimeAndStatusFilterTest() throws IOException, SolrServerException, Exception {
    SummaryFilterServiceRequest serviceRequest = SummaryFilterServiceRequest.builder()
        .statusFilter(StatusFilterType.getStatusFilterTypeByValue(SolrConstants.ALL))
        .timeFilter(TimeFilterType.getTimeFilterTypeByValue(SolrConstants.ALL)).build();
    when(this.solrReviewProductCollectionRepositoryBean
        .getAssigneeListByFilterRequestAndActivatedAndViewableFlag(DEFAULT_STORE_ID, serviceRequest, Boolean.FALSE,
            Boolean.FALSE)).thenReturn(assigneeList);
    List<AssigneeResponse> response = this.summaryFilterService
        .getAssigneeListByFilterRequestAndActivatedAndViewableFlag(DEFAULT_STORE_ID, serviceRequest, Boolean.FALSE,
            Boolean.FALSE);
    verify(this.solrReviewProductCollectionRepositoryBean)
        .getAssigneeListByFilterRequestAndActivatedAndViewableFlag(DEFAULT_STORE_ID, serviceRequest, Boolean.FALSE,
            Boolean.FALSE);
    Assertions.assertNotNull(response);
    Assertions.assertTrue(response.size() == 1);
    Assertions.assertEquals(SolrConstants.ASSIGNED_TO_PREFIX, response.get(0).getAssignee());
  }

  @Test
  public void getBusinessPartnersByTimeAndStatusFilterExceptionTest() throws IOException, SolrServerException {
    SummaryFilterServiceRequest summaryFilterServiceRequest =
        SummaryFilterServiceRequest.builder().statusFilter(StatusFilterType.ALL).timeFilter(TimeFilterType.ALL)
            .searchKeyword(StringUtils.EMPTY).build();
    when(this.solrReviewProductCollectionRepositoryBean
        .getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(eq(Constants.DEFAULT_STORE_ID),
            any(SummaryFilterServiceRequest.class), eq(Boolean.FALSE), eq(Boolean.FALSE), eq(PAGE), eq(SIZE)))
        .thenThrow(RuntimeException.class);
    Page<ProductBusinessPartnerMapperResponse> responsePage = null;
    try {
      responsePage = this.summaryFilterService.getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID,
          summaryFilterServiceRequest, Boolean.FALSE, Boolean.FALSE, PAGE, SIZE);
    } catch (Exception e) {
    } finally {
      verify(this.solrReviewProductCollectionRepositoryBean).getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(eq(Constants.DEFAULT_STORE_ID),
          summaryFilterServiceRequestArgumentCaptor.capture(), eq(Boolean.FALSE), eq(Boolean.FALSE), eq(PAGE), eq(SIZE));
      Assertions.assertNull(responsePage);
    }
  }

  @Test
  public void getVariantHistoryByProductSkuAndKeywordTest() throws IOException, SolrServerException {
    when(solrHistoryCollectionRepository
        .findProductHistoryByProductSkuAndKeyword(STORE_ID, historyRequest, PAGE, SIZE))
        .thenReturn(getVariantList());
    when(updatedProductHistoryService
        .getProductEditHistoryByAuditTrailId(Arrays.asList(AUDIT_TRAIL_ID, AUDIT_TRAIL_ID1), PAGE, SIZE, 2))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    Page<HistoryResponse> variantHistoryResponses =
        summaryFilterService.getProductHistoryByProductSkuAndKeyword(STORE_ID, historyRequest, PAGE, SIZE);
    verify(solrHistoryCollectionRepository)
        .findProductHistoryByProductSkuAndKeyword(STORE_ID, historyRequest, PAGE, SIZE);
    verify(updatedProductHistoryService)
        .getProductEditHistoryByAuditTrailId(Arrays.asList(AUDIT_TRAIL_ID, AUDIT_TRAIL_ID1), PAGE, SIZE, 2);
  }

  @Test
  public void getVariantHistoryByProductSkuAndKeywordExceptionTest() throws IOException, SolrServerException {
    when(solrHistoryCollectionRepository
        .findProductHistoryByProductSkuAndKeyword(STORE_ID, historyRequest, PAGE, SIZE))
        .thenThrow(SolrServerException.class);
    when(updatedProductHistoryService
        .getProductEditHistoryByAuditTrailId(new ArrayList<>(), PAGE, SIZE, 0))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    Page<HistoryResponse> variantHistoryResponses =
        summaryFilterService.getProductHistoryByProductSkuAndKeyword(STORE_ID, historyRequest, PAGE, SIZE);
    verify(solrHistoryCollectionRepository)
        .findProductHistoryByProductSkuAndKeyword(STORE_ID, historyRequest, PAGE, SIZE);
    verify(updatedProductHistoryService)
        .getProductEditHistoryByAuditTrailId(new ArrayList<>(), PAGE, SIZE, 0);
  }

  private SolrDocumentList getVariantList() {
    SolrDocumentList variantHistoryList = new SolrDocumentList();
    SolrDocument variantHistory1 = new SolrDocument();
    variantHistory1.setField(SolrFieldNames.ID, "1");
    SolrDocument variantHistory2 = new SolrDocument();
    variantHistory2.setField(SolrFieldNames.ID,"2");
    variantHistoryList.add(variantHistory1);
    variantHistoryList.add(variantHistory2);
    variantHistoryList.setNumFound(2);
    return variantHistoryList;
  }

  @Test
  public void getVariantHistoryByProductSkuAndKeywordFromDBTest() {
    when(updatedProductHistoryService.findProductHistoryByProductSkuAndKeyword(historyRequest, PAGE, SIZE))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    historyRequest.setBeforeThreeMonths(true);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.MONTH, -8);
    historyRequest.setStartDate(calendar.getTime());
    calendar = Calendar.getInstance();
    calendar.add(Calendar.MONTH, -4);
    historyRequest.setEndDate(calendar.getTime());
    Page<HistoryResponse> variantHistoryResponses =
        summaryFilterService.getProductHistoryByProductSkuAndKeyword(STORE_ID, historyRequest, PAGE, SIZE);
    verify(updatedProductHistoryService).findProductHistoryByProductSkuAndKeyword(historyRequest, PAGE, SIZE);
  }


  @Test
  public void getVariantHistoryByProductSkuAndKeywordFromDBEmptyDateTest() {
    when(updatedProductHistoryService.findProductHistoryByProductSkuAndKeyword(historyRequest, PAGE, SIZE))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    historyRequest.setBeforeThreeMonths(true);
    historyRequest.setStartDate(null);
    historyRequest.setEndDate(null);
    Page<HistoryResponse> variantHistoryResponses =
        summaryFilterService.getProductHistoryByProductSkuAndKeyword(STORE_ID, historyRequest, PAGE, SIZE);
    verify(updatedProductHistoryService).findProductHistoryByProductSkuAndKeyword(historyRequest, PAGE, SIZE);
  }

  @Test
  public void getProductUpdateHistoryByRequestTest() throws Exception {
    Mockito.when(this.solrHistoryCollectionRepository.findProductUpdateHistoryByRequest(STORE_ID,
      historyUpdateRequest, PAGE, SIZE)).thenReturn(solrDocumentList);
    Mockito.when(this.updatedProductHistoryService.getProductUpdateHistoryByAuditTrailId(
        anyList(), eq(PAGE), eq(SIZE), eq(solrDocumentList.getNumFound())))
      .thenReturn(new PageImpl<>(Collections.singletonList(historyUpdateResponse)));
    Page<HistoryUpdateResponse> response =
      summaryFilterService.getProductUpdateHistoryByRequest(STORE_ID, REQUEST_ID,
        historyUpdateRequest, PAGE, SIZE);
    Mockito.verify(this.updatedProductHistoryService)
      .getProductUpdateHistoryByAuditTrailId(anyList(), eq(PAGE), eq(SIZE),
        eq(solrDocumentList.getNumFound()));
    Mockito.verify(this.solrHistoryCollectionRepository)
      .findProductUpdateHistoryByRequest(STORE_ID, historyUpdateRequest, PAGE, SIZE);
    Mockito.verify(this.xProductOutbound)
      .getPickupPointDetailsByListOfPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    Assertions.assertEquals(PICKUP_POINT_NAME, response.getContent().get(0).getPickupPointName());
  }

  @Test
  public void getProductUpdateHistoryByRequest_withInvalidSkuTest() throws Exception {
    historyUpdateRequest.setProductSku(INVALID_PRODUCT_SKU);
    Mockito.when(this.solrHistoryCollectionRepository.findProductUpdateHistoryByRequest(STORE_ID,
      historyUpdateRequest, PAGE, SIZE)).thenReturn(solrDocumentList);
    Mockito.when(this.updatedProductHistoryService.getProductUpdateHistoryByAuditTrailId(
        anyList(), eq(PAGE), eq(SIZE), eq(solrDocumentList.getNumFound())))
      .thenReturn(new PageImpl<>(Collections.singletonList(historyUpdateResponse)));
    Page<HistoryUpdateResponse> response =
      summaryFilterService.getProductUpdateHistoryByRequest(STORE_ID, REQUEST_ID,
        historyUpdateRequest, PAGE, SIZE);
    Mockito.verify(this.updatedProductHistoryService)
      .getProductUpdateHistoryByAuditTrailId(anyList(), eq(PAGE), eq(SIZE),
        eq(solrDocumentList.getNumFound()));
    Mockito.verify(this.solrHistoryCollectionRepository)
      .findProductUpdateHistoryByRequest(STORE_ID, historyUpdateRequest, PAGE, SIZE);
    Mockito.verify(this.xProductOutbound)
      .getPickupPointDetailsByListOfPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    Assertions.assertEquals(PICKUP_POINT_NAME, response.getContent().get(0).getPickupPointName());
    Assertions.assertEquals(VALID_PRODUCT_SKU, historyUpdateRequest.getProductSku());
  }

  @Test
  public void getProductUpdateHistoryByRequest_emptyProductSkuTest() throws Exception {
    historyUpdateRequest.setProductSku(null);
    Mockito.when(this.solrHistoryCollectionRepository.findProductUpdateHistoryByRequest(STORE_ID,
      historyUpdateRequest, PAGE, SIZE)).thenReturn(solrDocumentList);
    Mockito.when(this.updatedProductHistoryService.getProductUpdateHistoryByAuditTrailId(
        anyList(), eq(PAGE), eq(SIZE), eq(solrDocumentList.getNumFound())))
      .thenReturn(new PageImpl<>(Collections.singletonList(historyUpdateResponse)));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      Page<HistoryUpdateResponse> response =
          summaryFilterService.getProductUpdateHistoryByRequest(STORE_ID, REQUEST_ID,
              historyUpdateRequest, PAGE, SIZE);
    });
  }

  @Test
  public void getProductUpdateHistoryByRequest_emptyPickupPointCodeTest() throws Exception {
    historyUpdateRequest.setPickupPointCode(null);
    Mockito.when(this.solrHistoryCollectionRepository.findProductUpdateHistoryByRequest(STORE_ID,
      historyUpdateRequest, PAGE, SIZE)).thenReturn(solrDocumentList);
    Mockito.when(this.updatedProductHistoryService.getProductUpdateHistoryByAuditTrailId(
        anyList(), eq(PAGE), eq(SIZE), eq(solrDocumentList.getNumFound())))
      .thenReturn(new PageImpl<>(Collections.singletonList(historyUpdateResponse)));
    Page<HistoryUpdateResponse> response =
      summaryFilterService.getProductUpdateHistoryByRequest(STORE_ID, REQUEST_ID,
        historyUpdateRequest, PAGE, SIZE);
    Mockito.verify(this.updatedProductHistoryService)
      .getProductUpdateHistoryByAuditTrailId(anyList(), eq(PAGE), eq(SIZE),
        eq(solrDocumentList.getNumFound()));
    Mockito.verify(this.solrHistoryCollectionRepository)
      .findProductUpdateHistoryByRequest(STORE_ID, historyUpdateRequest, PAGE, SIZE);
    Mockito.verify(this.xProductOutbound)
      .getPickupPointDetailsByListOfPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    Assertions.assertEquals(PICKUP_POINT_NAME, response.getContent().get(0).getPickupPointName());
  }

  @Test
  public void getProductUpdateHistoryByRequest_emptyRequestTest() throws Exception {
    historyUpdateRequest.setProductSku(null);
    historyUpdateRequest.setPickupPointCode(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      summaryFilterService.getProductUpdateHistoryByRequest(STORE_ID, REQUEST_ID,
          historyUpdateRequest, PAGE, SIZE);
    });
  }

  @Test
  public void getProductUpdateHistoryByRequest_solrExceptionTest() throws Exception {
    Mockito.when(this.solrHistoryCollectionRepository.findProductUpdateHistoryByRequest(STORE_ID,
      historyUpdateRequest, PAGE, SIZE)).thenThrow(SolrServerException.class);
    Mockito.when(this.updatedProductHistoryService.getProductUpdateHistoryByAuditTrailId(
      Collections.emptyList(), PAGE, SIZE, 0)).thenReturn(new PageImpl<>(Collections.emptyList()));
    summaryFilterService.getProductUpdateHistoryByRequest(STORE_ID, REQUEST_ID,
      historyUpdateRequest, PAGE, SIZE);
    Mockito.verify(this.updatedProductHistoryService)
      .getProductUpdateHistoryByAuditTrailId(anyList(), eq(PAGE), eq(SIZE),
        eq(Long.valueOf(0)));
    Mockito.verify(this.solrHistoryCollectionRepository)
      .findProductUpdateHistoryByRequest(STORE_ID, historyUpdateRequest, PAGE, SIZE);
  }

  @Test
  public void getProductUpdateHistoryByRequest_beforeOneMonthTest() throws Exception {
    Mockito.when(this.updatedProductHistoryService.findProductUpdateHistoryByProductSkuAndKeyword(
        historyBeforeOneMonthUpdateRequest, PAGE, SIZE))
      .thenReturn(new PageImpl<>(Collections.singletonList(historyUpdateResponse)));
    Page<HistoryUpdateResponse> response =
      summaryFilterService.getProductUpdateHistoryByRequest(STORE_ID, REQUEST_ID,
        historyBeforeOneMonthUpdateRequest, PAGE, SIZE);
    Mockito.verify(this.updatedProductHistoryService)
      .findProductUpdateHistoryByProductSkuAndKeyword(historyBeforeOneMonthUpdateRequest, PAGE,
        SIZE);
    Mockito.verify(this.xProductOutbound)
      .getPickupPointDetailsByListOfPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    Assertions.assertEquals(PICKUP_POINT_NAME, response.getContent().get(0).getPickupPointName());
  }

  @Test
  public void getProductUpdateHistoryByRequest_beforeOneMonth_nullStartDateTest() throws Exception {
    historyBeforeOneMonthUpdateRequest.setStartDate(null);
    Mockito.when(this.updatedProductHistoryService.findProductUpdateHistoryByProductSkuAndKeyword(
        historyBeforeOneMonthUpdateRequest, PAGE, SIZE))
      .thenReturn(new PageImpl<>(Collections.singletonList(historyUpdateResponse)));
    Page<HistoryUpdateResponse> response =
      summaryFilterService.getProductUpdateHistoryByRequest(STORE_ID, REQUEST_ID,
        historyBeforeOneMonthUpdateRequest, PAGE, SIZE);
    Mockito.verify(this.updatedProductHistoryService)
      .findProductUpdateHistoryByProductSkuAndKeyword(historyBeforeOneMonthUpdateRequest, PAGE,
        SIZE);
    Mockito.verify(this.xProductOutbound)
      .getPickupPointDetailsByListOfPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    Assertions.assertEquals(PICKUP_POINT_NAME, response.getContent().get(0).getPickupPointName());
  }

  @Test
  public void getProductUpdateHistoryByRequest_beforeOneMonth_nullEndDateTest() throws Exception {
    historyBeforeOneMonthUpdateRequest.setEndDate(null);
    Mockito.when(this.updatedProductHistoryService.findProductUpdateHistoryByProductSkuAndKeyword(
        historyBeforeOneMonthUpdateRequest, PAGE, SIZE))
      .thenReturn(new PageImpl<>(Collections.singletonList(historyUpdateResponse)));
    Page<HistoryUpdateResponse> response =
      summaryFilterService.getProductUpdateHistoryByRequest(STORE_ID, REQUEST_ID,
        historyBeforeOneMonthUpdateRequest, PAGE, SIZE);
    Mockito.verify(this.updatedProductHistoryService)
      .findProductUpdateHistoryByProductSkuAndKeyword(historyBeforeOneMonthUpdateRequest, PAGE,
        SIZE);
    Mockito.verify(this.xProductOutbound)
      .getPickupPointDetailsByListOfPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    Assertions.assertEquals(PICKUP_POINT_NAME, response.getContent().get(0).getPickupPointName());
  }

  /*@Test
  public void getProductUpdateHistoryByRequest_withInvalidSkuTest() throws Exception {
    historyUpdateRequest.setProductSku(INVALID_PRODUCT_SKU);
    Mockito.when(this.solrHistoryCollectionRepository.findProductUpdateHistoryByRequest(STORE_ID,
      historyUpdateRequest, false, PAGE, SIZE)).thenReturn(solrDocumentList);
    Mockito.when(this.updatedProductHistoryService.getProductUpdateHistoryByAuditTrailId(
      Mockito.anyListOf(String.class), eq(PAGE), eq(SIZE), eq(solrDocumentList.getNumFound())))
      .thenReturn(new PageImpl<>(Collections.singletonList(historyUpdateResponse)));
    Page<HistoryUpdateResponse> response =
      summaryFilterService.getProductUpdateHistoryByRequest(STORE_ID, REQUEST_ID,
        historyUpdateRequest, PAGE, SIZE);
    Mockito.verify(this.updatedProductHistoryService)
      .getProductUpdateHistoryByAuditTrailId(Mockito.anyListOf(String.class), eq(PAGE), eq(SIZE),
        eq(solrDocumentList.getNumFound()));
    Mockito.verify(this.solrHistoryCollectionRepository)
      .findProductUpdateHistoryByRequest(STORE_ID, historyUpdateRequest, false, PAGE, SIZE);
    Mockito.verify(this.xProductOutbound)
      .getPickupPointDetailsByListOfPickupPointCodes(Collections.singletonList(PICKUP_POINT_CODE));
    Assertions.assertEquals(PICKUP_POINT_NAME, response.getContent().get(0).getPickupPointName());
    Assertions.assertEquals(VALID_PRODUCT_SKU, historyUpdateRequest.getProductSku());
  }*/
}