package com.gdn.x.mta.distributiontask.dao.api.impl;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import com.gdn.x.mta.distributiontask.dao.util.CustomBoostQueryBuilder;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.type.BrandApprovalStatus;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
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
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.x.mta.distributiontask.dao.api.ProductServiceRepository;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductUpdateProductToSolrEventModel;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.dto.DistributionTaskMultipleFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.PrimaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.SummaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.enums.VendorProductSolrFieldNames;
import com.gdn.x.mta.distributiontask.model.solr.SolrConstants;
import com.gdn.x.mta.distributiontask.model.solr.VendorProductSolr;
import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;


public class SolrVendorProductCollectionRepositoryImplTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_NAME = "productName";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String IMAGE_VIOLATION = "blur,good";
  private static final String CATEGORY_NAME = "categoryName";
  private static final String VENDOR_CODE = "vendorCode";
  private static final String STORE_ID = "storeId";
  private static final String BRAND = "brand";
  private static final String TWO_TO_THREE_DAYS_OLD = "between2until3daysOld";
  private static final String THREE_DAYS_AGO = "moreThan3Days";
  private static final String ALL = "all";
  private static final int PAGE = 0;
  private static final int SIZE = 100;

  private final VendorProductSolr vendorProductSolr = new VendorProductSolr();
  private final CategoryResponse categoryResponse = new CategoryResponse();
  private PrimaryFilterDTO primaryFilterDTO;
  private final List<WorkflowState> pendingStates = new ArrayList<>(Arrays
      .asList(WorkflowState.IN_REVIEW, WorkflowState.EXCEEDED_SLA, WorkflowState.REJECTED, WorkflowState.QC_REJECTED));
  private final CategoryHierarchyResponse categoryHierarchyResponse = new CategoryHierarchyResponse();
  private SummaryFilterDTO summaryFilterDTO;
  private final Pageable pageable = PageRequest.of(0, 10);
  private final SolrDocumentList solrDocumentList = new SolrDocumentList();
  private final SolrDocumentList solrDocumentListWithOneRecord = new SolrDocumentList();
  private final Product product = new Product();
  private ProductListRequest productListRequest;
  private ProductReviewer productReviewer;

  @InjectMocks
  private SolrVendorProductCollectionRepositoryImpl solrVendorProductCollectionRepository;

  @Mock
  @Qualifier(value = "vendorProductCollectionClient")
  private CloudSolrClient cloudSolrClient;

  @Mock
  private ProductServiceRepository productServiceRepository;

  @Mock
  private CustomBoostQueryBuilder customBoostQueryBuilder;

  private QueryResponse queryResponse;

  @Mock
  private QueryResponse queryResponseMock;

  @Captor
  private ArgumentCaptor<SolrQuery> solrQuery;

  @Captor
  private ArgumentCaptor<String> solrQueryArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<String>> solrQueryArgumentCaptorList;

  @Captor
  private ArgumentCaptor<List<SolrInputDocument>> solrInputDocumentListCaptor;

  @Captor
  private ArgumentCaptor<SolrInputDocument> solrInputDocumentCaptor;

  @Captor
  private ArgumentCaptor<SolrInputDocument> solrInputDocumentArgumentCaptor;

  private static final String vendorCode = "vendorCode";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    queryResponse = new QueryResponse();

    categoryResponse.setCategoryCode(CATEGORY_CODE);
    categoryResponse.setName(CATEGORY_NAME);
    vendorProductSolr.setCategoryCodes(List.of(CATEGORY_CODE));
    vendorProductSolr.setState(WorkflowState.IN_REVIEW);
    vendorProductSolr.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    vendorProductSolr.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    vendorProductSolr.setProductName(PRODUCT_NAME);
    vendorProductSolr.setProductCode(PRODUCT_CODE);
    vendorProductSolr.setReviewType(ReviewType.CONTENT.name());

    categoryHierarchyResponse.setCategoryCode(CATEGORY_CODE);
    categoryHierarchyResponse.setCategoryHierarchy(Collections.singletonList(categoryResponse));
    primaryFilterDTO = new PrimaryFilterDTO();

    summaryFilterDTO =
        SummaryFilterDTO.builder().timeFilterType(TimeFilterType.ALL).vendorCode(VENDOR_CODE)
            .postLive(false).build();

    product.setProductCode(PRODUCT_CODE);
    product.setState(WorkflowState.IN_REVIEW);
    product.setCurrentVendor(new Vendor());
    product.getCurrentVendor().setVendorCode(VENDOR_CODE);
    SolrDocument solrDocument = new SolrDocument();
    solrDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, VendorProductSolrFieldNames.PRODUCT_CODE);
    solrDocumentListWithOneRecord.add(solrDocument);

    productListRequest = new ProductListRequest();
    productListRequest.setCategoryCode(CATEGORY_CODE);
    productListRequest.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    productListRequest.setTimeFilterType(TWO_TO_THREE_DAYS_OLD);
    productListRequest.setProductName(PRODUCT_NAME);

    productReviewer = new ProductReviewer();
    productReviewer.setProductCode(PRODUCT_CODE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(cloudSolrClient);
    Mockito.verifyNoMoreInteractions(productServiceRepository);
  }

  @Test
   void getFilterCounts() throws IOException, SolrServerException {
    when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    QueryResponse queryResponse = this.solrVendorProductCollectionRepository
        .getFilterCounts(Constants.DEFAULT_STORE_ID, vendorCode, Boolean.FALSE, Boolean.FALSE,
          false);
    verify(this.cloudSolrClient).query(solrQuery.capture());
    Assertions.assertNotNull(queryResponse);
    Assertions.assertEquals(solrQuery.getValue().getFacetFields().length, 3);
  }

  @Test
   void getFinalQCCountsTest() throws IOException, SolrServerException {
    when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    QueryResponse queryResponse = solrVendorProductCollectionRepository.getFinalQCCounts(Constants.DEFAULT_STORE_ID);
    Assertions.assertNotNull(queryResponse);
    verify(this.cloudSolrClient).query(solrQuery.capture());
  }

  @Test
   void getDistributionListCountsTest() throws IOException, SolrServerException {
    when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    QueryResponse queryResponse =
        solrVendorProductCollectionRepository.getDistributionListCounts(Constants.DEFAULT_STORE_ID);
    Assertions.assertNotNull(queryResponse);
    verify(this.cloudSolrClient).query(solrQuery.capture());
  }

  @Test
   void getBusinessPartnerList() throws IOException, SolrServerException {
    primaryFilterDTO.setAssignment(true);
    primaryFilterDTO.setBrandPending(true);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.ALL);
    primaryFilterDTO.setContentPending(true);
    primaryFilterDTO.setImagePending(true);
    primaryFilterDTO.setEdited(true);
    primaryFilterDTO.setPostLive(true);
    primaryFilterDTO.setVendorCode(vendorCode);
    primaryFilterDTO.setKeyword(PRODUCT_NAME);
    primaryFilterDTO.setRevised(true);
    primaryFilterDTO.setRestrictedKeyword(true);
    when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    QueryResponse queryResponse = this.solrVendorProductCollectionRepository
        .getBusinessPartnerList(Constants.DEFAULT_STORE_ID, primaryFilterDTO, pendingStates, PAGE, SIZE);
    verify(this.cloudSolrClient).query(solrQuery.capture());
    Assertions.assertNotNull(queryResponse);
    Assertions.assertEquals(solrQuery.getValue().getFilterQueries().length, 10);
    Assertions.assertEquals(solrQuery.getValue().getSortField(),
        VendorProductSolrFieldNames.COPY_BUSINESS_PARTNER_NAME + StringUtils.SPACE + SolrQuery.ORDER.asc);
  }

  @Test
   void getBusinessPartnerListAllFalse() throws IOException, SolrServerException {
    primaryFilterDTO.setAssignment(false);
    primaryFilterDTO.setBrandPending(false);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.FIVE_DAYS_AGO);
    primaryFilterDTO.setContentPending(false);
    primaryFilterDTO.setImagePending(false);
    primaryFilterDTO.setEdited(false);
    primaryFilterDTO.setPostLive(null);
    primaryFilterDTO.setVendorCode(vendorCode);
    primaryFilterDTO.setKeyword(SolrConstants.EXTERNAL);
    primaryFilterDTO.setRevised(false);
    primaryFilterDTO.setRestrictedKeyword(false);
    when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    QueryResponse queryResponse = this.solrVendorProductCollectionRepository
        .getBusinessPartnerList(Constants.DEFAULT_STORE_ID, primaryFilterDTO, pendingStates, PAGE, SIZE);
    verify(this.cloudSolrClient).query(solrQuery.capture());
    Assertions.assertNotNull(queryResponse);
    Assertions.assertEquals(solrQuery.getValue().getFilterQueries().length, 9);
    Assertions.assertEquals(solrQuery.getValue().getSortField(),
        VendorProductSolrFieldNames.COPY_BUSINESS_PARTNER_NAME + StringUtils.SPACE + SolrQuery.ORDER.asc);
  }

  @Test
   void getBusinessPartnerListTodayTest() throws IOException, SolrServerException {
    primaryFilterDTO.setAssignment(false);
    primaryFilterDTO.setBrandPending(false);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.TODAY);
    primaryFilterDTO.setContentPending(false);
    primaryFilterDTO.setImagePending(false);
    primaryFilterDTO.setEdited(false);
    primaryFilterDTO.setPostLive(false);
    primaryFilterDTO.setVendorCode(vendorCode);
    primaryFilterDTO.setKeyword(SolrConstants.EXTERNAL);
    primaryFilterDTO.setRevised(true);
    primaryFilterDTO.setRestrictedKeyword(true);
    when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    QueryResponse queryResponse = this.solrVendorProductCollectionRepository
        .getBusinessPartnerList(Constants.DEFAULT_STORE_ID, primaryFilterDTO, pendingStates, PAGE, SIZE);
    verify(this.cloudSolrClient).query(solrQuery.capture());
    Assertions.assertNotNull(queryResponse);
    Assertions.assertEquals(solrQuery.getValue().getFilterQueries().length, 10);
    Assertions.assertEquals(solrQuery.getValue().getSortField(),
        VendorProductSolrFieldNames.COPY_BUSINESS_PARTNER_NAME + StringUtils.SPACE + SolrQuery.ORDER.asc);
  }

  @Test
   void getBusinessPartnerListYesterdayTest() throws IOException, SolrServerException {
    primaryFilterDTO.setAssignment(false);
    primaryFilterDTO.setBrandPending(false);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.YESTERDAY);
    primaryFilterDTO.setContentPending(false);
    primaryFilterDTO.setImagePending(false);
    primaryFilterDTO.setEdited(false);
    primaryFilterDTO.setPostLive(false);
    primaryFilterDTO.setVendorCode(vendorCode);
    primaryFilterDTO.setKeyword(SolrConstants.EXTERNAL);
    primaryFilterDTO.setRevised(true);
    primaryFilterDTO.setRestrictedKeyword(true);
    when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    QueryResponse queryResponse = this.solrVendorProductCollectionRepository
        .getBusinessPartnerList(Constants.DEFAULT_STORE_ID, primaryFilterDTO, pendingStates, PAGE, SIZE);
    verify(this.cloudSolrClient).query(solrQuery.capture());
    Assertions.assertNotNull(queryResponse);
    Assertions.assertEquals(solrQuery.getValue().getFilterQueries().length, 10);
    Assertions.assertEquals(solrQuery.getValue().getSortField(),
        VendorProductSolrFieldNames.COPY_BUSINESS_PARTNER_NAME + StringUtils.SPACE + SolrQuery.ORDER.asc);
  }

  @Test
   void getBusinessPartnerList2DaysAgoTest() throws IOException, SolrServerException {
    primaryFilterDTO.setAssignment(false);
    primaryFilterDTO.setBrandPending(false);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.TWO_DAYS_AGO);
    primaryFilterDTO.setContentPending(false);
    primaryFilterDTO.setImagePending(false);
    primaryFilterDTO.setEdited(false);
    primaryFilterDTO.setPostLive(false);
    primaryFilterDTO.setVendorCode(vendorCode);
    primaryFilterDTO.setKeyword(SolrConstants.EXTERNAL);
    primaryFilterDTO.setRevised(true);
    primaryFilterDTO.setRestrictedKeyword(true);
    when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    QueryResponse queryResponse = this.solrVendorProductCollectionRepository
        .getBusinessPartnerList(Constants.DEFAULT_STORE_ID, primaryFilterDTO, pendingStates, PAGE, SIZE);
    verify(this.cloudSolrClient).query(solrQuery.capture());
    Assertions.assertNotNull(queryResponse);
    Assertions.assertEquals(solrQuery.getValue().getFilterQueries().length, 10);
    Assertions.assertEquals(solrQuery.getValue().getSortField(),
        VendorProductSolrFieldNames.COPY_BUSINESS_PARTNER_NAME + StringUtils.SPACE + SolrQuery.ORDER.asc);
  }

  @Test
   void getBusinessPartnerList3To5DaysTest() throws IOException, SolrServerException {
    primaryFilterDTO.setAssignment(false);
    primaryFilterDTO.setBrandPending(false);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.THREE_TO_FIVE_DAYS_AGO);
    primaryFilterDTO.setContentPending(false);
    primaryFilterDTO.setImagePending(false);
    primaryFilterDTO.setEdited(false);
    primaryFilterDTO.setPostLive(false);
    primaryFilterDTO.setVendorCode(vendorCode);
    primaryFilterDTO.setKeyword(SolrConstants.EXTERNAL);
    primaryFilterDTO.setRevised(true);
    primaryFilterDTO.setRestrictedKeyword(true);
    when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    QueryResponse queryResponse = this.solrVendorProductCollectionRepository
        .getBusinessPartnerList(Constants.DEFAULT_STORE_ID, primaryFilterDTO, pendingStates, PAGE, SIZE);
    verify(this.cloudSolrClient).query(solrQuery.capture());
    Assertions.assertNotNull(queryResponse);
    Assertions.assertEquals(solrQuery.getValue().getFilterQueries().length, 10);
    Assertions.assertEquals(solrQuery.getValue().getSortField(),
        VendorProductSolrFieldNames.COPY_BUSINESS_PARTNER_NAME + StringUtils.SPACE + SolrQuery.ORDER.asc);
  }

  @Test
   void deleteDocumentFromSolrTest() throws Exception {
    Mockito.when(this.cloudSolrClient.deleteByQuery(Mockito.anyString())).thenReturn(new UpdateResponse());
    solrVendorProductCollectionRepository.deleteDocumentFromSolr(new ArrayList<>(), true);
    Mockito.verify(this.cloudSolrClient).deleteByQuery(solrQueryArgumentCaptor.capture());
    Assertions.assertEquals(SolrConstants.QUERY_PRODUCT_CODE + SolrConstants.LIKE_QUERY,
        solrQueryArgumentCaptor.getValue());
  }

  @Test
   void deleteDocumentFromSolrTest_withProductCode() throws Exception {
    Mockito.when(this.cloudSolrClient.deleteById(Mockito.anyList())).thenReturn(new UpdateResponse());
    solrVendorProductCollectionRepository.deleteDocumentFromSolr(List.of(PRODUCT_CODE), true);
    Mockito.verify(this.cloudSolrClient).deleteById(solrQueryArgumentCaptorList.capture());
    Assertions.assertEquals(List.of(PRODUCT_CODE), solrQueryArgumentCaptorList.getValue());
  }

  @Test
   void deleteDocumentFromSolrTest_withNoQuery() throws Exception {
    Mockito.when(this.cloudSolrClient.deleteById(Mockito.anyString())).thenReturn(new UpdateResponse());
    solrVendorProductCollectionRepository.deleteDocumentFromSolr(new ArrayList<>(), false);
  }

  @Test
   void addDocumentToSolrWithCategoryHierarchyTest() throws Exception {
    Mockito.when(this.cloudSolrClient.add(Mockito.anyList()))
        .thenReturn(new UpdateResponse());
    Mockito.when(this.productServiceRepository
        .getCategoryHierarchyByCategoryCodes(Mockito.any(CategoryCodeRequest.class)))
        .thenReturn(Collections.singletonList(categoryHierarchyResponse));
    vendorProductSolr.setPredictedBrand(BRAND);
    solrVendorProductCollectionRepository
        .addDocumentToSolrWithCategoryHierarchy(List.of(vendorProductSolr));
    Mockito.verify(this.cloudSolrClient).add(solrInputDocumentListCaptor.capture());
    Mockito.verify(this.productServiceRepository)
        .getCategoryHierarchyByCategoryCodes(Mockito.any(CategoryCodeRequest.class));
    Assertions.assertNotNull(solrInputDocumentListCaptor.getValue());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, solrInputDocumentListCaptor.getValue().get(0)
        .getField(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE).getValue());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, solrInputDocumentListCaptor.getValue().get(0)
        .getField(VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME).getValue());
    Assertions.assertEquals(PRODUCT_CODE, solrInputDocumentListCaptor.getValue().get(0)
        .getField(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
    Assertions.assertEquals(PRODUCT_NAME, solrInputDocumentListCaptor.getValue().get(0)
        .getField(VendorProductSolrFieldNames.PRODUCT_NAME).getValue());
    Assertions.assertEquals(BRAND, solrInputDocumentListCaptor.getValue().get(0)
        .getField(VendorProductSolrFieldNames.PREDICTED_BRAND).getValue());
    Assertions.assertEquals(WorkflowState.IN_REVIEW.toString(),
        solrInputDocumentListCaptor.getValue().get(0).getField(VendorProductSolrFieldNames.STATE)
            .getValue());
  }

  @Test
   void addDocumentToSolrWithCategoryHierarchy_nullReviewTypeTest() throws Exception {
    vendorProductSolr.setReviewType(null);
    Mockito.when(this.cloudSolrClient.add(Mockito.anyList()))
      .thenReturn(new UpdateResponse());
    Mockito.when(this.productServiceRepository
        .getCategoryHierarchyByCategoryCodes(Mockito.any(CategoryCodeRequest.class)))
      .thenReturn(Collections.singletonList(categoryHierarchyResponse));
    solrVendorProductCollectionRepository
      .addDocumentToSolrWithCategoryHierarchy(Collections.singletonList(vendorProductSolr));
    Mockito.verify(this.cloudSolrClient).add(solrInputDocumentListCaptor.capture());
    Mockito.verify(this.productServiceRepository)
      .getCategoryHierarchyByCategoryCodes(Mockito.any(CategoryCodeRequest.class));
    Assertions.assertNotNull(solrInputDocumentListCaptor.getValue());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, solrInputDocumentListCaptor.getValue().get(0)
      .getField(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE).getValue());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, solrInputDocumentListCaptor.getValue().get(0)
      .getField(VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME).getValue());
    Assertions.assertEquals(PRODUCT_CODE, solrInputDocumentListCaptor.getValue().get(0)
      .getField(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
    Assertions.assertEquals(PRODUCT_NAME, solrInputDocumentListCaptor.getValue().get(0)
      .getField(VendorProductSolrFieldNames.PRODUCT_NAME).getValue());
    Assertions.assertEquals(WorkflowState.IN_REVIEW.toString(),
        solrInputDocumentListCaptor.getValue().get(0).getField(VendorProductSolrFieldNames.STATE)
          .getValue());
  }

  @Test
   void addDocumentToSolrWithCategoryHierarchy_brandStatusImageViolationsTest() throws Exception {
    vendorProductSolr.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    vendorProductSolr.setImageViolations(IMAGE_VIOLATION);
    vendorProductSolr.setDistributionMappingStatus(2);
    vendorProductSolr.setProductCreationType("PRODUCT_CREATION_TYPE");
    Mockito.when(this.cloudSolrClient.add(Mockito.anyList()))
      .thenReturn(new UpdateResponse());
    Mockito.when(this.productServiceRepository
        .getCategoryHierarchyByCategoryCodes(Mockito.any(CategoryCodeRequest.class)))
      .thenReturn(Collections.singletonList(categoryHierarchyResponse));
    solrVendorProductCollectionRepository
      .addDocumentToSolrWithCategoryHierarchy(Collections.singletonList(vendorProductSolr));
    Mockito.verify(this.cloudSolrClient).add(solrInputDocumentListCaptor.capture());
    Mockito.verify(this.productServiceRepository)
      .getCategoryHierarchyByCategoryCodes(Mockito.any(CategoryCodeRequest.class));
    Assertions.assertNotNull(solrInputDocumentListCaptor.getValue());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, solrInputDocumentListCaptor.getValue().get(0)
      .getField(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE).getValue());
    Assertions.assertEquals(BUSINESS_PARTNER_NAME, solrInputDocumentListCaptor.getValue().get(0)
      .getField(VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME).getValue());
    Assertions.assertEquals(PRODUCT_CODE, solrInputDocumentListCaptor.getValue().get(0)
      .getField(VendorProductSolrFieldNames.PRODUCT_CODE).getValue());
    Assertions.assertEquals(PRODUCT_NAME, solrInputDocumentListCaptor.getValue().get(0)
      .getField(VendorProductSolrFieldNames.PRODUCT_NAME).getValue());
    Assertions.assertEquals(2, solrInputDocumentListCaptor.getValue().get(0)
            .getField(VendorProductSolrFieldNames.DISTRIBUTION_MAPPING_STATUS).getValue());
    Assertions.assertEquals("PRODUCT_CREATION_TYPE", solrInputDocumentListCaptor.getValue().get(0)
        .getField(VendorProductSolrFieldNames.PRODUCT_CREATION_TYPE).getValue());
    Assertions.assertEquals(WorkflowState.IN_REVIEW.toString(),
        solrInputDocumentListCaptor.getValue().get(0).getField(VendorProductSolrFieldNames.STATE)
          .getValue());
  }

  @Test
   void addDocumentToSolrWithCategoryHierarchy_exceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.add(Mockito.anyList()))
        .thenThrow(RuntimeException.class);
    Mockito.when(this.productServiceRepository
        .getCategoryHierarchyByCategoryCodes(Mockito.any(CategoryCodeRequest.class)))
        .thenReturn(Collections.singletonList(categoryHierarchyResponse));
    try {
      solrVendorProductCollectionRepository
          .addDocumentToSolrWithCategoryHierarchy(Collections.singletonList(vendorProductSolr));
    } finally {
      Mockito.verify(this.cloudSolrClient).add(solrInputDocumentListCaptor.capture());
      Mockito.verify(this.productServiceRepository)
          .getCategoryHierarchyByCategoryCodes(Mockito.any(CategoryCodeRequest.class));
    }
  }

  @Test
   void getVendorProductsBySolrQueryTest() throws IOException, SolrServerException {
    solrDocumentList.add(new SolrDocument());
    ReflectionTestUtils
        .setField(queryResponse, "_results", solrDocumentList, SolrDocumentList.class);
    queryResponse.getResults().setNumFound(1);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class)))
        .thenReturn(queryResponse);
    solrVendorProductCollectionRepository.getVendorProductsBySolrQuery(STORE_ID, summaryFilterDTO,
        List.of(WorkflowState.IN_REVIEW), pageable);
    Mockito.verify(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
  }

  @Test
   void getAllProductDetailsBySolrQueryTest() throws IOException, SolrServerException {
    solrDocumentList.add(new SolrDocument());
    ReflectionTestUtils.setField(queryResponse, "_results", solrDocumentList, SolrDocumentList.class);
    queryResponse.getResults().setNumFound(1);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponse);
    solrVendorProductCollectionRepository
        .getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(new DistributionTaskMultipleFilterDTO(), pageable);
    Mockito.verify(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
  }


  @Test
   void getFilterProductSummaryBySolrQueryTest() throws IOException, SolrServerException {
    solrDocumentList.add(new SolrDocument());
    ReflectionTestUtils
        .setField(queryResponse, "_results", solrDocumentList, SolrDocumentList.class);
    queryResponse.getResults().setNumFound(1);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class)))
        .thenReturn(queryResponse);
    solrVendorProductCollectionRepository.getFilterProductSummaryBySolrQuery(STORE_ID, new ProductListRequest(),
        new ArrayList<>(), pageable);
    Mockito.verify(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
  }

  @Test
   void getFilterProductSummaryBySolrQueryWithProductListTest() throws IOException, SolrServerException {
    solrDocumentList.add(new SolrDocument());
    ReflectionTestUtils
        .setField(queryResponse, "_results", solrDocumentList, SolrDocumentList.class);
    queryResponse.getResults().setNumFound(1);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class)))
        .thenReturn(queryResponse);
    solrVendorProductCollectionRepository.getFilterProductSummaryBySolrQuery(STORE_ID, productListRequest,
        List.of(WorkflowState.PASSED), pageable);
    Mockito.verify(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
  }

  @Test
   void getFilterProductSummaryBySolrQueryWithProductList3DaysAgoTest() throws IOException, SolrServerException {
    this.productListRequest.setTimeFilterType(THREE_DAYS_AGO);
    solrDocumentList.add(new SolrDocument());
    ReflectionTestUtils
        .setField(queryResponse, "_results", solrDocumentList, SolrDocumentList.class);
    queryResponse.getResults().setNumFound(1);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class)))
        .thenReturn(queryResponse);
    solrVendorProductCollectionRepository.getFilterProductSummaryBySolrQuery(STORE_ID, productListRequest,
        List.of(WorkflowState.PASSED), pageable);
    Mockito.verify(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
  }

  @Test
   void getFilterProductSummaryBySolrQueryWithProductListTimeALLTest() throws IOException, SolrServerException {
    this.productListRequest.setTimeFilterType(ALL);
    solrDocumentList.add(new SolrDocument());
    ReflectionTestUtils
        .setField(queryResponse, "_results", solrDocumentList, SolrDocumentList.class);
    queryResponse.getResults().setNumFound(1);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class)))
        .thenReturn(queryResponse);
    solrVendorProductCollectionRepository.getFilterProductSummaryBySolrQuery(STORE_ID, productListRequest,
        List.of(WorkflowState.PASSED), pageable);
    Mockito.verify(this.cloudSolrClient).query(Mockito.any(SolrQuery.class));
  }

  @Test
   void executeAtomicUpdateTest() throws Exception {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, PRODUCT_CODE);
    solrInputDocument.setField(VendorProductSolrFieldNames.VENDOR_CODE, VENDOR_CODE);
    Mockito.when(this.cloudSolrClient.add(solrInputDocument)).thenReturn(new UpdateResponse());
    this.solrVendorProductCollectionRepository.executeAtomicUpdate(solrInputDocument);
    verify(cloudSolrClient).add(solrInputDocumentCaptor.capture());
    Assertions.assertTrue(
        solrInputDocumentCaptor.getValue().containsKey(VendorProductSolrFieldNames.PRODUCT_CODE));
    Assertions.assertTrue(
        solrInputDocumentCaptor.getValue().containsKey(VendorProductSolrFieldNames.VENDOR_CODE));
  }

  @Test
   void executeAtomicUpdateForListOfInputDocumentsTest() throws Exception {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.setField(VendorProductSolrFieldNames.PRODUCT_CODE, PRODUCT_CODE);
    solrInputDocument.setField(VendorProductSolrFieldNames.VENDOR_CODE, VENDOR_CODE);
    Mockito.when(this.cloudSolrClient.add(solrInputDocument)).thenReturn(new UpdateResponse());
    this.solrVendorProductCollectionRepository
        .executeAtomicUpdateForListOfInputDocuments(List.of(solrInputDocument));
    verify(cloudSolrClient).add(solrInputDocumentListCaptor.capture());
    Assertions.assertTrue(solrInputDocumentListCaptor.getValue().get(0)
        .containsKey(VendorProductSolrFieldNames.PRODUCT_CODE));
    Assertions.assertTrue(solrInputDocumentListCaptor.getValue().get(0)
        .containsKey(VendorProductSolrFieldNames.VENDOR_CODE));
  }

  @Test
   void executeAtomicUpdateTest_expectException() throws Exception {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    Mockito.when(this.cloudSolrClient.add(solrInputDocument)).thenThrow(SolrServerException.class);
    try {
      this.solrVendorProductCollectionRepository.executeAtomicUpdate(solrInputDocument);
    } finally {
      verify(cloudSolrClient).add(solrInputDocumentCaptor.capture());
    }
  }

  @Test
   void executeAtomicUpdateForListOfInputDocumentsTest_expectException() throws Exception {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    Mockito.when(this.cloudSolrClient.add(List.of(solrInputDocument)))
        .thenThrow(SolrServerException.class);
    try {
      this.solrVendorProductCollectionRepository
          .executeAtomicUpdateForListOfInputDocuments(List.of(solrInputDocument));
    } finally {
      verify(cloudSolrClient).add(solrInputDocumentListCaptor.capture());
    }
  }

  @Test
   void getReviewConfigCountsByVendorTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class)))
        .thenReturn(new QueryResponse());
    solrVendorProductCollectionRepository.getReviewConfigCountsByVendor(STORE_ID, VENDOR_CODE);
    Mockito.verify(this.cloudSolrClient).query(solrQuery.capture());
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(VENDOR_CODE + SolrConstants.COLON + VENDOR_CODE,
        solrQuery.getValue().getFilterQueries()[1]);
    Assertions.assertEquals(VendorProductSolrFieldNames.POST_LIVE,
        solrQuery.getValue().getFacetFields()[0]);
  }

  @Test
   void getReviewConfigCountsByVendorTest_expectException() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class)))
        .thenThrow(SolrServerException.class);
      Assertions.assertThrows(SolrServerException.class,()->
        solrVendorProductCollectionRepository.getReviewConfigCountsByVendor(STORE_ID, VENDOR_CODE));
      Mockito.verify(this.cloudSolrClient).query(solrQuery.capture());
  }

  @Test
   void updateSolrOnContentApprovalOrSave() throws IOException, SolrServerException {
    product.setCategoryCode(CATEGORY_CODE);
    Mockito.when(this.productServiceRepository
        .getCategoryHierarchyByCategoryCodes(Mockito.any(CategoryCodeRequest.class)))
        .thenReturn(Collections.singletonList(categoryHierarchyResponse));
    solrVendorProductCollectionRepository.updateSolrOnContentApprovalOrSave(product, productReviewer, true);
    Mockito.verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
    Mockito.verify(productServiceRepository).getCategoryHierarchyByCategoryCodes(Mockito.any(CategoryCodeRequest.class));
    Assertions.assertNotNull(
        solrInputDocumentArgumentCaptor.getValue().getField(VendorProductSolrFieldNames.CATEGORY_CODES));
  }

  @Test
   void updateSolrOnContentApprovalOrSaveCategoryChangedFalse() throws IOException, SolrServerException {
    product.setCategoryCode(CATEGORY_CODE);
    solrVendorProductCollectionRepository.updateSolrOnContentApprovalOrSave(product, productReviewer, false);
    Mockito.verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
    Assertions.assertNull(
        solrInputDocumentArgumentCaptor.getValue().getField(VendorProductSolrFieldNames.CATEGORY_CODES));
  }

  @Test
   void updateSolrOnApprovalOrSaveTest() throws IOException, SolrServerException {
    PDTProductUpdateProductToSolrEventModel product = new PDTProductUpdateProductToSolrEventModel();
    product.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    product.setCategoryCodes(List.of(CATEGORY_CODE));
    Mockito.when(this.productServiceRepository
            .getCategoryHierarchyByCategoryCodes(Mockito.any(CategoryCodeRequest.class)))
        .thenReturn(Collections.singletonList(categoryHierarchyResponse));
    solrVendorProductCollectionRepository.updateSolrOnApprovalOrSave(product, true);
    Mockito.verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
    Mockito.verify(productServiceRepository).getCategoryHierarchyByCategoryCodes(Mockito.any(CategoryCodeRequest.class));
    Assertions.assertNotNull(
        solrInputDocumentArgumentCaptor.getValue().getField(VendorProductSolrFieldNames.CATEGORY_CODES));
  }

  @Test
   void updateSolrOnApprovalOrSaveCategoryChangedFalseTest() throws IOException, SolrServerException {
    PDTProductUpdateProductToSolrEventModel product = new PDTProductUpdateProductToSolrEventModel();
    product.setBrandApprovalStatus(BrandApprovalStatus.APPROVED.name());
    product.setCategoryCodes(List.of(CATEGORY_CODE));
    solrVendorProductCollectionRepository.updateSolrOnApprovalOrSave(product, false);
    Mockito.verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
    Assertions.assertNull(
        solrInputDocumentArgumentCaptor.getValue().getField(VendorProductSolrFieldNames.CATEGORY_CODES));
  }

  @Test
   void updateSolrOnImageApprovalOrSave() throws IOException, SolrServerException {
    product.setImageViolations(IMAGE_VIOLATION);
    solrVendorProductCollectionRepository.updateSolrOnImageApproval(product, productReviewer);
    Mockito.verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
  }

  @Test
   void updateSolrOnBrandApprovalOrRejectionTest() throws IOException, SolrServerException {
    solrVendorProductCollectionRepository.updateSolrOnBrandApprovalOrRejection(PRODUCT_CODE,
      BRAND, BrandApprovalStatus.APPROVED.name());
    Mockito.verify(cloudSolrClient).add(solrInputDocumentArgumentCaptor.capture());
    Assertions.assertNotNull(
        solrInputDocumentArgumentCaptor.getValue().getField(VendorProductSolrFieldNames.PRODUCT_CODE));
    Assertions.assertNotNull(
        solrInputDocumentArgumentCaptor.getValue().getField(VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS));
    Assertions.assertNotNull(
        solrInputDocumentArgumentCaptor.getValue().getField(VendorProductSolrFieldNames.BRAND));
  }

  @Test
   void getDocumentTest() throws IOException, SolrServerException {
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponseMock);
    Mockito.when(queryResponseMock.getResults()).thenReturn(solrDocumentListWithOneRecord);
    solrVendorProductCollectionRepository.getSolrDocument(product.getProductCode());
    Mockito.verify(cloudSolrClient).query(solrQuery.capture());
    Mockito.verify(queryResponseMock, times(2)).getResults();
  }

  @Test
   void getDocumentTestEmptyList() throws IOException, SolrServerException {
    Mockito.when(cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponseMock);
    Mockito.when(queryResponseMock.getResults()).thenReturn(new SolrDocumentList());
    solrVendorProductCollectionRepository.getSolrDocument(product.getProductCode());
    Mockito.verify(cloudSolrClient).query(solrQuery.capture());
    Mockito.verify(queryResponseMock, times(1)).getResults();
  }

  @Test
   void getReviewConfigCountsByVendorAndConfigTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class)))
        .thenReturn(new QueryResponse());
    solrVendorProductCollectionRepository.getReviewConfigCountsByVendorAndConfig(STORE_ID, VENDOR_CODE, Boolean.FALSE);
    Mockito.verify(this.cloudSolrClient).query(solrQuery.capture());
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(VENDOR_CODE + SolrConstants.COLON + VENDOR_CODE,
        solrQuery.getValue().getFilterQueries()[1]);
    Assertions.assertEquals(VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE,
        solrQuery.getValue().getFacetFields()[0]);
  }

  @Test
   void getReviewConfigCountsByVendorAndConfig_expectException() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class)))
      .thenThrow(SolrServerException.class);
    try {
      Assertions.assertThrows(Exception.class,
        () -> solrVendorProductCollectionRepository.getReviewConfigCountsByVendorAndConfig(STORE_ID,
          VENDOR_CODE, false));
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQuery.capture());
    }
  }

  @Test
   void findProductBusinessPartnerMapperTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(new QueryResponse());
    solrVendorProductCollectionRepository.findProductBusinessPartnerMapper(pendingStates, null, pageable, STORE_ID);
    Mockito.verify(this.cloudSolrClient).query(solrQuery.capture());
    Assertions.assertNotNull(solrQuery);
    Assertions.assertEquals(3, solrQuery.getValue().getFilterQueries().length);
  }

  @Test
   void findProductBusinessPartnerMapperExceptionTest() throws Exception {
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class)))
      .thenThrow(SolrServerException.class);
    try {
      Assertions.assertThrows(Exception.class,
        () -> solrVendorProductCollectionRepository.findProductBusinessPartnerMapper(pendingStates,
          null, pageable, STORE_ID));
    } finally {
      Mockito.verify(this.cloudSolrClient).query(solrQuery.capture());
    }
  }

  @Test
   void getFilteredAndBoostedProductsFromSolrTest() throws Exception {
    ReflectionTestUtils.setField(solrVendorProductCollectionRepository, "appealProductEnabled",
      true);
    when(customBoostQueryBuilder.getBoostQueryForAutoAssignment(true)).thenReturn(new SolrQuery());
    solrDocumentListWithOneRecord.setNumFound(100);
    solrDocumentListWithOneRecord.setStart(100);
    solrDocumentListWithOneRecord.setNumFoundExact(true);
    Mockito.when(queryResponseMock.getResults()).thenReturn(solrDocumentListWithOneRecord);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(new QueryResponse());
    solrVendorProductCollectionRepository.getFilteredAndBoostedProductsFromSolr(STORE_ID, new SummaryFilterDTO(),
        pageable);
    Mockito.verify(this.cloudSolrClient).query(solrQuery.capture());
  }

  @Test
   void getFilteredAndBoostedProductsFromSolrTestForQueryResultTrue() throws Exception {
    when(customBoostQueryBuilder.getBoostQueryForAutoAssignment(false)).thenReturn(new SolrQuery());
    solrDocumentListWithOneRecord.setNumFound(100);
    solrDocumentListWithOneRecord.setStart(100);
    solrDocumentListWithOneRecord.setNumFoundExact(true);
    Mockito.when(queryResponseMock.getResults()).thenReturn(solrDocumentListWithOneRecord);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(queryResponseMock);
    solrVendorProductCollectionRepository.getFilteredAndBoostedProductsFromSolr(STORE_ID, new SummaryFilterDTO(),
        pageable);
    Mockito.verify(this.cloudSolrClient).query(solrQuery.capture());
  }

  @Test
   void getFilteredAndBoostedProductsFromSolrTestForQuertResponseFalse() throws Exception {
    when(customBoostQueryBuilder.getBoostQueryForAutoAssignment(false)).thenReturn(new SolrQuery());
    Mockito.when(queryResponseMock.getResults()).thenReturn(null);
    Mockito.when(this.cloudSolrClient.query(Mockito.any(SolrQuery.class))).thenReturn(null);
    solrVendorProductCollectionRepository.getFilteredAndBoostedProductsFromSolr(STORE_ID, new SummaryFilterDTO(),
        pageable);
    Mockito.verify(this.cloudSolrClient).query(solrQuery.capture());
  }
}
