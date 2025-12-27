package com.gdn.mta.product.service.solr;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
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

import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.IntervalFacet;
import org.apache.solr.client.solrj.response.QueryResponse;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gda.mta.product.dto.DalamProductListRequest;
import com.gdn.common.web.param.PageableHelper;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionAddEvent;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionAddEventFields;
import com.gdn.mta.domain.event.modal.SolrReviewProductCollectionDeleteEvent;
import com.gdn.mta.product.entity.ApplicationConfigProperties;
import com.gdn.mta.product.entity.ProductBusinessPartnerMapper;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.repository.CategoryRepository;
import com.gdn.mta.product.repository.SolrReviewProductCollectionRepository;
import com.gdn.mta.product.service.ApplicationConfigPropertiesService;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import com.gdn.partners.pbp.commons.util.ApplicationConfigPropertiesNames;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;
import com.gdn.partners.pbp.model.productlevel3.ProductCollectionCountResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

public class SolrReviewProductCollectionServiceBeanTest {

  private static final String STORE_ID = "10001";
  private static final String ID = "ID";
  private static final String PRODUCT_ID_1 = "productId";
  private static final String PRODUCT_CODE_1 = "productCode";
  private static final String PRODUCT_NAME_1 = "productName";
  private static final String BRAND_1 = "brand";
  private static final String CATEGORY_CODE_1 = "categoryCode";
  private static final String CATEGORY_NAME_1 = "categoryName";
  private static final String BUSINESS_PARTNER_CODE_1 = "businessPartnerCode";
  private static final String BUSINESS_PARTNER_NAME_1 = "businessPartnerName";
  private static final String STATE_1 = "DRAFT";
  private static final String STATE_3 = "IN_PROGRESS";
  private static final String STATE_4 = "DELETED";
  private static final String STATE_5 = "ACTIVE";
  private static final String PRODUCT_ID_2 = "productId2";
  private static final String PRODUCT_CODE_2 = "productCode2";
  private static final String PRODUCT_NAME_2 = "productName2";
  private static final String BRAND_2 = "brand2";
  private static final String CATEGORY_CODE_2 = "categoryCode2";
  private static final String CATEGORY_NAME_2 = "categoryName2";
  private static final String BUSINESS_PARTNER_CODE_2 = "businessPartnerCode2";
  private static final String BUSINESS_PARTNER_NAME_2 = "businessPartnerName2";
  private static final String PRODUCT_ID_3 = "productId3";
  private static final String PRODUCT_CODE_3 = "productCode3";
  private static final String PRODUCT_NAME_3 = "productName3";
  private static final String STATE_2 = "NEED_CORRECTION";
  private static final String CREATED_BY = "createdBy";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String ASSIGNED_BY = "assignedBy";
  private static final String BRAND_NAME = "brandName";
  private static final String DEFAULT_USERNAME = "user-name";
  private static final Date CREATED_DATE = new Date();
  private static final Date LAST_UPDATED_DATE = new Date();
  private static final long TOTAL_NUM_FOUND = 100;
  private static final int FACET_COUNT = 10;
  private Pageable pageable = PageRequest.of(0, 10);
  private static  final String DELTA_REINDEX_HOUR_THRESHOLD = "deltaReindexHourThreshold";
  private static  final String DELTA_REINDEX_BATCH_SIZE = "deltaReindexBatchSize";
  private static final String USE_PCB_SWITCH = "usePCB";
  private static final String VALUE = "100";
  private static final String TRUE = "true";

  @Mock
  private ProductService productService;

  @Mock
  private CategoryRepository categoryRepository;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ApplicationConfigPropertiesService applicationConfigPropertiesService;

  @Mock
  private SolrReviewProductCollectionRepository solrReviewProductCollectionRepository;

  @Mock
  private QueryResponse queryResponse;

  @InjectMocks
  private SolrReviewProductCollectionServiceBean solrReviewProductCollectionServiceBean;

  @Captor
  private ArgumentCaptor<SolrReviewProductCollectionAddEvent>
      screeningSolrProductCollectionAddEventArgumentCaptor;

  @Captor
  private ArgumentCaptor<SolrReviewProductCollectionAddEvent>
      solrReviewProductCollectionAddEventArgumentCaptor;

  @Captor
  private ArgumentCaptor<SolrReviewProductCollectionAddEventFields>
      solrReviewProductCollectionAddEventFieldsArgumentCaptor;

  @Captor
  private ArgumentCaptor<SolrReviewProductCollectionDeleteEvent> solrReviewProductCollectionDeleteEventArgumentCaptor;

  @Captor
  private ArgumentCaptor<ApplicationConfigProperties> applicationConfigPropertiesArgumentCaptor;

  @Captor
  private ArgumentCaptor<SolrInputDocument> solrInputDocumentArgumentCaptor;

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
  private ProductSystemParameterService productSystemParameterService;


  private ProductCollection productCollection1;
  private ProductCollection productCollection2;
  private ProductCollection productCollection3;
  private CategoryResponse categoryResponse;
  private ApplicationConfigProperties applicationConfigProperties;
  private Page<ProductCollection> productCollectionPage;
  private Page<ProductBusinessPartnerMapper> productBusinessPartnerMappers;
  private ProductBusinessPartnerMapper productBusinessPartnerMapper;
  private DalamProductListRequest dalamProductListRequest;
  private SolrProductCollectionDTO solrProductCollectionDTO;
  private ProductSystemParameter productSystemParameter;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    SolrReviewProductCollectionAddEvent solrReviewProductCollectionAddEvent = new SolrReviewProductCollectionAddEvent();
    solrReviewProductCollectionAddEvent.setSolrReviewProductCollectionAddEventFieldsList(
        solrReviewProductCollectionAddEventFieldsArgumentCaptor.getAllValues());
    productCollection1 = new ProductCollection(PRODUCT_ID_1, PRODUCT_CODE_1, PRODUCT_NAME_1, BRAND_1, CATEGORY_CODE_1,
        CATEGORY_NAME_1, BUSINESS_PARTNER_CODE_1, BUSINESS_PARTNER_NAME_1, false, false, STATE_1,
        CREATED_BY, CREATED_DATE, STORE_ID);
    productCollection1.setAssignedTo(ASSIGNED_TO);
    productCollection1.setAssignedBy(ASSIGNED_BY);
    productCollection2 = new ProductCollection(PRODUCT_ID_2, PRODUCT_CODE_2, PRODUCT_NAME_2, BRAND_2, CATEGORY_CODE_2,
        CATEGORY_NAME_2, BUSINESS_PARTNER_CODE_2, BUSINESS_PARTNER_NAME_2, false, false, STATE_2,
        CREATED_BY, CREATED_DATE, STORE_ID);
    productCollection2.setAssignedTo(ASSIGNED_TO);
    productCollection2.setAssignedBy(ASSIGNED_BY);
    categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(CATEGORY_CODE_1);
    categoryResponse.setName(CATEGORY_NAME_1);
    productCollection3 =
        new ProductCollection(PRODUCT_ID_3, PRODUCT_CODE_3, PRODUCT_NAME_3, BRAND_2, CATEGORY_CODE_2, CATEGORY_NAME_2,
            BUSINESS_PARTNER_CODE_2, BUSINESS_PARTNER_NAME_2, true, false, STATE_3, CREATED_BY, CREATED_DATE, STORE_ID);
    applicationConfigProperties = new ApplicationConfigProperties();
    applicationConfigProperties.setPropertyName(ApplicationConfigPropertiesNames.SOLR_REVIEW_COLLECTION_LAST_DELTA_RUN_TIME);
    applicationConfigProperties.setValue(String.valueOf(LAST_UPDATED_DATE.getTime()));
    productCollectionPage = new PageImpl<>(Arrays.asList(productCollection1, productCollection2));
    dalamProductListRequest =
        DalamProductListRequest.builder().page(0).size(0).viewable(Boolean.TRUE).activated(Boolean.TRUE).build();

    Date createdDate = Calendar.getInstance().getTime();
    solrProductCollectionDTO =
        new SolrProductCollectionDTO.Builder().setProductCode(PRODUCT_CODE_1)
            .setProductId(PRODUCT_ID_1).setBrand(BRAND_1)
            .setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1).setBusinessPartnerName(BUSINESS_PARTNER_NAME_1)
            .setCategoryCode(CATEGORY_CODE_1).setCategoryName(CATEGORY_NAME_1).setProductName(PRODUCT_NAME_1)
            .setCreatedBy(DEFAULT_USERNAME).setCreatedDate(createdDate).setStoreId(STORE_ID)
            .build();
    productCollectionPage = new PageImpl<>(Arrays.asList(productCollection1, productCollection2, productCollection3));

    productBusinessPartnerMapper = new ProductBusinessPartnerMapper();
    productBusinessPartnerMapper.setBusinessPartnerCode(BUSINESS_PARTNER_CODE_1);
    productBusinessPartnerMapper.setBusinessPartnerName(BUSINESS_PARTNER_NAME_1);
    productSystemParameter = new ProductSystemParameter();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(categoryRepository);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(applicationConfigPropertiesService);
    Mockito.verifyNoMoreInteractions(solrReviewProductCollectionRepository);
    verifyNoMoreInteractions(queryResponse, intervalFacets, intervalFacet, count1, count2, count3, count4, count5);
  }

  @Test
  public void testFullReindex() throws Exception {
    Mockito.doReturn(productCollectionPage).when(productService)
        .getProductsByStoreIdAndActivatedAndViewable(eq(STORE_ID), eq(false), eq(false), any(PageRequest.class));
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    doNothing().when(this.solrReviewProductCollectionRepository).deleteAllScreeningDocumentsFromSolr();
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_1)).thenReturn(categoryResponses);
    solrReviewProductCollectionServiceBean.fullReindexCollection(STORE_ID, true);
    verify(productService).getProductsByStoreIdAndActivatedAndViewable(
        eq(STORE_ID), eq(false), eq(false), any(PageRequest.class));
    verify(categoryRepository).findHierarchyByCategoryCode(CATEGORY_CODE_1);
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST),
        screeningSolrProductCollectionAddEventArgumentCaptor.capture());
    verify(this.solrReviewProductCollectionRepository).deleteAllScreeningDocumentsFromSolr();
    assertEquals(1, screeningSolrProductCollectionAddEventArgumentCaptor.getAllValues().size());
    assertEquals(STORE_ID, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getStoreId());
    assertEquals(PRODUCT_ID_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductId());
    assertEquals(PRODUCT_CODE_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductCode());
    assertEquals(ASSIGNED_TO, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getAssignedTo());
    assertEquals(PRODUCT_NAME_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductName());
    assertEquals(Collections.singletonList(CATEGORY_NAME_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryNames());
    assertEquals(Collections.singletonList(CATEGORY_CODE_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryCodes());
  }

  @Test
  public void testFullReindexInProgressProducts() throws Exception {
    Mockito.doReturn(productCollectionPage).when(productService)
        .getProductsByStoreIdAndActivatedAndReviewPending(eq(STORE_ID), eq(true), eq(true), any(PageRequest.class));
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    doNothing().when(this.solrReviewProductCollectionRepository).deleteAllInProgressDocumentsFromSolr();
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_1)).thenReturn(categoryResponses);
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_2)).thenReturn(categoryResponses);
    solrReviewProductCollectionServiceBean.fullReindexCollection(STORE_ID, false);
    verify(productService).getProductsByStoreIdAndActivatedAndReviewPending(
        eq(STORE_ID), eq(true), eq(true), any(PageRequest.class));
    verify(categoryRepository).findHierarchyByCategoryCode(CATEGORY_CODE_1);
    verify(categoryRepository).findHierarchyByCategoryCode(CATEGORY_CODE_2);
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST),
        screeningSolrProductCollectionAddEventArgumentCaptor.capture());
    verify(this.solrReviewProductCollectionRepository).deleteAllInProgressDocumentsFromSolr();
    assertEquals(1, screeningSolrProductCollectionAddEventArgumentCaptor.getAllValues().size());
    assertEquals(STORE_ID, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getStoreId());
    assertEquals(PRODUCT_ID_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductId());
    assertEquals(PRODUCT_CODE_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductCode());
    assertEquals(ASSIGNED_TO, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getAssignedTo());
    assertEquals(PRODUCT_NAME_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductName());
    assertEquals(Collections.singletonList(CATEGORY_NAME_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryNames());
    assertEquals(Collections.singletonList(CATEGORY_CODE_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryCodes());
  }

  @Test
  public void testDeltaReindex() throws Exception {
    List<ProductCollection> productCollectionList = new ArrayList<>();
    productCollection2.setId(ID);
    productSystemParameter.setVariable(STORE_ID);
    productSystemParameter.setValue(VALUE);
    productCollectionList.add(productCollection1);
    productCollectionList.add(productCollection2);
    Page<ProductCollection> productCollectionPage = new PageImpl<>(productCollectionList);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, DELTA_REINDEX_HOUR_THRESHOLD))
        .thenReturn(productSystemParameter);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, DELTA_REINDEX_BATCH_SIZE))
        .thenReturn(productSystemParameter);
    when(productService.getProductsByStoreIdAndUpdatedDateBetween(eq(STORE_ID), any(Date.class), any(Date.class),
        any(PageRequest.class))).thenReturn(productCollectionPage);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, USE_PCB_SWITCH))
        .thenReturn(productSystemParameter);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_1)).thenReturn(categoryResponses);
    solrReviewProductCollectionServiceBean.deltaReindexCollection(STORE_ID);
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST),
        screeningSolrProductCollectionAddEventArgumentCaptor.capture());
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST),
        solrReviewProductCollectionDeleteEventArgumentCaptor.capture());
    verify(productService).getProductsByStoreIdAndUpdatedDateBetween(
        eq(STORE_ID), any(Date.class), any(Date.class), any(PageRequest.class));
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,DELTA_REINDEX_HOUR_THRESHOLD);
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,DELTA_REINDEX_BATCH_SIZE);
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,USE_PCB_SWITCH);
    assertEquals(1, screeningSolrProductCollectionAddEventArgumentCaptor.getAllValues().size());
    assertEquals(STORE_ID, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getStoreId());
    assertEquals(PRODUCT_ID_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductId());
    assertEquals(PRODUCT_CODE_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductCode());
    assertEquals(ASSIGNED_TO, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getAssignedTo());
    assertEquals(PRODUCT_NAME_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductName());
    assertEquals(ID, solrReviewProductCollectionDeleteEventArgumentCaptor.getValue().getIds().get(0));
    assertEquals(Collections.singletonList(CATEGORY_NAME_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryNames());
    assertEquals(Collections.singletonList(CATEGORY_CODE_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryCodes());
  }

  @Test
  public void testDeltaReindexWithStateInProgress() throws Exception {
    List<ProductCollection> productCollectionList = new ArrayList<>();
    productSystemParameter.setVariable(STORE_ID);
    productSystemParameter.setValue(VALUE);
    productCollection2.setState(STATE_3);
    productCollection2.setActivated(Boolean.TRUE);
    productCollection2.setReviewPending(Boolean.TRUE);
    productCollection2.setId(ID);
    productCollectionList.add(productCollection1);
    productCollectionList.add(productCollection2);
    Page<ProductCollection> productCollectionPage = new PageImpl<>(productCollectionList);
    when(productService.getProductsByStoreIdAndUpdatedDateBetween(
        eq(STORE_ID), any(Date.class), any(Date.class),
        any(PageRequest.class))).thenReturn(productCollectionPage);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, DELTA_REINDEX_HOUR_THRESHOLD))
        .thenReturn(productSystemParameter);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, DELTA_REINDEX_BATCH_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, USE_PCB_SWITCH))
        .thenReturn(productSystemParameter);
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_1)).thenReturn(categoryResponses);
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_2)).thenReturn(categoryResponses);
    solrReviewProductCollectionServiceBean.deltaReindexCollection(STORE_ID);
    verify(productService).getProductsByStoreIdAndUpdatedDateBetween(
        eq(STORE_ID), any(Date.class), any(Date.class), any(PageRequest.class));
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,DELTA_REINDEX_HOUR_THRESHOLD);
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,DELTA_REINDEX_BATCH_SIZE);
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,USE_PCB_SWITCH);
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST),
        screeningSolrProductCollectionAddEventArgumentCaptor.capture());
    assertEquals(2, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().size());
    assertEquals(STORE_ID, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getStoreId());
    assertEquals(PRODUCT_ID_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductId());
    assertEquals(PRODUCT_CODE_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductCode());
    assertEquals(ASSIGNED_TO, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getAssignedTo());
    assertEquals(PRODUCT_NAME_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductName());
    assertEquals(Collections.singletonList(CATEGORY_NAME_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryNames());
    assertEquals(Collections.singletonList(CATEGORY_CODE_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryCodes());
  }

  @Test
  public void testDeltaReindexWithStateActivePostlive() throws Exception {
    List<ProductCollection> productCollectionList = new ArrayList<>();
    productSystemParameter.setVariable(STORE_ID);
    productSystemParameter.setValue(VALUE);
    productCollection2.setState(STATE_5);
    productCollection2.setActivated(Boolean.TRUE);
    productCollection2.setReviewPending(Boolean.TRUE);
    productCollection2.setId(ID);
    productCollectionList.add(productCollection1);
    productCollectionList.add(productCollection2);
    Page<ProductCollection> productCollectionPage = new PageImpl<>(productCollectionList);
    when(productService.getProductsByStoreIdAndUpdatedDateBetween(
        eq(STORE_ID), any(Date.class), any(Date.class),
        any(PageRequest.class))).thenReturn(productCollectionPage);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, DELTA_REINDEX_HOUR_THRESHOLD))
        .thenReturn(productSystemParameter);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, DELTA_REINDEX_BATCH_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, USE_PCB_SWITCH))
        .thenReturn(productSystemParameter);
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_1)).thenReturn(categoryResponses);
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_2)).thenReturn(categoryResponses);
    solrReviewProductCollectionServiceBean.deltaReindexCollection(STORE_ID);
    verify(productService).getProductsByStoreIdAndUpdatedDateBetween(
        eq(STORE_ID), any(Date.class), any(Date.class), any(PageRequest.class));
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,DELTA_REINDEX_HOUR_THRESHOLD);
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,DELTA_REINDEX_BATCH_SIZE);
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,USE_PCB_SWITCH);
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST),
        screeningSolrProductCollectionAddEventArgumentCaptor.capture());
    assertEquals(STORE_ID, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getStoreId());
    assertEquals(PRODUCT_ID_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductId());
    assertEquals(PRODUCT_CODE_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductCode());
    assertEquals(ASSIGNED_TO, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getAssignedTo());
    assertEquals(PRODUCT_NAME_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductName());
    assertEquals(Collections.singletonList(CATEGORY_NAME_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryNames());
    assertEquals(Collections.singletonList(CATEGORY_CODE_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryCodes());
  }

  @Test
  public void testDeltaReindexWithStateActiveReviewPendingFalse() throws Exception {
    List<ProductCollection> productCollectionList = new ArrayList<>();
    productSystemParameter.setVariable(STORE_ID);
    productSystemParameter.setValue(VALUE);
    productCollection2.setState(STATE_5);
    productCollection2.setActivated(Boolean.TRUE);
    productCollection2.setReviewPending(Boolean.FALSE);
    productCollection2.setId(ID);
    productCollectionList.add(productCollection1);
    productCollectionList.add(productCollection2);
    Page<ProductCollection> productCollectionPage = new PageImpl<>(productCollectionList);
    when(productService.getProductsByStoreIdAndUpdatedDateBetween(
        eq(STORE_ID), any(Date.class), any(Date.class),
        any(PageRequest.class))).thenReturn(productCollectionPage);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, DELTA_REINDEX_HOUR_THRESHOLD))
        .thenReturn(productSystemParameter);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, DELTA_REINDEX_BATCH_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, USE_PCB_SWITCH))
        .thenReturn(productSystemParameter);
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_1)).thenReturn(categoryResponses);
    solrReviewProductCollectionServiceBean.deltaReindexCollection(STORE_ID);
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST), any());
    verify(productService).getProductsByStoreIdAndUpdatedDateBetween(
        eq(STORE_ID), any(Date.class), any(Date.class), any(PageRequest.class));
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,DELTA_REINDEX_HOUR_THRESHOLD);
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,DELTA_REINDEX_BATCH_SIZE);
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,USE_PCB_SWITCH);
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST),
        screeningSolrProductCollectionAddEventArgumentCaptor.capture());
    assertEquals(1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().size());
    assertEquals(STORE_ID, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getStoreId());
    assertEquals(PRODUCT_ID_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductId());
    assertEquals(PRODUCT_CODE_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductCode());
    assertEquals(ASSIGNED_TO, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getAssignedTo());
    assertEquals(PRODUCT_NAME_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductName());
    assertEquals(Collections.singletonList(CATEGORY_NAME_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryNames());
    assertEquals(Collections.singletonList(CATEGORY_CODE_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryCodes());
  }


  @Test
  public void testDeltaReindexWithStateInDeleted() throws Exception {
    List<ProductCollection> productCollectionList = new ArrayList<>();
    productSystemParameter.setVariable(STORE_ID);
    productSystemParameter.setValue(VALUE);
    productCollection2.setState(STATE_4);
    productCollection2.setActivated(Boolean.FALSE);
    productCollection2.setId(ID);
    productCollectionList.add(productCollection1);
    productCollectionList.add(productCollection2);
    Page<ProductCollection> productCollectionPage = new PageImpl<>(productCollectionList);
    when(productService.getProductsByStoreIdAndUpdatedDateBetween(
        eq(STORE_ID), any(Date.class), any(Date.class),
        any(PageRequest.class))).thenReturn(productCollectionPage);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, DELTA_REINDEX_HOUR_THRESHOLD))
        .thenReturn(productSystemParameter);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, DELTA_REINDEX_BATCH_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, USE_PCB_SWITCH))
        .thenReturn(productSystemParameter);
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_1)).thenReturn(categoryResponses);
    solrReviewProductCollectionServiceBean.deltaReindexCollection(STORE_ID);
    verify(productService).getProductsByStoreIdAndUpdatedDateBetween(
        eq(STORE_ID), any(Date.class), any(Date.class), any(PageRequest.class));
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,DELTA_REINDEX_HOUR_THRESHOLD);
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,DELTA_REINDEX_BATCH_SIZE);
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,USE_PCB_SWITCH);
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST),
        screeningSolrProductCollectionAddEventArgumentCaptor.capture());
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST),
        solrReviewProductCollectionDeleteEventArgumentCaptor.capture());
    assertEquals(ID, solrReviewProductCollectionDeleteEventArgumentCaptor.getValue().getIds().get(0));
    assertEquals(1, screeningSolrProductCollectionAddEventArgumentCaptor.getAllValues().size());
    assertEquals(STORE_ID, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getStoreId());
    assertEquals(PRODUCT_ID_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductId());
    assertEquals(PRODUCT_CODE_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductCode());
    assertEquals(ASSIGNED_TO, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getAssignedTo());
    assertEquals(PRODUCT_NAME_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductName());
    assertEquals(ID, solrReviewProductCollectionDeleteEventArgumentCaptor.getValue().getIds().get(0));
    assertEquals(Collections.singletonList(CATEGORY_NAME_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryNames());
    assertEquals(Collections.singletonList(CATEGORY_CODE_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryCodes());
  }

  @Test
  public void testDeltaReindexWithMarkForDeleteTrue() throws Exception {
    List<ProductCollection> productCollectionList = new ArrayList<>();
    productSystemParameter.setVariable(STORE_ID);
    productSystemParameter.setValue(VALUE);
    productCollection2.setMarkForDelete(true);
    productCollection2.setState(STATE_1);
    productCollection2.setId(ID);
    productCollectionList.add(productCollection1);
    productCollectionList.add(productCollection2);
    Page<ProductCollection> productCollectionPage = new PageImpl<>(productCollectionList);
    when(productService.getProductsByStoreIdAndUpdatedDateBetween(
        eq(STORE_ID), any(Date.class), any(Date.class),
        any(PageRequest.class))).thenReturn(productCollectionPage);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, DELTA_REINDEX_HOUR_THRESHOLD))
        .thenReturn(productSystemParameter);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, DELTA_REINDEX_BATCH_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, USE_PCB_SWITCH))
        .thenReturn(productSystemParameter);
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_1)).thenReturn(categoryResponses);
    solrReviewProductCollectionServiceBean.deltaReindexCollection(STORE_ID);
    verify(productService).getProductsByStoreIdAndUpdatedDateBetween(
        eq(STORE_ID), any(Date.class), any(Date.class), any(PageRequest.class));
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,DELTA_REINDEX_HOUR_THRESHOLD);
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,DELTA_REINDEX_BATCH_SIZE);
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,USE_PCB_SWITCH);
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST),
        screeningSolrProductCollectionAddEventArgumentCaptor.capture());
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST),
        solrReviewProductCollectionDeleteEventArgumentCaptor.capture());
    assertEquals(ID, solrReviewProductCollectionDeleteEventArgumentCaptor.getValue().getIds().get(0));
    assertEquals(1, screeningSolrProductCollectionAddEventArgumentCaptor.getAllValues().size());
    assertEquals(STORE_ID, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getStoreId());
    assertEquals(PRODUCT_ID_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductId());
    assertEquals(PRODUCT_CODE_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductCode());
    assertEquals(ASSIGNED_TO, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getAssignedTo());
    assertEquals(PRODUCT_NAME_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductName());
    assertEquals(ID, solrReviewProductCollectionDeleteEventArgumentCaptor.getValue().getIds().get(0));
    assertEquals(Collections.singletonList(CATEGORY_NAME_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryNames());
    assertEquals(Collections.singletonList(CATEGORY_CODE_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryCodes());
  }

  @Test
  public void deltaReindexProductCodeWithReviewPendingFalseTest() throws Exception {
    List<ProductCollection> productCollectionList = new ArrayList<>();
    productCollection2.setMarkForDelete(false);
    productCollection2.setState(STATE_1);
    productCollection2.setId(ID);
    productCollection2.setReviewPending(Boolean.FALSE);
    productCollectionList.add(productCollection2);
    Page<ProductCollection> productCollectionPage = new PageImpl<>(productCollectionList);
    when(productService.getProductByStoreIdAndProductCode(eq(STORE_ID), eq(PRODUCT_CODE_2), any(PageRequest.class)))
        .thenReturn(productCollectionPage);
    solrReviewProductCollectionServiceBean.deltaReindexProductCode(STORE_ID, PRODUCT_CODE_2);
    verify(productService).getProductByStoreIdAndProductCode(eq(STORE_ID), eq(PRODUCT_CODE_2), any(PageRequest.class));
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST),
        solrReviewProductCollectionDeleteEventArgumentCaptor.capture());
    assertEquals(ID, solrReviewProductCollectionDeleteEventArgumentCaptor.getValue().getIds().get(0));
  }

  @Test
  public void deltaReindexProductCodeWithStateInNeedCorrectionTest() throws Exception {
    List<ProductCollection> productCollectionList = new ArrayList<>();
    productCollection2.setMarkForDelete(false);
    productCollection2.setState(STATE_2);
    productCollection2.setId(ID);
    productCollection2.setReviewPending(Boolean.TRUE);
    productCollectionList.add(productCollection2);
    Page<ProductCollection> productCollectionPage = new PageImpl<>(productCollectionList);
    when(productService.getProductByStoreIdAndProductCode(eq(STORE_ID), eq(PRODUCT_CODE_2), any(PageRequest.class)))
        .thenReturn(productCollectionPage);
    solrReviewProductCollectionServiceBean.deltaReindexProductCode(STORE_ID, PRODUCT_CODE_2);
    verify(productService).getProductByStoreIdAndProductCode(eq(STORE_ID), eq(PRODUCT_CODE_2), any(PageRequest.class));
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST),
        solrReviewProductCollectionDeleteEventArgumentCaptor.capture());
    assertEquals(ID, solrReviewProductCollectionDeleteEventArgumentCaptor.getValue().getIds().get(0));
  }

  @Test
  public void deltaReindexProductCodeWithMarkForDeleteTrueTest() throws Exception {
    List<ProductCollection> productCollectionList = new ArrayList<>();
    productCollection2.setMarkForDelete(true);
    productCollection2.setState(STATE_1);
    productCollection2.setId(ID);
    productCollectionList.add(productCollection2);
    Page<ProductCollection> productCollectionPage = new PageImpl<>(productCollectionList);
    when(productService.getProductByStoreIdAndProductCode(eq(STORE_ID), eq(PRODUCT_CODE_2), any(PageRequest.class)))
        .thenReturn(productCollectionPage);
    solrReviewProductCollectionServiceBean.deltaReindexProductCode(STORE_ID, PRODUCT_CODE_2);
    verify(productService).getProductByStoreIdAndProductCode(eq(STORE_ID), eq(PRODUCT_CODE_2), any(PageRequest.class));
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST),
        solrReviewProductCollectionDeleteEventArgumentCaptor.capture());
    assertEquals(ID, solrReviewProductCollectionDeleteEventArgumentCaptor.getValue().getIds().get(0));
  }

  @Test
  public void deltaReindexProductCodeWithStateInDraftTest() throws Exception {
    List<ProductCollection> productCollectionList = new ArrayList<>();
    productCollection2.setMarkForDelete(false);
    productCollection2.setState(STATE_1);
    productCollection2.setId(ID);
    productCollection2.setReviewPending(Boolean.TRUE);
    productCollectionList.add(productCollection2);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_2)).thenReturn(categoryResponses);
    Page<ProductCollection> productCollectionPage = new PageImpl<>(productCollectionList);
    when(productService.getProductByStoreIdAndProductCode(eq(STORE_ID), eq(PRODUCT_CODE_2), any(PageRequest.class)))
        .thenReturn(productCollectionPage);
    solrReviewProductCollectionServiceBean.deltaReindexProductCode(STORE_ID, PRODUCT_CODE_2);
    verify(productService).getProductByStoreIdAndProductCode(eq(STORE_ID), eq(PRODUCT_CODE_2), any(PageRequest.class));
    verify(categoryRepository).findHierarchyByCategoryCode(CATEGORY_CODE_2);
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST),
        screeningSolrProductCollectionAddEventArgumentCaptor.capture());
    assertEquals(PRODUCT_CODE_2, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductCode());

  }

  @Test
  public void addProductToReviewProductCollectionTest() throws Exception {
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_1)).thenReturn(categoryResponses);
    this.solrReviewProductCollectionServiceBean
        .publishKafkaEventToAddProductToReviewProductCollection(productCollection1);
    verify(categoryRepository).findHierarchyByCategoryCode(CATEGORY_CODE_1);
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST),
        screeningSolrProductCollectionAddEventArgumentCaptor.capture());
    assertEquals(STORE_ID, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getStoreId());
    assertEquals(PRODUCT_ID_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductId());
    assertEquals(PRODUCT_CODE_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductCode());
    assertEquals(ASSIGNED_TO, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getAssignedTo());
    assertEquals(PRODUCT_NAME_1, screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
        .getSolrReviewProductCollectionAddEventFieldsList().get(0).getProductName());
    assertEquals(Collections.singletonList(CATEGORY_NAME_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryNames());
    assertEquals(Collections.singletonList(CATEGORY_CODE_1),
        screeningSolrProductCollectionAddEventArgumentCaptor.getValue()
            .getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryCodes());
  }

  @Test
  public void updateAssignedToInReviewProductCollectionWithSolrServerExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.solrReviewProductCollectionRepository)
        .updateAssignedToInSolrCollection(PRODUCT_ID_1, SolrFieldNames.ASSIGNED_TO);
    this.solrReviewProductCollectionServiceBean.updateAssignedToInReviewProductCollection(PRODUCT_ID_1, SolrFieldNames.ASSIGNED_TO);
    Mockito.verify(this.solrReviewProductCollectionRepository)
        .updateAssignedToInSolrCollection(PRODUCT_ID_1, SolrFieldNames.ASSIGNED_TO);
  }

  @Test
  public void updateAssignedToInReviewProductCollectionWithIOExceptionTest() throws Exception {
    Mockito.doThrow(IOException.class).when(this.solrReviewProductCollectionRepository)
        .updateAssignedToInSolrCollection(PRODUCT_ID_1, SolrFieldNames.ASSIGNED_TO);
    this.solrReviewProductCollectionServiceBean.updateAssignedToInReviewProductCollection(PRODUCT_ID_1, SolrFieldNames.ASSIGNED_TO);
    Mockito.verify(this.solrReviewProductCollectionRepository)
        .updateAssignedToInSolrCollection(PRODUCT_ID_1, SolrFieldNames.ASSIGNED_TO);
  }

  @Test
  public void updateAssignedToInReviewProductCollectionTest() throws Exception {
    Mockito.doNothing().when(this.solrReviewProductCollectionRepository)
        .updateAssignedToInSolrCollection(PRODUCT_ID_1, SolrFieldNames.ASSIGNED_TO);
    this.solrReviewProductCollectionServiceBean.updateAssignedToInReviewProductCollection(PRODUCT_ID_1, SolrFieldNames.ASSIGNED_TO);
    Mockito.verify(this.solrReviewProductCollectionRepository)
        .updateAssignedToInSolrCollection(PRODUCT_ID_1, SolrFieldNames.ASSIGNED_TO);
  }

  @Test
  public void getInReviewProductsTest() throws IOException, SolrServerException {
    Mockito.when(solrReviewProductCollectionRepository
        .getInReviewProducts(STORE_ID, PRODUCT_CODE_1, BUSINESS_PARTNER_CODE_1, CATEGORY_CODE_1))
        .thenReturn(queryResponse);
    Mockito.when(this.queryResponse.getIntervalFacets()).thenReturn(intervalFacets);
    Mockito.when(this.intervalFacets.get(0)).thenReturn(intervalFacet);
    Mockito.when(this.intervalFacet.getIntervals()).thenReturn(Arrays.asList(count1, count2, count3, count4, count5));
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
    ProductCollectionCountResponse response = this.solrReviewProductCollectionServiceBean
        .getInReviewProducts(STORE_ID, PRODUCT_CODE_1, BUSINESS_PARTNER_CODE_1, CATEGORY_CODE_1);
    Mockito.verify(solrReviewProductCollectionRepository)
        .getInReviewProducts(STORE_ID, PRODUCT_CODE_1, BUSINESS_PARTNER_CODE_1, CATEGORY_CODE_1);
    verify(this.queryResponse).getIntervalFacets();
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
    Assertions.assertNotNull(response);
    Assertions.assertEquals(response.getToday(), FACET_COUNT);
    Assertions.assertEquals(response.getYesterday(), FACET_COUNT);
    Assertions.assertEquals(response.getThreeUntilFiveDaysAgo(), FACET_COUNT);
    Assertions.assertEquals(response.getMoreThan5Days(), FACET_COUNT);
    Assertions.assertEquals(response.getTwoDaysAgo(), FACET_COUNT);
  }

  @Test
  public void updateProductToReviewProductCollectionTest() throws Exception {
    ReflectionTestUtils.setField(solrReviewProductCollectionServiceBean, "reviewProductCollectionSolrEventEnabled",
        true);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_1)).thenReturn(categoryResponses);
    Mockito.doNothing().when(this.solrReviewProductCollectionRepository)
        .updateProductToSolrCollection(Mockito.any(SolrInputDocument.class));
    this.solrReviewProductCollectionServiceBean.addProductToReviewProductCollection(productCollection1);
    verify(categoryRepository).findHierarchyByCategoryCode(CATEGORY_CODE_1);
    Mockito.verify(kafkaProducer).send(Mockito.any(),Mockito.eq("productCode"),Mockito.any());
  }

  @Test
  public void updateProductToReviewProductCollectionSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(solrReviewProductCollectionServiceBean, "reviewProductCollectionSolrEventEnabled",
        false);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_1)).thenReturn(categoryResponses);
    Mockito.doNothing().when(this.solrReviewProductCollectionRepository)
        .updateProductToSolrCollection(Mockito.any(SolrInputDocument.class));
    this.solrReviewProductCollectionServiceBean.addProductToReviewProductCollection(productCollection1);
    verify(categoryRepository).findHierarchyByCategoryCode(CATEGORY_CODE_1);
    Mockito.verify(this.solrReviewProductCollectionRepository)
        .updateProductToSolrCollection(Mockito.any(SolrInputDocument.class));
  }

  @Test
  public void updateProductToReviewProductCollectionExceptionTest() throws Exception {
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_1)).thenReturn(categoryResponses);
    Mockito.doThrow(SolrServerException.class).when(this.solrReviewProductCollectionRepository)
        .updateProductToSolrCollection(Mockito.any(SolrInputDocument.class));
    this.solrReviewProductCollectionServiceBean.addProductToReviewProductCollection(productCollection1);
    Mockito.verify(this.solrReviewProductCollectionRepository)
        .updateProductToSolrCollection(solrInputDocumentArgumentCaptor.capture());
    verify(categoryRepository).findHierarchyByCategoryCode(CATEGORY_CODE_1);
  }

  @Test
  public void updateAssignedToInReviewProductCollectionWithRouteExceptionTest() throws Exception {
    Mockito.doThrow(CloudSolrClient.RouteException.class).when(this.solrReviewProductCollectionRepository)
        .updateAssignedToInSolrCollection(PRODUCT_ID_1, SolrFieldNames.ASSIGNED_TO);
    this.solrReviewProductCollectionServiceBean
        .updateAssignedToInReviewProductCollection(PRODUCT_ID_1, SolrFieldNames.ASSIGNED_TO);
    Mockito.verify(this.solrReviewProductCollectionRepository)
        .updateAssignedToInSolrCollection(PRODUCT_ID_1, SolrFieldNames.ASSIGNED_TO);
  }

  @Test
  public void findProductsForDalamProcessTest() throws IOException, SolrServerException {
    Page<SolrProductCollectionDTO> solrProductCollectionDTOPage =
        new PageImpl<>(Arrays.asList(solrProductCollectionDTO));
    Mockito.when(this.solrReviewProductCollectionRepository.findDalamProductsList(dalamProductListRequest, pageable))
        .thenReturn(solrProductCollectionDTOPage);
    Page<ProductCollection> result =
        this.solrReviewProductCollectionServiceBean.findProductsForDalamProcess(dalamProductListRequest, pageable);
    Mockito.verify(this.solrReviewProductCollectionRepository).findDalamProductsList(dalamProductListRequest,pageable);
    assertEquals(PRODUCT_NAME_1, result.getContent().get(0).getProductName());
    assertEquals(CATEGORY_NAME_1, result.getContent().get(0).getCategoryName());
    assertEquals(CATEGORY_CODE_1, result.getContent().get(0).getCategoryCode());
    assertEquals(BUSINESS_PARTNER_CODE_1, result.getContent().get(0).getBusinessPartnerCode());
    assertEquals(BUSINESS_PARTNER_NAME_1, result.getContent().get(0).getBusinessPartnerName());
    assertEquals(10, result.getSize());
  }

  @Test
  public void findProductBusinessPartnerMapperTest() throws Exception {
    Mockito.when(this.solrReviewProductCollectionRepository
        .findProductBusinessPartnerMapper(STORE_ID, false, false, false, null, pageable))
        .thenReturn(new PageImpl<>(Arrays.asList(productBusinessPartnerMapper)));
    Page<ProductBusinessPartnerMapper> result = this.solrReviewProductCollectionServiceBean
        .findProductBusinessPartnerMapper(STORE_ID, false, false, false, null, pageable);
    Mockito.verify(this.solrReviewProductCollectionRepository)
            .findProductBusinessPartnerMapper(STORE_ID, false, false, false, null, pageable);
    assertEquals(BUSINESS_PARTNER_CODE_1, result.getContent().get(0).getBusinessPartnerCode());
    assertEquals(BUSINESS_PARTNER_NAME_1, result.getContent().get(0).getBusinessPartnerName());
  }

  @Test
  public void testDeltaReindexWithMarkForDeleteFalseSwitch() throws Exception {
    ProductSystemParameter pcbSystemParameter = new ProductSystemParameter();
    pcbSystemParameter.setVariable(STORE_ID);
    pcbSystemParameter.setValue(TRUE);
    productSystemParameter.setVariable(STORE_ID);
    productSystemParameter.setValue(VALUE);
    List<ProductCollection> productCollectionList = new ArrayList<>();
    productSystemParameter.setVariable(STORE_ID);
    productSystemParameter.setValue(VALUE);
    productCollection2.setMarkForDelete(true);
    productCollection2.setState(STATE_1);
    productCollection2.setId(ID);
    productCollectionList.add(productCollection1);
    productCollectionList.add(productCollection2);
    Page<ProductCollection> productCollectionPage = new PageImpl<>(productCollectionList);
    when(productService.getProductsByStoreIdAndUpdatedDateBetween(
        eq(STORE_ID), any(Date.class), any(Date.class),
        any(PageRequest.class))).thenReturn(productCollectionPage);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, DELTA_REINDEX_HOUR_THRESHOLD))
        .thenReturn(productSystemParameter);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, DELTA_REINDEX_BATCH_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, USE_PCB_SWITCH))
        .thenReturn(pcbSystemParameter);
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_1)).thenReturn(categoryResponses);
    solrReviewProductCollectionServiceBean.deltaReindexCollection(STORE_ID);
    verify(productService).getProductsByStoreIdAndUpdatedDateBetween(
        eq(STORE_ID), any(Date.class), any(Date.class), any(PageRequest.class));
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,DELTA_REINDEX_HOUR_THRESHOLD);
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,DELTA_REINDEX_BATCH_SIZE);
    verify(productSystemParameterService).findByStoreIdAndVariable(STORE_ID,USE_PCB_SWITCH);
    verify(categoryRepository).findHierarchyByCategoryCode(CATEGORY_CODE_1);
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST),
        screeningSolrProductCollectionAddEventArgumentCaptor.capture());
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST),
        solrReviewProductCollectionDeleteEventArgumentCaptor.capture());
  }

  @Test
  public void testDeltaReindexNull() throws Exception {
    ProductSystemParameter pcbSystemParameter = new ProductSystemParameter();
    pcbSystemParameter.setVariable(STORE_ID);
    pcbSystemParameter.setValue(TRUE);
    List<ProductCollection> productCollectionList = new ArrayList<>();
    productSystemParameter.setVariable(STORE_ID);
    productSystemParameter.setValue(VALUE);
    productCollection2.setMarkForDelete(true);
    productCollection1.setMarkForDelete(true);
    productCollection2.setState(STATE_1);
    productCollection2.setId(ID);
    productCollectionList.add(productCollection1);
    productCollectionList.add(productCollection2);
    Page<ProductCollection> productCollectionPage = new PageImpl<>(productCollectionList);
    when(productService.getProductsByStoreIdAndUpdatedDateBetween(
        eq(STORE_ID), any(Date.class), any(Date.class),
        any(PageRequest.class))).thenReturn(productCollectionPage);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, DELTA_REINDEX_HOUR_THRESHOLD))
        .thenReturn(productSystemParameter);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, DELTA_REINDEX_BATCH_SIZE))
        .thenReturn(productSystemParameter);
    Mockito.when(productSystemParameterService.findByStoreIdAndVariable(STORE_ID, USE_PCB_SWITCH))
        .thenReturn(pcbSystemParameter);
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_1)).thenReturn(categoryResponses);
    solrReviewProductCollectionServiceBean.deltaReindexCollection(STORE_ID);
    verify(productService).getProductsByStoreIdAndUpdatedDateBetween(
        eq(STORE_ID), any(Date.class), any(Date.class), any(PageRequest.class));
    verify(kafkaProducer).send(eq(DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST),
        solrReviewProductCollectionDeleteEventArgumentCaptor.capture());
  }

  @Test
  public void updateProductToReviewProductCollectionKafkaProducerTest() throws Exception {
    ReflectionTestUtils.setField(solrReviewProductCollectionServiceBean, "reviewProductCollectionSolrEventEnabled",
        true);
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    categoryResponses.add(categoryResponse);
    when(categoryRepository.findHierarchyByCategoryCode(CATEGORY_CODE_1)).thenReturn(categoryResponses);
    SolrReviewProductCollectionAddEventFields solrReviewProductCollectionAddEventFields =
        ConverterUtil.toScreeningSolrProductCollectionAddEvent(productCollection1);
    SolrInputDocument solrInputDocument = ConverterUtil.toSolrInputDocument(solrReviewProductCollectionAddEventFields);
    Mockito.doNothing().when(this.solrReviewProductCollectionRepository)
        .updateProductToSolrCollection(solrInputDocument);
    this.solrReviewProductCollectionServiceBean.addProductToReviewProductCollection(productCollection1);
    verify(categoryRepository).findHierarchyByCategoryCode(CATEGORY_CODE_1);
    Mockito.verify(kafkaProducer).send(Mockito.eq(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST),Mockito.eq("productCode"),
        solrReviewProductCollectionAddEventArgumentCaptor.capture());
    Assertions.assertEquals(categoryResponses.get(0).getCategoryCode(),solrReviewProductCollectionAddEventArgumentCaptor.getValue().getSolrReviewProductCollectionAddEventFieldsList().get(0).getCategoryCodes().get(0));
  }

  @Test
  public void deleteProductFromReviewProductCollectionTest() {
    SolrReviewProductCollectionDeleteEvent solrReviewProductCollectionDeleteEvent =
        new SolrReviewProductCollectionDeleteEvent();
    solrReviewProductCollectionDeleteEvent.setIds(Collections.singletonList(ID));
    solrReviewProductCollectionServiceBean.deleteProductFromReviewProductCollection(ID);
    Mockito.verify(kafkaProducer).send(com.gdn.mta.domain.event.config.DomainEventName.SOLR_DELETE_REVIEW_PRODUCT_REQUEST,
        solrReviewProductCollectionDeleteEvent);
  }

  @Test
  public void updateBrandApprovedInReviewProductCollectionTest() {
    SolrReviewProductCollectionAddEvent solrReviewProductCollectionAddEvent =
        CommonUtils.getSolrReviewProductCollectionAddEvent(ID, true, BRAND_NAME);
    solrReviewProductCollectionServiceBean.updateBrandApprovedInReviewProductCollection(ID, true, BRAND_NAME);
    Mockito.verify(kafkaProducer).send(DomainEventName.SOLR_ADD_REVIEW_PRODUCT_REQUEST, solrReviewProductCollectionAddEvent);
  }
}