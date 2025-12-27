package com.gdn.x.mta.distributiontask.service.impl;

import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringJoiner;

import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gdn.x.message.model.constants.KafkaEventNames;
import com.gdn.x.message.mq.model.MessageEmailRequest;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.VendorSearchAutoHealEventModel;
import com.gdn.x.mta.distributiontask.model.dto.ProductAndReviewerDetailsDTO;
import com.gdn.x.mta.distributiontask.model.dto.SummaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.type.BrandApprovalStatus;
import com.gdn.x.mta.distributiontask.model.type.ProductReviewType;
import com.gdn.x.mta.distributiontask.model.type.ReviewType;
import com.gdn.x.mta.distributiontask.service.api.ProductReviewerService;
import com.gdn.x.mta.distributiontask.service.impl.config.KafkaPublisher;
import org.apache.commons.lang3.StringUtils;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.response.FacetField;
import org.apache.solr.client.solrj.response.IntervalFacet;
import org.apache.solr.client.solrj.response.PivotField;
import org.apache.solr.client.solrj.response.QueryResponse;
import org.apache.solr.common.SolrDocument;
import org.apache.solr.common.SolrDocumentList;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.apache.solr.common.util.NamedList;
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
import org.springframework.data.domain.Sort;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.ProductBusinessPartnerMapperResponse;
import com.gdn.x.mta.distributiontask.dao.api.SolrVendorProductCollectionRepository;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrBatchAddDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrDeleteDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductUpdateProductToSolrEventModel;
import com.gdn.x.mta.distributiontask.model.ConfigProperties;
import com.gdn.x.mta.distributiontask.model.ConfigPropertiesConstants;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductReviewer;
import com.gdn.x.mta.distributiontask.model.Vendor;
import com.gdn.x.mta.distributiontask.model.dto.DistributionTaskMultipleFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.PrimaryFilterDTO;
import com.gdn.x.mta.distributiontask.model.dto.ProductBusinessPartnerMapper;
import com.gdn.x.mta.distributiontask.model.enums.VendorProductSolrFieldNames;
import com.gdn.x.mta.distributiontask.model.solr.SolrConstants;
import com.gdn.x.mta.distributiontask.model.solr.VendorProductSolr;
import com.gdn.x.mta.distributiontask.model.type.TimeFilterType;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.service.api.ConfigPropertiesService;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.mta.distributiontask.service.api.publisher.SolrReindexPublisherService;
import com.gdn.x.mta.distributiontask.service.impl.util.ProductUtils;

public class SolrVendorCollectionServiceImplTest {

  private static final String STORE_ID = "storeId";
  private static final String PRODUCT_CODE = "productCode";
  private static final String VENDOR_CODE = "vendorCode";
  private static final String REVIEWER_ASSIGNED = "assignedTo";
  private static final String CONTENT = "content";
  public static final String KEYWORD = "keyword";
  private static final String POSTLIVE = "postLive";
  private static final String PRELIVE = "preLive";
  private static final String EDITED = "edited";
  private static final String REVISED = "revised";
  private static final String NEWLY_ADDED = "newlyAdded";
  private static final String IMAGE = "IMAGE";
  private static final String BRAND = "Brand";
  private static final String BRAND_STATUS = "BrandStatus";

  private Pageable pageable = PageRequest.of(0, 100);
  private Product product = new Product();
  private Product productDeleted = new Product();

  private List<WorkflowState> pendingStates = new ArrayList<>(Arrays
      .asList(WorkflowState.IN_REVIEW, WorkflowState.EXCEEDED_SLA, WorkflowState.REJECTED, WorkflowState.QC_REJECTED));

  private static final int FACET_COUNT = 10;
  private static final long COUNT = 10;
  private static final long TOTAL_NUM_FOUND = 100;
  private static final int PAGE = 0;
  private static final int SIZE = 100;
  private SolrDocumentList solrDocumentList;
  private SolrDocumentList businessPartnersList;
  private static final String vendorCode = "vendorCode";
  private static final String CONTENT_APPROVER_ASSIGNEE_IMAGE_APPROVER_ASSIGNEE = "contentApproverAssignee,imageApproverAssignee";

  @Mock
  private SolrVendorProductCollectionRepository solrVendorProductCollectionRepository;

  @Mock
  private ProductService productService;

  @Mock
  private SolrReindexPublisherService solrReindexPublisherService;

  @Mock
  private ConfigPropertiesService configPropertiesService;

  @Mock
  private ProductReviewerService productReviewerService;

  @InjectMocks
  private SolrVendorCollectionServiceImpl solrVendorCollectionServiceImpl;

  @Mock
  private QueryResponse queryResponse;

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
  private FacetField facetField4;
  @Mock
  private FacetField.Count count6;
  @Mock
  private FacetField.Count count7;
  @Mock
  private FacetField.Count count8;

  @Mock
  private FacetField.Count count9;

  @Mock
  private FacetField.Count count10;
  @Mock
  private FacetField.Count count11;
  @Mock
  private FacetField.Count count12;
  @Mock
  private FacetField.Count count13;

  private List<PivotField> pivotFields;
  private List<PivotField> pivotFields1;
  private List<PivotField> pivotFieldsWithDiffValue;
  private List<PivotField> pivotFieldsWithNullValue;
  private ProductReviewer productReviewer = new ProductReviewer();
  @Mock
  private NamedList namedList;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ProductUtils productUtils;

  @Captor
  private ArgumentCaptor<List<VendorProductSolr>> vendorProductSolrListArgumentCaptor;

  private PrimaryFilterDTO primaryFilterDTO;
  private ConfigProperties applicationConfigProperties;

  private static final Date LAST_UPDATED_DATE = new Date();

  @Captor
  private ArgumentCaptor<List<SolrInputDocument>> solrInputDocumentListCaptor;

  @Captor
  private ArgumentCaptor<SolrInputDocument> solrInputDocumentCaptor;

  @Captor
  private ArgumentCaptor<PDTProductSolrBatchAddDomainEventModel> pdtProductSolrBatchAddDomainEventModelArgumentCaptor;

  @Captor
  private ArgumentCaptor<MessageEmailRequest> messageEmailRequestArgumentCaptor;

  @BeforeEach
  public void init() {
    MockitoAnnotations.openMocks(this);
    ReflectionTestUtils.setField(solrVendorCollectionServiceImpl, "pdtCollectionReindexBatchSize", 50);
    ReflectionTestUtils.setField(solrVendorCollectionServiceImpl, "resetLastUpdatedDeltaReindexHoursBeforeCheck", 3);
    ReflectionTestUtils.setField(solrVendorCollectionServiceImpl, "resetLastUpdatedDeltaReindexHoursBeforeSet", 2);
    ReflectionTestUtils.setField(solrVendorCollectionServiceImpl, "resetLastUpdatedDeltaReindexEmailAddress", "gmail");
    ReflectionTestUtils.setField(solrVendorCollectionServiceImpl, "resetLastUpdatedDeltaReindexEmailAddressCc", "gmail");
    product.setProductCode(PRODUCT_CODE);
    product.setState(WorkflowState.IN_REVIEW);
    product.setCurrentVendor(new Vendor());
    product.getCurrentVendor().setVendorCode(VENDOR_CODE);

    productDeleted.setProductCode(PRODUCT_CODE);
    productDeleted.setState(WorkflowState.IN_REVIEW);
    productDeleted.setCurrentVendor(new Vendor());
    productDeleted.getCurrentVendor().setVendorCode(VENDOR_CODE);
    productDeleted.setMarkForDelete(true);

    applicationConfigProperties = new ConfigProperties();
    applicationConfigProperties.setPropertyName(ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME);
    applicationConfigProperties.setValue(String.valueOf(LAST_UPDATED_DATE.getTime()));

    solrDocumentList = new SolrDocumentList();
    SolrDocument product = new SolrDocument();
    product.setField(VendorProductSolrFieldNames.CATEGORY_CODES ,
        Arrays.asList(VendorProductSolrFieldNames.CATEGORY_CODES));
    product.setField(VendorProductSolrFieldNames.CATEGORY_NAMES ,
        List.of(VendorProductSolrFieldNames.CATEGORY_NAMES));
    product.setField(VendorProductSolrFieldNames.PRODUCT_CODE ,
        List.of(VendorProductSolrFieldNames.PRODUCT_CODE));
    solrDocumentList.setNumFound(TOTAL_NUM_FOUND);
    solrDocumentList.add(product);
    businessPartnersList = new SolrDocumentList();
    businessPartnersList.setNumFound(TOTAL_NUM_FOUND);
    SolrDocument businessPartner = new SolrDocument();
    businessPartner.addField(VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME, VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME);
    businessPartner.addField(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE, VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE);
    businessPartnersList.add(businessPartner);
    pivotFields = new ArrayList<>();
    PivotField pivotField = new PivotField(VendorProductSolrFieldNames.APPROVER_ASSIGNEE, SolrConstants.NOT_APPLICABLE,
        SIZE, new ArrayList<>(), new HashMap<>(), new HashMap<>(), new ArrayList<>());
    PivotField pivotField1 =
        new PivotField(VendorProductSolrFieldNames.APPROVER_ASSIGNEE, SolrConstants.NOT_APPLICABLE, SIZE, null,
            new HashMap<>(), new HashMap<>(), new ArrayList<>());
    PivotField pivotField2 = new PivotField(VendorProductSolrFieldNames.APPROVER_ASSIGNEE, SolrConstants.NOT_APPLICABLE, SIZE,
        new ArrayList<>(), new HashMap<>(), new HashMap<>(), new ArrayList<>());
    PivotField pivotField3 = new PivotField(VendorProductSolrFieldNames.APPROVER_ASSIGNEE, SolrConstants.CONTENT_PENDING,
        SIZE, new ArrayList<>(), new HashMap<>(), new HashMap<>(), new ArrayList<>());
    pivotFields1 = new ArrayList<>();
    pivotFieldsWithDiffValue = new ArrayList<>();
    pivotFieldsWithNullValue = new ArrayList<>();
    pivotFieldsWithNullValue.add(pivotField3);
    pivotFieldsWithDiffValue.add(pivotField2);
    pivotFields1.add(pivotField1);
    pivotFields.add(pivotField);
    ReflectionTestUtils.setField(solrVendorCollectionServiceImpl, "pendingStates", new ArrayList<>(Arrays
        .asList(WorkflowState.IN_REVIEW.name(), WorkflowState.EXCEEDED_SLA.name(), WorkflowState.REJECTED.name(),
            WorkflowState.QC_REJECTED.name())));
    ReflectionTestUtils.setField(solrVendorCollectionServiceImpl, "contentPendingStates", new ArrayList<>(Arrays
        .asList(WorkflowState.PASSED.name(), WorkflowState.IMAGE_NEED_CORRECTION.name())));
    ReflectionTestUtils.setField(solrVendorCollectionServiceImpl, "imagePendingStates", new ArrayList<>(Arrays
        .asList(WorkflowState.PASSED.name(), WorkflowState.CONTENT_NEED_CORRECTION.name())));
    primaryFilterDTO = new PrimaryFilterDTO();
    productReviewer.setProductCode(PRODUCT_CODE);
    when(
      this.productReviewerService.findProductReviewerByProductCode(Mockito.anyString())).thenReturn(
      productReviewer);
    when(this.productReviewerService.findProductReviewerByProductCodesAndMarkForDeleteFalse(eq(STORE_ID),
      Mockito.anyList())).thenReturn(Collections.singletonList(productReviewer));
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(solrVendorProductCollectionRepository);
    Mockito.verifyNoMoreInteractions(productService);
    Mockito.verifyNoMoreInteractions(solrReindexPublisherService);
    Mockito.verifyNoMoreInteractions(configPropertiesService);
    Mockito.verifyNoMoreInteractions(count11);
    Mockito.verifyNoMoreInteractions(facetField3);
    Mockito.verifyNoMoreInteractions(facetField4);
    Mockito.verifyNoMoreInteractions(productReviewerService);
    Mockito.verifyNoMoreInteractions(productUtils);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
  }

  @Test
   void fullReindexPDTProductSolrTest() throws Exception {
    Mockito.doNothing()
        .when(solrVendorProductCollectionRepository)
        .deleteDocumentFromSolr(new ArrayList<>(), true);
    when(productReviewerService.findProductReviewerByProductCodesAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyList())).thenReturn(
        Collections.singletonList(productReviewer));
    when(productService.findByStoreIdAndMarkForDeleteFalseOrderByCreatedDateDesc(STORE_ID, pageable))
        .thenReturn(new PageImpl<>(Collections.singletonList(product), pageable, 1));
    when(solrReindexPublisherService.publishPDTProductSolrBatchAddDomainEventModelForReindex(
        Mockito.any(PDTProductSolrBatchAddDomainEventModel.class)))
        .thenReturn(new PDTProductSolrBatchAddDomainEventModel());
    when(configPropertiesService.findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
        ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME)).thenReturn(null);
    when(this.configPropertiesService.save(Mockito.any(ConfigProperties.class)))
        .thenReturn(new ConfigProperties());
    solrVendorCollectionServiceImpl.fullReindexPDTProductSolr(STORE_ID);
    verify(solrVendorProductCollectionRepository)
        .deleteDocumentFromSolr(new ArrayList<>(), true);
    verify(productService).findByStoreIdAndMarkForDeleteFalseOrderByCreatedDateDesc(STORE_ID, pageable);
    verify(solrReindexPublisherService)
        .publishPDTProductSolrBatchAddDomainEventModelForReindex(Mockito.any(PDTProductSolrBatchAddDomainEventModel.class));
    verify(configPropertiesService)
        .findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
            ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME);
    verify(configPropertiesService).save(Mockito.any(ConfigProperties.class));
    verify(productReviewerService).findProductReviewerByProductCodesAndMarkForDeleteFalse(eq(STORE_ID),
      Mockito.anyList());
  }

  @Test
   void deltaReindexPDTProductSolrProductCodeDeleteTest() throws Exception {
    product.setMarkForDelete(true);
    Mockito.doNothing()
        .when(solrVendorProductCollectionRepository)
        .deleteDocumentFromSolr(List.of(PRODUCT_CODE), false);
    when(productService.getProductByCode(PRODUCT_CODE))
        .thenReturn(product);
    when(configPropertiesService.findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
        ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME)).thenReturn(null);
    when(this.configPropertiesService.save(Mockito.any(ConfigProperties.class)))
        .thenReturn(new ConfigProperties());
    solrVendorCollectionServiceImpl.deltaReindexPDTProductSolr(STORE_ID, PRODUCT_CODE);
    verify(solrVendorProductCollectionRepository)
        .deleteDocumentFromSolr(List.of(PRODUCT_CODE), false);
    verify(productService).getProductByCode(PRODUCT_CODE);
    verify(productReviewerService).findProductReviewerByProductCode(PRODUCT_CODE);
  }

  @Test
   void deltaReindexPDTProductSolrProductCodeAddTest() throws Exception {
    when(productService.getProductByCode(PRODUCT_CODE)).thenReturn(product);
    when(productReviewerService.findProductReviewerByProductCode(PRODUCT_CODE)).thenReturn(productReviewer);
    when(configPropertiesService.findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
        ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME)).thenReturn(null);
    when(solrReindexPublisherService.publishPDTProductSolrBatchAddDomainEventModelForReindex(
        Mockito.any(PDTProductSolrBatchAddDomainEventModel.class)))
        .thenReturn(new PDTProductSolrBatchAddDomainEventModel());
    when(this.configPropertiesService.save(Mockito.any(ConfigProperties.class))).thenReturn(new ConfigProperties());
    solrVendorCollectionServiceImpl.deltaReindexPDTProductSolr(STORE_ID, PRODUCT_CODE);
    verify(solrReindexPublisherService).publishPDTProductSolrBatchAddDomainEventModelForReindex(
        Mockito.any(PDTProductSolrBatchAddDomainEventModel.class));
    verify(productService).getProductByCode(PRODUCT_CODE);
    verify(productReviewerService).findProductReviewerByProductCode(PRODUCT_CODE);
  }

  @Test
   void deltaReindexPDTProductSolrTest() throws Exception {
    Pageable newPageable = PageRequest.of(0, 100, Sort.by(Sort.Order.asc(Constants.PRODUCT_CODE), Sort.Order.asc(Constants.UPDATED_DATE)));
    Mockito.doNothing().when(solrVendorProductCollectionRepository)
        .deleteDocumentFromSolr(List.of(PRODUCT_CODE), false);
    when(productReviewerService.findProductReviewerByProductCodes(STORE_ID,
      Arrays.asList(PRODUCT_CODE, PRODUCT_CODE))).thenReturn(
        Collections.singletonList(productReviewer));
    when(productService
        .findByStoreIdAndUpdatedDateBetweenOrderByProductCodeAscAndUpdatedDateAsc(eq(STORE_ID), Mockito.any(), Mockito.any(),
            eq(newPageable))).thenReturn(new PageImpl<>(Arrays.asList(product, productDeleted), pageable, 2));
    when(solrReindexPublisherService.publishPDTProductSolrBatchAddDomainEventModelForReindex(
        Mockito.any(PDTProductSolrBatchAddDomainEventModel.class)))
        .thenReturn(new PDTProductSolrBatchAddDomainEventModel());
    when(solrReindexPublisherService.publishPDTProductSolrBatchDeleteDomainEventModelForReindex(
        Mockito.any(PDTProductSolrDeleteDomainEventModel.class)))
        .thenReturn(new PDTProductSolrDeleteDomainEventModel());
    when(configPropertiesService.findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
        ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME)).thenReturn(applicationConfigProperties);
    when(this.configPropertiesService.save(Mockito.any(ConfigProperties.class))).thenReturn(new ConfigProperties());
    solrVendorCollectionServiceImpl.deltaReindexPDTProductSolr(STORE_ID, StringUtils.SPACE);
    verify(productService)
        .findByStoreIdAndUpdatedDateBetweenOrderByProductCodeAscAndUpdatedDateAsc(eq(STORE_ID), Mockito.any(), Mockito.any(),
            eq(newPageable));
    verify(solrReindexPublisherService).publishPDTProductSolrBatchAddDomainEventModelForReindex(
        Mockito.any(PDTProductSolrBatchAddDomainEventModel.class));
    verify(solrReindexPublisherService).publishPDTProductSolrBatchDeleteDomainEventModelForReindex(
        Mockito.any(PDTProductSolrDeleteDomainEventModel.class));
    verify(configPropertiesService).findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
        ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME);
    verify(configPropertiesService).save(Mockito.any(ConfigProperties.class));
    verify(productReviewerService).findProductReviewerByProductCodes(eq(STORE_ID),
      Mockito.anyList());
  }

  @Test
   void deltaReindexPDTProductSolrResetLastUpdatedDateTest() throws Exception {
    Pageable newPageable = PageRequest.of(0, 100, Sort.by(Sort.Order.asc(Constants.PRODUCT_CODE), Sort.Order.asc(Constants.UPDATED_DATE)));
    Mockito.doNothing().when(solrVendorProductCollectionRepository)
            .deleteDocumentFromSolr(List.of(PRODUCT_CODE), false);
    when(productReviewerService.findProductReviewerByProductCodes(STORE_ID,
            Arrays.asList(PRODUCT_CODE, PRODUCT_CODE))).thenReturn(
        Collections.singletonList(productReviewer));
    when(productService
            .findByStoreIdAndUpdatedDateBetweenOrderByProductCodeAscAndUpdatedDateAsc(eq(STORE_ID), Mockito.any(), Mockito.any(),
                    eq(newPageable))).thenReturn(new PageImpl<>(Arrays.asList(product, productDeleted), pageable, 2));
    when(solrReindexPublisherService.publishPDTProductSolrBatchAddDomainEventModelForReindex(
            Mockito.any(PDTProductSolrBatchAddDomainEventModel.class)))
            .thenReturn(new PDTProductSolrBatchAddDomainEventModel());
    when(solrReindexPublisherService.publishPDTProductSolrBatchDeleteDomainEventModelForReindex(
            Mockito.any(PDTProductSolrDeleteDomainEventModel.class)))
            .thenReturn(new PDTProductSolrDeleteDomainEventModel());
    applicationConfigProperties.setValue(String.valueOf(LAST_UPDATED_DATE.getTime() - (4 * 60 * 60 * 1000)));
    when(configPropertiesService.findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
            ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME)).thenReturn(applicationConfigProperties);
    when(this.configPropertiesService.save(Mockito.any(ConfigProperties.class))).thenReturn(new ConfigProperties());
    solrVendorCollectionServiceImpl.deltaReindexPDTProductSolr(STORE_ID, StringUtils.SPACE);
    verify(productService)
            .findByStoreIdAndUpdatedDateBetweenOrderByProductCodeAscAndUpdatedDateAsc(eq(STORE_ID), Mockito.any(), Mockito.any(),
                    eq(newPageable));
    verify(solrReindexPublisherService).publishPDTProductSolrBatchAddDomainEventModelForReindex(
            Mockito.any(PDTProductSolrBatchAddDomainEventModel.class));
    verify(solrReindexPublisherService).publishPDTProductSolrBatchDeleteDomainEventModelForReindex(
            Mockito.any(PDTProductSolrDeleteDomainEventModel.class));
    verify(configPropertiesService).findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
            ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME);
    verify(configPropertiesService).save(Mockito.any(ConfigProperties.class));
    verify(productReviewerService).findProductReviewerByProductCodes(eq(STORE_ID),
            Mockito.anyList());
    verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(), messageEmailRequestArgumentCaptor.capture());
  }


  @Test
   void deltaReindexPDTProductSolrEmailExceptionTest() throws Exception {
    Pageable newPageable = PageRequest.of(0, 100, Sort.by(Sort.Order.asc(Constants.PRODUCT_CODE), Sort.Order.asc(Constants.UPDATED_DATE)));
    Mockito.doNothing().when(solrVendorProductCollectionRepository)
            .deleteDocumentFromSolr(List.of(PRODUCT_CODE), false);
    when(productReviewerService.findProductReviewerByProductCodes(STORE_ID,
            Arrays.asList(PRODUCT_CODE, PRODUCT_CODE))).thenReturn(
        Collections.singletonList(productReviewer));
    when(productService
            .findByStoreIdAndUpdatedDateBetweenOrderByProductCodeAscAndUpdatedDateAsc(eq(STORE_ID), Mockito.any(), Mockito.any(),
                    eq(newPageable))).thenReturn(new PageImpl<>(Arrays.asList(product, productDeleted), pageable, 2));
    when(solrReindexPublisherService.publishPDTProductSolrBatchAddDomainEventModelForReindex(
            Mockito.any(PDTProductSolrBatchAddDomainEventModel.class)))
            .thenReturn(new PDTProductSolrBatchAddDomainEventModel());
    when(solrReindexPublisherService.publishPDTProductSolrBatchDeleteDomainEventModelForReindex(
            Mockito.any(PDTProductSolrDeleteDomainEventModel.class)))
            .thenReturn(new PDTProductSolrDeleteDomainEventModel());
    Mockito.doThrow(RuntimeException.class).when(kafkaProducer)
            .send(Mockito.anyString(), Mockito.anyString(), messageEmailRequestArgumentCaptor.capture());
    applicationConfigProperties.setValue(String.valueOf(LAST_UPDATED_DATE.getTime() - (4 * 60 * 60 * 1000)));
    when(configPropertiesService.findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
            ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME)).thenReturn(applicationConfigProperties);
    when(this.configPropertiesService.save(Mockito.any(ConfigProperties.class))).thenReturn(new ConfigProperties());
    solrVendorCollectionServiceImpl.deltaReindexPDTProductSolr(STORE_ID, StringUtils.SPACE);
    verify(productService)
            .findByStoreIdAndUpdatedDateBetweenOrderByProductCodeAscAndUpdatedDateAsc(eq(STORE_ID), Mockito.any(), Mockito.any(),
                    eq(newPageable));
    verify(solrReindexPublisherService).publishPDTProductSolrBatchAddDomainEventModelForReindex(
            Mockito.any(PDTProductSolrBatchAddDomainEventModel.class));
    verify(solrReindexPublisherService).publishPDTProductSolrBatchDeleteDomainEventModelForReindex(
            Mockito.any(PDTProductSolrDeleteDomainEventModel.class));
    verify(configPropertiesService).findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
            ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME);
    verify(configPropertiesService).save(Mockito.any(ConfigProperties.class));
    verify(productReviewerService).findProductReviewerByProductCodes(eq(STORE_ID),
            Mockito.anyList());
    verify(kafkaProducer).send(Mockito.eq(KafkaEventNames.SEND_EMAIL_TO_OTHERS_EVENT), Mockito.anyString(), messageEmailRequestArgumentCaptor.capture());
  }

  @Test
   void deltaReindexPDTProductExceptionSolrTest() throws Exception {
    Pageable newPageable = PageRequest.of(0, 100, Sort.by(Sort.Order.asc(Constants.PRODUCT_CODE), Sort.Order.asc(Constants.UPDATED_DATE)));
    when(productService
        .findByStoreIdAndUpdatedDateBetweenOrderByProductCodeAscAndUpdatedDateAsc(eq(STORE_ID), Mockito.any(), Mockito.any(),
            eq(newPageable))).thenThrow(RuntimeException.class);
    when(configPropertiesService.findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
        ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME)).thenReturn(applicationConfigProperties);
    solrVendorCollectionServiceImpl.deltaReindexPDTProductSolr(STORE_ID, StringUtils.SPACE);
    verify(productService)
        .findByStoreIdAndUpdatedDateBetweenOrderByProductCodeAscAndUpdatedDateAsc(eq(STORE_ID), Mockito.any(), Mockito.any(),
            eq(newPageable));
    verify(configPropertiesService, times(1)).findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
        ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME);
  }

  @Test
   void fullReindexPDTProductSolr_exceptionTest() throws Exception {
    Mockito.doThrow(Exception.class)
        .when(solrVendorProductCollectionRepository)
        .deleteDocumentFromSolr(new ArrayList<>(), true);
    try {
      solrVendorCollectionServiceImpl.fullReindexPDTProductSolr(STORE_ID);
    } catch (Exception e) {

    } finally {
      verify(solrVendorProductCollectionRepository)
          .deleteDocumentFromSolr(new ArrayList<>(), true);
    }
  }

  @Test
   void fullReindexPDTProductSolrTest_multiplePublish() throws Exception {
    ReflectionTestUtils.setField(solrVendorCollectionServiceImpl, "pdtCollectionReindexBatchSize", 1);
    Mockito.doNothing()
        .when(solrVendorProductCollectionRepository)
        .deleteDocumentFromSolr(new ArrayList<>(), true);
    when(productReviewerService.findProductReviewerByProductCodesAndMarkForDeleteFalse(Mockito.anyString(), Mockito.anyList())).thenReturn(
        Collections.singletonList(productReviewer));
    when(productService.findByStoreIdAndMarkForDeleteFalseOrderByCreatedDateDesc(STORE_ID, pageable))
        .thenReturn(new PageImpl<>(Arrays.asList(product, product, product), pageable, 3));
    when(solrReindexPublisherService.publishPDTProductSolrBatchAddDomainEventModelForReindex(
        Mockito.any(PDTProductSolrBatchAddDomainEventModel.class)))
        .thenReturn(new PDTProductSolrBatchAddDomainEventModel());
    when(configPropertiesService.findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
        ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME)).thenReturn(null);
    when(this.configPropertiesService.save(Mockito.any(ConfigProperties.class)))
        .thenReturn(new ConfigProperties());
    solrVendorCollectionServiceImpl.fullReindexPDTProductSolr(STORE_ID);
    verify(solrVendorProductCollectionRepository)
        .deleteDocumentFromSolr(new ArrayList<>(), true);
    verify(productService).findByStoreIdAndMarkForDeleteFalseOrderByCreatedDateDesc(STORE_ID, pageable);
    verify(solrReindexPublisherService, times(2))
        .publishPDTProductSolrBatchAddDomainEventModelForReindex(Mockito.any(PDTProductSolrBatchAddDomainEventModel.class));
    verify(configPropertiesService)
        .findByStoreIdAndPropertyNameAndMarkForDeleteFalse(STORE_ID,
            ConfigPropertiesConstants.SOLR_PDT_COLLECTION_LAST_DELTA_RUN_TIME);
    verify(configPropertiesService).save(Mockito.any(ConfigProperties.class));
    verify(productReviewerService).findProductReviewerByProductCodesAndMarkForDeleteFalse(eq(STORE_ID),
      Mockito.anyList());
  }

  @Test
   void getFinalQcCounts() throws IOException, SolrServerException {
    when(this.solrVendorProductCollectionRepository.getFinalQCCounts(Constants.DEFAULT_STORE_ID))
        .thenReturn(queryResponse);
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getIntervalFacets()).thenReturn(intervalFacets);
    when(this.queryResponse.getFacetFields()).thenReturn(Arrays.asList(facetField1, facetField2));
    when(this.intervalFacets.get(0)).thenReturn(intervalFacet);
    when(this.intervalFacet.getIntervals()).thenReturn(Arrays.asList(count1, count2, count3, count4));
    when(this.count1.getCount()).thenReturn(FACET_COUNT);
    when(this.count2.getCount()).thenReturn(FACET_COUNT);
    when(this.count3.getCount()).thenReturn(FACET_COUNT);
    when(this.count4.getCount()).thenReturn(FACET_COUNT);
    when(this.count1.getKey()).thenReturn(SolrConstants.TODAY_FACET_INTERVAL);
    when(this.count2.getKey()).thenReturn(SolrConstants.YESTERDAY_FACET_INTERVAL);
    when(this.count3.getKey()).thenReturn(SolrConstants.TWO_TO_THREE_DAYS_AGO_FACET_INTERVAL);
    when(this.count4.getKey()).thenReturn(SolrConstants.THREE_DAYS_AGO_FACET_INTERVAL);
    Map<String, Object> filterCounts =
        this.solrVendorCollectionServiceImpl.getFinalQcCounts(Constants.DEFAULT_STORE_ID);
    verify(this.solrVendorProductCollectionRepository)
        .getFinalQCCounts(Constants.DEFAULT_STORE_ID);
    verify(this.queryResponse).getIntervalFacets();
    verify(this.intervalFacets).get(0);
    verify(this.intervalFacet).getIntervals();
    verify(this.count1).getCount();
    verify(this.count2).getCount();
    verify(this.count3).getCount();
    verify(this.count4).getCount();
    verify(this.count1).getKey();
    verify(this.count2).getKey();
    verify(this.count3).getKey();
    verify(this.count4).getKey();
    Assertions.assertNotNull(filterCounts);
  }

  @Test
   void getFinalQcCountsEmptyTest() throws IOException, SolrServerException {
    this.solrVendorCollectionServiceImpl.getFinalQcCounts(Constants.DEFAULT_STORE_ID);
    verify(this.solrVendorProductCollectionRepository)
        .getFinalQCCounts(Constants.DEFAULT_STORE_ID);
  }

  @Test
   void getDistributionListCounts() throws IOException, SolrServerException {
    when(this.solrVendorProductCollectionRepository.getDistributionListCounts(Constants.DEFAULT_STORE_ID))
        .thenReturn(queryResponse);
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getIntervalFacets()).thenReturn(intervalFacets);
    when(this.queryResponse.getFacetFields()).thenReturn(Collections.singletonList(facetField1));
    when(this.intervalFacets.get(0)).thenReturn(intervalFacet);
    when(this.intervalFacet.getIntervals()).thenReturn(Arrays.asList(count1, count2, count3, count4, count5));
    when(this.count1.getCount()).thenReturn(FACET_COUNT);
    when(this.count2.getCount()).thenReturn(FACET_COUNT);
    when(this.count3.getCount()).thenReturn(FACET_COUNT);
    when(this.count4.getCount()).thenReturn(FACET_COUNT);
    when(this.count1.getKey()).thenReturn(SolrConstants.TODAY_FACET_INTERVAL);
    when(this.count2.getKey()).thenReturn(SolrConstants.YESTERDAY_FACET_INTERVAL);
    when(this.count3.getKey()).thenReturn(SolrConstants.TWO_TO_THREE_DAYS_AGO_FACET_INTERVAL);
    when(this.count4.getKey()).thenReturn(SolrConstants.THREE_DAYS_AGO_FACET_INTERVAL);
    when(this.facetField1.getName()).thenReturn(VendorProductSolrFieldNames.STATE);
    when(this.facetField1.getValues()).thenReturn(Arrays.asList(count6, count7, count8, count9, count10));
    when(this.count6.getName()).thenReturn(WorkflowState.UNASSIGNED.name());
    when(this.count7.getName()).thenReturn(WorkflowState.IN_REVIEW.name());
    when(this.count8.getName()).thenReturn(WorkflowState.PASSED.name());
    when(this.count9.getName()).thenReturn(WorkflowState.PASSED.name());
    when(this.count10.getName()).thenReturn(WorkflowState.PASSED.name());
    when(this.count6.getCount()).thenReturn(COUNT);
    when(this.count7.getCount()).thenReturn((long) 11);
    when(this.count8.getCount()).thenReturn((long) 15);
    when(this.count9.getCount()).thenReturn((long) 17);
    when(this.count10.getCount()).thenReturn((long) 20);
    Map<String, Object> countMap = new HashMap<>();
    this.solrVendorCollectionServiceImpl.getDistributionListCounts(Constants.DEFAULT_STORE_ID, countMap);
    verify(this.solrVendorProductCollectionRepository)
        .getDistributionListCounts(Constants.DEFAULT_STORE_ID);
    verify(this.queryResponse).getIntervalFacets();
    verify(this.queryResponse).getFacetFields();
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
    verify(this.facetField1).getValues();
    verify(this.count6).getName();
    verify(this.count7, times(2)).getName();
    verify(this.count8, times(2)).getName();
    verify(this.count9, times(2)).getName();
    verify(this.count10, times(2)).getName();
    verify(this.count6).getCount();
    verify(this.count7).getCount();
  }

  @Test
   void getDistributionListCounts1() throws IOException, SolrServerException {
    when(this.solrVendorProductCollectionRepository.getDistributionListCounts(Constants.DEFAULT_STORE_ID))
        .thenReturn(queryResponse);
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getIntervalFacets()).thenReturn(intervalFacets);
    when(this.queryResponse.getFacetFields()).thenReturn(Collections.singletonList(facetField1));
    when(this.intervalFacets.get(0)).thenReturn(intervalFacet);
    when(this.intervalFacet.getIntervals()).thenReturn(Arrays.asList(count1, count2, count3, count4, count5));
    when(this.count1.getCount()).thenReturn(FACET_COUNT);
    when(this.count2.getCount()).thenReturn(FACET_COUNT);
    when(this.count3.getCount()).thenReturn(FACET_COUNT);
    when(this.count4.getCount()).thenReturn(FACET_COUNT);
    when(this.count1.getKey()).thenReturn(SolrConstants.TODAY_FACET_INTERVAL);
    when(this.count2.getKey()).thenReturn(SolrConstants.YESTERDAY_FACET_INTERVAL);
    when(this.count3.getKey()).thenReturn(SolrConstants.TWO_TO_THREE_DAYS_AGO_FACET_INTERVAL);
    when(this.count4.getKey()).thenReturn(SolrConstants.THREE_DAYS_AGO_FACET_INTERVAL);
    when(this.facetField1.getName()).thenReturn(VendorProductSolrFieldNames.STATE);
    when(this.facetField1.getValues()).thenReturn(Arrays.asList(count6, count7, count8, count9, count10));
    when(this.count6.getName()).thenReturn(WorkflowState.UNASSIGNED.name());
    when(this.count7.getName()).thenReturn(WorkflowState.IN_REVIEW.name());
    when(this.count8.getName()).thenReturn(WorkflowState.PASSED.name());
    when(this.count9.getName()).thenReturn(WorkflowState.PASSED.name());
    when(this.count10.getName()).thenReturn(WorkflowState.REJECTED.name());
    when(this.count6.getCount()).thenReturn(COUNT);
    when(this.count7.getCount()).thenReturn((long) 11);
    when(this.count8.getCount()).thenReturn((long) 15);
    when(this.count9.getCount()).thenReturn((long) 17);
    when(this.count10.getCount()).thenReturn((long) 20);
    Map<String, Object> countMap = new HashMap<>();
    this.solrVendorCollectionServiceImpl.getDistributionListCounts(Constants.DEFAULT_STORE_ID, countMap);
    verify(this.solrVendorProductCollectionRepository)
        .getDistributionListCounts(Constants.DEFAULT_STORE_ID);
    verify(this.queryResponse).getIntervalFacets();
    verify(this.queryResponse).getFacetFields();
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
    verify(this.facetField1).getValues();
    verify(this.count6).getName();
    verify(this.count7, times(2)).getName();
    verify(this.count8, times(2)).getName();
    verify(this.count9, times(2)).getName();
    verify(this.count10, times(2)).getName();
    verify(this.count6).getCount();
    verify(this.count7).getCount();
  }

  @Test
   void getDistributionListCountsEmptyResponse() throws IOException, SolrServerException {
    Map<String, Object> countMap = new HashMap<>();
    this.solrVendorCollectionServiceImpl.getDistributionListCounts(Constants.DEFAULT_STORE_ID, countMap);
    verify(this.solrVendorProductCollectionRepository)
        .getDistributionListCounts(Constants.DEFAULT_STORE_ID);
  }

  @Test
   void getFilterCounts() throws IOException, SolrServerException {
    when(this.solrVendorProductCollectionRepository
        .getFilterCounts(Constants.DEFAULT_STORE_ID,vendorCode, Boolean.FALSE, Boolean.FALSE,
          Boolean.FALSE))
        .thenReturn(queryResponse);
    when(this.queryResponse.getResults()).thenReturn(solrDocumentList);
    when(this.queryResponse.getIntervalFacets()).thenReturn(intervalFacets);
    when(this.queryResponse.getFacetFields()).thenReturn(Arrays.asList(facetField1, facetField2,
      facetField3, facetField4, facetField4));
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
    when(this.facetField1.getName()).thenReturn(VendorProductSolrFieldNames.BRAND_APPROVAL_STATUS);
    when(this.facetField2.getName()).thenReturn(VendorProductSolrFieldNames.ASSIGNED);
    when(this.facetField3.getName()).thenReturn(VendorProductSolrFieldNames.REVIEW_TYPE);
    when(this.facetField4.getName()).thenReturn(
        VendorProductSolrFieldNames.RESTRICTED_KEYWORDS_PRESENT)
      .thenReturn(VendorProductSolrFieldNames.PRODUCT_CODE);
    when(this.facetField1.getValues()).thenReturn(List.of(count6));
    when(this.facetField2.getValues()).thenReturn(Arrays.asList(count7, count8));
    when(this.facetField3.getValues()).thenReturn(Arrays.asList(count9, count10, count12));
    when(this.facetField4.getValues()).thenReturn(Arrays.asList(count11, count13));
    when(this.count6.getName()).thenReturn(BrandApprovalStatus.DRAFT.getValue());
    when(this.count7.getName()).thenReturn(Boolean.TRUE.toString());
    when(this.count9.getName()).thenReturn(String.valueOf(ReviewType.CONTENT.getValue()));
    when(this.count10.getName()).thenReturn(String.valueOf(ReviewType.IMAGE.getValue()));
    when(this.count12.getName()).thenReturn(String.valueOf(ReviewType.CONTENT_AND_IMAGE.getValue()));
    when(this.count11.getName()).thenReturn(Boolean.TRUE.toString());
    when(this.count13.getName()).thenReturn(Boolean.FALSE.toString());
    when(this.count8.getName()).thenReturn(Boolean.FALSE.toString());
    when(this.count7.getCount()).thenReturn((long) 11);
    when(this.count8.getCount()).thenReturn((long) 15);
    when(this.count9.getCount()).thenReturn((long) 11);
    when(this.count10.getCount()).thenReturn((long) 15);
    when(this.count11.getCount()).thenReturn((long) 15);
    when(this.count6.getCount()).thenReturn(COUNT);
    when(this.queryResponse.getFacetPivot()).thenReturn(namedList);
    when(this.namedList.get(CONTENT_APPROVER_ASSIGNEE_IMAGE_APPROVER_ASSIGNEE)).thenReturn(pivotFields1);
    Map<String, Object> filterCounts = this.solrVendorCollectionServiceImpl
        .getFilterCounts(Constants.DEFAULT_STORE_ID, vendorCode, Boolean.FALSE, Boolean.FALSE,
          Boolean.FALSE);
    verify(this.solrVendorProductCollectionRepository)
        .getFilterCounts(Constants.DEFAULT_STORE_ID, vendorCode, Boolean.FALSE, Boolean.FALSE,
          Boolean.FALSE);
    verify(this.queryResponse).getIntervalFacets();
    verify(this.queryResponse).getFacetFields();
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
    verify(this.count8).getCount();
    verify(this.count8).getName();
    verify(this.count11).getName();
    verify(this.count11).getCount();
    verify(this.facetField3).getName();
    verify(this.facetField4, times(3)).getName();
    verify(this.facetField3).getValues();
    verify(this.facetField4).getValues();
    Assertions.assertNotNull(filterCounts);
    Assertions.assertEquals(filterCounts.get(SolrConstants.CONTENT_PENDING), Long.valueOf(11));
    Assertions.assertEquals(filterCounts.get(SolrConstants.IMAGE_PENDING), Long.valueOf(15));
    Assertions.assertEquals(filterCounts.get(SolrConstants.UNASSIGNED), Long.valueOf(15));
    Assertions.assertEquals(filterCounts.get(SolrConstants.ASSIGNED), Long.valueOf(11));
    Assertions.assertEquals(filterCounts.get(SolrConstants.RESTRICTED_KEYWORD), Long.valueOf(15));
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
    primaryFilterDTO.setVendorCode(VENDOR_CODE);
    primaryFilterDTO.setKeyword(KEYWORD);
    Mockito.when(solrVendorProductCollectionRepository.getBusinessPartnerList(STORE_ID, primaryFilterDTO, pendingStates, PAGE, SIZE))
        .thenReturn(queryResponse);
    Mockito.when(queryResponse.getResults()).thenReturn(businessPartnersList);
    List<ProductBusinessPartnerMapperResponse> businessPartnerList =
        solrVendorCollectionServiceImpl.getBusinessPartnerList(STORE_ID, primaryFilterDTO, pendingStates, PAGE, SIZE);
    Mockito.verify(queryResponse, times(2)).getResults();
    Mockito.verify(solrVendorProductCollectionRepository)
        .getBusinessPartnerList(STORE_ID, primaryFilterDTO, pendingStates, PAGE, SIZE);
    Assertions.assertNotNull(businessPartnerList);
    Assertions.assertEquals(1, businessPartnerList.size());
  }

  @Test
   void getBusinessPartnerListEmptyList() throws IOException, SolrServerException {
    primaryFilterDTO.setAssignment(true);
    primaryFilterDTO.setBrandPending(true);
    primaryFilterDTO.setTimeFilterType(TimeFilterType.ALL);
    primaryFilterDTO.setContentPending(true);
    primaryFilterDTO.setImagePending(true);
    primaryFilterDTO.setEdited(true);
    primaryFilterDTO.setPostLive(true);
    primaryFilterDTO.setVendorCode(VENDOR_CODE);
    primaryFilterDTO.setKeyword(KEYWORD);
    Mockito.when(solrVendorProductCollectionRepository.getBusinessPartnerList(STORE_ID, primaryFilterDTO, pendingStates, PAGE, SIZE))
        .thenReturn(queryResponse);
    Mockito.when(queryResponse.getResults()).thenReturn(new SolrDocumentList());
    List<ProductBusinessPartnerMapperResponse> businessPartnerList =
        solrVendorCollectionServiceImpl.getBusinessPartnerList(STORE_ID, primaryFilterDTO, pendingStates, PAGE, SIZE);
    Mockito.verify(queryResponse, times(1)).getResults();
    Mockito.verify(solrVendorProductCollectionRepository)
        .getBusinessPartnerList(STORE_ID, primaryFilterDTO, pendingStates, PAGE, SIZE);
    Assertions.assertNotNull(businessPartnerList);
    Assertions.assertEquals(0, businessPartnerList.size());
  }

  @Test
   void addProductToSolrTest() throws Exception {
    ProductReviewer productReviewer = ProductReviewer.builder().build();
    product.setRejectedCount(0);
    Mockito.doNothing().when(this.solrVendorProductCollectionRepository)
        .addDocumentToSolrWithCategoryHierarchy(Mockito.anyList());
    solrVendorCollectionServiceImpl.addProductToSolr(product, productReviewer,
      new ImageQcProcessedAndBrandResponse());
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .addDocumentToSolrWithCategoryHierarchy(vendorProductSolrListArgumentCaptor.capture());
    Assertions.assertNotNull(vendorProductSolrListArgumentCaptor.getValue());
    Assertions.assertEquals(WorkflowState.IN_REVIEW,
        vendorProductSolrListArgumentCaptor.getValue().get(0).getState());
    Assertions.assertEquals(SolrConstants.NOT_APPLICABLE,
        vendorProductSolrListArgumentCaptor.getValue().get(0).getProductApproverAssignee());
    Assertions.assertEquals(VENDOR_CODE,
        vendorProductSolrListArgumentCaptor.getValue().get(0).getVendorCode());
  }

  @Test
   void deleteProductFromSolrTest() throws Exception {
    PDTProductSolrDeleteDomainEventModel pdtProductSolrDeleteDomainEventModel =
        PDTProductSolrDeleteDomainEventModel.builder().productCodes(List.of(PRODUCT_CODE)).build();
    solrVendorCollectionServiceImpl.deleteProductFromSolr(PRODUCT_CODE);
    Mockito.verify(this.solrReindexPublisherService)
        .publishPDTProductSolrBatchDeleteDomainEventModelForReindex(pdtProductSolrDeleteDomainEventModel);
  }

  @Test
   void assignProductToVendorAtomicUpdateTest() throws Exception {
    Mockito.doNothing().when(this.solrVendorProductCollectionRepository)
        .executeAtomicUpdateForListOfInputDocuments(Mockito.anyList());
    solrVendorCollectionServiceImpl
        .assignProductToVendorAtomicUpdate(STORE_ID, VENDOR_CODE, List.of(PRODUCT_CODE));
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .executeAtomicUpdateForListOfInputDocuments(solrInputDocumentListCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, solrInputDocumentListCaptor.getValue().get(0).get(
            VendorProductSolrFieldNames.PRODUCT_CODE)
        .getValue());
    Assertions.assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, VENDOR_CODE),
        solrInputDocumentListCaptor.getValue().get(0).get(VendorProductSolrFieldNames.VENDOR_CODE)
            .getFirstValue());
  }

  @Test
   void updateReviewerByProductCodeTest() throws Exception {
    solrVendorCollectionServiceImpl
        .updateReviewerByProductCodes(List.of(PRODUCT_CODE), REVIEWER_ASSIGNED, new Date());
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .executeAtomicUpdateForListOfInputDocuments(solrInputDocumentListCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, solrInputDocumentListCaptor.getValue().get(0).get(
            VendorProductSolrFieldNames.PRODUCT_CODE)
        .getValue());
    Assertions.assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, REVIEWER_ASSIGNED),
        solrInputDocumentListCaptor.getValue().get(0)
            .get(VendorProductSolrFieldNames.APPROVER_ASSIGNEE).getFirstValue());
  }


  @Test
   void updateImageQcResponseToSolrTest() throws Exception {
    product.setImageViolations(new StringJoiner(",").add(IMAGE).add(IMAGE).toString());
    solrVendorCollectionServiceImpl.updateImageQcResponseToSolr(product, productReviewer);
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .executeAtomicUpdate(solrInputDocumentCaptor.capture());
    Assertions.assertNotNull(solrInputDocumentCaptor.getValue());
    Assertions.assertEquals(PRODUCT_CODE, solrInputDocumentCaptor.getValue().get(
            VendorProductSolrFieldNames.PRODUCT_CODE)
        .getValue());
    Assertions.assertEquals(
        Collections.singletonMap(SolrConstants.SET_CLAUSE, Collections.singletonList(IMAGE)),
        solrInputDocumentCaptor.getValue().get(VendorProductSolrFieldNames.IMAGE_VIOLATIONS)
            .getFirstValue());
  }

  @Test
   void getReviewConfigCountsByVendorTest() throws Exception {
    Mockito.when(this.solrVendorProductCollectionRepository
        .getReviewConfigCountsByVendor(STORE_ID, VENDOR_CODE)).thenReturn(queryResponse);
    when(this.queryResponse.getFacetFields()).thenReturn(List.of(facetField1));
    when(this.facetField1.getName()).thenReturn(VendorProductSolrFieldNames.POST_LIVE);
    when(this.count6.getCount()).thenReturn(Long.valueOf(FACET_COUNT));
    when(this.count7.getCount()).thenReturn(Long.valueOf(FACET_COUNT));
    when(this.count6.getName()).thenReturn(Boolean.TRUE.toString());
    when(this.count7.getName()).thenReturn(Boolean.FALSE.toString());
    when(this.facetField1.getValues()).thenReturn(Arrays.asList(count6, count7));
    Map<String, Object> response =
        solrVendorCollectionServiceImpl.getReviewConfigCountsByVendor(STORE_ID, VENDOR_CODE);
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .getReviewConfigCountsByVendor(STORE_ID, VENDOR_CODE);
    Assertions.assertEquals(Long.valueOf(FACET_COUNT), response.get(POSTLIVE));
    Assertions.assertEquals(Long.valueOf(FACET_COUNT), response.get(PRELIVE));
  }

  @Test
   void getReviewConfigCountsByVendorAndConfigTest() throws Exception {
    Mockito.when(this.solrVendorProductCollectionRepository
        .getReviewConfigCountsByVendorAndConfig(STORE_ID, VENDOR_CODE, Boolean.TRUE)).thenReturn(queryResponse);
    when(this.queryResponse.getFacetFields()).thenReturn(List.of(facetField1));
    when(this.facetField1.getName()).thenReturn(VendorProductSolrFieldNames.PRODUCT_REVIEW_TYPE);
    when(this.count6.getCount()).thenReturn(Long.valueOf(FACET_COUNT));
    when(this.count7.getCount()).thenReturn(Long.valueOf(FACET_COUNT));
    when(this.count8.getCount()).thenReturn(Long.valueOf(FACET_COUNT));
    when(this.count6.getName()).thenReturn(ProductReviewType.NEWLY_ADDED.getValue());
    when(this.count7.getName()).thenReturn(ProductReviewType.EDITED.getValue());
    when(this.count8.getName()).thenReturn(ProductReviewType.REVISED.getValue());
    when(this.count9.getName()).thenReturn(STORE_ID);
    when(this.facetField1.getValues()).thenReturn(Arrays.asList(count6, count7, count8, count9));
    Map<String, Object> response =
        solrVendorCollectionServiceImpl.getReviewConfigCountsByVendorAndConfig(STORE_ID, VENDOR_CODE, Boolean.TRUE);
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .getReviewConfigCountsByVendorAndConfig(STORE_ID, VENDOR_CODE, Boolean.TRUE);
    Assertions.assertEquals(Long.valueOf(FACET_COUNT), response.get(EDITED));
    Assertions.assertEquals(Long.valueOf(FACET_COUNT), response.get(REVISED));
    Assertions.assertEquals(Long.valueOf(FACET_COUNT), response.get(NEWLY_ADDED));
  }

  @Test
   void updateSolrOnApprovalOrSaveTest() throws Exception {
    PDTProductUpdateProductToSolrEventModel pdtProductUpdateProductToSolrEventModel =
        new PDTProductUpdateProductToSolrEventModel();
    pdtProductUpdateProductToSolrEventModel.setProductCode(PRODUCT_CODE);
    pdtProductUpdateProductToSolrEventModel.setCategoryCodes(
        List.of(VendorProductSolrFieldNames.CATEGORY_CODES));
    Mockito.doReturn(solrDocumentList.get(0)).when(solrVendorProductCollectionRepository)
        .getSolrDocument(product.getProductCode());
    solrVendorCollectionServiceImpl.updateSolrOnApprovalOrSave(pdtProductUpdateProductToSolrEventModel);
    Mockito.verify(solrVendorProductCollectionRepository).getSolrDocument(PRODUCT_CODE);
    Mockito.verify(solrVendorProductCollectionRepository)
        .updateSolrOnApprovalOrSave(pdtProductUpdateProductToSolrEventModel, Boolean.FALSE);
  }

  @Test
  void updateSolrOnApprovalOrSave_nullTest() throws Exception {
    PDTProductUpdateProductToSolrEventModel pdtProductUpdateProductToSolrEventModel =
        new PDTProductUpdateProductToSolrEventModel();
    pdtProductUpdateProductToSolrEventModel.setProductCode(PRODUCT_CODE);
    pdtProductUpdateProductToSolrEventModel.setCategoryCodes(Arrays.asList(VendorProductSolrFieldNames.CATEGORY_CODES));
    Mockito.doReturn(null).when(solrVendorProductCollectionRepository)
        .getSolrDocument(product.getProductCode());
    solrVendorCollectionServiceImpl.updateSolrOnApprovalOrSave(pdtProductUpdateProductToSolrEventModel);
    Mockito.verify(solrVendorProductCollectionRepository).getSolrDocument(PRODUCT_CODE);
  }

  @Test
   void updateSolrOnApprovalOrSaveWithCategoryChangeTest() throws Exception {
    PDTProductUpdateProductToSolrEventModel pdtProductUpdateProductToSolrEventModel =
        new PDTProductUpdateProductToSolrEventModel();
    pdtProductUpdateProductToSolrEventModel.setProductCode(PRODUCT_CODE);
    pdtProductUpdateProductToSolrEventModel.setCategoryCodes(List.of(PRODUCT_CODE));
    Mockito.doReturn(solrDocumentList.get(0)).when(solrVendorProductCollectionRepository)
        .getSolrDocument(product.getProductCode());
    solrVendorCollectionServiceImpl.updateSolrOnApprovalOrSave(pdtProductUpdateProductToSolrEventModel);
    Mockito.verify(solrVendorProductCollectionRepository).getSolrDocument(PRODUCT_CODE);
    Mockito.verify(solrVendorProductCollectionRepository)
        .updateSolrOnApprovalOrSave(pdtProductUpdateProductToSolrEventModel, Boolean.TRUE);
  }

  @Test
   void updateSolrOnApprovalOrSaveContentWithoutCategoryChange() throws IOException, SolrServerException {
    product.setCategoryCode(VendorProductSolrFieldNames.CATEGORY_CODES);
    Mockito.doReturn(solrDocumentList.get(0)).when(solrVendorProductCollectionRepository)
        .getSolrDocument(product.getProductCode());
    solrVendorCollectionServiceImpl.updateSolrOnApprovalOrSave(product, productReviewer, CONTENT);
    Mockito.verify(solrVendorProductCollectionRepository).getSolrDocument(product.getProductCode());
    Mockito.verify(solrVendorProductCollectionRepository).updateSolrOnContentApprovalOrSave(product, productReviewer, Boolean.FALSE);
  }

  @Test
   void updateSolrOnApprovalOrSaveContentWithCategoryChange() throws IOException, SolrServerException {
    Mockito.doReturn(solrDocumentList.get(0)).when(solrVendorProductCollectionRepository)
        .getSolrDocument(product.getProductCode());
    solrVendorCollectionServiceImpl.updateSolrOnApprovalOrSave(product, productReviewer, CONTENT);
    Mockito.verify(solrVendorProductCollectionRepository).getSolrDocument(product.getProductCode());
    Mockito.verify(solrVendorProductCollectionRepository).updateSolrOnContentApprovalOrSave(product, productReviewer, Boolean.TRUE);
  }

  @Test
   void getProductResponseForAutoAssignmentTest() throws Exception {
    List<VendorProductSolr> vendorProductSolrList = new ArrayList<>();
    Page<VendorProductSolr> vendorProductSolrPage = new PageImpl<>(vendorProductSolrList);
    Mockito.when(
        this.solrVendorProductCollectionRepository.getFilteredAndBoostedProductsFromSolr(eq(STORE_ID), Mockito.any(),
            eq(pageable))).thenReturn(vendorProductSolrPage);
    solrVendorCollectionServiceImpl.getProductResponseForAutoAssignment(STORE_ID, new SummaryFilterDTO(), pageable);
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .getFilteredAndBoostedProductsFromSolr(eq(STORE_ID), Mockito.any(), eq(pageable));
  }

  @Test
   void updateSolrOnApprovalOrSaveContentWithCategoryChangeSolrException() throws IOException, SolrServerException {
    Mockito.doThrow(SolrException.class).when(solrVendorProductCollectionRepository)
        .getSolrDocument(product.getProductCode());
    solrVendorCollectionServiceImpl.updateSolrOnApprovalOrSave(product, productReviewer, CONTENT);
    Mockito.verify(solrVendorProductCollectionRepository).getSolrDocument(product.getProductCode());
  }

  @Test
   void updateSolrOnApprovalOrSaveContentWithCategoryChangeSolrServerException() throws IOException, SolrServerException {
    Mockito.doThrow(SolrServerException.class).when(solrVendorProductCollectionRepository)
        .getSolrDocument(product.getProductCode());
    solrVendorCollectionServiceImpl.updateSolrOnApprovalOrSave(product, productReviewer, CONTENT);
    Mockito.verify(solrVendorProductCollectionRepository).getSolrDocument(product.getProductCode());
  }

  @Test
   void updateSolrOnApprovalOrSaveContentWithCategoryChangeIOException() throws IOException, SolrServerException {
    Mockito.doThrow(IOException.class).when(solrVendorProductCollectionRepository)
        .getSolrDocument(product.getProductCode());
    solrVendorCollectionServiceImpl.updateSolrOnApprovalOrSave(product, productReviewer, CONTENT);
    Mockito.verify(solrVendorProductCollectionRepository).getSolrDocument(product.getProductCode());
  }

  @Test
   void updateSolrOnApprovalOrSaveImage() throws IOException, SolrServerException {
    solrVendorCollectionServiceImpl.updateSolrOnApprovalOrSave(product, productReviewer, IMAGE);
    Mockito.verify(solrVendorProductCollectionRepository).updateSolrOnImageApproval(product, productReviewer);
  }

  @Test
   void updateSolrOnBrandApprovalAndRejectionTest() throws IOException, SolrServerException {
    solrVendorCollectionServiceImpl
        .updateSolrOnBrandApprovalAndRejection(Collections.singletonList(PRODUCT_CODE), BRAND, BRAND_STATUS);
    verify(solrVendorProductCollectionRepository)
        .updateSolrOnBrandApprovalOrRejection(PRODUCT_CODE, BRAND, BRAND_STATUS);
  }

  @Test
   void updateSolrOnBrandApprovalAndRejectionIOExceptionTest() throws IOException, SolrServerException {
    Mockito.doThrow(IOException.class).when(solrVendorProductCollectionRepository)
        .updateSolrOnBrandApprovalOrRejection(PRODUCT_CODE, BRAND, BRAND_STATUS);
    solrVendorCollectionServiceImpl
        .updateSolrOnBrandApprovalAndRejection(Collections.singletonList(PRODUCT_CODE), BRAND, BRAND_STATUS);
    verify(solrVendorProductCollectionRepository)
        .updateSolrOnBrandApprovalOrRejection(PRODUCT_CODE, BRAND, BRAND_STATUS);
  }

  @Test
   void updateSolrOnBrandApprovalAndRejectionSolrServerExceptionTest() throws IOException, SolrServerException {
    Mockito.doThrow(SolrServerException.class).when(solrVendorProductCollectionRepository)
        .updateSolrOnBrandApprovalOrRejection(PRODUCT_CODE, BRAND, BRAND_STATUS);
    solrVendorCollectionServiceImpl
        .updateSolrOnBrandApprovalAndRejection(Collections.singletonList(PRODUCT_CODE), BRAND, BRAND_STATUS);
    verify(solrVendorProductCollectionRepository)
        .updateSolrOnBrandApprovalOrRejection(PRODUCT_CODE, BRAND, BRAND_STATUS);
  }

  @Test
   void updateSolrOnBrandApprovalAndRejectionSolrExceptionTest() throws IOException, SolrServerException {
    Mockito.doThrow(SolrException.class).when(solrVendorProductCollectionRepository)
        .updateSolrOnBrandApprovalOrRejection(PRODUCT_CODE, BRAND, BRAND_STATUS);
    solrVendorCollectionServiceImpl
        .updateSolrOnBrandApprovalAndRejection(Collections.singletonList(PRODUCT_CODE), BRAND, BRAND_STATUS);
    verify(solrVendorProductCollectionRepository)
        .updateSolrOnBrandApprovalOrRejection(PRODUCT_CODE, BRAND, BRAND_STATUS);
  }

  @Test
   void updateSolrOnProductSentBackToVendorTest() throws Exception {
    when(productReviewerService.findProductReviewerByProductCode(PRODUCT_CODE)).thenReturn(productReviewer);
    solrVendorCollectionServiceImpl.updateSolrOnProductSentBackToVendor(product);
    verify(solrReindexPublisherService).publishPDTProductSolrBatchAddDomainEventModelForReindex(
        pdtProductSolrBatchAddDomainEventModelArgumentCaptor.capture());
    verify(productReviewerService).findProductReviewerByProductCode(PRODUCT_CODE);
    Assertions.assertEquals(PRODUCT_CODE,
        pdtProductSolrBatchAddDomainEventModelArgumentCaptor.getValue().getPdtProductSolrAddDomainEventModelList()
            .get(0).getProductCode());
    verify(productReviewerService).findProductReviewerByProductCode(PRODUCT_CODE);
  }

  @Test
   void clearReviewerDetailsAndUpdateStateTest() throws Exception {
    Mockito.doNothing().when(this.solrVendorProductCollectionRepository)
        .executeAtomicUpdate(Mockito.any(SolrInputDocument.class));
    solrVendorCollectionServiceImpl
        .clearReviewerDetailsAndUpdateState(PRODUCT_CODE, WorkflowState.QC_REJECTED);
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .executeAtomicUpdate(solrInputDocumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, solrInputDocumentCaptor.getValue().get(
            VendorProductSolrFieldNames.PRODUCT_CODE)
        .getValue());
    Assertions.assertEquals(
        Collections.singletonMap(SolrConstants.SET_CLAUSE, WorkflowState.QC_REJECTED.toString()),
        solrInputDocumentCaptor.getValue().get(VendorProductSolrFieldNames.STATE).getFirstValue());
    Assertions.assertEquals(
        Collections.singletonMap(SolrConstants.SET_CLAUSE, SolrConstants.NOT_APPLICABLE),
        solrInputDocumentCaptor.getValue()
            .get(VendorProductSolrFieldNames.APPROVER_ASSIGNEE).getFirstValue());
  }

  @Test
   void getVendorProductsListTest() throws Exception {
    VendorProductSolr vendorProductSolr = new VendorProductSolr();
    vendorProductSolr.setCategoryNames(List.of(PRODUCT_CODE));
    vendorProductSolr.setCategoryCodes(List.of(PRODUCT_CODE));
    vendorProductSolr.setProductChannel(Arrays.asList(Constants.RETAIL, Constants.B2B));
    Page<VendorProductSolr> vendorProductSolrPage = new PageImpl<>(List.of(vendorProductSolr));
    Mockito.when(solrVendorProductCollectionRepository.getVendorProductsBySolrQuery(Mockito.anyString(),
            Mockito.any(SummaryFilterDTO.class), Mockito.anyList(), Mockito.any(Pageable.class)))
        .thenReturn(vendorProductSolrPage);
    Page<ProductAndReviewerDetailsDTO> productAndReviewerDetailsDTOS =
        solrVendorCollectionServiceImpl.getVendorProductsList(STORE_ID, new SummaryFilterDTO(), new ArrayList<>(),
            PageRequest.of(0, 1));
    Assertions.assertTrue(
        productAndReviewerDetailsDTOS.getContent().get(0).getProduct().isB2bActivated());
    Assertions.assertTrue(
        productAndReviewerDetailsDTOS.getContent().get(0).getProduct().isB2cActivated());
    Mockito.verify(solrVendorProductCollectionRepository)
        .getVendorProductsBySolrQuery(Mockito.anyString(), Mockito.any(SummaryFilterDTO.class), Mockito.anyList(),
            Mockito.any(Pageable.class));
  }

  @Test
   void getVendorProductsListVendorAutoHealTest() throws Exception {
    ReflectionTestUtils.setField(this.solrVendorCollectionServiceImpl, "vendorSearchAutoHeal", true);
    VendorProductSolr vendorProductSolr = new VendorProductSolr();
    vendorProductSolr.setCategoryNames(List.of(PRODUCT_CODE));
    vendorProductSolr.setCategoryCodes(List.of(PRODUCT_CODE));
    vendorProductSolr.setProductChannel(Arrays.asList(Constants.RETAIL, Constants.B2B));
    Page<VendorProductSolr> vendorProductSolrPage = new PageImpl<>(List.of(vendorProductSolr));
    Mockito.when(solrVendorProductCollectionRepository.getVendorProductsBySolrQuery(Mockito.anyString(),
            Mockito.any(SummaryFilterDTO.class), Mockito.anyList(), Mockito.any(Pageable.class)))
        .thenReturn(vendorProductSolrPage);
    Page<ProductAndReviewerDetailsDTO> productAndReviewerDetailsDTOS =
        solrVendorCollectionServiceImpl.getVendorProductsList(STORE_ID, new SummaryFilterDTO(), new ArrayList<>(),
            PageRequest.of(0, 1));
    Assertions.assertTrue(
        productAndReviewerDetailsDTOS.getContent().get(0).getProduct().isB2bActivated());
    Assertions.assertTrue(
        productAndReviewerDetailsDTOS.getContent().get(0).getProduct().isB2cActivated());
    Mockito.verify(solrVendorProductCollectionRepository)
        .getVendorProductsBySolrQuery(Mockito.anyString(), Mockito.any(SummaryFilterDTO.class), Mockito.anyList(),
            Mockito.any(Pageable.class));
  }

  @Test
   void getVendorProductsListVendorAutoHealNullResultTest() throws Exception {
    ReflectionTestUtils.setField(this.solrVendorCollectionServiceImpl, "vendorSearchAutoHeal", true);
    VendorProductSolr vendorProductSolr = new VendorProductSolr();
    vendorProductSolr.setCategoryNames(List.of(PRODUCT_CODE));
    vendorProductSolr.setCategoryCodes(List.of(PRODUCT_CODE));
    vendorProductSolr.setProductChannel(Arrays.asList(Constants.RETAIL, Constants.B2B));
    Page<VendorProductSolr> vendorProductSolrPage = new PageImpl<>(new ArrayList<>());
    Mockito.when(solrVendorProductCollectionRepository.getVendorProductsBySolrQuery(Mockito.anyString(),
            Mockito.any(SummaryFilterDTO.class), Mockito.anyList(), Mockito.any(Pageable.class)))
        .thenReturn(vendorProductSolrPage);
    Page<ProductAndReviewerDetailsDTO> productAndReviewerDetailsDTOS =
        solrVendorCollectionServiceImpl.getVendorProductsList(STORE_ID, new SummaryFilterDTO(), new ArrayList<>(),
            PageRequest.of(0, 1));
    Mockito.verify(solrVendorProductCollectionRepository)
        .getVendorProductsBySolrQuery(Mockito.anyString(), Mockito.any(SummaryFilterDTO.class), Mockito.anyList(),
            Mockito.any(Pageable.class));
  }

  @Test
   void getVendorProductsListVendorAutoHealEmptyResultNotProductCodeTest() throws Exception {
    ReflectionTestUtils.setField(this.solrVendorCollectionServiceImpl, "vendorSearchAutoHeal", true);
    VendorProductSolr vendorProductSolr = new VendorProductSolr();
    vendorProductSolr.setCategoryNames(List.of(PRODUCT_CODE));
    vendorProductSolr.setCategoryCodes(List.of(PRODUCT_CODE));
    vendorProductSolr.setProductChannel(Arrays.asList(Constants.RETAIL, Constants.B2B));
    Page<VendorProductSolr> vendorProductSolrPage = new PageImpl<>(new ArrayList<>());
    Mockito.when(solrVendorProductCollectionRepository.getVendorProductsBySolrQuery(Mockito.anyString(),
            Mockito.any(SummaryFilterDTO.class), Mockito.anyList(), Mockito.any(Pageable.class)))
        .thenReturn(vendorProductSolrPage);
    SummaryFilterDTO summaryFilterDTO = new SummaryFilterDTO();
    summaryFilterDTO.setKeyword(POSTLIVE);
    solrVendorCollectionServiceImpl.getVendorProductsList(STORE_ID, summaryFilterDTO, new ArrayList<>(),
        PageRequest.of(0, 1));
    Mockito.verify(solrVendorProductCollectionRepository)
        .getVendorProductsBySolrQuery(Mockito.anyString(), Mockito.any(SummaryFilterDTO.class), Mockito.anyList(),
            Mockito.any(Pageable.class));
    Mockito.verify(productUtils).isProductCode(POSTLIVE);
  }

  @Test
   void getVendorProductsListVendorAutoHealEmptyResultProductCodeTest() throws Exception {
    ReflectionTestUtils.setField(this.solrVendorCollectionServiceImpl, "vendorSearchAutoHeal", true);
    VendorProductSolr vendorProductSolr = new VendorProductSolr();
    vendorProductSolr.setCategoryNames(List.of(PRODUCT_CODE));
    vendorProductSolr.setCategoryCodes(List.of(PRODUCT_CODE));
    vendorProductSolr.setProductChannel(Arrays.asList(Constants.RETAIL, Constants.B2B));
    Page<VendorProductSolr> vendorProductSolrPage = new PageImpl<>(new ArrayList<>());
    Mockito.when(solrVendorProductCollectionRepository.getVendorProductsBySolrQuery(Mockito.anyString(),
            Mockito.any(SummaryFilterDTO.class), Mockito.anyList(), Mockito.any(Pageable.class)))
        .thenReturn(vendorProductSolrPage);
    SummaryFilterDTO summaryFilterDTO = new SummaryFilterDTO();
    summaryFilterDTO.setKeyword(PRODUCT_CODE);
    Mockito.when(productUtils.isProductCode(PRODUCT_CODE)).thenReturn(true);
    Mockito.when(productService.checkIfVendorAutoHealKeyExists(PRODUCT_CODE)).thenReturn(PRODUCT_CODE);
    solrVendorCollectionServiceImpl.getVendorProductsList(STORE_ID, summaryFilterDTO, new ArrayList<>(),
        PageRequest.of(0, 1));
    Mockito.verify(solrVendorProductCollectionRepository)
        .getVendorProductsBySolrQuery(Mockito.anyString(), Mockito.any(SummaryFilterDTO.class), Mockito.anyList(),
            Mockito.any(Pageable.class));
    Mockito.verify(productUtils).isProductCode(PRODUCT_CODE);
    Mockito.verify(productService).checkIfVendorAutoHealKeyExists(PRODUCT_CODE);
  }

  @Test
   void getVendorProductsListVendorAutoHealSuccessTest() throws Exception {
    ReflectionTestUtils.setField(this.solrVendorCollectionServiceImpl, "vendorSearchAutoHeal", true);
    VendorProductSolr vendorProductSolr = new VendorProductSolr();
    vendorProductSolr.setCategoryNames(List.of(PRODUCT_CODE));
    vendorProductSolr.setCategoryCodes(List.of(PRODUCT_CODE));
    vendorProductSolr.setProductChannel(Arrays.asList(Constants.RETAIL, Constants.B2B));
    Page<VendorProductSolr> vendorProductSolrPage = new PageImpl<>(new ArrayList<>());
    Mockito.when(solrVendorProductCollectionRepository.getVendorProductsBySolrQuery(Mockito.anyString(),
            Mockito.any(SummaryFilterDTO.class), Mockito.anyList(), Mockito.any(Pageable.class)))
        .thenReturn(vendorProductSolrPage);
    SummaryFilterDTO summaryFilterDTO = new SummaryFilterDTO();
    summaryFilterDTO.setKeyword(PRODUCT_CODE);
    Mockito.when(productUtils.isProductCode(PRODUCT_CODE)).thenReturn(true);
    solrVendorCollectionServiceImpl.getVendorProductsList(STORE_ID, summaryFilterDTO, new ArrayList<>(),
        PageRequest.of(0, 1));
    Mockito.verify(solrVendorProductCollectionRepository)
        .getVendorProductsBySolrQuery(Mockito.anyString(), Mockito.any(SummaryFilterDTO.class), Mockito.anyList(),
            Mockito.any(Pageable.class));
    Mockito.verify(productUtils).isProductCode(PRODUCT_CODE);
    Mockito.verify(productService).checkIfVendorAutoHealKeyExists(PRODUCT_CODE);
    Mockito.verify(productService).cacheVendorAutoHealKey(PRODUCT_CODE);
    VendorSearchAutoHealEventModel vendorSearchAutoHealEventModel = new VendorSearchAutoHealEventModel();
    vendorSearchAutoHealEventModel.setStoreId(STORE_ID);
    vendorSearchAutoHealEventModel.setProductCode(summaryFilterDTO.getKeyword());
    Mockito.verify(kafkaProducer)
        .send(DomainEventName.VENDOR_SEARCH_AUTO_HEAL_EVENT_NAME, summaryFilterDTO.getKeyword(),
            vendorSearchAutoHealEventModel);
  }

  @Test
   void autoApprovalReviewerDetailsAndUpdateStateTest() throws Exception {
    Mockito.doNothing().when(this.solrVendorProductCollectionRepository)
        .executeAtomicUpdate(Mockito.any(SolrInputDocument.class));
    solrVendorCollectionServiceImpl
        .autoApprovalReviewerDetailsAndUpdateState(PRODUCT_CODE, WorkflowState.PASSED);
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .executeAtomicUpdate(solrInputDocumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, solrInputDocumentCaptor.getValue().get(
            VendorProductSolrFieldNames.PRODUCT_CODE)
        .getValue());
    Assertions.assertEquals(
        Collections.singletonMap(SolrConstants.SET_CLAUSE, WorkflowState.PASSED.toString()),
        solrInputDocumentCaptor.getValue().get(VendorProductSolrFieldNames.STATE).getFirstValue());
    Assertions.assertEquals(
        Collections.singletonMap(SolrConstants.SET_CLAUSE, SolrConstants.AUTO_APPROVED),
        solrInputDocumentCaptor.getValue()
            .get(VendorProductSolrFieldNames.APPROVER_ASSIGNEE).getFirstValue());
  }

  @Test
   void updateStateOnSolrTest() throws Exception {
    Mockito.doNothing().when(this.solrVendorProductCollectionRepository)
        .executeAtomicUpdate(Mockito.any(SolrInputDocument.class));
    solrVendorCollectionServiceImpl
        .updateStateOnSolr(PRODUCT_CODE, WorkflowState.QC_REJECTED);
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .executeAtomicUpdate(solrInputDocumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, solrInputDocumentCaptor.getValue().get(
            VendorProductSolrFieldNames.PRODUCT_CODE)
        .getValue());
    Assertions.assertEquals(
        Collections.singletonMap(SolrConstants.SET_CLAUSE, WorkflowState.QC_REJECTED.toString()),
        solrInputDocumentCaptor.getValue().get(VendorProductSolrFieldNames.STATE).getFirstValue());
  }

  @Test
   void updateStateAndMfdTrueOnSolrTest() throws Exception {
    Mockito.doNothing().when(this.solrVendorProductCollectionRepository)
        .executeAtomicUpdate(Mockito.any(SolrInputDocument.class));
    solrVendorCollectionServiceImpl
        .updateStateAndMfdTrueOnSolr(PRODUCT_CODE, WorkflowState.QC_REJECTED);
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .executeAtomicUpdate(solrInputDocumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, solrInputDocumentCaptor.getValue().get(
            VendorProductSolrFieldNames.PRODUCT_CODE)
        .getValue());
    Assertions.assertEquals(
        Collections.singletonMap(SolrConstants.SET_CLAUSE, WorkflowState.QC_REJECTED.toString()),
        solrInputDocumentCaptor.getValue().get(VendorProductSolrFieldNames.STATE).getFirstValue());
  }

  @Test
   void updatePostLiveFlagTest() throws Exception {
    Mockito.doNothing().when(this.solrVendorProductCollectionRepository)
        .executeAtomicUpdate(Mockito.any(SolrInputDocument.class));
    solrVendorCollectionServiceImpl
        .updatePostLiveFlag(PRODUCT_CODE, true);
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .executeAtomicUpdate(solrInputDocumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, solrInputDocumentCaptor.getValue().get(
            VendorProductSolrFieldNames.PRODUCT_CODE)
        .getValue());
    Assertions.assertEquals(Collections.singletonMap(SolrConstants.SET_CLAUSE, true),
        solrInputDocumentCaptor.getValue().get(VendorProductSolrFieldNames.POST_LIVE)
            .getFirstValue());
  }

  @Test
   void findProductBusinessPartnerMapperTest() throws Exception {
    SolrDocument businessPartner = new SolrDocument();
    businessPartner.addField(VendorProductSolrFieldNames.BUSINESS_PARTNER_NAME, BRAND);
    businessPartner.addField(VendorProductSolrFieldNames.BUSINESS_PARTNER_CODE, BRAND_STATUS);
    businessPartnersList.add(businessPartner);
    Mockito.when(this.solrVendorProductCollectionRepository
        .findProductBusinessPartnerMapper(pendingStates, null, pageable, STORE_ID)).thenReturn(queryResponse);
    when(this.queryResponse.getResults()).thenReturn(businessPartnersList);
    Page<ProductBusinessPartnerMapper> businessPartnerMapper =
        solrVendorCollectionServiceImpl.findProductBusinessPartnerMapper(pendingStates, null, pageable, STORE_ID);
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .findProductBusinessPartnerMapper(pendingStates, null, pageable, STORE_ID);
    Assertions.assertEquals(businessPartnerMapper.getContent().size(), 2);
  }

  @Test
   void findProductBusinessPartnerMapperEmptyTest() throws Exception {
    Mockito.when(this.solrVendorProductCollectionRepository
        .findProductBusinessPartnerMapper(pendingStates, null, pageable, STORE_ID)).thenReturn(queryResponse);
    when(this.queryResponse.getResults()).thenReturn(new SolrDocumentList());
    Page<ProductBusinessPartnerMapper> businessPartnerMapper =
        solrVendorCollectionServiceImpl.findProductBusinessPartnerMapper(pendingStates, null, pageable, STORE_ID);
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .findProductBusinessPartnerMapper(pendingStates, null, pageable, STORE_ID);
    Assertions.assertEquals(businessPartnerMapper.getContent().size(), 0);
  }

  @Test
   void getFilterProductSummaryTest() throws Exception {
    List<VendorProductSolr> vendorProductSolrList = new ArrayList<>();
    Page<VendorProductSolr> vendorProductSolrPage = new PageImpl<>(vendorProductSolrList);
    Mockito.when(this.solrVendorProductCollectionRepository
        .getFilterProductSummaryBySolrQuery(eq(STORE_ID), Mockito.any(), Mockito.any(), eq(pageable))).thenReturn(vendorProductSolrPage);
    solrVendorCollectionServiceImpl.getFilterProductSummary(STORE_ID, new ProductListRequest(), new ArrayList<>(), pageable);
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .getFilterProductSummaryBySolrQuery(eq(STORE_ID), Mockito.any(), Mockito.any(), eq(pageable));
  }

  @Test
   void getAllProductDetailsWithMultipleFilterSolrTest() throws Exception {
    List<VendorProductSolr> vendorProductSolrList = new ArrayList<>();
    Page<VendorProductSolr> vendorProductSolrPage = new PageImpl<>(vendorProductSolrList);
    Mockito.when(this.solrVendorProductCollectionRepository
        .getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(Mockito.any(DistributionTaskMultipleFilterDTO.class), eq(pageable)))
        .thenReturn(vendorProductSolrPage);
    solrVendorCollectionServiceImpl.getAllProductDetailsWithMultipleFilterSolr(
        new DistributionTaskMultipleFilterDTO(), pageable, STORE_ID);
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(Mockito.any(DistributionTaskMultipleFilterDTO.class), eq(pageable));
  }

  @Test
  void getDistributionProductsListVendorAutoHealTest() throws Exception {
    ReflectionTestUtils.setField(this.solrVendorCollectionServiceImpl,
        "distributionListSearchAutoHeal", true);
    VendorProductSolr vendorProductSolr = new VendorProductSolr();
    vendorProductSolr.setCategoryNames(List.of(PRODUCT_CODE));
    vendorProductSolr.setCategoryCodes(List.of(PRODUCT_CODE));
    vendorProductSolr.setProductChannel(Arrays.asList(Constants.RETAIL, Constants.B2B));
    Page<VendorProductSolr> vendorProductSolrPage =
        new PageImpl<>(List.of(vendorProductSolr));
    Mockito.when(
            solrVendorProductCollectionRepository.getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(
                Mockito.any(DistributionTaskMultipleFilterDTO.class), Mockito.any(Pageable.class)))
        .thenReturn(vendorProductSolrPage);
    solrVendorCollectionServiceImpl.getAllProductDetailsWithMultipleFilterSolr(
        new DistributionTaskMultipleFilterDTO(), pageable, STORE_ID);
    Mockito.verify(solrVendorProductCollectionRepository)
        .getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(
            Mockito.any(DistributionTaskMultipleFilterDTO.class), Mockito.any(Pageable.class));
  }

  @Test
  void getDistributionProductsListVendorAutoHealNullResultTest() throws Exception {
    ReflectionTestUtils.setField(this.solrVendorCollectionServiceImpl,
        "distributionListSearchAutoHeal", true);
    VendorProductSolr vendorProductSolr = new VendorProductSolr();
    vendorProductSolr.setCategoryNames(List.of(PRODUCT_CODE));
    vendorProductSolr.setCategoryCodes(List.of(PRODUCT_CODE));
    vendorProductSolr.setProductChannel(Arrays.asList(Constants.RETAIL, Constants.B2B));
    Page<VendorProductSolr> vendorProductSolrPage = new PageImpl<>(new ArrayList<>());
    Mockito.when(
            solrVendorProductCollectionRepository.getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(
                Mockito.any(DistributionTaskMultipleFilterDTO.class), Mockito.any(Pageable.class)))
        .thenReturn(vendorProductSolrPage);
    solrVendorCollectionServiceImpl.getAllProductDetailsWithMultipleFilterSolr(
        new DistributionTaskMultipleFilterDTO(), pageable, STORE_ID);
    Mockito.verify(solrVendorProductCollectionRepository)
        .getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(
            Mockito.any(DistributionTaskMultipleFilterDTO.class), Mockito.any(Pageable.class));
  }

  @Test
  void getDistributionProductsListVendorAutoHealEmptyResultNotProductCodeTest() throws Exception {
    ReflectionTestUtils.setField(this.solrVendorCollectionServiceImpl,
        "distributionListSearchAutoHeal", true);
    VendorProductSolr vendorProductSolr = new VendorProductSolr();
    vendorProductSolr.setCategoryNames(List.of(PRODUCT_CODE));
    vendorProductSolr.setCategoryCodes(List.of(PRODUCT_CODE));
    vendorProductSolr.setProductChannel(Arrays.asList(Constants.RETAIL, Constants.B2B));
    Page<VendorProductSolr> vendorProductSolrPage = new PageImpl<>(new ArrayList<>());
    Mockito.when(
            solrVendorProductCollectionRepository.getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(
                Mockito.any(DistributionTaskMultipleFilterDTO.class), Mockito.any(Pageable.class)))
        .thenReturn(vendorProductSolrPage);
    solrVendorCollectionServiceImpl.getAllProductDetailsWithMultipleFilterSolr(
        new DistributionTaskMultipleFilterDTO(), pageable, STORE_ID);
    Mockito.verify(solrVendorProductCollectionRepository)
        .getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(
            Mockito.any(DistributionTaskMultipleFilterDTO.class), Mockito.any(Pageable.class));
  }

  @Test
  void getDistributionProductsListVendorAutoHealEmptyResultProductCodeTest() throws Exception {
    ReflectionTestUtils.setField(this.solrVendorCollectionServiceImpl,
        "distributionListSearchAutoHeal", true);
    VendorProductSolr vendorProductSolr = new VendorProductSolr();
    vendorProductSolr.setCategoryNames(List.of(PRODUCT_CODE));
    vendorProductSolr.setCategoryCodes(List.of(PRODUCT_CODE));
    vendorProductSolr.setProductChannel(Arrays.asList(Constants.RETAIL, Constants.B2B));
    Page<VendorProductSolr> vendorProductSolrPage = new PageImpl<>(new ArrayList<>());
    Mockito.when(
            solrVendorProductCollectionRepository.getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(
                Mockito.any(DistributionTaskMultipleFilterDTO.class), Mockito.any(Pageable.class)))
        .thenReturn(vendorProductSolrPage);
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO =
        new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setProductName(PRODUCT_CODE);
    Mockito.when(productUtils.isProductCode(PRODUCT_CODE)).thenReturn(true);
    Mockito.when(productService.checkIfVendorAutoHealKeyExists(PRODUCT_CODE))
        .thenReturn(PRODUCT_CODE);
    solrVendorCollectionServiceImpl.getAllProductDetailsWithMultipleFilterSolr(
        distributionTaskMultipleFilterDTO, pageable, STORE_ID);
    Mockito.verify(solrVendorProductCollectionRepository)
        .getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(
            Mockito.any(DistributionTaskMultipleFilterDTO.class), Mockito.any(Pageable.class));
    Mockito.verify(productUtils).isProductCode(PRODUCT_CODE);
    Mockito.verify(productService).checkIfVendorAutoHealKeyExists(PRODUCT_CODE);
  }

  @Test
  void getDistributionProductsListVendorAutoHealNotProductCodeTest() throws Exception {
    ReflectionTestUtils.setField(this.solrVendorCollectionServiceImpl,
        "distributionListSearchAutoHeal", true);
    VendorProductSolr vendorProductSolr = new VendorProductSolr();
    vendorProductSolr.setCategoryNames(List.of(PRODUCT_CODE));
    vendorProductSolr.setCategoryCodes(List.of(PRODUCT_CODE));
    vendorProductSolr.setProductChannel(Arrays.asList(Constants.RETAIL, Constants.B2B));
    Page<VendorProductSolr> vendorProductSolrPage = new PageImpl<>(new ArrayList<>());
    Mockito.when(
            solrVendorProductCollectionRepository.getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(
                Mockito.any(DistributionTaskMultipleFilterDTO.class), Mockito.any(Pageable.class)))
        .thenReturn(vendorProductSolrPage);
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO =
        new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setProductName(PRODUCT_CODE);
    Mockito.when(productUtils.isProductCode(PRODUCT_CODE)).thenReturn(false);
    Mockito.when(productService.checkIfVendorAutoHealKeyExists(PRODUCT_CODE))
        .thenReturn(PRODUCT_CODE);
    solrVendorCollectionServiceImpl.getAllProductDetailsWithMultipleFilterSolr(
        distributionTaskMultipleFilterDTO, pageable, STORE_ID);
    Mockito.verify(solrVendorProductCollectionRepository)
        .getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(
            Mockito.any(DistributionTaskMultipleFilterDTO.class), Mockito.any(Pageable.class));
    Mockito.verify(productUtils).isProductCode(PRODUCT_CODE);
  }

  @Test
  void getDistributionProductsListVendorAutoHealSuccessTest() throws Exception {
    ReflectionTestUtils.setField(this.solrVendorCollectionServiceImpl,
        "distributionListSearchAutoHeal", true);
    VendorProductSolr vendorProductSolr = new VendorProductSolr();
    vendorProductSolr.setCategoryNames(List.of(PRODUCT_CODE));
    vendorProductSolr.setCategoryCodes(List.of(PRODUCT_CODE));
    vendorProductSolr.setProductChannel(Arrays.asList(Constants.RETAIL, Constants.B2B));
    Page<VendorProductSolr> vendorProductSolrPage = new PageImpl<>(new ArrayList<>());
    Mockito.when(
            solrVendorProductCollectionRepository.getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(
                Mockito.any(DistributionTaskMultipleFilterDTO.class), Mockito.any(Pageable.class)))
        .thenReturn(vendorProductSolrPage);
    DistributionTaskMultipleFilterDTO distributionTaskMultipleFilterDTO =
        new DistributionTaskMultipleFilterDTO();
    distributionTaskMultipleFilterDTO.setProductName(PRODUCT_CODE);
    Mockito.when(productUtils.isProductCode(PRODUCT_CODE)).thenReturn(true);
    solrVendorCollectionServiceImpl.getAllProductDetailsWithMultipleFilterSolr(
        distributionTaskMultipleFilterDTO, pageable, STORE_ID);
    Mockito.verify(solrVendorProductCollectionRepository)
        .getAllProductDetailsWithMultipleFilterForDistributionListBySolrQuery(
            Mockito.any(DistributionTaskMultipleFilterDTO.class), Mockito.any(Pageable.class));
    Mockito.verify(productUtils).isProductCode(PRODUCT_CODE);
    Mockito.verify(productService).checkIfVendorAutoHealKeyExists(PRODUCT_CODE);
    Mockito.verify(productService).cacheVendorAutoHealKey(PRODUCT_CODE);
    VendorSearchAutoHealEventModel vendorSearchAutoHealEventModel =
        new VendorSearchAutoHealEventModel();
    vendorSearchAutoHealEventModel.setStoreId(STORE_ID);
    vendorSearchAutoHealEventModel.setProductCode(
        distributionTaskMultipleFilterDTO.getProductName());
    Mockito.verify(kafkaProducer).send(DomainEventName.VENDOR_SEARCH_AUTO_HEAL_EVENT_NAME,
        distributionTaskMultipleFilterDTO.getProductName(), vendorSearchAutoHealEventModel);
  }

  @Test
   void updateProductOnRetryStatusUpdateTest() throws Exception {
    product.setReviewType(ReviewType.CONTENT);
    this.solrVendorCollectionServiceImpl.updateProductOnRetryStatusUpdate(product);
    Mockito.verify(this.solrVendorProductCollectionRepository)
      .executeAtomicUpdate(Mockito.any(SolrInputDocument.class));
  }


  @Test
   void testPublishSolrAddPDTProductBatchEvent_EmptyList() throws Exception {
    ReflectionTestUtils.setField(solrVendorCollectionServiceImpl, "replaceEmptyReviewType", true);
    solrVendorCollectionServiceImpl.publishSolrAddPDTProductBatchEvent(Collections.emptyList());
    verify(solrReindexPublisherService).publishPDTProductSolrBatchAddDomainEventModelForReindex(Mockito.any());
  }

  @Test
   void testPublishSolrAddPDTProductBatchEvent_NullReviewInList() throws Exception {
    ReflectionTestUtils.setField(solrVendorCollectionServiceImpl, "replaceEmptyReviewType", true);
    Product productWithNullReview = new Product();
    productWithNullReview.setReviewType(null);
    ProductAndReviewerDetailsDTO dtoWithNullReview = new ProductAndReviewerDetailsDTO(productWithNullReview, new ProductReviewer());
    List<ProductAndReviewerDetailsDTO> listWithNullReview = Collections.singletonList(dtoWithNullReview);
    solrVendorCollectionServiceImpl.publishSolrAddPDTProductBatchEvent(listWithNullReview);

    verify(solrReindexPublisherService, times(1)).publishPDTProductSolrBatchAddDomainEventModelForReindex(Mockito.any());
  }

  @Test
   void testPublishSolrAddPDTProductBatchEvent_NonNullReviewInList() throws Exception {
    ReflectionTestUtils.setField(solrVendorCollectionServiceImpl, "replaceEmptyReviewType", true);
    Product productWithNonNullReview = new Product();
    productWithNonNullReview.setReviewType(ReviewType.CONTENT);
    ProductAndReviewerDetailsDTO dtoWithNonNullReview =
      new ProductAndReviewerDetailsDTO(productWithNonNullReview, new ProductReviewer());
    List<ProductAndReviewerDetailsDTO> listWithNonNullReview = Collections.singletonList(dtoWithNonNullReview);

    solrVendorCollectionServiceImpl.publishSolrAddPDTProductBatchEvent(listWithNonNullReview);
    verify(solrReindexPublisherService, times(1)).publishPDTProductSolrBatchAddDomainEventModelForReindex(Mockito.any());
  }

}
