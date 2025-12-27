package com.gdn.mta.product.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.anyList;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.EnumMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.HistoryUpdateRequest;
import com.gda.mta.product.dto.response.AuditTrailListRequest;
import com.gda.mta.product.dto.response.HistoryUpdateResponse;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.UpdatedProductHistoryCustomRepository;
import com.gdn.mta.product.service.config.PreOrderConfig;
import org.apache.commons.lang3.StringUtils;
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
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.mockito.verification.VerificationMode;
import org.slf4j.MDC;
import org.springframework.context.ApplicationContext;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.HistoryRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.response.HistoryResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.product.commons.annotation.LogAuditUpdateProduct;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.repository.AuditTrailUpdateProductBackupRepository;
import com.gdn.mta.product.repository.UpdatedProductHistoryRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.SolrHistoryCollectionRepository;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.mta.product.service.converter.UpdateProductItemLevel3ModelConverter;
import com.gdn.mta.product.service.util.AuditTrailMandatoryRequestParameterUtil;
import com.gdn.mta.product.service.util.LogAuditUpdateProductAnnotationUtil;
import com.gdn.mta.product.valueobject.UpdateProductItemLevel3Model;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.dto.ProductDTO;
import com.gdn.x.product.rest.web.model.request.B2bFieldsRequest;
import com.gdn.x.product.rest.web.model.request.ItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;

public class UpdatedProductHistoryServiceBeanTest {

  private static final String DEFAULT_GDN_SKU = "Bibli-01-01-3";
  private static final String DEFAULT_GDN_SKU_2 = "Bibli-01-01-4";
  private static final String DEFAULT_PRODUCT_SKU = "Bibli-01-01";
  private static final String GDN_NAME = "Bibli";
  private static final int DEFAULT_PAGE = 0;
  private static final int DEFAULT_SIZE = 10;
  private static final String DEFAULT_BP_CODE = "BLI-010235";
  private static final String DEFAULT_ACCESS_CHANNEL = "Access channel";
  private static final String DEFAULT_ACTION_KEY = "OFFLINE_TO_ONLINE";
  private static final String DEFAULT_ACTIVITY = "test activity";
  private static final String DEFAULT_OLD_VALUE = "old value";
  private static final String DEFAULT_NEW_VALUE = "new value";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_PRODUCT_CODE = "MTA-0001";
  private static final String SYSTEM = "System";
  private static final String RICH_TEXT = "<p>&nbsp;</p>";
  private static final String FORMATTED_TEXT = "<p> </p>";
  private static final VerificationMode TWO_TIMES = Mockito.times(2);
  private static final String DEFAULT_REQUEST_ID = "requestId";
  public static final String CLIENT_HOST_KEY = "clientHost";
  public static final String AUDIT_TRAIL_ID = "123";
  public static final String DEFAULT_USER = "defaultUser";
  public static final Long AUDIT_TRAIL_ID1 = 126L;
  public static final boolean NEED_CORRECTION = false;
  public static final String MERCHANT_SKU =  "merchantSku";
  public static final String PRODUCT_NAME =  "productName";
  public static final String USP =  "usp";
  public static final String URL1 =  "url1";
  public static final String URL2=  "url2";
  public static final String PRODUCT_SKU = "productSku";
  public static final String ACTIVITY = "activity";
  public static final String OLD_VALUE = "oldValue";
  public static final String NEW_VALUE = "newValue";
  public static final String CHANGED_BY = "changedBy";
  public static final String BUSINESSPARTNER_CODE = "businessPartnerCode";
  public static final String[] attributes = new String[]{"RAM#_#4GB", "OS#_#WINDOWS"};
  public static final String[] attributesUpdated= new String[]{"OS#_#IOS", "RAM#_#4GB"};
  public static final String PICKUP_POINT_CODE = "pickupPointCode";
  public static final String REQUEST_ID = "requestId";
  public static final Double PRICE = 12.3;
  public static final String MESSAGE = "message";
  private static final String PRODUCT_ID = "product_id";
  private static final String STORE_ID = "10001";
  private static final String SIZE_CHART_CODE = "SizeChartCode";
  Pageable pageable = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
  private HistoryRequest historyRequest;
  private B2bFieldsRequest b2bFieldsRequest;

  @InjectMocks
  private UpdatedProductHistoryServiceBean updatedProductHistoryService;

  @Mock
  private UpdatedProductHistoryRepository updatedProductHistoryRepository;

  @Mock
  private UpdatedProductHistoryCustomRepository updatedProductHistoryCustomRepository;

  @Mock
  private LogAuditUpdateProductAnnotationUtil logAuditUpdateProductAnnotationUtil;

  @Mock
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Mock
  private UpdateProductItemLevel3ModelConverter updateProductItemLevel3ModelConverter;

  @Mock
  private ProductSystemParameterService productSystemParameterService;

  @Mock
  private AuditTrailUpdateProductBackupRepository auditTrailUpdateProductBackupRepository;

  @Mock
  private SolrHistoryCollectionRepository solrHistoryCollectionRepository;

  @Mock
  private XProductOutbound xProductOutbound;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private UpdatedProductHistoryService updatedProductHistoryServiceMock;

  @Mock
  private ApplicationContext applicationContext;

  @Mock
  private BusinessPartnerRepository businessPartnerRepository;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private PreOrderConfig preOrderConfig;

  @Captor
  private ArgumentCaptor<List<SolrInputDocument>> solrListArgumentCaptor;

  private ProductBusinessPartner productBusinessPartner;
  private Page<UpdatedProductHistory> logAuditTrailUpdatedProductPage;
  private ProductLevel3UpdateRequest productLevel3UpdateRequest;
  private AuditTrailDto auditTrailRequest;
  private HistoryUpdateRequest historyUpdateRequest = new HistoryUpdateRequest();
  private ProfileResponse profileResponse = new ProfileResponse();

  @Captor
  private ArgumentCaptor<List<UpdatedProductHistory>> listArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<SolrInputDocument>> solrInputDocumentArgumentCaptor;

  @SuppressWarnings("unchecked")
  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
    List<UpdatedProductHistory> logList = new ArrayList<>();
    when(this.updatedProductHistoryRepository.saveAll(anyList())).thenReturn(logList);
    Page<UpdatedProductHistory> page = new PageImpl<UpdatedProductHistory>(logList);

    this.productBusinessPartner = new ProductBusinessPartner();

    UpdatedProductHistory updatedProductHistory1 = new UpdatedProductHistory();
    updatedProductHistory1.setGdnSku(DEFAULT_GDN_SKU);
    updatedProductHistory1.setGdnName(GDN_NAME);
    UpdatedProductHistory updatedProductHistory2 = new UpdatedProductHistory();
    updatedProductHistory2.setGdnSku(Constants.DEFAULT);
    updatedProductHistory2.setGdnName(GDN_NAME);
    logAuditTrailUpdatedProductPage =
        new PageImpl<>(Arrays.asList(updatedProductHistory1, updatedProductHistory2));
    productLevel3UpdateRequest =
        ProductLevel3UpdateRequest.builder().businessPartnerCode(DEFAULT_BP_CODE).productSku(DEFAULT_PRODUCT_SKU)
            .productName(GDN_NAME).build();
    doNothing().when(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.capture());
    auditTrailRequest = new AuditTrailDto();
    auditTrailRequest.setBusinessPartnerCode(DEFAULT_BP_CODE);
    auditTrailRequest.setGdnSku(DEFAULT_GDN_SKU);
    auditTrailRequest.setActionKey(DEFAULT_ACTION_KEY);
    auditTrailRequest.setOldValue(DEFAULT_OLD_VALUE);
    auditTrailRequest.setNewValue(DEFAULT_NEW_VALUE);
    auditTrailRequest.setAttributeName(null);
    auditTrailRequest.setProductSku(DEFAULT_PRODUCT_SKU);
    auditTrailRequest.setName(FORMATTED_TEXT);

    b2bFieldsRequest = new B2bFieldsRequest();
    historyRequest = new HistoryRequest();
    historyRequest.setProductSku(DEFAULT_PRODUCT_SKU);
    when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.AUDIT_TRAIL_DELETE_BATCH_SIZE))
        .thenReturn(new ProductSystemParameter("auditTrailDeleteBatchSize", "12", "auditTrailDeleteBatchSize", false));
    when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.AUDIT_TRAIL_DELETE_BATCH_SIZE_SOLR))
        .thenReturn(new ProductSystemParameter("auditTrailDeleteBatchSizeSolr", "12", "auditTrailDeleteBatchSize", false));
    when(applicationContext.getBean(UpdatedProductHistoryService.class)).thenReturn(updatedProductHistoryServiceMock);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER);
    when(mandatoryParameterHelper.getChannelId()).thenReturn(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER);
    when(mandatoryParameterHelper.getClientId()).thenReturn(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER);
    when(mandatoryParameterHelper.getUsername()).thenReturn(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);

    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setSalesChannel(Arrays.asList(Constants.B2B_SELLER_CHANNEL, Constants.B2C_SELLER_CHANNEL));
  }

  @AfterEach
  public void finalizeTest() {
    MDC.clear();
    Mockito.verifyNoMoreInteractions(this.updatedProductHistoryRepository);
    Mockito.verifyNoMoreInteractions(this.solrHistoryCollectionRepository);
    Mockito.verifyNoMoreInteractions(this.updateProductItemLevel3ModelConverter);
    Mockito.verifyNoMoreInteractions(this.updatedProductHistoryCustomRepository);
    Mockito.verifyNoMoreInteractions(this.businessPartnerRepository);
    Mockito.verifyNoMoreInteractions(this.kafkaProducer, this.objectMapper);
  }

  public Map<UpdateProductActivity, String> generatePropertyMapper() {
    Map<UpdateProductActivity, String> map = new EnumMap<>(UpdateProductActivity.class);
    Field[] fields = UpdateProductItemLevel3Model.class.getDeclaredFields();
    for (Field field : fields) {
      LogAuditUpdateProduct annotation = field.getAnnotation(LogAuditUpdateProduct.class);
      if (annotation != null) {
        map.put(annotation.value(), field.getName());
      }
    }
    return map;
  }

  public UpdateProductItemLevel3Model generateOldValues() {
    UpdateProductItemLevel3Model model =
        UpdateProductItemLevel3Model.builder().productName("old").description("old").usp("USP_old")
            .availableStockLevel2(10).synchronizeStock(false).price(1000.0).lateFulfillment(false)
            .salePrice(1000.0).wholesaleRules(StringUtils.EMPTY).sizeChartChanged(true).sizeChartCode(null).build();
    return model;
  }

  public UpdateProductItemLevel3Model generateNewValues() {
    UpdateProductItemLevel3Model model =
        UpdateProductItemLevel3Model.builder().productName("new").description("new").usp("")
            .synchronizeStock(false).salePrice(1000.0).lateFulfillment(true).salePrice(1000.0)
            .wholesaleRules("new wholesale price").sizeChartCode(SIZE_CHART_CODE).build();
    return model;
  }

  @SuppressWarnings("unchecked")
  @Test
  public void createAuditTest() throws Exception {
    List<UpdatedProductHistory> logList = new ArrayList<>();
    this.updatedProductHistoryService.createAudit(logList, false);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void saveUpdatedProductHistoryToSolrTest() {
    UpdatedProductHistory updatedProductHistory = new UpdatedProductHistory();
    updatedProductHistory.setProductSku(PRODUCT_SKU);
    this.updatedProductHistoryService.saveUpdatedProductHistoryToSolr(Collections.singletonList(updatedProductHistory));
    verify(solrHistoryCollectionRepository).addDocument(solrListArgumentCaptor.capture());
  }

  @Test
  public void saveUpdatedProductHistoryToSolrEmptyTest() {
    this.updatedProductHistoryService.saveUpdatedProductHistoryToSolr(new ArrayList<>());
    Mockito.verify(solrHistoryCollectionRepository, times(0)).addDocument(Mockito.anyList());
  }

  @Test
  public void createAuditSolrUpdateFalseTest() {
    List<UpdatedProductHistory> logList = new ArrayList<>();
    this.updatedProductHistoryService.createAudit(logList, true);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void getAuditLogsForProductTest() {
    Pageable pageable = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
    when(productBusinessPartnerRepository.findProductSkuByItemSku(DEFAULT_GDN_SKU)).thenReturn(DEFAULT_PRODUCT_SKU);
    when(updatedProductHistoryRepository
        .findByGdnSkuAndProductSkuAndOnlineStatusTrue(DEFAULT_GDN_SKU, DEFAULT_PRODUCT_SKU, PageRequest.of(0, 10)))
        .thenReturn(logAuditTrailUpdatedProductPage);
    this.updatedProductHistoryService.getAuditLogsForProduct(pageable, DEFAULT_GDN_SKU);
    Mockito.verify(productBusinessPartnerRepository).findProductSkuByItemSku(DEFAULT_GDN_SKU);
    Mockito.verify(updatedProductHistoryRepository)
        .findByGdnSkuAndProductSkuAndOnlineStatusTrue(DEFAULT_GDN_SKU, DEFAULT_PRODUCT_SKU, PageRequest.of(0, 10));
  }

  @Test
  public void testSaveUpdateProductLevel3Audit_whenDescription() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    savedProductValue.setDescription("this is testing value. this is testing value. this is testing value. this is "
        + "testing value. this is testing value. this is testing value. this is testing value. this is testing value."
        + " this is testing value. this is testing value. this is testing value. this is testing value. this is "
        + "testing value. this is testing value. this is testing value. this is testing value. this is testing value."
        + " this is testing value. this is testing value. this is testing value. this is testing value.");
    String[] attributes = new String[]{"OS#_#IOS"};
    savedProductValue.setAttributesMap(attributes);
    UpdateProductItemLevel3Model updatedProductValue = generateNewValues();
    updatedProductValue.setAttributesMap(attributes);
    when(
        this.logAuditUpdateProductAnnotationUtil
            .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
        generatePropertyMapper());
    when(this.updatedProductHistoryRepository.saveAll(anyList()))
        .thenAnswer(new Answer<List<UpdatedProductHistory>>() {
      @Override
      public List<UpdatedProductHistory> answer(InvocationOnMock invocationOnMock) throws Throwable {
        Object args[] = invocationOnMock.getArguments();
        UpdatedProductHistory logAuditDetail = ((List<UpdatedProductHistory>)args[0]).get(1);
        return null;
      }
    });
    this.updatedProductHistoryService.saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU,
        savedProductValue, updatedProductValue, null, DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION,
        StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3Audit_merchantSku() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    savedProductValue.setMerchantSku("merchantSku");
    savedProductValue.setFreeSample(true);
    UpdateProductItemLevel3Model updatedProductValue = generateOldValues();
    updatedProductValue.setMerchantSku(null);
    updatedProductValue.setFreeSample(false);
    when(
        this.logAuditUpdateProductAnnotationUtil
            .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
        generatePropertyMapper());
    when(this.updatedProductHistoryRepository.saveAll(anyList()))
        .thenReturn(Arrays.asList(new UpdatedProductHistory()));
    this.updatedProductHistoryService.saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU,
        savedProductValue, updatedProductValue, null, DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION,
        StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3Audit_stockUpdate() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    savedProductValue.setMerchantSku("merchantSku");
    savedProductValue.setFreeSample(true);
    UpdateProductItemLevel3Model updatedProductValue =
        UpdateProductItemLevel3Model.builder().availableStockLevel2(32).build();
    when(
      this.logAuditUpdateProductAnnotationUtil
        .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
      generatePropertyMapper());
    when(this.updatedProductHistoryRepository.saveAll(anyList()))
      .thenReturn(Arrays.asList(new UpdatedProductHistory()));
    this.updatedProductHistoryService.saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU,
      savedProductValue, updatedProductValue, null, DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION,
        StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3AuditSizeChartEdit() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    savedProductValue.setMerchantSku("merchantSku");
    savedProductValue.setFreeSample(true);
    UpdateProductItemLevel3Model updatedProductValue =
        UpdateProductItemLevel3Model.builder().sizeChartCode(SIZE_CHART_CODE).build();
    when(
      this.logAuditUpdateProductAnnotationUtil
        .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
      generatePropertyMapper());
    when(this.updatedProductHistoryRepository.saveAll(anyList()))
      .thenReturn(Arrays.asList(new UpdatedProductHistory()));
    this.updatedProductHistoryService.saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU,
      savedProductValue, updatedProductValue, null, DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION,
      StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3Audit_sameStockTest() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    savedProductValue.setMerchantSku("merchantSku");
    savedProductValue.setFreeSample(true);
    savedProductValue.setAvailableStockLevel2(32);
    UpdateProductItemLevel3Model updatedProductValue =
        UpdateProductItemLevel3Model.builder().availableStockLevel2(32).build();
    when(
      this.logAuditUpdateProductAnnotationUtil
        .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
      generatePropertyMapper());
    when(this.updatedProductHistoryRepository.saveAll(anyList()))
      .thenReturn(Arrays.asList(new UpdatedProductHistory()));
    this.updatedProductHistoryService.saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU,
      savedProductValue, updatedProductValue, null, DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION,
        StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3Audit_stockUpdateWithNullOldValue() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    savedProductValue.setMerchantSku("merchantSku");
    savedProductValue.setFreeSample(true);
    savedProductValue.setAvailableStockLevel2(null);
    UpdateProductItemLevel3Model updatedProductValue =
        UpdateProductItemLevel3Model.builder().availableStockLevel2(32).build();
    when(
      this.logAuditUpdateProductAnnotationUtil
        .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
      generatePropertyMapper());
    when(this.updatedProductHistoryRepository.saveAll(anyList()))
      .thenReturn(Arrays.asList(new UpdatedProductHistory()));
    this.updatedProductHistoryService.saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU,
      savedProductValue, updatedProductValue, null, DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION,
        StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }
  @Test
  public void testSaveUpdateProductLevel3Audit_PickupPointCode() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    savedProductValue.setMerchantSku("merchantSku");
    UpdateProductItemLevel3Model updatedProductValue = generateOldValues();
    updatedProductValue.setMerchantSku(null);
    updatedProductValue.setPickupPointCode(PICKUP_POINT_CODE);
    when(this.logAuditUpdateProductAnnotationUtil
        .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(generatePropertyMapper());
    when(this.updatedProductHistoryRepository.saveAll(Mockito.anyList()))
        .thenReturn(Arrays.asList(new UpdatedProductHistory()));
    this.updatedProductHistoryService
        .saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, savedProductValue, updatedProductValue, null,
            DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION, StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3Audit_PickupPointCodeWithEventSwitchOn() throws Exception {
    ReflectionTestUtils.setField(updatedProductHistoryService,"productHistoryUpdateThroughEvent", true);
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    savedProductValue.setMerchantSku("merchantSku");
    UpdateProductItemLevel3Model updatedProductValue = generateOldValues();
    updatedProductValue.setMerchantSku(null);
    updatedProductValue.setPickupPointCode(PICKUP_POINT_CODE);
    when(this.logAuditUpdateProductAnnotationUtil
        .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(generatePropertyMapper());
    this.updatedProductHistoryService
        .saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, savedProductValue, updatedProductValue, null,
            DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION, StringUtils.EMPTY);
    verify(kafkaProducer, times(2)).send(anyString(), anyString(),any(AuditTrailListRequest.class));
  }

  @Test
  public void testSaveUpdateProductLevel3Audit_merchantSku_null() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = new UpdateProductItemLevel3Model();
    savedProductValue.setMerchantSku(null);
    UpdateProductItemLevel3Model updatedProductValue = new UpdateProductItemLevel3Model();
    updatedProductValue.setMerchantSku("");
    when(this.logAuditUpdateProductAnnotationUtil
        .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(generatePropertyMapper());
    when(this.updatedProductHistoryRepository.saveAll(Mockito.anyList()))
        .thenReturn(Arrays.asList(new UpdatedProductHistory()));
    this.updatedProductHistoryService
        .saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, savedProductValue, updatedProductValue, null,
            DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION, StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void testSaveUpdateProductLevel3AuditPreorderTest() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    savedProductValue.setPreOrder(true);
    savedProductValue.setBuyable(false);
    savedProductValue.setUrlVideo(RICH_TEXT);
    String[] attributes = new String[] {"OS#_#IOS"};
    savedProductValue.setAttributesMap(attributes);
    UpdateProductItemLevel3Model updatedProductValue = generateNewValues();
    updatedProductValue.setAttributesMap(attributes);
    updatedProductValue.setPreOrder(true);
    updatedProductValue.setBuyable(false);
    updatedProductValue.setUrlVideo(FORMATTED_TEXT);
    when(this.logAuditUpdateProductAnnotationUtil
        .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(generatePropertyMapper());
    when(this.updatedProductHistoryRepository.saveAll(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    this.updatedProductHistoryService
        .saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, savedProductValue, updatedProductValue, null,
            DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION, StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3AuditNullValueTest() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    savedProductValue.setPreOrder(true);
    savedProductValue.setBuyable(false);
    savedProductValue.setUrlVideo(RICH_TEXT);
    String[] attributes = new String[] {"OS#_#"};
    savedProductValue.setAttributesMap(attributes);
    UpdateProductItemLevel3Model updatedProductValue = generateNewValues();
    updatedProductValue.setAttributesMap(attributes);
    updatedProductValue.setPreOrder(true);
    updatedProductValue.setBuyable(false);
    updatedProductValue.setUrlVideo(FORMATTED_TEXT);
    when(this.logAuditUpdateProductAnnotationUtil
        .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(generatePropertyMapper());
    when(this.updatedProductHistoryRepository.saveAll(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    this.updatedProductHistoryService
        .saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, savedProductValue, updatedProductValue, null,
            DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION, StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3AuditNullAttributes() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    savedProductValue.setDescription("this is testing value. this is testing value. this is testing value. this is "
        + "testing value. this is testing value. this is testing value. this is testing value. this is testing value."
        + " this is testing value. this is testing value. this is testing value. this is testing value. this is "
        + "testing value. this is testing value. this is testing value. this is testing value. this is testing value."
        + " this is testing value. this is testing value. this is testing value. this is testing value.");
    String[] attributes = new String[]{"OS#_#IOS"};
    UpdateProductItemLevel3Model updatedProductValue = generateNewValues();
    when(
        this.logAuditUpdateProductAnnotationUtil
            .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
        generatePropertyMapper());
    when(this.updatedProductHistoryRepository.saveAll(Mockito.anyList()))
        .thenAnswer(new Answer<List<UpdatedProductHistory>>() {
          @Override
          public List<UpdatedProductHistory> answer(InvocationOnMock invocationOnMock) throws Throwable {
            Object args[] = invocationOnMock.getArguments();
            UpdatedProductHistory logAuditDetail = ((List<UpdatedProductHistory>)args[0]).get(1);
            return null;
          }
        });
    this.updatedProductHistoryService.saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU,
        savedProductValue, updatedProductValue, null, DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION,
        StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3AuditWholeSaleFlagUpdateTest() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    UpdateProductItemLevel3Model updatedProductValue = generateOldValues();
    updatedProductValue.setWholesalePriceActivated(String.valueOf(Boolean.TRUE));
    when(this.logAuditUpdateProductAnnotationUtil
        .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(generatePropertyMapper());
    this.updatedProductHistoryService
        .saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, savedProductValue, updatedProductValue, null,
            DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION, StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(listArgumentCaptor.capture());
    Assertions.assertEquals(UpdateProductActivity.WHOLE_PRICE_FLAG.getDesc(),
        listArgumentCaptor.getValue().get(0).getActivity());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3Audit() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    String[] attributes = new String[]{"OS#_#IOS"};
    savedProductValue.setAttributesMap(attributes);
    UpdateProductItemLevel3Model updatedProductValue = generateNewValues();
    updatedProductValue.setAttributesMap(attributes);
    when(
        this.logAuditUpdateProductAnnotationUtil
            .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
        generatePropertyMapper());
    this.updatedProductHistoryService.saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU,
        savedProductValue, updatedProductValue, null, DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION,
        StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3AuditTest() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    String[] attributes = new String[]{"OS#_#IOS"};
    savedProductValue.setAttributesMap(attributes);
    UpdateProductItemLevel3Model updatedProductValue = generateNewValues();
    updatedProductValue.setAttributesMap(attributes);
    when(
        this.logAuditUpdateProductAnnotationUtil
            .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
        generatePropertyMapper());
    this.updatedProductHistoryService.saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU,
        savedProductValue, updatedProductValue, null, DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION,
        USP);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3Audit_NullValueTest() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    String[] attributes = new String[]{"OS#_#IOS"};
    savedProductValue.setAttributesMap(attributes);
    UpdateProductItemLevel3Model updatedProductValue = generateNewValues();
    updatedProductValue.setAttributesMap(attributes);
    MDC.clear();
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, SYSTEM);
    MDC.put(AuditTrailMandatoryRequestParameterUtil.CLIENT_HOST_KEY, SYSTEM);
    when(
        this.logAuditUpdateProductAnnotationUtil
            .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
        generatePropertyMapper());
    this.updatedProductHistoryService.saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU,
        savedProductValue, updatedProductValue, null, DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION,
        StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3AuditWithDescription() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    UpdateProductItemLevel3Model updatedProductValue = generateNewValues();
    when(
            this.logAuditUpdateProductAnnotationUtil
                    .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
            generatePropertyMapper());
    this.updatedProductHistoryService.saveUpdateProductLevel3AuditWithDescription(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, DEFAULT_GDN_SKU,
            savedProductValue, updatedProductValue, DEFAULT_ACCESS_CHANNEL);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3AuditWithDescriptionWithEventSwitchOn() throws Exception {
    ReflectionTestUtils.setField(updatedProductHistoryService,"productHistoryUpdateThroughEvent", true);
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    UpdateProductItemLevel3Model updatedProductValue = generateNewValues();
    when(
        this.logAuditUpdateProductAnnotationUtil
            .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
        generatePropertyMapper());
    this.updatedProductHistoryService.saveUpdateProductLevel3AuditWithDescription(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, DEFAULT_GDN_SKU,
        savedProductValue, updatedProductValue, DEFAULT_ACCESS_CHANNEL);
    verify(kafkaProducer, times(6)).send(anyString(), any(),any(AuditTrailListRequest.class));
  }

  @Test
  public void testSaveUpdateProductLevel3AuditWithDescription2() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    UpdateProductItemLevel3Model updatedProductValue = generateNewValues();
    savedProductValue.setPickupPointCode("pickupPoint");
    updatedProductValue.setPickupPointCode(StringUtils.EMPTY);
    savedProductValue.setMerchantSku("merchantSku");
    updatedProductValue.setMerchantSku(StringUtils.EMPTY);
    when(
        this.logAuditUpdateProductAnnotationUtil
            .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
        generatePropertyMapper());
    this.updatedProductHistoryService.saveUpdateProductLevel3AuditWithDescription(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, DEFAULT_GDN_SKU,
        savedProductValue, updatedProductValue, DEFAULT_ACCESS_CHANNEL);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3AuditWithDescription3() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    UpdateProductItemLevel3Model updatedProductValue = generateNewValues();
    savedProductValue.setPickupPointCode("pickupPoint");
    updatedProductValue.setPickupPointCode(StringUtils.EMPTY);
    savedProductValue.setMerchantSku("merchantSku");
    updatedProductValue.setMerchantSku("merchantSku2");
    when(
        this.logAuditUpdateProductAnnotationUtil
            .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
        generatePropertyMapper());
    this.updatedProductHistoryService.saveUpdateProductLevel3AuditWithDescription(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, DEFAULT_GDN_SKU,
        savedProductValue, updatedProductValue, DEFAULT_ACCESS_CHANNEL);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3AuditWithUpdatedNewValue() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, SYSTEM);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_HOST_KEY);
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    UpdateProductItemLevel3Model updatedProductValue = generateNewValues();
    updatedProductValue.setSalePrice(1001.20);
    when(
        this.logAuditUpdateProductAnnotationUtil
            .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
        generatePropertyMapper());
    this.updatedProductHistoryService.saveUpdateProductLevel3AuditWithDescription(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, DEFAULT_GDN_SKU,
        savedProductValue, updatedProductValue, DEFAULT_ACCESS_CHANNEL);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveCreateProductLevel3Audit() throws Exception {
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    profileResponse.setFlags(Map.of(Constants.BLIBLI_OMG,true));
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, SYSTEM);
    ProductAndItemActivationRequest itemRequest = new ProductAndItemActivationRequest();

    ItemActivationRequest item = new ItemActivationRequest();
    ItemPickupPointActivationRequest itemPickupPointActivationRequest = new ItemPickupPointActivationRequest();
    item.setItemSku(DEFAULT_GDN_SKU);

    ItemViewConfigRequest itemViewConfig = new ItemViewConfigRequest();
    itemViewConfig.setChannel(Constants.B2B_CHANNEL);
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    Set<ItemViewConfigRequest> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);
    itemPickupPointActivationRequest.setItemViewConfigs(itemViewConfigs);
    productBusinessPartner.setB2cActivated(true);
    productBusinessPartner.setPreOrder(true);

    PriceRequest price = new PriceRequest();
    price.setListPrice(10000);
    price.setOfferPrice(1000);
    Set<PriceRequest> prices = new HashSet<>();
    prices.add(price);
    itemPickupPointActivationRequest.setPrice(prices);
    item.setItemPickupPoints(Arrays.asList(itemPickupPointActivationRequest));

    List<ItemActivationRequest> items = new ArrayList<>();
    items.add(item);
    itemRequest.setItems(items);

    ProductDTO product = new ProductDTO();
    product.setProductCode(DEFAULT_PRODUCT_CODE);
    itemRequest.setProduct(product);

    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_GDN_SKU);
    productItemBusinessPartner.setPreOrderQuota(10);
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku(DEFAULT_GDN_SKU_2);
    List<ProductItemBusinessPartner> productItemBusinessPartners = new ArrayList<>();
    productItemBusinessPartners.add(productItemBusinessPartner);
    productItemBusinessPartners.add(productItemBusinessPartner2);
    this.productBusinessPartner.setProductItemBusinessPartners(productItemBusinessPartners);
    this.productBusinessPartner.setStoreId(DEFAULT_STORE_ID);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);

    this.updatedProductHistoryService.saveCreateProductLevel3Audit(DEFAULT_BP_CODE, itemRequest,
        this.productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository, Mockito.times(1))
        .getCreatedByItemBusinessPartnerByGdnProductItemSku(DEFAULT_GDN_SKU);
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void testSaveCreateProductLevel3AuditWithHistoryEventSwitchOn() throws Exception {
    ReflectionTestUtils.setField(updatedProductHistoryService,"productHistoryUpdateThroughEvent", true);
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(false);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, SYSTEM);
    ProductAndItemActivationRequest itemRequest = new ProductAndItemActivationRequest();

    ItemActivationRequest item = new ItemActivationRequest();
    ItemPickupPointActivationRequest itemPickupPointActivationRequest = new ItemPickupPointActivationRequest();
    item.setItemSku(DEFAULT_GDN_SKU);

    ItemViewConfigRequest itemViewConfig = new ItemViewConfigRequest();
    itemViewConfig.setChannel(Constants.B2B_CHANNEL);
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    Set<ItemViewConfigRequest> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);
    itemPickupPointActivationRequest.setItemViewConfigs(itemViewConfigs);
    productBusinessPartner.setB2cActivated(true);

    PriceRequest price = new PriceRequest();
    price.setListPrice(10000);
    price.setOfferPrice(1000);
    Set<PriceRequest> prices = new HashSet<>();
    prices.add(price);
    itemPickupPointActivationRequest.setPrice(prices);
    item.setItemPickupPoints(Arrays.asList(itemPickupPointActivationRequest));

    List<ItemActivationRequest> items = new ArrayList<>();
    items.add(item);
    itemRequest.setItems(items);

    ProductDTO product = new ProductDTO();
    product.setProductCode(DEFAULT_PRODUCT_CODE);
    itemRequest.setProduct(product);

    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_GDN_SKU);
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku(DEFAULT_GDN_SKU_2);
    List<ProductItemBusinessPartner> productItemBusinessPartners = new ArrayList<>();
    productItemBusinessPartners.add(productItemBusinessPartner);
    productItemBusinessPartners.add(productItemBusinessPartner2);
    this.productBusinessPartner.setProductItemBusinessPartners(productItemBusinessPartners);
    this.productBusinessPartner.setStoreId(DEFAULT_STORE_ID);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);

    this.updatedProductHistoryService.saveCreateProductLevel3Audit(DEFAULT_BP_CODE, itemRequest,
        this.productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository, Mockito.times(1))
        .getCreatedByItemBusinessPartnerByGdnProductItemSku(DEFAULT_GDN_SKU);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(kafkaProducer, times(7)).send(anyString(),any(),any(AuditTrailListRequest.class));
  }

  @Test
  public void testSaveCreateProductLevel3AuditB2CSalesChannel() throws Exception {
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, SYSTEM);
    ProductAndItemActivationRequest itemRequest = new ProductAndItemActivationRequest();

    ItemActivationRequest item = new ItemActivationRequest();
    ItemPickupPointActivationRequest itemPickupPointActivationRequest = new ItemPickupPointActivationRequest();
    item.setItemSku(DEFAULT_GDN_SKU);

    ItemViewConfigRequest itemViewConfig = new ItemViewConfigRequest();
    itemViewConfig.setChannel(Constants.B2B_CHANNEL);
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    Set<ItemViewConfigRequest> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);
    itemPickupPointActivationRequest.setItemViewConfigs(itemViewConfigs);
    productBusinessPartner.setB2cActivated(true);
    productBusinessPartner.setPreOrder(false);

    PriceRequest price = new PriceRequest();
    price.setListPrice(10000);
    price.setOfferPrice(1000);
    Set<PriceRequest> prices = new HashSet<>();
    prices.add(price);
    itemPickupPointActivationRequest.setPrice(prices);
    item.setItemPickupPoints(Arrays.asList(itemPickupPointActivationRequest));

    List<ItemActivationRequest> items = new ArrayList<>();
    items.add(item);
    itemRequest.setItems(items);

    ProductDTO product = new ProductDTO();
    product.setProductCode(DEFAULT_PRODUCT_CODE);
    itemRequest.setProduct(product);

    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_GDN_SKU);
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku(DEFAULT_GDN_SKU_2);
    List<ProductItemBusinessPartner> productItemBusinessPartners = new ArrayList<>();
    productItemBusinessPartners.add(productItemBusinessPartner);
    productItemBusinessPartners.add(productItemBusinessPartner2);
    this.productBusinessPartner.setProductItemBusinessPartners(productItemBusinessPartners);
    this.productBusinessPartner.setStoreId(DEFAULT_STORE_ID);
    profileResponse.getCompany().setSalesChannel(Arrays.asList(Constants.B2C_SELLER_CHANNEL));
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);

    this.updatedProductHistoryService.saveCreateProductLevel3Audit(DEFAULT_BP_CODE, itemRequest,
        this.productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository, Mockito.times(1))
        .getCreatedByItemBusinessPartnerByGdnProductItemSku(DEFAULT_GDN_SKU);
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void testSaveCreateProductLevel3AuditCreatedByEmpty() throws Exception {
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, SYSTEM);
    ProductAndItemActivationRequest itemRequest = new ProductAndItemActivationRequest();

    ItemActivationRequest item = new ItemActivationRequest();
    ItemPickupPointActivationRequest itemPickupPointActivationRequest = new ItemPickupPointActivationRequest();

    item.setItemSku(DEFAULT_GDN_SKU);

    ItemViewConfigRequest itemViewConfig = new ItemViewConfigRequest();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    Set<ItemViewConfigRequest> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);
    itemPickupPointActivationRequest.setItemViewConfigs(itemViewConfigs);

    PriceRequest price = new PriceRequest();
    price.setListPrice(10000);
    price.setOfferPrice(20000);
    Set<PriceRequest> prices = new HashSet<>();
    prices.add(price);
    itemPickupPointActivationRequest.setPrice(prices);
    item.setItemPickupPoints(Arrays.asList(itemPickupPointActivationRequest));

    List<ItemActivationRequest> items = new ArrayList<>();
    items.add(item);
    itemRequest.setItems(items);

    ProductDTO product = new ProductDTO();
    product.setProductCode(DEFAULT_PRODUCT_CODE);
    itemRequest.setProduct(product);
    productBusinessPartner.setB2bActivated(true);
    productBusinessPartner.setPreOrder(null);
    profileResponse.setFlags(Map.of(Constants.BLIBLI_OMG,true));

    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_GDN_SKU);
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku(DEFAULT_GDN_SKU_2);
    List<ProductItemBusinessPartner> productItemBusinessPartners = new ArrayList<>();
    productItemBusinessPartners.add(productItemBusinessPartner);
    productItemBusinessPartners.add(productItemBusinessPartner2);
    this.productBusinessPartner.setProductItemBusinessPartners(productItemBusinessPartners);
    this.productBusinessPartner.setStoreId(DEFAULT_STORE_ID);

    when(productBusinessPartnerRepository.getCreatedByItemBusinessPartnerByGdnProductItemSku(DEFAULT_GDN_SKU)).thenReturn(SYSTEM);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);

    this.updatedProductHistoryService.saveCreateProductLevel3Audit(DEFAULT_BP_CODE, itemRequest,
        this.productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository, Mockito.times(1))
        .getCreatedByItemBusinessPartnerByGdnProductItemSku(DEFAULT_GDN_SKU);
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void testSaveCreateProductLevel3AuditB2bSalesChannel() throws Exception {
    Mockito.when(preOrderConfig.isPoQuotaFeatureSwitch()).thenReturn(true);
    profileResponse.setFlags(Map.of(Constants.BLIBLI_OMG,true));
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, SYSTEM);
    ProductAndItemActivationRequest itemRequest = new ProductAndItemActivationRequest();

    ItemActivationRequest item = new ItemActivationRequest();
    ItemPickupPointActivationRequest itemPickupPointActivationRequest = new ItemPickupPointActivationRequest();

    item.setItemSku(DEFAULT_GDN_SKU);

    ItemViewConfigRequest itemViewConfig = new ItemViewConfigRequest();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    Set<ItemViewConfigRequest> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);
    itemPickupPointActivationRequest.setItemViewConfigs(itemViewConfigs);

    PriceRequest price = new PriceRequest();
    price.setListPrice(10000);
    price.setOfferPrice(20000);
    Set<PriceRequest> prices = new HashSet<>();
    prices.add(price);
    itemPickupPointActivationRequest.setPrice(prices);
    item.setItemPickupPoints(Arrays.asList(itemPickupPointActivationRequest));

    List<ItemActivationRequest> items = new ArrayList<>();
    items.add(item);
    itemRequest.setItems(items);

    ProductDTO product = new ProductDTO();
    product.setProductCode(DEFAULT_PRODUCT_CODE);
    itemRequest.setProduct(product);
    productBusinessPartner.setB2bActivated(true);
    productBusinessPartner.setPreOrder(true);

    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_GDN_SKU);
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku(DEFAULT_GDN_SKU_2);
    List<ProductItemBusinessPartner> productItemBusinessPartners = new ArrayList<>();
    productItemBusinessPartners.add(productItemBusinessPartner);
    productItemBusinessPartners.add(productItemBusinessPartner2);
    this.productBusinessPartner.setProductItemBusinessPartners(productItemBusinessPartners);
    this.productBusinessPartner.setStoreId(DEFAULT_STORE_ID);
    profileResponse.getCompany().setSalesChannel(Arrays.asList(Constants.B2C_SELLER_CHANNEL));

    when(productBusinessPartnerRepository.getCreatedByItemBusinessPartnerByGdnProductItemSku(DEFAULT_GDN_SKU)).thenReturn(SYSTEM);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);

    this.updatedProductHistoryService.saveCreateProductLevel3Audit(DEFAULT_BP_CODE, itemRequest,
        this.productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository, Mockito.times(1))
        .getCreatedByItemBusinessPartnerByGdnProductItemSku(DEFAULT_GDN_SKU);
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void saveCreateProductLevel3AuditCreatedByEmptyB2bAndB2cTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, SYSTEM);
    ProductAndItemActivationRequest itemRequest = new ProductAndItemActivationRequest();

    ItemActivationRequest item = new ItemActivationRequest();
    ItemPickupPointActivationRequest itemPickupPointActivationRequest = new ItemPickupPointActivationRequest();
    itemPickupPointActivationRequest.setB2bFields(b2bFieldsRequest);

    item.setItemSku(DEFAULT_GDN_SKU);

    ItemViewConfigRequest itemViewConfig = new ItemViewConfigRequest();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    Set<ItemViewConfigRequest> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);
    itemPickupPointActivationRequest.setItemViewConfigs(itemViewConfigs);

    PriceRequest price = new PriceRequest();
    price.setListPrice(10000);
    price.setOfferPrice(20000);
    Set<PriceRequest> prices = new HashSet<>();
    prices.add(price);
    itemPickupPointActivationRequest.setPrice(prices);
    item.setItemPickupPoints(Arrays.asList(itemPickupPointActivationRequest));

    List<ItemActivationRequest> items = new ArrayList<>();
    items.add(item);
    itemRequest.setItems(items);

    ProductDTO product = new ProductDTO();
    product.setProductCode(DEFAULT_PRODUCT_CODE);
    itemRequest.setProduct(product);

    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_GDN_SKU);
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku(DEFAULT_GDN_SKU_2);
    List<ProductItemBusinessPartner> productItemBusinessPartners = new ArrayList<>();
    productItemBusinessPartners.add(productItemBusinessPartner);
    productItemBusinessPartners.add(productItemBusinessPartner2);
    this.productBusinessPartner.setProductItemBusinessPartners(productItemBusinessPartners);
    this.productBusinessPartner.setStoreId(DEFAULT_STORE_ID);
    productBusinessPartner.setB2bActivated(true);
    productBusinessPartner.setB2cActivated(true);

    when(productBusinessPartnerRepository.getCreatedByItemBusinessPartnerByGdnProductItemSku(DEFAULT_GDN_SKU)).thenReturn(SYSTEM);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);

    this.updatedProductHistoryService.saveCreateProductLevel3Audit(DEFAULT_BP_CODE, itemRequest,
        this.productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository, Mockito.times(1))
        .getCreatedByItemBusinessPartnerByGdnProductItemSku(DEFAULT_GDN_SKU);
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void saveCreateProductLevel3AuditCreatedByEmptyB2bAndB2cFalseTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, SYSTEM);
    ProductAndItemActivationRequest itemRequest = new ProductAndItemActivationRequest();

    ItemActivationRequest item = new ItemActivationRequest();
    ItemPickupPointActivationRequest itemPickupPointActivationRequest = new ItemPickupPointActivationRequest();
    b2bFieldsRequest.setBasePrice(PRICE);
    itemPickupPointActivationRequest.setB2bFields(b2bFieldsRequest);

    item.setItemSku(DEFAULT_GDN_SKU);

    ItemViewConfigRequest itemViewConfig = new ItemViewConfigRequest();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    Set<ItemViewConfigRequest> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);
    itemPickupPointActivationRequest.setItemViewConfigs(itemViewConfigs);

    PriceRequest price = new PriceRequest();
    price.setListPrice(10000);
    price.setOfferPrice(20000);
    Set<PriceRequest> prices = new HashSet<>();
    prices.add(price);
    itemPickupPointActivationRequest.setPrice(prices);
    item.setItemPickupPoints(Arrays.asList(itemPickupPointActivationRequest));

    List<ItemActivationRequest> items = new ArrayList<>();
    items.add(item);
    itemRequest.setItems(items);

    ProductDTO product = new ProductDTO();
    product.setProductCode(DEFAULT_PRODUCT_CODE);
    itemRequest.setProduct(product);

    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_GDN_SKU);
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku(DEFAULT_GDN_SKU_2);
    List<ProductItemBusinessPartner> productItemBusinessPartners = new ArrayList<>();
    productItemBusinessPartners.add(productItemBusinessPartner);
    productItemBusinessPartners.add(productItemBusinessPartner2);
    this.productBusinessPartner.setProductItemBusinessPartners(productItemBusinessPartners);
    this.productBusinessPartner.setStoreId(DEFAULT_STORE_ID);

    when(productBusinessPartnerRepository.getCreatedByItemBusinessPartnerByGdnProductItemSku(DEFAULT_GDN_SKU)).thenReturn(SYSTEM);
    when(businessPartnerRepository.filterDetailByBusinessPartnerCode(any())).thenReturn(profileResponse);

    this.updatedProductHistoryService.saveCreateProductLevel3Audit(DEFAULT_BP_CODE, itemRequest,
        this.productBusinessPartner);
    Mockito.verify(this.productBusinessPartnerRepository, Mockito.times(1))
        .getCreatedByItemBusinessPartnerByGdnProductItemSku(DEFAULT_GDN_SKU);
    verify(businessPartnerRepository).filterDetailByBusinessPartnerCode(any());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void testSaveUpdateProductLevel3AuditWithDiffArgs() throws Exception {
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(DEFAULT_GDN_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    Mockito.when(this.xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any()))
        .thenReturn(itemPickupPointCodeByItemSkus);
    this.updatedProductHistoryService
        .saveUpdateProductLevel3AuditForWholeSale(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, DEFAULT_ACTIVITY, DEFAULT_OLD_VALUE,
            DEFAULT_NEW_VALUE, DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU);
    Mockito.verify(this.updatedProductHistoryRepository, Mockito.times(1)).saveAll(Mockito.anyList());
    Mockito.verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3AuditWithDiffArgsAndHistoryEventSwitchOn() throws Exception {
    ReflectionTestUtils.setField(updatedProductHistoryService,"productHistoryUpdateThroughEvent", true);
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(DEFAULT_GDN_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    Mockito.when(this.xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any()))
        .thenReturn(itemPickupPointCodeByItemSkus);
    this.updatedProductHistoryService
        .saveUpdateProductLevel3AuditForWholeSale(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, DEFAULT_ACTIVITY, DEFAULT_OLD_VALUE,
            DEFAULT_NEW_VALUE, DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU);
    Mockito.verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    Mockito.verify(kafkaProducer).send(anyString(), anyString(), any(AuditTrailListRequest.class));
  }


  @Test
  public void saveUpdateProductLevel3AuditForMigrationTest() throws Exception {
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(DEFAULT_GDN_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    Mockito.when(this.xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any()))
        .thenReturn(itemPickupPointCodeByItemSkus);
    this.updatedProductHistoryService.saveUpdateProductLevel3AuditForMigration(DEFAULT_BP_CODE, DEFAULT_GDN_SKU);
    Mockito.verify(this.updatedProductHistoryRepository, Mockito.times(1)).saveAll(Mockito.anyList());
    Mockito.verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void saveUpdateProductLevel3AuditWithEventSwitchOnTest() throws Exception {
    ReflectionTestUtils.setField(updatedProductHistoryService,"productHistoryUpdateThroughEvent", true);
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(DEFAULT_GDN_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    Mockito.when(this.xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any()))
        .thenReturn(itemPickupPointCodeByItemSkus);
    this.updatedProductHistoryService.saveUpdateProductLevel3AuditForMigration(DEFAULT_BP_CODE, DEFAULT_GDN_SKU);
    Mockito.verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    Mockito.verify(kafkaProducer).send(anyString(),any(),any(AuditTrailListRequest.class));
  }

  @Test
  public void testCreateAuditTrail() throws Exception {
    List<String> skus = new ArrayList<>();
    UpdateProductItemLevel3Model updateProductItemLevel3Model = new UpdateProductItemLevel3Model();
    updateProductItemLevel3Model.setMerchantSku(DEFAULT_GDN_SKU);
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    skus.add(DEFAULT_GDN_SKU);
    when(
            this.logAuditUpdateProductAnnotationUtil
                    .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
            generatePropertyMapper());
    when(this.updateProductItemLevel3ModelConverter.convertFromProductAndItemsResponse(Mockito.any(ProductAndItemsResponse.class)))
            .thenReturn(updateProductItemLevel3Model);
    this.updatedProductHistoryService.createAuditTrailForAllItemSkus(DEFAULT_BP_CODE, skus, DEFAULT_GDN_SKU, productAndItemsResponse, productAndItemsResponse
            , DEFAULT_ACCESS_CHANNEL);
    Mockito.verify(this.updateProductItemLevel3ModelConverter, TWO_TIMES)
            .convertFromProductAndItemsResponse(Mockito.any(ProductAndItemsResponse.class));
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void testCreateAuditTrailWithException() throws Exception {
    List<String> skus = new ArrayList<>();
    UpdateProductItemLevel3Model updateProductItemLevel3Model = new UpdateProductItemLevel3Model();
    updateProductItemLevel3Model.setMerchantSku(DEFAULT_GDN_SKU);
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    skus.add(DEFAULT_GDN_SKU);
    doThrow(new RuntimeException(ErrorMessages.PRODUCT_NOT_FOUND)).when(logAuditUpdateProductAnnotationUtil)
        .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class);
    when(this.updateProductItemLevel3ModelConverter.convertFromProductAndItemsResponse(
        Mockito.any(ProductAndItemsResponse.class))).thenReturn(updateProductItemLevel3Model);
    this.updatedProductHistoryService.createAuditTrailForAllItemSkus(DEFAULT_BP_CODE, skus, DEFAULT_GDN_SKU,
        productAndItemsResponse, productAndItemsResponse, DEFAULT_ACCESS_CHANNEL);
    Mockito.verify(this.updateProductItemLevel3ModelConverter, TWO_TIMES)
        .convertFromProductAndItemsResponse(Mockito.any(ProductAndItemsResponse.class));
  }

  @Test
  public void isPriceChangedForSkuTest() throws Exception {
    Date date = new Date();
    when(updatedProductHistoryRepository.countByGdnSkuAndAccessTimeGreaterThanEqualAndActivityInAndOnlineStatusTrue(
        DEFAULT_GDN_SKU, date, Arrays.asList(UpdateProductActivity.NORMAL_PRICE.getDesc(),
            UpdateProductActivity.SELLING_PRICE.getDesc()))).thenReturn(1L);
    Assertions.assertTrue(updatedProductHistoryService.isPriceChangedForSku(DEFAULT_GDN_SKU, date).getResult());
    Mockito.verify(updatedProductHistoryRepository)
        .countByGdnSkuAndAccessTimeGreaterThanEqualAndActivityInAndOnlineStatusTrue(DEFAULT_GDN_SKU, date,
            Arrays.asList(UpdateProductActivity.NORMAL_PRICE.getDesc(), UpdateProductActivity.SELLING_PRICE.getDesc()));
  }

  @Test
  public void isPriceChangedForSkuNoChangeTest() throws Exception {
    Date date = new Date();
    when(updatedProductHistoryRepository.countByGdnSkuAndAccessTimeGreaterThanEqualAndActivityInAndOnlineStatusTrue(
        DEFAULT_GDN_SKU, date, Arrays.asList(UpdateProductActivity.NORMAL_PRICE.getDesc(),
            UpdateProductActivity.SELLING_PRICE.getDesc()))).thenReturn(0L);
    Assertions.assertFalse(updatedProductHistoryService.isPriceChangedForSku(DEFAULT_GDN_SKU, date).getResult());
    Mockito.verify(updatedProductHistoryRepository)
        .countByGdnSkuAndAccessTimeGreaterThanEqualAndActivityInAndOnlineStatusTrue(DEFAULT_GDN_SKU, date,
            Arrays.asList(UpdateProductActivity.NORMAL_PRICE.getDesc(), UpdateProductActivity.SELLING_PRICE.getDesc()));
  }

  @Test
  public void isPriceChangedForSkuAndPPCodeTest() throws Exception {
    Date date = new Date();
    when(
        updatedProductHistoryRepository.findFirstByGdnSkuAndPickupPointCodeAndAccessTimeGreaterThanEqualAndActivityInAndOnlineStatusTrue(
            DEFAULT_GDN_SKU, PICKUP_POINT_CODE, date, Arrays.asList(UpdateProductActivity.NORMAL_PRICE.getDesc(),
                UpdateProductActivity.SELLING_PRICE.getDesc()))).thenReturn(new UpdatedProductHistory());
    Assertions.assertTrue(
        updatedProductHistoryService.isPriceChangedForSkuAndPPCode(DEFAULT_GDN_SKU, date, PICKUP_POINT_CODE)
            .getResult());
    Mockito.verify(updatedProductHistoryRepository)
        .findFirstByGdnSkuAndPickupPointCodeAndAccessTimeGreaterThanEqualAndActivityInAndOnlineStatusTrue(DEFAULT_GDN_SKU,
            PICKUP_POINT_CODE, date,
            Arrays.asList(UpdateProductActivity.NORMAL_PRICE.getDesc(), UpdateProductActivity.SELLING_PRICE.getDesc()));
  }

  @Test
  public void isPriceChangedForSkuAndPPCodeNoChangeTest() throws Exception {
    Date date = new Date();
    when(
        updatedProductHistoryRepository.findFirstByGdnSkuAndPickupPointCodeAndAccessTimeGreaterThanEqualAndActivityInAndOnlineStatusTrue(
            DEFAULT_GDN_SKU, PICKUP_POINT_CODE, date, Arrays.asList(UpdateProductActivity.NORMAL_PRICE.getDesc(),
                UpdateProductActivity.SELLING_PRICE.getDesc()))).thenReturn(null);
    Assertions.assertFalse(
        updatedProductHistoryService.isPriceChangedForSkuAndPPCode(DEFAULT_GDN_SKU, date, PICKUP_POINT_CODE)
            .getResult());
    Mockito.verify(updatedProductHistoryRepository)
        .findFirstByGdnSkuAndPickupPointCodeAndAccessTimeGreaterThanEqualAndActivityInAndOnlineStatusTrue(DEFAULT_GDN_SKU,
            PICKUP_POINT_CODE, date,
            Arrays.asList(UpdateProductActivity.NORMAL_PRICE.getDesc(), UpdateProductActivity.SELLING_PRICE.getDesc()));
  }

  @Test
  public void testSaveUpdateProductLevel3AuditWithHTMLTextTest() throws Exception {
    UpdateProductItemLevel3Model savedProductValue =
        UpdateProductItemLevel3Model.builder().description(RICH_TEXT).build();
    UpdateProductItemLevel3Model updatedProductValue =
        UpdateProductItemLevel3Model.builder().description(FORMATTED_TEXT).build();
    Mockito.when(this.logAuditUpdateProductAnnotationUtil
        .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(generatePropertyMapper());
    this.updatedProductHistoryService
        .saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU,
            savedProductValue, updatedProductValue, DEFAULT_ACCESS_CHANNEL, DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU,
            NEED_CORRECTION, StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(listArgumentCaptor.capture());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void getVariantEditHistoryTest() throws Exception {
    Mockito.when(updatedProductHistoryRepository
        .findByProductSkuOrderByAccessTimeDesc(DEFAULT_PRODUCT_SKU, PageRequest.of(0, 20)))
        .thenReturn(logAuditTrailUpdatedProductPage);
    Page<HistoryResponse> variantHistoryResponses =
        updatedProductHistoryService.getProductEditHistory(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, StringUtils.EMPTY, 0, 20);
    Mockito.verify(updatedProductHistoryRepository)
        .findByProductSkuOrderByAccessTimeDesc(DEFAULT_PRODUCT_SKU, PageRequest.of(0, 20));
    Assertions.assertEquals(DEFAULT_GDN_SKU, variantHistoryResponses.getContent().get(0).getGdnSku());
    Assertions.assertEquals(GDN_NAME, variantHistoryResponses.getContent().get(0).getGdnName());
    Assertions.assertEquals(Constants.ALL_VARIANTS, variantHistoryResponses.getContent().get(1).getGdnSku());
  }

  @Test
  public void getVariantEditHistoryWithKeywordTest() throws Exception {
    Mockito.when(updatedProductHistoryRepository
        .findByProductSkuAndKeyword(DEFAULT_PRODUCT_SKU, GDN_NAME, PageRequest.of(0, 20)))
        .thenReturn(logAuditTrailUpdatedProductPage);
    Page<HistoryResponse> variantHistoryResponses =
        updatedProductHistoryService.getProductEditHistory(DEFAULT_STORE_ID, DEFAULT_PRODUCT_SKU, GDN_NAME, 0, 20);
    Mockito.verify(updatedProductHistoryRepository)
        .findByProductSkuAndKeyword(DEFAULT_PRODUCT_SKU, GDN_NAME, PageRequest.of(0, 20));
    Assertions.assertEquals(DEFAULT_GDN_SKU, variantHistoryResponses.getContent().get(0).getGdnSku());
    Assertions.assertEquals(GDN_NAME, variantHistoryResponses.getContent().get(0).getGdnName());
    Assertions.assertEquals(Constants.ALL_VARIANTS, variantHistoryResponses.getContent().get(1).getGdnSku());
  }

  @Test
  public void getVariantEditHistoryByAuditTrailIdTest() {
    Mockito.when(updatedProductHistoryRepository.findByAuditTrailIdInOrderByAccessTimeDesc(Arrays.asList(AUDIT_TRAIL_ID)))
        .thenReturn(logAuditTrailUpdatedProductPage.getContent());
    Page<HistoryResponse> variantHistoryResponses =
        updatedProductHistoryService.getProductEditHistoryByAuditTrailId(Arrays.asList(AUDIT_TRAIL_ID), 0, 20, 2);
    Mockito.verify(updatedProductHistoryRepository).findByAuditTrailIdInOrderByAccessTimeDesc(Arrays.asList(AUDIT_TRAIL_ID));
    Assertions.assertEquals(DEFAULT_GDN_SKU, variantHistoryResponses.getContent().get(0).getGdnSku());
    Assertions.assertEquals(GDN_NAME, variantHistoryResponses.getContent().get(0).getGdnName());
    Assertions.assertEquals(Constants.ALL_VARIANTS, variantHistoryResponses.getContent().get(1).getGdnSku());
  }

  @Test
  public void deleteFromDbTest() throws Exception {
    when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.UPDATE_HISTORY_DELETE_BATCH_SIZE))
        .thenReturn(new ProductSystemParameter("updateHistoryDeleteBatchSize", "1000", "updateHistoryDeleteBatchSize", false));
    when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.UPDATE_HISTORY_DELETE_PAGES_SIZE))
        .thenReturn(new ProductSystemParameter("updateHistoryDeletePagesSize", "2", "updateHistoryDeletePagesSize", Boolean.FALSE));
    when(updatedProductHistoryRepository.findAuditTrailIdByAccessTime(any(Date.class), any())).thenReturn(new ArrayList<>());
    this.updatedProductHistoryService.deleteFromDb(DEFAULT_STORE_ID);
    verify(applicationContext).getBean(UpdatedProductHistoryService.class);
    verify(updatedProductHistoryServiceMock).deleteUpdatedProductHistoryByAuditTrialIds(new ArrayList<>());
    verify(updatedProductHistoryRepository).findAuditTrailIdByAccessTime(any(Date.class), any());
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.AUDIT_TRAIL_DELETE_BATCH_SIZE);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.UPDATE_HISTORY_DELETE_BATCH_SIZE);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.UPDATE_HISTORY_DELETE_PAGES_SIZE);
  }

  @Test
  public void deleteFromDbauditTrailIdLessTest() throws Exception {
    when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.UPDATE_HISTORY_DELETE_BATCH_SIZE))
        .thenReturn(new ProductSystemParameter("updateHistoryDeleteBatchSize", "1000", "updateHistoryDeleteBatchSize", false));
    when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.UPDATE_HISTORY_DELETE_PAGES_SIZE))
        .thenReturn(new ProductSystemParameter("updateHistoryDeletePagesSize", "2", "updateHistoryDeletePagesSize", Boolean.FALSE));
    when(updatedProductHistoryRepository.findAuditTrailIdByAccessTime(any(Date.class), any())).thenReturn(Arrays.asList(AUDIT_TRAIL_ID));
    this.updatedProductHistoryService.deleteFromDb(DEFAULT_STORE_ID);
    verify(applicationContext).getBean(UpdatedProductHistoryService.class);
    verify(updatedProductHistoryServiceMock).deleteUpdatedProductHistoryByAuditTrialIds(Arrays.asList(AUDIT_TRAIL_ID));
    verify(updatedProductHistoryRepository).findAuditTrailIdByAccessTime(any(Date.class), any());
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.AUDIT_TRAIL_DELETE_BATCH_SIZE);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.UPDATE_HISTORY_DELETE_BATCH_SIZE);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.UPDATE_HISTORY_DELETE_PAGES_SIZE);
  }

  @Test
  public void deleteFromDbauditTrailIdLessPageSizeTest() throws Exception {
    when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.UPDATE_HISTORY_DELETE_BATCH_SIZE))
        .thenReturn(new ProductSystemParameter("updateHistoryDeleteBatchSize", "1", "updateHistoryDeleteBatchSize", false));
    when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.UPDATE_HISTORY_DELETE_PAGES_SIZE))
        .thenReturn(new ProductSystemParameter("updateHistoryDeletePagesSize", "2", "updateHistoryDeletePagesSize", Boolean.FALSE));
    when(updatedProductHistoryRepository.findAuditTrailIdByAccessTime(any(Date.class), any())).thenReturn(Arrays.asList(AUDIT_TRAIL_ID));
    this.updatedProductHistoryService.deleteFromDb(DEFAULT_STORE_ID);
    verify(applicationContext, times(2)).getBean(UpdatedProductHistoryService.class);
    verify(updatedProductHistoryServiceMock, times(2)).deleteUpdatedProductHistoryByAuditTrialIds(Arrays.asList(AUDIT_TRAIL_ID));
    verify(updatedProductHistoryRepository, times(2)).findAuditTrailIdByAccessTime(any(Date.class), any());
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.AUDIT_TRAIL_DELETE_BATCH_SIZE);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.UPDATE_HISTORY_DELETE_BATCH_SIZE);
    verify(productSystemParameterService, times(2))
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.UPDATE_HISTORY_DELETE_PAGES_SIZE);
  }

  @Test
  public void deleteFromDbWith0DaysTest() throws Exception {
    when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.UPDATE_HISTORY_DELETE_BATCH_SIZE))
        .thenReturn(new ProductSystemParameter("updateHistoryDeleteBatchSize", "1000", "updateHistoryDeleteBatchSize", false));
    when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.AUDIT_TRAIL_DELETE_BATCH_SIZE))
        .thenReturn(new ProductSystemParameter("auditTrailDeleteBatchSize", "0", "auditTrailDeleteBatchSize", false));
    this.updatedProductHistoryService.deleteFromDb(DEFAULT_STORE_ID);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.AUDIT_TRAIL_DELETE_BATCH_SIZE);
  }

  @Test
  public void deleteFromSolrTest() throws Exception {
    this.updatedProductHistoryService.deleteFromSolr(DEFAULT_STORE_ID);
    verify(solrHistoryCollectionRepository).deleteHistoryFromSolr(any(Integer.class), any(Date.class));
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.AUDIT_TRAIL_DELETE_BATCH_SIZE_SOLR);
  }

  @Test
  public void deleteUpdatedProductHistoryByAuditTrialIds() throws Exception {
    this.updatedProductHistoryService.deleteUpdatedProductHistoryByAuditTrialIds(new ArrayList<>());
    verify(auditTrailUpdateProductBackupRepository).saveBackupByAuditTrailIds(new ArrayList<>());
    verify(updatedProductHistoryRepository).deleteByAuditTrailIds(new ArrayList<>());
  }

  @Test
  public void deleteUpdatedProductHistoryByAuditTrialIdsException() throws Exception {
    doThrow(new RuntimeException(ErrorMessages.PRODUCT_NOT_FOUND)).when(auditTrailUpdateProductBackupRepository)
        .saveBackupByAuditTrailIds(Mockito.anyList());
    this.updatedProductHistoryService.deleteUpdatedProductHistoryByAuditTrialIds(new ArrayList<>());
    verify(auditTrailUpdateProductBackupRepository).saveBackupByAuditTrailIds(new ArrayList<>());
  }

  @Test
  public void deleteFromSolrWith0DaysTest() throws Exception {
    when(productSystemParameterService
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.AUDIT_TRAIL_DELETE_BATCH_SIZE_SOLR))
        .thenReturn(new ProductSystemParameter("auditTrailDeleteBatchSizeSolr", "0", "auditTrailDeleteBatchSize", false));
    this.updatedProductHistoryService.deleteFromSolr(DEFAULT_STORE_ID);
    verify(productSystemParameterService)
        .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.AUDIT_TRAIL_DELETE_BATCH_SIZE_SOLR);
  }

  @Test
  public void deleteFromSolr_ExceptionTest() throws Exception {
    doThrow(new RuntimeException(ErrorMessages.PRODUCT_NOT_FOUND)).when(solrHistoryCollectionRepository)
        .deleteHistoryFromSolr(any(Integer.class), any(Date.class));
    try {
      this.updatedProductHistoryService.deleteFromSolr(DEFAULT_STORE_ID);
    } finally {
      verify(solrHistoryCollectionRepository).deleteHistoryFromSolr(any(Integer.class), any(Date.class));
      verify(productSystemParameterService)
          .findByStoreIdAndVariable(DEFAULT_STORE_ID, SystemParameterConstants.AUDIT_TRAIL_DELETE_BATCH_SIZE_SOLR);
    }
  }

  @Test
  public void createUpdateLogisticAuditTest() throws Exception {
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(DEFAULT_GDN_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    Mockito.when(this.xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any()))
        .thenReturn(itemPickupPointCodeByItemSkus);
    updatedProductHistoryService
        .createProductL3AuditLog(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, DEFAULT_PRODUCT_SKU, DEFAULT_ACCESS_CHANNEL,
            UpdateProductActivity.LOGISTIC.getDesc(), String.valueOf(Collections.EMPTY_LIST),
            String.valueOf(Collections.singletonList("GOJEK")), false, StringUtils.EMPTY);
    verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
    Mockito.verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
  }

  @Test
  public void createUpdateLogisticAuditTestWithHistoryEventSwitchOn() throws Exception {
    ReflectionTestUtils.setField(updatedProductHistoryService,"productHistoryUpdateThroughEvent", true);
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(DEFAULT_GDN_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    Mockito.when(this.xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any()))
        .thenReturn(itemPickupPointCodeByItemSkus);
    updatedProductHistoryService
        .createProductL3AuditLog(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, DEFAULT_PRODUCT_SKU, DEFAULT_ACCESS_CHANNEL,
            UpdateProductActivity.LOGISTIC.getDesc(), String.valueOf(Collections.EMPTY_LIST),
            String.valueOf(Collections.singletonList("GOJEK")), false, StringUtils.EMPTY);
    Mockito.verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    verify(kafkaProducer).send(anyString(), anyString(), any(AuditTrailListRequest.class));
  }

  @Test
  public void createUpdateLogisticAuditPickupPointCodeNotEmptyTest() throws JsonProcessingException {
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(DEFAULT_GDN_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    Mockito.when(this.xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any()))
        .thenReturn(itemPickupPointCodeByItemSkus);
    updatedProductHistoryService
        .createProductL3AuditLog(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, DEFAULT_PRODUCT_SKU, DEFAULT_ACCESS_CHANNEL,
            UpdateProductActivity.LOGISTIC.getDesc(), String.valueOf(Collections.EMPTY_LIST),
            String.valueOf(Collections.singletonList("GOJEK")), false, PICKUP_POINT_CODE);
    verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void createUpdateLogisticAuditExceptionTest() throws Exception {
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(DEFAULT_GDN_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    Mockito.doThrow(new NullPointerException()).when(this.xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      updatedProductHistoryService
          .createProductL3AuditLog(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, DEFAULT_PRODUCT_SKU, DEFAULT_ACCESS_CHANNEL,
              UpdateProductActivity.LOGISTIC.getDesc(), String.valueOf(Collections.EMPTY_LIST),
              String.valueOf(Collections.singletonList("GOJEK")), false, StringUtils.EMPTY);
    });
    Mockito.verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
  }

  @Test
  public void createUpdateLogisticAuditTest2() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
    MDC.put(AuditTrailMandatoryRequestParameterUtil.CLIENT_HOST_KEY, SYSTEM);
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(DEFAULT_GDN_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    Mockito.when(this.xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any()))
        .thenReturn(itemPickupPointCodeByItemSkus);
    updatedProductHistoryService
        .createProductL3AuditLog(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, DEFAULT_PRODUCT_SKU, DEFAULT_ACCESS_CHANNEL,
            UpdateProductActivity.LOGISTIC.getDesc(), String.valueOf(Collections.EMPTY_LIST),
            String.valueOf(Collections.singletonList("GOJEK")), false, StringUtils.EMPTY);
    verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
    Mockito.verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
  }

  @Test
  public void addAuditLogsForProductHistoryUpdateTest() throws Exception {
    List<UpdatedProductHistory> auditLogs = new ArrayList<>();
    updatedProductHistoryService.addAuditLogsForProductHistoryUpdate(auditTrailRequest, auditLogs, DEFAULT_ACCESS_CHANNEL);
    Assertions.assertEquals(DEFAULT_BP_CODE, auditLogs.get(0).getBusinessPartnerCode());
  }

  @Test
  public void addAuditLogsForProductHistoryUpdatePickupPointUpdateTest() throws Exception {
    List<UpdatedProductHistory> auditLogs = new ArrayList<>();
    auditTrailRequest.setActionKey(String.valueOf(UpdateProductActivity.PICKUP_POINT_DELETED));
    auditTrailRequest.setNewValue(StringUtils.EMPTY);
    updatedProductHistoryService.addAuditLogsForProductHistoryUpdate(auditTrailRequest, auditLogs, DEFAULT_ACCESS_CHANNEL);
    Assertions.assertEquals(DEFAULT_BP_CODE, auditLogs.get(0).getBusinessPartnerCode());
  }

  @Test
  public void addAuditLogsForProductHistoryUpdatePickupPointUpdateActionIsEmptyTest() throws Exception {
    List<UpdatedProductHistory> auditLogs = new ArrayList<>();
    auditTrailRequest.setActionKey(String.valueOf(UpdateProductActivity.PICKUP_POINT_DELETED));
    auditTrailRequest.setNewValue(StringUtils.EMPTY);
    auditTrailRequest.setAttributeName(StringUtils.EMPTY);
    updatedProductHistoryService.addAuditLogsForProductHistoryUpdate(auditTrailRequest, auditLogs, DEFAULT_ACCESS_CHANNEL);
    Assertions.assertEquals("pickup point deleted",auditLogs.get(0).getActivity());
  }

  @Test
  public void addAuditLogsForProductHistoryUpdateOldValEqualToNewValTest() throws Exception {
    this.auditTrailRequest.setNewValue(DEFAULT_OLD_VALUE);
    List<UpdatedProductHistory> auditLogs = new ArrayList<>();
    updatedProductHistoryService.addAuditLogsForProductHistoryUpdate(auditTrailRequest, auditLogs, DEFAULT_ACCESS_CHANNEL);
    Assertions.assertEquals(auditLogs, new ArrayList<>());
  }

  @Test
  public void testSaveUpdateProductLevel3AuditForBuyableFlagChange() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = new UpdateProductItemLevel3Model();
    savedProductValue.setBuyable(true);
    UpdateProductItemLevel3Model updatedProductValue = new UpdateProductItemLevel3Model();
    updatedProductValue.setBuyable(false);
    when(
        this.logAuditUpdateProductAnnotationUtil
            .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
        generatePropertyMapper());
    this.updatedProductHistoryService.saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU,
        savedProductValue, updatedProductValue, null, DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION,
        StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void testSaveUpdateProductLevel3AuditNeedCorrection() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    savedProductValue.setMerchantSku("merchantSku");
    UpdateProductItemLevel3Model updatedProductValue = generateOldValues();
    updatedProductValue.setMerchantSku(null);
    when(
        this.logAuditUpdateProductAnnotationUtil
            .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
        generatePropertyMapper());
    when(this.updatedProductHistoryRepository.saveAll(anyList()))
        .thenReturn(Arrays.asList(new UpdatedProductHistory()));
    this.updatedProductHistoryService.saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU,
        savedProductValue, updatedProductValue, null, DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, true, StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void findProductHistoryByProductSkuAndKeywordTest() throws Exception {
    Mockito.when(updatedProductHistoryRepository
        .findByProductSkuAndAccessTimeBetweenAndOnlineStatusTrueOrderByAccessTimeDesc(eq(DEFAULT_PRODUCT_SKU),
            Mockito.any(), Mockito.any(), eq(PageRequest.of(0, 20))))
        .thenReturn(logAuditTrailUpdatedProductPage);
    Page<HistoryResponse> responses = updatedProductHistoryService.findProductHistoryByProductSkuAndKeyword(historyRequest, 0, 20);
    Mockito.verify(updatedProductHistoryRepository)
        .findByProductSkuAndAccessTimeBetweenAndOnlineStatusTrueOrderByAccessTimeDesc(eq(DEFAULT_PRODUCT_SKU),
            Mockito.any(), Mockito.any(), eq(PageRequest.of(0, 20)));
    Assertions.assertEquals(DEFAULT_GDN_SKU, responses.getContent().get(0).getGdnSku());
    Assertions.assertEquals(GDN_NAME, responses.getContent().get(0).getGdnName());
    Assertions.assertEquals(Constants.ALL_VARIANTS, responses.getContent().get(1).getGdnSku());
  }

  @Test
  public void findProductHistoryByProductSkuAndKeywordWithKeywordTest() throws Exception {
    Mockito.when(updatedProductHistoryRepository
        .findByProductSkuAndAccessTimeBetweenAndVariantNameAndOnlineStatusTrue(eq(DEFAULT_PRODUCT_SKU), Mockito.any(),
            Mockito.any(), eq(SYSTEM), eq(PageRequest.of(0, 20))))
        .thenReturn(logAuditTrailUpdatedProductPage);
    historyRequest.setKeyword(SYSTEM);
    Page<HistoryResponse> responses = updatedProductHistoryService.findProductHistoryByProductSkuAndKeyword(historyRequest, 0, 20);
    Mockito.verify(updatedProductHistoryRepository)
        .findByProductSkuAndAccessTimeBetweenAndVariantNameAndOnlineStatusTrue(eq(DEFAULT_PRODUCT_SKU), Mockito.any(),
            Mockito.any(), eq(SYSTEM), eq(PageRequest.of(0, 20)));
    Assertions.assertEquals(DEFAULT_GDN_SKU, responses.getContent().get(0).getGdnSku());
    Assertions.assertEquals(GDN_NAME, responses.getContent().get(0).getGdnName());
    Assertions.assertEquals(Constants.ALL_VARIANTS, responses.getContent().get(1).getGdnSku());
  }


  @Test
  public void findProductHistoryByProductSkuAndKeywordWithKeywordAndActivityTest() throws Exception {
    Mockito.when(updatedProductHistoryRepository
        .findByProductSkuAndAccessTimeBetweenAndActivityAndOnlineStatusTrue(eq(DEFAULT_PRODUCT_SKU), Mockito.any(),
            Mockito.any(), eq(DEFAULT_ACTIVITY), eq(PageRequest.of(0, 20))))
        .thenReturn(logAuditTrailUpdatedProductPage);
    historyRequest.setKeyword(DEFAULT_ACTIVITY);
    historyRequest.setSearchField(Constants.ACTIVITY);
    Page<HistoryResponse> responses = updatedProductHistoryService.findProductHistoryByProductSkuAndKeyword(historyRequest, 0, 20);
    Mockito.verify(updatedProductHistoryRepository)
        .findByProductSkuAndAccessTimeBetweenAndActivityAndOnlineStatusTrue(eq(DEFAULT_PRODUCT_SKU), Mockito.any(),
            Mockito.any(), eq(DEFAULT_ACTIVITY), eq(PageRequest.of(0, 20)));
    Assertions.assertEquals(DEFAULT_GDN_SKU, responses.getContent().get(0).getGdnSku());
    Assertions.assertEquals(GDN_NAME, responses.getContent().get(0).getGdnName());
    Assertions.assertEquals(Constants.ALL_VARIANTS, responses.getContent().get(1).getGdnSku());
  }

  @Test
  public void addAuditLogsForVatUpdateTest() throws Exception {
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BP_CODE);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_GDN_SKU);
    productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku(DEFAULT_GDN_SKU);
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(DEFAULT_GDN_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    Mockito.when(this.xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any()))
        .thenReturn(itemPickupPointCodeByItemSkus);

    updatedProductHistoryService
        .addAuditLogsForVatUpdate(Arrays.asList(productItemBusinessPartner), GDN_NAME, Boolean.FALSE.toString(),
            Boolean.TRUE.toString());

    Mockito.verify(this.updatedProductHistoryRepository).saveAll(listArgumentCaptor.capture());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));

    Assertions.assertEquals(DEFAULT_PRODUCT_SKU, listArgumentCaptor.getValue().get(0).getProductSku());
    Assertions.assertEquals(DEFAULT_GDN_SKU, listArgumentCaptor.getValue().get(0).getGdnSku());
    Assertions.assertEquals(GDN_NAME, listArgumentCaptor.getValue().get(0).getGdnName());
    Assertions.assertEquals(DEFAULT_BP_CODE, listArgumentCaptor.getValue().get(0).getBusinessPartnerCode());
    Assertions.assertEquals(UpdateProductActivity.APPLICABLE_FOR_VAT.getDesc(),
        listArgumentCaptor.getValue().get(0).getActivity());
    Assertions.assertEquals(Boolean.FALSE.toString(), listArgumentCaptor.getValue().get(0).getOldValues());
    Assertions.assertEquals(Boolean.TRUE.toString(), listArgumentCaptor.getValue().get(0).getNewValues());
    Assertions.assertEquals(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        listArgumentCaptor.getValue().get(0).getChangedBy());
  }

  @Test
  public void addAuditLogsForVatUpdateTestWithEventSwitchOn() throws Exception {
    ReflectionTestUtils.setField(updatedProductHistoryService, "productHistoryUpdateThroughEvent", true);
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    productBusinessPartner.setBusinessPartnerId(DEFAULT_BP_CODE);
    productBusinessPartner.setGdnProductSku(DEFAULT_PRODUCT_SKU);
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_GDN_SKU);
    productItemBusinessPartner.setProductBusinessPartner(productBusinessPartner);
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku(DEFAULT_GDN_SKU);
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(DEFAULT_GDN_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    Mockito.when(this.xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any()))
        .thenReturn(itemPickupPointCodeByItemSkus);
    updatedProductHistoryService
        .addAuditLogsForVatUpdate(Arrays.asList(productItemBusinessPartner), GDN_NAME, Boolean.FALSE.toString(),
            Boolean.TRUE.toString());

    verify(kafkaProducer).send(anyString(), anyString(),any(AuditTrailListRequest.class));
}

  @Test
  public void addAuditLogsForVatUpdate_nullL3Test() throws Exception {
    ProductItemBusinessPartner productItemBusinessPartner = new ProductItemBusinessPartner();
    productItemBusinessPartner.setGdnProductItemSku(DEFAULT_GDN_SKU);
    productItemBusinessPartner.setProductBusinessPartner(null);
    ProductItemBusinessPartner productItemBusinessPartner2 = new ProductItemBusinessPartner();
    productItemBusinessPartner2.setGdnProductItemSku(DEFAULT_GDN_SKU);

    updatedProductHistoryService
      .addAuditLogsForVatUpdate(Arrays.asList(productItemBusinessPartner), GDN_NAME, Boolean.FALSE.toString(),
        Boolean.TRUE.toString());
    Assertions.assertNull(productItemBusinessPartner.getProductBusinessPartner());
  }

  @Test
  public void createUpdateLogisticAudit_needRevisionTrueTest() throws Exception {
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(DEFAULT_GDN_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    Mockito.when(this.xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any()))
        .thenReturn(itemPickupPointCodeByItemSkus);
    updatedProductHistoryService
      .createProductL3AuditLog(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, DEFAULT_PRODUCT_SKU, DEFAULT_ACCESS_CHANNEL,
        UpdateProductActivity.LOGISTIC.getDesc(), String.valueOf(Collections.EMPTY_LIST),
        String.valueOf(Collections.singletonList("GOJEK")), true, StringUtils.EMPTY);
    verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
    Mockito.verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
  }

  @Test
  public void updateProductHistoryDeltailListTest() throws Exception {
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(DEFAULT_GDN_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    Mockito.when(this.xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any()))
        .thenReturn(itemPickupPointCodeByItemSkus);
    when(this.updatedProductHistoryRepository.saveAll(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    List<UpdatedProductHistory> updatedProductHistoryList = new ArrayList<>();
    UpdatedProductHistory updatedProductHistory = new UpdatedProductHistory();
    updatedProductHistory.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    updatedProductHistory.setProductSku(PRODUCT_SKU);
    updatedProductHistory.setGdnName(GDN_NAME);
    updatedProductHistory.setActivity(ACTIVITY);
    updatedProductHistory.setOldValues(OLD_VALUE);
    updatedProductHistory.setNewValues(NEW_VALUE);
    updatedProductHistory.setChangedBy(CHANGED_BY);
    updatedProductHistory.setRequestId(REQUEST_ID);
    updatedProductHistory.setClientHost(DEFAULT_ACCESS_CHANNEL);
    updatedProductHistory.setGdnSku(DEFAULT_GDN_SKU);
    updatedProductHistoryService.updateProductHistoryDeltailList(updatedProductHistoryList);
    Mockito.verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
  }

  @Test
  public void updateProductHistoryDeltailListTestWithHistoryEventSwitchOn() throws Exception {
    ReflectionTestUtils.setField(updatedProductHistoryService, "productHistoryUpdateThroughEvent", true);
    GdnRestListResponse<ItemSkuPickupPointCodeResponse> itemPickupPointCodeByItemSkus = new GdnRestListResponse<>();
    List<ItemSkuPickupPointCodeResponse> itemSkuPickupPointList = new ArrayList<>();
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(DEFAULT_GDN_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointCodeByItemSkus.setContent(itemSkuPickupPointList);
    Mockito.when(this.xProductOutbound.getItemPickupPointCodeByItemSkus(Mockito.any()))
        .thenReturn(itemPickupPointCodeByItemSkus);
    when(this.updatedProductHistoryRepository.saveAll(Mockito.anyList()))
        .thenReturn(new ArrayList<>());
    List<UpdatedProductHistory> updatedProductHistoryList = new ArrayList<>();
    UpdatedProductHistory updatedProductHistory = new UpdatedProductHistory();
    updatedProductHistory.setBusinessPartnerCode(BUSINESSPARTNER_CODE);
    updatedProductHistory.setProductSku(PRODUCT_SKU);
    updatedProductHistory.setGdnName(GDN_NAME);
    updatedProductHistory.setActivity(ACTIVITY);
    updatedProductHistory.setOldValues(OLD_VALUE);
    updatedProductHistory.setNewValues(NEW_VALUE);
    updatedProductHistory.setChangedBy(CHANGED_BY);
    updatedProductHistory.setRequestId(REQUEST_ID);
    updatedProductHistory.setClientHost(DEFAULT_ACCESS_CHANNEL);
    updatedProductHistory.setGdnSku(DEFAULT_GDN_SKU);
    updatedProductHistoryList.add(updatedProductHistory);
    updatedProductHistoryService.updateProductHistoryDeltailList(updatedProductHistoryList);
    Mockito.verify(xProductOutbound).getItemPickupPointCodeByItemSkus(Mockito.any());
    Mockito.verify(kafkaProducer).send(anyString(),anyString(),any(AuditTrailListRequest.class));
  }

  @Test
  public void testSaveUpdateProductLevel3_attributeRemovedTest() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    savedProductValue.setPreOrder(true);
    savedProductValue.setBuyable(false);
    savedProductValue.setUrlVideo(RICH_TEXT);
    String[] attributes = new String[] {"OS#_#IOS"};
    savedProductValue.setAttributesMap(attributes);
    UpdateProductItemLevel3Model updatedProductValue = generateNewValues();
//    updatedProductValue.setAttributesMap(attributes);
    updatedProductValue.setPreOrder(true);
    updatedProductValue.setBuyable(false);
    updatedProductValue.setUrlVideo(FORMATTED_TEXT);
    when(this.logAuditUpdateProductAnnotationUtil
      .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(generatePropertyMapper());
    when(this.updatedProductHistoryRepository.saveAll(anyList()))
      .thenReturn(new ArrayList<>());
    this.updatedProductHistoryService
      .saveUpdateProductLevel3Audit(DEFAULT_BP_CODE, DEFAULT_GDN_SKU, savedProductValue, updatedProductValue, null,
        DEFAULT_PRODUCT_SKU, DEFAULT_GDN_SKU, NEED_CORRECTION, StringUtils.EMPTY);
    Mockito.verify(this.updatedProductHistoryRepository).saveAll(Mockito.anyList());
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
  }

  @Test
  public void getMerchantModifiedFieldsTest() throws Exception {
    UpdateProductItemLevel3Model savedProductValue = generateOldValues();
    savedProductValue.setMerchantSku(MERCHANT_SKU);
    savedProductValue.setDescription(RICH_TEXT);
    savedProductValue.setProductName(PRODUCT_NAME);
    savedProductValue.setUsp(USP);
    savedProductValue.setAttributesMap(attributes);
    savedProductValue.setUrlVideo(URL1);
    savedProductValue.setInstallationFlag(StringUtils.EMPTY);
    UpdateProductItemLevel3Model updatedProductValue = generateOldValues();
    updatedProductValue.setMerchantSku(null);
    updatedProductValue.setDescription(FORMATTED_TEXT);
    updatedProductValue.setProductName(null);
    updatedProductValue.setInstallationFlag(null);
    updatedProductValue.setUsp(StringUtils.SPACE);
    updatedProductValue.setAttributesMap(attributesUpdated);
    when(this.logAuditUpdateProductAnnotationUtil
            .getLogAuditUpdateProductPropertyMap(UpdateProductItemLevel3Model.class)).thenReturn(
        generatePropertyMapper());
    List<String> modifiedFields =
        this.updatedProductHistoryService.getMerchantModifiedFields(savedProductValue, updatedProductValue);
    Assertions.assertTrue(modifiedFields.contains(MERCHANT_SKU));
    Assertions.assertEquals(4, modifiedFields.size());
  }

  @Test
  public void getOfflineProductHistoryByItemSkuAndPickupPointCodeTest(){
    when(this.updatedProductHistoryRepository.findByGdnSkuAndPickupPointCodeAndOnlineStatusFalseOrderByAccessTimeDesc(DEFAULT_GDN_SKU,
            PICKUP_POINT_CODE, pageable)).thenReturn(logAuditTrailUpdatedProductPage);
    Page<UpdatedProductHistory> updatedProductHistories = updatedProductHistoryService.getOfflineProductHistoryByItemSkuAndPickupPointCode(
            DEFAULT_STORE_ID, DEFAULT_GDN_SKU, PICKUP_POINT_CODE, pageable);
    Mockito.verify(updatedProductHistoryRepository)
        .findByGdnSkuAndPickupPointCodeAndOnlineStatusFalseOrderByAccessTimeDesc(DEFAULT_GDN_SKU,
            PICKUP_POINT_CODE, pageable);
    Assertions.assertEquals(DEFAULT_GDN_SKU, updatedProductHistories.getContent().get(0).getGdnSku());
    Assertions.assertEquals(GDN_NAME, updatedProductHistories.getContent().get(0).getGdnName());
  }

  @Test
  public void getOfflineProductHistoryByItemSkuAndPickupPointCodePickupPointNullTest() {
    Assertions.assertThrows(Exception.class, () -> {
      updatedProductHistoryService.getOfflineProductHistoryByItemSkuAndPickupPointCode(DEFAULT_STORE_ID, null,
          PICKUP_POINT_CODE, pageable);
    });
  }

  @Test
  public void getOfflineProductHistoryByItemSkuAndPickupPointCodeItemSkuNullTest() {
    Assertions.assertThrows(Exception.class, () -> {
      updatedProductHistoryService.getOfflineProductHistoryByItemSkuAndPickupPointCode(DEFAULT_STORE_ID,
          DEFAULT_GDN_SKU, null, pageable);
    });
  }

  @Test
  public void getProductUpdateHistoryByAuditTrailIdTest() {
    Mockito.when(this.updatedProductHistoryRepository.findByAuditTrailIdInOrderByAccessTimeDesc(
        Collections.singletonList(AUDIT_TRAIL_ID)))
      .thenReturn(logAuditTrailUpdatedProductPage.getContent());
    Page<HistoryUpdateResponse> response =
      this.updatedProductHistoryService.getProductUpdateHistoryByAuditTrailId(
        Collections.singletonList(AUDIT_TRAIL_ID), DEFAULT_PAGE, DEFAULT_SIZE, DEFAULT_SIZE);
    Mockito.verify(this.updatedProductHistoryRepository).findByAuditTrailIdInOrderByAccessTimeDesc(
      Collections.singletonList(AUDIT_TRAIL_ID));
    Assertions.assertEquals(DEFAULT_GDN_SKU, response.getContent().get(0).getGdnSku());
    Assertions.assertEquals(Constants.ALL_VARIANTS, response.getContent().get(1).getGdnSku());
  }

  @Test
  public void findProductUpdateHistoryByProductSkuAndKeywordMppTrueTest() {
    Mockito.when(
        this.updatedProductHistoryCustomRepository.findByHistoryUpdateRequest(historyUpdateRequest,
                PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE))).thenReturn(logAuditTrailUpdatedProductPage);
    Page<HistoryUpdateResponse> response =
        this.updatedProductHistoryService.findProductUpdateHistoryByProductSkuAndKeyword(
            historyUpdateRequest, DEFAULT_PAGE, DEFAULT_SIZE);
    Mockito.verify(this.updatedProductHistoryCustomRepository)
        .findByHistoryUpdateRequest(historyUpdateRequest, PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE));
    Assertions.assertEquals(DEFAULT_GDN_SKU, response.getContent().get(0).getGdnSku());
    Assertions.assertEquals(Constants.ALL_VARIANTS, response.getContent().get(1).getGdnSku());
  }

  @Test
  public void addToUpdatedProductHistoryTest() {
    this.updatedProductHistoryService.addToUpdatedProductHistory(DEFAULT_GDN_SKU, ACTIVITY, DEFAULT_BP_CODE, REQUEST_ID,
        DEFAULT_USER, OLD_VALUE, NEW_VALUE, PRODUCT_SKU, PRODUCT_NAME, PICKUP_POINT_CODE, true);
  }

  @Test
  public void deleteUpdateProductHistoryByStoreIdAndProductIdTest() {
    updatedProductHistoryService.deleteUpdateProductHistoryByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
    verify(updatedProductHistoryRepository).deleteByStoreIdAndProductId(STORE_ID, PRODUCT_ID);
  }
}
