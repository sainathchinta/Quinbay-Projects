package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gda.mta.product.dto.BulkDataForRecategorizationRequest;
import com.gda.mta.product.dto.CategoryProductCodeMappingRequest;
import com.gda.mta.product.dto.CategoryProductSkuMappingRequest;
import com.gda.mta.product.dto.CategoryUserMappingRequest;
import com.gda.mta.product.dto.ProductSkuToSalesCatalogMappingRequest;
import com.gda.mta.product.dto.RecategorizationRequest;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.config.RecategorizationStatus;
import com.gdn.mta.product.entity.CategoryProductCodeMapping;
import com.gdn.mta.product.entity.CategoryProductSkuMapping;
import com.gdn.mta.product.entity.CategoryUserMapping;
import com.gdn.mta.product.entity.ProductSkuSalesCatalogMapping;
import com.gdn.mta.product.entity.Recategorization;
import com.gdn.mta.product.repository.CategoryProductCodeMappingRepository;
import com.gdn.mta.product.repository.CategoryProductSkuMappingRepository;
import com.gdn.mta.product.repository.CategoryUserMappingRepository;
import com.gdn.mta.product.repository.ProductSkuSalesCatalogMappingRepository;
import com.gdn.mta.product.repository.RecategorizationRepository;
import com.gdn.partners.pbp.service.tools.ProductCollectionToolsService;
import org.slf4j.MDC;


/**
 * Created by hardikbohra on 07/06/18.
 */
public class RecategorizationServiceBeanTest {

  private static final String RECAT_NAME = "recatName";
  private static final String RECAT_ID = "recatId";
  private static final String STATUS = "status";
  private static final String OLD_STATUS = "oldStatus";
  private static final String EXCEL_FILE_PATH = "excelFile";
  private static final String REQUEST_ID = "requestId";
  private static final String STORE_ID = "storeId";
  private static final String USERNAME = "username";
  private static final String USERNAME_EMAIL_ID = "usernameEmailId";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String CATALOG_CODE = "catalogCode";
  private static final String BP_CODE = "bpCode";

  @InjectMocks
  private RecategorizationServiceBean serviceBean;

  @Mock
  private RecategorizationRepository recategorizationRepository;

  @Mock
  private CategoryUserMappingRepository categoryUserMappingRepository;

  @Mock
  private CategoryProductCodeMappingRepository categoryProductCodeMappingRepository;

  @Mock
  private CategoryProductSkuMappingRepository categoryProductSkuMappingRepository;

  @Mock
  private ProductSkuSalesCatalogMappingRepository productSkuSalesCatalogMappingRepository;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private ProductCollectionToolsService productCollectionToolsService;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(recategorizationRepository);
    Mockito.verifyNoMoreInteractions(categoryProductCodeMappingRepository);
    Mockito.verifyNoMoreInteractions(categoryProductSkuMappingRepository);
    Mockito.verifyNoMoreInteractions(categoryUserMappingRepository);
    Mockito.verifyNoMoreInteractions(productCollectionToolsService);
    Mockito.verifyNoMoreInteractions(productSkuSalesCatalogMappingRepository);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
  }

  @Test
  public void saveTest_whenRecatNotFound() throws Exception {
    RecategorizationRequest recategorizationRequest = new RecategorizationRequest(RECAT_NAME, STATUS, EXCEL_FILE_PATH);
    Recategorization recategorization = new Recategorization(RECAT_NAME, STATUS, EXCEL_FILE_PATH);
    recategorizationRequest.setId(RECAT_ID);
    recategorization.setId(RECAT_ID);
    Mockito.when(recategorizationRepository.save(Mockito.any(Recategorization.class))).thenReturn(recategorization);
    String response = serviceBean.save(recategorizationRequest);
    Mockito.verify(recategorizationRepository).save(Mockito.any(Recategorization.class));
    Mockito.verify(recategorizationRepository).findById(Mockito.eq(recategorization.getId()));
  }

  @Test
  public void saveTest_whenRecatPresent() throws Exception {
    RecategorizationRequest recategorizationRequest = new RecategorizationRequest(RECAT_NAME, STATUS, EXCEL_FILE_PATH);
    Recategorization savedRecategorization = new Recategorization(RECAT_NAME, OLD_STATUS, EXCEL_FILE_PATH);
    Recategorization recategorization = new Recategorization(RECAT_NAME, STATUS, EXCEL_FILE_PATH);
    recategorizationRequest.setId(RECAT_ID);
    recategorization.setId(RECAT_ID);
    Mockito.when(recategorizationRepository.findById(Mockito.eq(recategorization.getId()))).thenReturn(Optional.of(savedRecategorization));
    Mockito.when(recategorizationRepository.save(Mockito.eq(savedRecategorization))).thenReturn(recategorization);
    String response = serviceBean.save(recategorizationRequest);
    Mockito.verify(recategorizationRepository).save(Mockito.eq(savedRecategorization));
    Mockito.verify(recategorizationRepository).findById(Mockito.eq(recategorization.getId()));
  }

  @Test
  public void processCategoryToProductCodeMappingTest() throws Exception {
    List<Object> row = new ArrayList<>();
    row.add("category1");
    row.add("productCode1");
    BulkDataForRecategorizationRequest recategorizationRequest = new BulkDataForRecategorizationRequest(Arrays.asList
        (row), RECAT_ID, REQUEST_ID, USERNAME, STORE_ID);
    CategoryProductCodeMappingRequest mappingRequest = new CategoryProductCodeMappingRequest(row.get(0).toString(),
        row.get(1).toString(), RecategorizationStatus.COMPLETED.toString(), RECAT_ID);
    mappingRequest.setStoreId(recategorizationRequest.getStoreId());
    mappingRequest.setRequestId(recategorizationRequest.getRequestId());
    mappingRequest.setUsername(recategorizationRequest.getUsername());
    serviceBean.processCategoryToProductCodeMapping(recategorizationRequest);
    Mockito.verify(productCollectionToolsService).moveProductCollectionCategory(REQUEST_ID, USERNAME, STORE_ID, row
        .get(1).toString(), row.get(0).toString());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.CATEGORY_TO_PRODUCT_CODE_MAPPING_SAVE_EVENT), Mockito.any());
  }

  @Test
  public void processCategoryToProductCodeMappingTest_whenException() throws Exception {
    List<Object> row = new ArrayList<>();
    row.add(CATEGORY_CODE);
    row.add(PRODUCT_CODE);
    BulkDataForRecategorizationRequest recategorizationRequest = new BulkDataForRecategorizationRequest(Arrays.asList
        (row), RECAT_ID, REQUEST_ID, USERNAME, STORE_ID);
    Mockito.doThrow(new Exception()).when(productCollectionToolsService).moveProductCollectionCategory(REQUEST_ID,
        USERNAME, STORE_ID, row.get(1).toString(), row.get(0).toString());
    serviceBean.processCategoryToProductCodeMapping(recategorizationRequest);
    Mockito.verify(productCollectionToolsService).moveProductCollectionCategory(REQUEST_ID, USERNAME, STORE_ID, row
        .get(1).toString(), row.get(0).toString());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.CATEGORY_TO_PRODUCT_CODE_MAPPING_SAVE_EVENT), Mockito.any());
  }

  @Test
  public void saveCategoryToProductCodeMappingTest() throws Exception {
    Recategorization recategorization = new Recategorization(RECAT_NAME, STATUS, EXCEL_FILE_PATH);
    recategorization.setId(RECAT_ID);
    Mockito.when(recategorizationRepository.findById(RECAT_ID)).thenReturn(Optional.of(recategorization));
    CategoryProductCodeMappingRequest mappingRequest = new CategoryProductCodeMappingRequest
        (CATEGORY_CODE, PRODUCT_CODE, STATUS, RECAT_ID);
    serviceBean.saveCategoryToProductCodeMapping(mappingRequest);
    Mockito.verify(categoryProductCodeMappingRepository).save(Mockito.any(CategoryProductCodeMapping.class));
    Mockito.verify(recategorizationRepository).findById(RECAT_ID);
  }

  @Test
  public void saveCategoryToProductCodeMappingTest_whenRecatNotPresent() throws Exception {
    CategoryProductCodeMappingRequest mappingRequest = new CategoryProductCodeMappingRequest
        (CATEGORY_CODE, PRODUCT_CODE, STATUS, RECAT_ID);
    try {
      serviceBean.saveCategoryToProductCodeMapping(mappingRequest);
    } catch (Exception ex) {
      Mockito.verify(recategorizationRepository).findById(RECAT_ID);
    }
  }

  @Test
  public void saveCategoryToProductSkuMappingTest() throws Exception {
    Recategorization recategorization = new Recategorization(RECAT_NAME, STATUS, EXCEL_FILE_PATH);
    recategorization.setId(RECAT_ID);
    Mockito.when(recategorizationRepository.findById(RECAT_ID)).thenReturn(Optional.of(recategorization));
    CategoryProductSkuMappingRequest mappingRequest = new CategoryProductSkuMappingRequest
        (CATEGORY_CODE, PRODUCT_SKU, RECAT_ID, STATUS);
    serviceBean.saveCategoryToProductSkuMapping(mappingRequest);
    Mockito.verify(categoryProductSkuMappingRepository).save(Mockito.any(CategoryProductSkuMapping.class));
    Mockito.verify(recategorizationRepository).findById(RECAT_ID);
  }

  @Test
  public void saveCategoryToProductSkuMappingTest_whenRecatNotPresent() throws Exception {
    CategoryProductSkuMappingRequest mappingRequest = new CategoryProductSkuMappingRequest
        (CATEGORY_CODE, PRODUCT_SKU, RECAT_ID, STATUS);
    try {
      serviceBean.saveCategoryToProductSkuMapping(mappingRequest);
    } catch (Exception ex) {
      Mockito.verify(recategorizationRepository).findById(RECAT_ID);
    }
  }

  @Test
  public void saveProductSkuToSalesCatalogMappingTest() throws Exception {
    Recategorization recategorization = new Recategorization(RECAT_NAME, STATUS, EXCEL_FILE_PATH);
    recategorization.setId(RECAT_ID);
    Mockito.when(recategorizationRepository.findById(RECAT_ID)).thenReturn(Optional.of(recategorization));
    ProductSkuToSalesCatalogMappingRequest mappingRequest = new ProductSkuToSalesCatalogMappingRequest
        (CATALOG_CODE, CATEGORY_CODE, PRODUCT_SKU, RECAT_ID, STATUS);
    serviceBean.saveProductSkuToSalesCatalogMapping(mappingRequest);
    Mockito.verify(productSkuSalesCatalogMappingRepository).save(Mockito.any(ProductSkuSalesCatalogMapping.class));
    Mockito.verify(recategorizationRepository).findById(RECAT_ID);
  }

  @Test
  public void saveProductSkuToSalesCatalogMappingTest_whenRecatNotPresent() throws Exception {
    ProductSkuToSalesCatalogMappingRequest mappingRequest = new ProductSkuToSalesCatalogMappingRequest
        (CATALOG_CODE, CATEGORY_CODE, PRODUCT_SKU, RECAT_ID, STATUS);
    try {
      serviceBean.saveProductSkuToSalesCatalogMapping(mappingRequest);
    } catch (Exception ex) {
      Mockito.verify(recategorizationRepository).findById(RECAT_ID);
    }
  }

  @Test
  public void saveCategoryToUserMappingTest() throws Exception {
    Recategorization recategorization = new Recategorization(RECAT_NAME, STATUS, EXCEL_FILE_PATH);
    recategorization.setId(RECAT_ID);
    Mockito.when(recategorizationRepository.findById(RECAT_ID)).thenReturn(Optional.of(recategorization));
    CategoryUserMappingRequest mappingRequest = new CategoryUserMappingRequest
        (CATEGORY_CODE, BP_CODE, USERNAME_EMAIL_ID, RECAT_ID, STATUS);
    CategoryUserMapping mapping = new CategoryUserMapping(mappingRequest.getRecatId(), mappingRequest.getCategoryCode
        (), mappingRequest.getBusinessPartnerCode(), mappingRequest.getUserEmailId(), mappingRequest.getStatus(),
        mappingRequest.getStoreId());
    serviceBean.saveCategoryToUserMapping(mappingRequest);
    Mockito.verify(categoryUserMappingRepository).save(Mockito.eq(mapping));
    Mockito.verify(recategorizationRepository).findById(RECAT_ID);
  }

  @Test
  public void saveCategoryToUserMappingTest_whenRecatNotPresent() throws Exception {
    CategoryUserMappingRequest mappingRequest = new CategoryUserMappingRequest
        (CATEGORY_CODE, BP_CODE, USERNAME_EMAIL_ID, RECAT_ID, STATUS);
    try {
      serviceBean.saveCategoryToUserMapping(mappingRequest);
    } catch (Exception ex) {
      Mockito.verify(recategorizationRepository).findById(RECAT_ID);
    }
  }
}
