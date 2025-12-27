package com.gdn.mta.product.service.solr;

import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;


import org.apache.solr.client.solrj.SolrClient;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
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
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.UpdatedProductHistoryRepository;
import com.gdn.mta.product.repository.SolrHistoryCollectionRepository;
import com.gdn.mta.product.repository.SolrIndexingRepository;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;

public class SolrIndexingServiceBeanTest {

  @InjectMocks
  private SolrIndexingServiceBean solrIndexingService;

  @Mock
  private SolrIndexingRepository repository;

  @Mock
  private ProductSystemParameterService systemParameterService;

  @Mock
  private UpdatedProductHistoryRepository updatedProductHistoryRepository;

  @Mock
  private SolrHistoryCollectionRepository solrHistoryCollectionRepository;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  private ProductService productService;

  private static final String SOURCE_COLLECTION = "SOURCE_COLLECTION";
  private static final String DESTINATION_COLLECTION = "DESTINATION_COLLECTION";
  private static final String SOLR_COLLECTION_URL = "SOLR_COLLECTION_URL";
  private static final String SOLR_CLOUD_URLS = "solrCloudUrls";
  private static final String SLASH = "/";
  private static final String COMMA = ",";
  private static final String STORE_ID = "STORE_ID";
  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String GDN_SKU = "GDN_SKU";
  private static final String GDN_NAME = "GDN_NAME";
  private static final String ACTIVITY = "ACTIVITY";
  private static final String ID = "id";

  private UpdatedProductHistory updatedProductHistory;

  @Captor
  private ArgumentCaptor<SolrClient> solrClient;

  @Captor
  private ArgumentCaptor<CloudSolrClient> cloudSolrClient;

  @Captor
  private ArgumentCaptor<List<SolrInputDocument>> solrInputDocumentArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(solrIndexingService, "solrConnectionUrl", SOLR_COLLECTION_URL);
    ReflectionTestUtils.setField(solrIndexingService, "solrCloudUrls", SOLR_CLOUD_URLS);

    updatedProductHistory = new UpdatedProductHistory();
    updatedProductHistory.setAuditTrailId(ID);
    updatedProductHistory.setAccessTime(new Date());
    updatedProductHistory.setGdnSku(GDN_SKU);
    updatedProductHistory.setProductSku(PRODUCT_SKU);
    updatedProductHistory.setGdnName(GDN_NAME);
    updatedProductHistory.setActivity(ACTIVITY);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(repository);
    Mockito.verifyNoMoreInteractions(systemParameterService);
    Mockito.verifyNoMoreInteractions(updatedProductHistoryRepository);
    Mockito.verifyNoMoreInteractions(solrHistoryCollectionRepository);
    Mockito.verifyNoMoreInteractions(productCollectionRepository);
    Mockito.verifyNoMoreInteractions(productService);
  }

  @Test
  public void updateAllTest() throws Exception {
    doNothing().when(this.repository).updateAll(solrClient.capture(), cloudSolrClient.capture());
    this.solrIndexingService.updateAll(SOURCE_COLLECTION, DESTINATION_COLLECTION);
    verify(this.repository).updateAll(solrClient.capture(), cloudSolrClient.capture());
  }

  @Test
  public void updateAllExceptionTest() throws Exception {
    doThrow(Exception.class).when(this.repository).updateAll(solrClient.capture(), cloudSolrClient.capture());
    this.solrIndexingService.updateAll(SOURCE_COLLECTION, DESTINATION_COLLECTION);
    verify(this.repository).updateAll(solrClient.capture(), cloudSolrClient.capture());
  }

  @Test
  public void reindexVariantHistoryByProductSkusTest() throws Exception {
    when(systemParameterService
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE)).thenReturn(
        new ProductSystemParameter(SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE, "1",
            SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE, false));
    when(updatedProductHistoryRepository.findByProductSkuOrderByAccessTimeDesc(PRODUCT_SKU, PageRequest.of(0, 1)))
        .thenReturn(new PageImpl<>(Arrays.asList(updatedProductHistory), PageRequest.of(0, 1), 2));
    when(updatedProductHistoryRepository.findByProductSkuOrderByAccessTimeDesc(PRODUCT_SKU, PageRequest.of(1, 1)))
        .thenReturn(new PageImpl<>(Arrays.asList(updatedProductHistory), PageRequest.of(0, 1), 2));
    doNothing().when(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.capture());

    solrIndexingService.reindexProductHistoryByProductSkus(STORE_ID, Arrays.asList(PRODUCT_SKU));

    verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE);
    verify(updatedProductHistoryRepository).findByProductSkuOrderByAccessTimeDesc(PRODUCT_SKU, PageRequest.of(0, 1));
    verify(updatedProductHistoryRepository).findByProductSkuOrderByAccessTimeDesc(PRODUCT_SKU, PageRequest.of(1, 1));
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(1));

    Assertions.assertEquals(ID, solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.ID));
    Assertions.assertEquals(PRODUCT_SKU, solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.PRODUCT_SKU));
    Assertions.assertEquals(GDN_SKU, solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.GDN_SKU));
    Assertions.assertEquals(GDN_NAME, solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.GDN_NAME));
    Assertions.assertEquals(ACTIVITY, solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.ACTIVITY));
    Assertions.assertNotNull(solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.ACCESS_TIME));
  }

  @Test
  public void reindexVariantHistoryByProductSkusEmptyResponseTest() throws Exception {
    when(systemParameterService
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE)).thenReturn(
        new ProductSystemParameter(SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE, "1",
            SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE, false));
    when(updatedProductHistoryRepository.findByProductSkuOrderByAccessTimeDesc(PRODUCT_SKU, PageRequest.of(0, 1)))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 1), 1));

    solrIndexingService.reindexProductHistoryByProductSkus(STORE_ID, Arrays.asList(PRODUCT_SKU));

    verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE);
    verify(updatedProductHistoryRepository).findByProductSkuOrderByAccessTimeDesc(PRODUCT_SKU, PageRequest.of(0, 1));
  }

  @Test
  public void deltaReindexHistoryCollectionTest() throws Exception {
    DateFormat dateFormat = new SimpleDateFormat(Constants.DATE_PATTERN);
    String strDate = dateFormat.format(new Date());

    when(systemParameterService
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE)).thenReturn(
        new ProductSystemParameter(SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE, "1",
            SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE, false));
    when(updatedProductHistoryRepository
        .findByAccessTimeBetweenOrderByAccessTime(Mockito.any(Date.class),
            Mockito.any(Date.class), Mockito.eq(PageRequest.of(0, 1))))
        .thenReturn(new PageImpl<>(Arrays.asList(updatedProductHistory), PageRequest.of(0, 1), 2));
    when(updatedProductHistoryRepository
        .findByAccessTimeBetweenOrderByAccessTime(Mockito.any(Date.class),
            Mockito.any(Date.class), Mockito.eq(PageRequest.of(1, 1))))
        .thenReturn(new PageImpl<>(Arrays.asList(updatedProductHistory), PageRequest.of(0, 1), 2));
    doNothing().when(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.capture());

    solrIndexingService.deltaReindexHistoryCollection(STORE_ID, strDate, strDate);

    verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE);
    verify(updatedProductHistoryRepository)
        .findByAccessTimeBetweenOrderByAccessTime(Mockito.any(Date.class),
            Mockito.any(Date.class), Mockito.eq(PageRequest.of(0, 1)));
    verify(updatedProductHistoryRepository)
        .findByAccessTimeBetweenOrderByAccessTime(Mockito.any(Date.class),
            Mockito.any(Date.class), Mockito.eq(PageRequest.of(1, 1)));
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(1));

    Assertions.assertEquals(ID,
        solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.ID));
    Assertions.assertEquals(PRODUCT_SKU,
        solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.PRODUCT_SKU));
    Assertions.assertEquals(GDN_SKU,
        solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.GDN_SKU));
    Assertions.assertEquals(GDN_NAME,
        solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.GDN_NAME));
    Assertions.assertEquals(ACTIVITY,
        solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.ACTIVITY));
    Assertions.assertNotNull(
        solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.ACCESS_TIME));
  }

  @Test
  public void deltaReindexHistoryCollectionEmptyEndDateTest() throws Exception {
    DateFormat dateFormat = new SimpleDateFormat(Constants.DATE_PATTERN);
    String strDate = dateFormat.format(new Date());

    when(systemParameterService
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE)).thenReturn(
        new ProductSystemParameter(SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE, "1",
            SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE, false));
    when(updatedProductHistoryRepository
        .findByAccessTimeBetweenOrderByAccessTime(Mockito.any(Date.class),
            Mockito.any(Date.class), Mockito.eq(PageRequest.of(0, 1))))
        .thenReturn(new PageImpl<>(Arrays.asList(updatedProductHistory), PageRequest.of(0, 1), 2));
    when(updatedProductHistoryRepository
        .findByAccessTimeBetweenOrderByAccessTime(Mockito.any(Date.class),
            Mockito.any(Date.class), Mockito.eq(PageRequest.of(1, 1))))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 1), 2));
    doNothing().when(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.capture());

    solrIndexingService.deltaReindexHistoryCollection(STORE_ID, strDate, "");

    verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE);
    verify(updatedProductHistoryRepository)
        .findByAccessTimeBetweenOrderByAccessTime(Mockito.any(Date.class),
            Mockito.any(Date.class), Mockito.eq(PageRequest.of(0, 1)));
    verify(updatedProductHistoryRepository)
        .findByAccessTimeBetweenOrderByAccessTime(Mockito.any(Date.class),
            Mockito.any(Date.class), Mockito.eq(PageRequest.of(1, 1)));
    verify(solrHistoryCollectionRepository).addDocument(solrInputDocumentArgumentCaptor.getAllValues().get(0));

    Assertions.assertEquals(ID,
        solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.ID));
    Assertions.assertEquals(PRODUCT_SKU,
        solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.PRODUCT_SKU));
    Assertions.assertEquals(GDN_SKU,
        solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.GDN_SKU));
    Assertions.assertEquals(GDN_NAME,
        solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.GDN_NAME));
    Assertions.assertEquals(ACTIVITY,
        solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.ACTIVITY));
    Assertions.assertNotNull(
        solrInputDocumentArgumentCaptor.getAllValues().get(0).get(0).getFieldValue(SolrFieldNames.ACCESS_TIME));
  }

  @Test
  public void deltaReindexHistoryCollectionExceptionTest() throws Exception {
    DateFormat dateFormat = new SimpleDateFormat(Constants.DATE_PATTERN);
    String strDate = dateFormat.format(new Date());

    when(systemParameterService
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE))
        .thenThrow(ApplicationRuntimeException.class);

    solrIndexingService.deltaReindexHistoryCollection(STORE_ID, strDate, "");

    verify(systemParameterService)
        .findByStoreIdAndVariable(STORE_ID, SystemParameterConstants.HISTORY_REINDEXING_PAGE_SIZE);
  }

  @Test
  public void deltaReindexPrdProductCollectionTest() throws ParseException {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode("product-code");
    productCollection.setProductName("product-name");
    productCollection.setUpdatedDate(new Date());
    List<ProductCollection> productCollectionList = new ArrayList<>();
    productCollectionList.add(productCollection);
    when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
        SystemParameterConstants.LAST_DELTA_REINDEX_TIME_FOR_PRD_PRODUCT_COLLECTION)).thenReturn(
        new ProductSystemParameter(SystemParameterConstants.LAST_DELTA_REINDEX_TIME_FOR_PRD_PRODUCT_COLLECTION, "1",
            SystemParameterConstants.LAST_DELTA_REINDEX_TIME_FOR_PRD_PRODUCT_COLLECTION, false));
    when(
        productCollectionRepository.findByMarkForDeleteFalseAndViewableTrueAndActivatedTrueAndUpdatedDateBetweenOrderByUpdatedDateAsc(
            Mockito.any(Date.class), Mockito.any(Date.class))).thenReturn(productCollectionList);
    Mockito.doNothing().when(productService).updateSolrProductCollection(productCollection);
    Mockito.doNothing().when(systemParameterService).update(Mockito.any());
    solrIndexingService.deltaReindexPrdProductCollection(STORE_ID, "26/12/2012", "26/12/2012");
    Mockito.verify(systemParameterService).findByStoreIdAndVariable(Mockito.any(), Mockito.any());
    Mockito.verify(productCollectionRepository)
        .findByMarkForDeleteFalseAndViewableTrueAndActivatedTrueAndUpdatedDateBetweenOrderByUpdatedDateAsc(
            Mockito.any(), Mockito.any());
    Mockito.verify(productService).updateSolrProductCollection(Mockito.any());
    Mockito.verify(systemParameterService).update(Mockito.any());
    }

  @Test
  public void deltaReindexPrdProductCollectionIndexTillAndIndexFromIsEmptyTest() throws ParseException {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode("product-code");
    productCollection.setProductName("product-name");
    productCollection.setUpdatedDate(new Date());
    List<ProductCollection> productCollectionList = new ArrayList<>();
    productCollectionList.add(productCollection);
    when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
        SystemParameterConstants.LAST_DELTA_REINDEX_TIME_FOR_PRD_PRODUCT_COLLECTION)).thenReturn(
        new ProductSystemParameter(SystemParameterConstants.LAST_DELTA_REINDEX_TIME_FOR_PRD_PRODUCT_COLLECTION, "1",
            SystemParameterConstants.LAST_DELTA_REINDEX_TIME_FOR_PRD_PRODUCT_COLLECTION, false));
    when(
        productCollectionRepository.findByMarkForDeleteFalseAndViewableTrueAndActivatedTrueAndUpdatedDateBetweenOrderByUpdatedDateAsc(
            Mockito.any(Date.class), Mockito.any(Date.class))).thenReturn(productCollectionList);
    Mockito.doNothing().when(productService).updateSolrProductCollection(productCollection);
    Mockito.doNothing().when(systemParameterService).update(Mockito.any());
    solrIndexingService.deltaReindexPrdProductCollection(STORE_ID, "", "");
    Mockito.verify(systemParameterService).findByStoreIdAndVariable(Mockito.any(), Mockito.any());
    Mockito.verify(productCollectionRepository)
        .findByMarkForDeleteFalseAndViewableTrueAndActivatedTrueAndUpdatedDateBetweenOrderByUpdatedDateAsc(
            Mockito.any(), Mockito.any());
    Mockito.verify(productService).updateSolrProductCollection(Mockito.any());
    Mockito.verify(systemParameterService).update(Mockito.any());
  }

  @Test
  public void deltaReindexPrdProductCollectionExceptionTest() {
    when(systemParameterService.findByStoreIdAndVariable(STORE_ID,
        SystemParameterConstants.LAST_DELTA_REINDEX_TIME_FOR_PRD_PRODUCT_COLLECTION)).thenReturn(
        new ProductSystemParameter(SystemParameterConstants.LAST_DELTA_REINDEX_TIME_FOR_PRD_PRODUCT_COLLECTION, "1",
            SystemParameterConstants.LAST_DELTA_REINDEX_TIME_FOR_PRD_PRODUCT_COLLECTION, false));
    when(
        productCollectionRepository.findByMarkForDeleteFalseAndViewableTrueAndActivatedTrueAndUpdatedDateBetweenOrderByUpdatedDateAsc(
            Mockito.any(Date.class), Mockito.any(Date.class))).thenThrow(ApplicationRuntimeException.class);
    Mockito.doNothing().when(systemParameterService).update(Mockito.any());
    solrIndexingService.deltaReindexPrdProductCollection(STORE_ID, "", "");
    Mockito.verify(productCollectionRepository)
        .findByMarkForDeleteFalseAndViewableTrueAndActivatedTrueAndUpdatedDateBetweenOrderByUpdatedDateAsc(
            Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterService).findByStoreIdAndVariable(Mockito.any(), Mockito.any());
    Mockito.verify(systemParameterService).update(Mockito.any());
  }

}