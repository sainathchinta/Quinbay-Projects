package com.gdn.mta.product.service.solr;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.SolrProductCollectionUpdateEvent;
import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.SolrActiveProductCollectionRepository;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
public class SolrActiveProductCollectionServiceBeanTest {
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_PRODUCT_CODE = "product-code";
  private static final String DEFAULT_CATEGORY_CODE = "category_code";
  private List<String> brands = new ArrayList<>();
  private List<String> categoryCodes = new ArrayList<>();

  @Mock
  private SolrActiveProductCollectionRepository solrActiveProductCollectionRepository;

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  private KafkaPublisher kafkaProducer;

  @InjectMocks
  private SolrActiveProductCollectionServiceBean solrActiveProductCollectionServiceBean;

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void finalizeTest() {
    Mockito.verifyNoMoreInteractions(this.solrActiveProductCollectionRepository);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
  }

  @Test
  public void testDeleteFromSolrProductCollection() {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setId(DEFAULT_PRODUCT_CODE);
    Mockito.when(this.productCollectionRepository
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE))
        .thenReturn(productCollection);
    this.solrActiveProductCollectionServiceBean.deleteFromSolrProductCollection(DEFAULT_STORE_ID,
        DEFAULT_PRODUCT_CODE);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
    Mockito.verify(this.productCollectionRepository).save(productCollection);
    SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent =
        CommonUtils.getSolrProductCollectionUpdateEvent(DEFAULT_PRODUCT_CODE);
    Mockito.verify(kafkaProducer).send(DomainEventName.SOLR_PRODUCT_COLLECTION_UPDATE, DEFAULT_PRODUCT_CODE,
        solrProductCollectionUpdateEvent);
  }

  @Test
  public void testDeleteFromSolrProductCollection_returnNull() {
    Mockito.when(this.productCollectionRepository
    .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE)).thenReturn(null);
    this.solrActiveProductCollectionServiceBean.deleteFromSolrProductCollection(DEFAULT_STORE_ID,
        DEFAULT_PRODUCT_CODE);
    Mockito.verify(this.productCollectionRepository)
        .findByStoreIdAndProductCode(DEFAULT_STORE_ID, DEFAULT_PRODUCT_CODE);
  }

  @Test
  public void testSyncInactiveProductCollection() {
    this.solrActiveProductCollectionServiceBean.syncInactiveProductCollection(DEFAULT_STORE_ID);
    Mockito.verify(this.productCollectionRepository).findInactiveProducts(DEFAULT_STORE_ID);
  }

  @Test
  public void testSyncInactiveProductCollectionTest() {
    Mockito.when(this.productCollectionRepository.findInactiveProducts(DEFAULT_STORE_ID)).thenReturn(
        Collections.singletonList(DEFAULT_PRODUCT_CODE));
    this.solrActiveProductCollectionServiceBean.syncInactiveProductCollection(DEFAULT_STORE_ID);
    Mockito.verify(this.productCollectionRepository).findInactiveProducts(DEFAULT_STORE_ID);
    SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent =
        CommonUtils.getSolrProductCollectionUpdateEvent(DEFAULT_PRODUCT_CODE);
    Mockito.verify(kafkaProducer).send(DomainEventName.SOLR_PRODUCT_COLLECTION_UPDATE, DEFAULT_PRODUCT_CODE,
        solrProductCollectionUpdateEvent);
  }

  @Test
  public void addSolrProductCollectionDocumentTest() {
    SolrProductCollectionDTO solrProductCollectionDTO = generateSolrProductCollectionDTO();
    this.solrActiveProductCollectionServiceBean.addSolrProductCollectionDocument(new SolrProductCollectionUpdateEvent(solrProductCollectionDTO));
    Mockito.verify(this.solrActiveProductCollectionRepository).addSolrProductCollectionDocument(solrProductCollectionDTO);
  }

  @Test
  public void getAllActiveBrandsByCategoryCodes() {
    brands.add("brand");
    Mockito.when(this.solrActiveProductCollectionRepository.getAllActiveBrandsByCategoryCodes(Mockito.anyList()))
        .thenReturn(brands);
    List<String> brands = this.solrActiveProductCollectionRepository.getAllActiveBrandsByCategoryCodes(categoryCodes);
    Mockito.verify(this.solrActiveProductCollectionRepository).getAllActiveBrandsByCategoryCodes(Mockito.anyList());
    Assertions.assertNotNull(brands);
    Assertions.assertEquals(1, brands.size());
  }

  @Test
  public void deleteSolrProductCollectionByDocumentId(){
    solrActiveProductCollectionServiceBean.deleteSolrProductCollectionByDocumentId(DEFAULT_PRODUCT_CODE);
    Mockito.verify(solrActiveProductCollectionRepository).deleteSolrProductCollectionByDocumentId(Mockito.anyString());
  }

  public SolrProductCollectionDTO generateSolrProductCollectionDTO() {
    SolrProductCollectionDTO solrProductCollectionDTO = new SolrProductCollectionDTO();
    solrProductCollectionDTO.setId(UUID.randomUUID().toString());
    solrProductCollectionDTO.setCategoryCode(DEFAULT_CATEGORY_CODE);
    solrProductCollectionDTO.setProductCode(DEFAULT_PRODUCT_CODE);
    solrProductCollectionDTO.setReviewPending(true);
    return solrProductCollectionDTO;
  }

  @Test
  public void deleteSolrProductCollectionDocumentTest() {
    solrActiveProductCollectionServiceBean.deleteSolrProductCollectionDocument(DEFAULT_PRODUCT_CODE);
    SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent =
        CommonUtils.getSolrProductCollectionUpdateEvent(DEFAULT_PRODUCT_CODE);
    Mockito.verify(kafkaProducer).send(DomainEventName.SOLR_PRODUCT_COLLECTION_UPDATE, DEFAULT_PRODUCT_CODE,
        solrProductCollectionUpdateEvent);
  }

  @Test
  public void deleteSolrProductCollectionDocumentExceptionTest() {
    SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent =
        CommonUtils.getSolrProductCollectionUpdateEvent(DEFAULT_PRODUCT_CODE);
    Mockito.doThrow(ApplicationRuntimeException.class).when(kafkaProducer)
        .send(DomainEventName.SOLR_PRODUCT_COLLECTION_UPDATE, DEFAULT_PRODUCT_CODE, solrProductCollectionUpdateEvent);
    solrActiveProductCollectionServiceBean.deleteSolrProductCollectionDocument(DEFAULT_PRODUCT_CODE);
    Mockito.verify(kafkaProducer).send(DomainEventName.SOLR_PRODUCT_COLLECTION_UPDATE, DEFAULT_PRODUCT_CODE,
        solrProductCollectionUpdateEvent);
  }
}
