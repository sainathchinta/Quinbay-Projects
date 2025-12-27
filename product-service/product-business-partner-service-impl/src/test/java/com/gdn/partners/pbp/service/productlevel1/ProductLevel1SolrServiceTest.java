package com.gdn.partners.pbp.service.productlevel1;

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
import org.slf4j.MDC;

import com.gdn.mta.domain.event.modal.SolrProductCollectionUpdateEvent;
import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.SolrActiveProductCollectionRepository;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;

public class ProductLevel1SolrServiceTest {

  private static final String STORE_ID = "10001";

  private static final String PRODUCT_COLLECTION_ID = "productCollectionId";

  @Mock
  private ProductCollectionRepository productCollectionRepository;

  @Mock
  private SolrActiveProductCollectionRepository solrActiveProductCollectionRepository;

  @InjectMocks
  private ProductLevel1SolrServiceBean productLevel1SolrServiceBean;

  @Mock
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @Captor
  private ArgumentCaptor<String> productCollectionIdCaptor;

  @Captor
  private ArgumentCaptor<ProductCollection> productCollectionArgumentCaptor;

  @Mock
  private KafkaPublisher kafkaProducer;

  private ProductCollection generateProductCollection() throws Exception {
    ProductCollection productCollection = new ProductCollection();
    return productCollection;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    ProductCollection productCollection = this.generateProductCollection();
    Mockito.when(
        this.productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.any(),
            Mockito.any())).thenReturn(productCollection);
    Mockito.doNothing().when(this.solrActiveProductCollectionRepository)
        .addSolrProductCollectionDocument(Mockito.any());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productCollectionRepository);
    Mockito.verifyNoMoreInteractions(this.solrActiveProductCollectionRepository);
    Mockito.verifyNoMoreInteractions(this.solrReviewProductCollectionService);
    Mockito.verifyNoMoreInteractions(this.kafkaProducer);
  }

  @Test
  public void updateTest() throws Exception {
    this.productLevel1SolrServiceBean.update(null);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.SOLR_PRODUCT_COLLECTION_UPDATE), Mockito.any(), Mockito.any());
  }

  @Test
  public void updateReviewPendingTrueTest() throws Exception {
    ProductCollection productCollection = generateProductCollection();
    productCollection.setReviewPending(true);
    Mockito.when(productCollectionRepository
        .findByStoreIdAndProductCodeAndMarkForDeleteFalse(Mockito.eq(STORE_ID), Mockito.any()))
        .thenReturn(productCollection);
    this.productLevel1SolrServiceBean.update(null);
    Mockito.verify(this.productCollectionRepository).findByStoreIdAndProductCodeAndMarkForDeleteFalse(
        Mockito.eq(STORE_ID), Mockito.any());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.SOLR_PRODUCT_COLLECTION_UPDATE), Mockito.any(), Mockito.any());
    Mockito.verify(solrReviewProductCollectionService).addProductToReviewProductCollection(productCollection);
  }

  @Test
  public void deleteByProductCollectionIdTest() {
    this.productLevel1SolrServiceBean.deleteByProductCollectionId(PRODUCT_COLLECTION_ID);
    SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent =
        CommonUtils.getSolrProductCollectionUpdateEvent(PRODUCT_COLLECTION_ID);
    Mockito.verify(kafkaProducer)
        .send(DomainEventName.SOLR_PRODUCT_COLLECTION_UPDATE, PRODUCT_COLLECTION_ID, solrProductCollectionUpdateEvent);
  }

}
