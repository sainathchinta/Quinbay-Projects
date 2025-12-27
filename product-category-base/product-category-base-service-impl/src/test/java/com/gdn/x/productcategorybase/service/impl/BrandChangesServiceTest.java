package com.gdn.x.productcategorybase.service.impl;

import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.IOException;

import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.apache.solr.common.SolrException;
import org.apache.solr.common.SolrInputDocument;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Qualifier;

import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.repository.brand.BrandRepository;

public class BrandChangesServiceTest {

  @InjectMocks
  private BrandChangeServiceBean brandService;

  @Mock
  private BrandRepository brandRepository;

  @Mock
  @Qualifier("brandCollectionClient")
  private CloudSolrClient solrClient;

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_BRAND_CODE = "BRD-00000";
  private static final String DEFAULT_BRAND_NAME = "blibli";
  private static final String ID = "id";
  private static final String BRAND_CODE = "brand_code";
  private static final String BRAND_NAME = "brand_value";
  private static final String UPDATED_DATE = "updated_date";
  private static final String DEFAULT_BRAND_SEQUENCE = "brand_sequence";


  private UpdateResponse generateUpdateResponse() {
    return new UpdateResponse();
  }

  private SolrInputDocument generateSolrInputDocument() {
    SolrInputDocument solrInputDocument = new SolrInputDocument();
    solrInputDocument.addField(ID, null);
    solrInputDocument.addField(BRAND_CODE, DEFAULT_BRAND_CODE);
    solrInputDocument.addField(BRAND_NAME, null);
    solrInputDocument.addField(DEFAULT_BRAND_SEQUENCE, null);
    solrInputDocument.addField(UPDATED_DATE, null);
    return solrInputDocument;
  }

  private Brand generateBrand() throws Exception {
    Brand brand = new Brand();
    brand.setBrandCode(DEFAULT_BRAND_CODE);
    brand.setBrandName(DEFAULT_BRAND_NAME);
    brand.setStoreId(DEFAULT_STORE_ID);
    brand.setId(BRAND_CODE);
    return brand;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MDC.put("storeId", DEFAULT_STORE_ID);
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void postTest() throws Exception {
    verifyNoMoreInteractions(this.brandRepository);
    verifyNoMoreInteractions(this.solrClient);
    MDC.clear();
  }

  @Test
  public void createSolrDocumentForBrandCollectionTest() throws Exception {
    Mockito.when(this.solrClient.add(generateSolrInputDocument())).thenReturn(generateUpdateResponse());
    Mockito.when(this.solrClient.commit()).thenReturn(new UpdateResponse());
    this.brandService.createSolrDocumentForBrandCollection(generateBrand());
    Mockito.verify(this.solrClient).add(Mockito.any(SolrInputDocument.class));
    Mockito.verify(this.solrClient).commit();
  }

  @Test
  public void createSolrDocumentForBrandCollectionWithSolrExceptionTest() throws Exception {
    Mockito.when(this.solrClient.add(generateSolrInputDocument())).thenReturn(generateUpdateResponse());
    Mockito.when(this.solrClient.commit()).thenThrow(SolrServerException.class);
    this.brandService.createSolrDocumentForBrandCollection(generateBrand());
    Mockito.verify(this.solrClient).add(Mockito.any(SolrInputDocument.class));
    Mockito.verify(this.solrClient).commit();
  }

  @Test
  public void createSolrDocumentForBrandCollectionWithUnknownExceptionTest() throws Exception {
    Mockito.when(this.solrClient.add(generateSolrInputDocument())).thenReturn(generateUpdateResponse());
    Mockito.when(this.solrClient.commit()).thenThrow(RuntimeException.class);
    this.brandService.createSolrDocumentForBrandCollection(generateBrand());
    Mockito.verify(this.solrClient).add(Mockito.any(SolrInputDocument.class));
    Mockito.verify(this.solrClient).commit();
  }

  @Test
  public void deleteSolrDocumentForBrandCollectionWithSolrExceptionTest() throws Exception {
    Mockito.when(this.brandRepository
        .findFirstByStoreIdAndBrandCodeAndMarkForDeleteTrue(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(generateBrand());
    Mockito.when(this.solrClient.deleteById(Mockito.anyString())).thenReturn(generateUpdateResponse());
    Mockito.when(this.solrClient.commit()).thenThrow(SolrServerException.class);
    this.brandService.deleteSolrDocumentFromBrandCollection(DEFAULT_BRAND_CODE, DEFAULT_STORE_ID);
    Mockito.verify(this.brandRepository)
        .findFirstByStoreIdAndBrandCodeAndMarkForDeleteTrue(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Mockito.verify(this.solrClient).deleteById(BRAND_CODE);
    Mockito.verify(this.solrClient).commit();
  }

  @Test
  public void deleteSolrDocumentForBrandCollectionWithUnkonwnExceptionTest() throws Exception {
    Mockito.when(this.brandRepository
        .findFirstByStoreIdAndBrandCodeAndMarkForDeleteTrue(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(generateBrand());
    Mockito.when(this.solrClient.deleteById(Mockito.anyString())).thenReturn(generateUpdateResponse());
    Mockito.when(this.solrClient.commit()).thenThrow(RuntimeException.class);
    this.brandService.deleteSolrDocumentFromBrandCollection(DEFAULT_BRAND_CODE, DEFAULT_STORE_ID);
    Mockito.verify(this.brandRepository)
        .findFirstByStoreIdAndBrandCodeAndMarkForDeleteTrue(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Mockito.verify(this.solrClient).deleteById(BRAND_CODE);
    Mockito.verify(this.solrClient).commit();
  }

  @Test
  public void deleteSolrDocumentForBrandCollectionTest() throws Exception {
    Mockito.when(this.brandRepository
        .findFirstByStoreIdAndBrandCodeAndMarkForDeleteTrue(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(generateBrand());
    Mockito.when(this.solrClient.deleteById(Mockito.anyString())).thenReturn(generateUpdateResponse());
    Mockito.when(this.solrClient.commit()).thenReturn(generateUpdateResponse());
    this.brandService.deleteSolrDocumentFromBrandCollection(DEFAULT_BRAND_CODE, DEFAULT_STORE_ID);
    Mockito.verify(this.brandRepository)
        .findFirstByStoreIdAndBrandCodeAndMarkForDeleteTrue(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    Mockito.verify(this.solrClient).deleteById(BRAND_CODE);
    Mockito.verify(this.solrClient).commit();
  }

  @Test
  public void deleteSolrDocumentForBrandCollectionWithArthmaticExceptionTest() throws Exception {
    Mockito.when(this.brandRepository
        .findFirstByStoreIdAndBrandCodeAndMarkForDeleteTrue(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(null);
    this.brandService.deleteSolrDocumentFromBrandCollection(DEFAULT_BRAND_CODE, DEFAULT_STORE_ID);
    Mockito.verify(this.brandRepository)
        .findFirstByStoreIdAndBrandCodeAndMarkForDeleteTrue(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
  }

  @Test
  public void updateBrandTest() throws SolrServerException, IOException {
    Mockito.when(this.solrClient.add(Mockito.any(SolrInputDocument.class))).thenReturn(generateUpdateResponse());
    brandService.updateProtectedBrand(DEFAULT_STORE_ID, true);
    Mockito.verify(solrClient).add(Mockito.any(SolrInputDocument.class));
  }

  @Test
  public void updateBrandSolrServerExceptionTest() throws SolrServerException, IOException {
    Mockito.when(this.solrClient.add(Mockito.any(SolrInputDocument.class))).thenThrow(SolrServerException.class);
    brandService.updateProtectedBrand(DEFAULT_STORE_ID, true);
    Mockito.verify(solrClient).add(Mockito.any(SolrInputDocument.class));
  }

  @Test
  public void updateBrandIOExceptionTest() throws SolrServerException, IOException {
    Mockito.when(this.solrClient.add(Mockito.any(SolrInputDocument.class))).thenThrow(IOException.class);
    brandService.updateProtectedBrand(DEFAULT_STORE_ID, true);
    Mockito.verify(solrClient).add(Mockito.any(SolrInputDocument.class));
  }

  @Test
  public void updateBrandSolrExceptionTest() throws SolrServerException, IOException {
    Mockito.when(this.solrClient.add(Mockito.any(SolrInputDocument.class))).thenThrow(SolrException.class);
    brandService.updateProtectedBrand(DEFAULT_STORE_ID, true);
    Mockito.verify(solrClient).add(Mockito.any(SolrInputDocument.class));
  }
}
