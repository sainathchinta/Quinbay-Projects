package com.gdn.x.product.outbound.impl;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;

import java.io.IOException;

import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CloudSolrClient;
import org.apache.solr.client.solrj.response.UpdateResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

public class ProductSolrOutboundImplTest {

  private static final String SOLR_URL_PATH = "http://localhost:8983/solr/";

  private static final String COLLECTION_NAME = "xproduct";

  @InjectMocks
  private ProductSolrOutboundImpl productSolrOutboundImpl;

  @Mock
  private CloudSolrClient cloudSolrClient;

  @BeforeEach
  public void init() {
    openMocks(this);
    ReflectionTestUtils.setField(productSolrOutboundImpl, "collectionName", COLLECTION_NAME);
  }

  @Test
  public void optimize() throws Exception {
    Mockito.when(this.cloudSolrClient.optimize(Mockito.anyString())).thenReturn(new UpdateResponse());
    this.productSolrOutboundImpl.optimize();
    verify(this.cloudSolrClient).optimize(Mockito.anyString());
  }

  @Test
  public void optimizeWithSolrServerException() throws Exception {
    Mockito.when(this.cloudSolrClient.optimize(Mockito.anyString())).thenThrow(SolrServerException.class);
    this.productSolrOutboundImpl.optimize();
    verify(this.cloudSolrClient).optimize(Mockito.anyString());
  }

  @Test
  public void optimizeIOException() throws Exception {
    Mockito.when(this.cloudSolrClient.optimize(Mockito.anyString())).thenThrow(IOException.class);
    this.productSolrOutboundImpl.optimize();
    verify(this.cloudSolrClient).optimize(Mockito.anyString());
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.cloudSolrClient);
  }

}
