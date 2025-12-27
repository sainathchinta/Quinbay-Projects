package com.gdn.partners.pbp.workflow.product;

import java.util.HashMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.service.solr.SolrReviewProductCollectionService;
import com.gdn.partners.pbp.publisher.Publisher;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1WipService;

public class ApproveDraftWorkflowWorkerTest {

  @Mock
  private ProductLevel1WipService productLevel1WipService;

  @Mock
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Mock
  private Publisher publisher;

  @InjectMocks
  private ApproveDraftWorkflowWorkerBean approveDraftWorkflowWorkerBean;

  @Mock
  private SolrReviewProductCollectionService solrReviewProductCollectionService;

  @SuppressWarnings("unchecked")
  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    Mockito.doReturn(new ProductCollection()).when(this.productLevel1WipService).approveDraft(Mockito.anyString(), Mockito.any());
    Mockito.doNothing().when(solrReviewProductCollectionService)
        .addProductToReviewProductCollection(Mockito.any(ProductCollection.class));
    Mockito.doNothing().when(this.publisher).publish(Mockito.anyMap());
    Mockito.doNothing().when(this.productLevel1HistoryService)
        .create(Mockito.anyString(), Mockito.anyString(), Mockito.anyString());
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.productLevel1WipService);
    Mockito.verifyNoMoreInteractions(this.publisher);
    Mockito.verifyNoMoreInteractions(this.productLevel1HistoryService);
    Mockito.verifyNoMoreInteractions(this.solrReviewProductCollectionService);
  }

  @SuppressWarnings("unchecked")
  @Test
  public void processTest() throws Exception {
    this.approveDraftWorkflowWorkerBean.process(new HashMap<String, Object>());
    Mockito.verify(this.productLevel1WipService).approveDraft(Mockito.any(), Mockito.any());
    Mockito.verify(this.publisher).publish(Mockito.anyMap());
    Mockito.verify(this.productLevel1HistoryService).create((String) Mockito.any(), Mockito.any(), Mockito.any());
    Mockito.verify(solrReviewProductCollectionService)
        .addProductToReviewProductCollection(Mockito.any(ProductCollection.class));
  }

}
