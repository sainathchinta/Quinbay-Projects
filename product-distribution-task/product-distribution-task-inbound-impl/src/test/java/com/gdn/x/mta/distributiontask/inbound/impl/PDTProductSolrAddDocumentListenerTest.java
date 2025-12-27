package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.dao.api.SolrVendorProductCollectionRepository;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrAddDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrBatchAddDomainEventModel;
import com.gdn.x.mta.distributiontask.model.solr.VendorProductSolr;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.List;

@ExtendWith(MockitoExtension.class)
public class PDTProductSolrAddDocumentListenerTest {

  private static final String PRODUCT_NAME = "productName";
  private static final String PRODUCT_CODE = "productCode";

  private PDTProductSolrAddDomainEventModel productSolrAddDomainEventModel;
  private PDTProductSolrBatchAddDomainEventModel productSolrBatchAddDomainEventModel;

  @Mock
  private SolrVendorProductCollectionRepository solrVendorProductCollectionRepository;

  @InjectMocks
  private PDTProductSolrAddDocumentListener pdtProductSolrAddDocumentListener;

  @Captor
  private ArgumentCaptor<List<VendorProductSolr>> listArgumentCaptor;

  @Mock
  private ObjectMapper objectMapper;

  private ObjectMapper mapper;

  @BeforeEach
  public void init() {
    productSolrAddDomainEventModel =
        PDTProductSolrAddDomainEventModel.builder().state(WorkflowState.IN_REVIEW.name())
            .productCode(PRODUCT_CODE).productName(PRODUCT_NAME).build();
    productSolrBatchAddDomainEventModel = PDTProductSolrBatchAddDomainEventModel.builder()
        .pdtProductSolrAddDomainEventModelList(
            Collections.singletonList(productSolrAddDomainEventModel))
        .build();
    mapper = new ObjectMapper();
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(solrVendorProductCollectionRepository);
  }

  @Test
   void onDomainEventConsumedTest() throws Exception {
    productSolrBatchAddDomainEventModel.getPdtProductSolrAddDomainEventModelList().get(0)
        .setPredictedBrand(PRODUCT_CODE);
    String message = mapper.writeValueAsString(productSolrBatchAddDomainEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductSolrBatchAddDomainEventModel.class)).thenReturn(productSolrBatchAddDomainEventModel);
    Mockito.doNothing().when(this.solrVendorProductCollectionRepository)
        .addDocumentToSolrWithCategoryHierarchy(Mockito.anyList());
    pdtProductSolrAddDocumentListener.onDomainEventConsumed(message);
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .addDocumentToSolrWithCategoryHierarchy(listArgumentCaptor.capture());
    Mockito.verify(objectMapper).readValue(message, PDTProductSolrBatchAddDomainEventModel.class);
    Assertions.assertEquals(PRODUCT_CODE, listArgumentCaptor.getValue().get(0).getPredictedBrand());
  }

  @Test
   void onDomainEventConsumed_emptyStateTest() throws Exception {
    String message = mapper.writeValueAsString(productSolrBatchAddDomainEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductSolrBatchAddDomainEventModel.class)).thenReturn(productSolrBatchAddDomainEventModel);
    productSolrBatchAddDomainEventModel.getPdtProductSolrAddDomainEventModelList().get(0).setState(
        StringUtils.EMPTY);
    Mockito.doNothing().when(this.solrVendorProductCollectionRepository)
        .addDocumentToSolrWithCategoryHierarchy(Mockito.anyList());
    pdtProductSolrAddDocumentListener.onDomainEventConsumed(message);
    Mockito.verify(this.solrVendorProductCollectionRepository)
        .addDocumentToSolrWithCategoryHierarchy(Mockito.anyList());
    Mockito.verify(objectMapper).readValue(message, PDTProductSolrBatchAddDomainEventModel.class);

  }

}
