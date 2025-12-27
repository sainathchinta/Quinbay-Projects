package com.gdn.x.mta.distributiontask.inbound.impl;

import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.dao.api.SolrVendorProductCollectionRepository;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrDeleteDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductForcedRollbackSLAExceedDomainEventModel;

public class PDTProductSolrDeleteDocumentListenerTest {

  @Mock
  private SolrVendorProductCollectionRepository solrVendorProductCollectionRepository;

  @Mock
  private ObjectMapper objectMapper;

  private ObjectMapper mapper;

  @InjectMocks
  private PDTProductSolrDeleteDocumentListener pdtProductSolrDeleteDocumentListener;

  private PDTProductSolrDeleteDomainEventModel pdtProductSolrDeleteDomainEventModel;
  private static final String ID = "ID";
  List<String> productCodes = new ArrayList();

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    mapper = new ObjectMapper();
    productCodes.add(ID);
    pdtProductSolrDeleteDomainEventModel = PDTProductSolrDeleteDomainEventModel.builder().productCodes(productCodes).build();
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(solrVendorProductCollectionRepository);
  }

  @Test
   void onDomainEventConsumed() throws Exception {
    String message = mapper.writeValueAsString(pdtProductSolrDeleteDomainEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductSolrDeleteDomainEventModel.class)).thenReturn(pdtProductSolrDeleteDomainEventModel);
    pdtProductSolrDeleteDocumentListener.onDomainEventConsumed(message);
    Mockito.verify(solrVendorProductCollectionRepository).deleteDocumentFromSolr(productCodes, false);
    Mockito.verify(objectMapper).readValue(message, PDTProductSolrDeleteDomainEventModel.class);
  }

  @Test
   void onDomainEventConsumedEmptyIds() throws Exception {
    pdtProductSolrDeleteDomainEventModel.setProductCodes(new ArrayList<>());
    String message = mapper.writeValueAsString(pdtProductSolrDeleteDomainEventModel);
    Mockito.when(objectMapper.readValue(message, PDTProductSolrDeleteDomainEventModel.class)).thenReturn(pdtProductSolrDeleteDomainEventModel);
    pdtProductSolrDeleteDocumentListener
        .onDomainEventConsumed(message);
    Mockito.verify(objectMapper).readValue(message, PDTProductSolrDeleteDomainEventModel.class);

  }
}
