package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.dao.api.SolrVendorProductCollectionRepository;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductCombinedUpdateToSolrEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductSolrAddDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductUpdateProductToSolrEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.enums.SellerBadgeConstants;
import com.gdn.x.mta.distributiontask.service.api.SolrVendorCollectionService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.Arrays;

public class PDTProductCombinedUpdateToSolrEventListenerTest {
  private static final String PRODUCT_CODE = "product-code";
  private static final String PDT_SOLR_UPDATE_EVENT =
      "com.gdn.x.product.distributiontask.product.combined.update.to.solr";

  @InjectMocks
  private PDTProductCombinedUpdateToSolrEventListener pdtProductCombinedUpdateToSolrEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private SolrVendorCollectionService solrVendorCollectionService;

  @Mock
  private SolrVendorProductCollectionRepository solrVendorProductCollectionRepository;

  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  private ObjectMapper mapper = new ObjectMapper();

  @BeforeEach
  public void init() {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, solrVendorCollectionService, solrVendorProductCollectionRepository);
  }

  @Test
   void onDomainEventConsumedAddToSolrTest() throws Exception {
    PDTProductCombinedUpdateToSolrEventModel pdtProductCombinedUpdateToSolrEventModel =
        PDTProductCombinedUpdateToSolrEventModel.builder()
            .pdtProductSolrAddDomainEventModel(new PDTProductSolrAddDomainEventModel()).build();
    String json = mapper.writeValueAsString(pdtProductCombinedUpdateToSolrEventModel);
    Mockito.when(kafkaTopicPropertiesConsumer.getPdtProductCombinedUpdateToSolrEvent())
        .thenReturn(PDT_SOLR_UPDATE_EVENT);

    Mockito.when(objectMapper.readValue(json, PDTProductCombinedUpdateToSolrEventModel.class))
        .thenReturn(pdtProductCombinedUpdateToSolrEventModel);
    Mockito.doNothing().when(solrVendorProductCollectionRepository)
        .addDocumentToSolrWithCategoryHierarchy(Mockito.anyList());

    pdtProductCombinedUpdateToSolrEventListener.onDomainEventConsumed(json);

    Mockito.verify(objectMapper).readValue(json, PDTProductCombinedUpdateToSolrEventModel.class);
    Mockito.verify(solrVendorProductCollectionRepository)
        .addDocumentToSolrWithCategoryHierarchy(Mockito.anyList());
    Mockito.verify(kafkaTopicPropertiesConsumer).getPdtProductCombinedUpdateToSolrEvent();
  }

  @Test
   void onDomainEventConsumedAddToSolrSellerTypeTest() throws Exception {
    PDTProductSolrAddDomainEventModel pdtProductSolrAddDomainEventModel = new PDTProductSolrAddDomainEventModel();
    pdtProductSolrAddDomainEventModel.setSellerType(1);
    pdtProductSolrAddDomainEventModel.setSellerBadge(SellerBadgeConstants.DIAMOND_MERCHANT.name());
    PDTProductCombinedUpdateToSolrEventModel pdtProductCombinedUpdateToSolrEventModel =
        PDTProductCombinedUpdateToSolrEventModel.builder()
            .pdtProductSolrAddDomainEventModel(pdtProductSolrAddDomainEventModel).build();
    String json = mapper.writeValueAsString(pdtProductCombinedUpdateToSolrEventModel);

    Mockito.when(objectMapper.readValue(json, PDTProductCombinedUpdateToSolrEventModel.class))
        .thenReturn(pdtProductCombinedUpdateToSolrEventModel);
    Mockito.doNothing().when(solrVendorProductCollectionRepository)
        .addDocumentToSolrWithCategoryHierarchy(Mockito.anyList());

    pdtProductCombinedUpdateToSolrEventListener.onDomainEventConsumed(json);

    Mockito.verify(objectMapper).readValue(json, PDTProductCombinedUpdateToSolrEventModel.class);
    Mockito.verify(solrVendorProductCollectionRepository)
        .addDocumentToSolrWithCategoryHierarchy(Mockito.anyList());
  }

  @Test
   void onDomainEventConsumedUpdateToSolrTest() throws Exception {
    PDTProductUpdateProductToSolrEventModel pdtProductUpdateProductToSolrEventModel =
        new PDTProductUpdateProductToSolrEventModel();
    PDTProductCombinedUpdateToSolrEventModel pdtProductCombinedUpdateToSolrEventModel =
        PDTProductCombinedUpdateToSolrEventModel.builder()
            .pdtProductUpdateProductToSolrEventModel(pdtProductUpdateProductToSolrEventModel).build();
    String json = mapper.writeValueAsString(pdtProductCombinedUpdateToSolrEventModel);

    Mockito.when(objectMapper.readValue(json, PDTProductCombinedUpdateToSolrEventModel.class))
        .thenReturn(pdtProductCombinedUpdateToSolrEventModel);
    Mockito.doNothing().when(solrVendorCollectionService)
        .updateSolrOnApprovalOrSave(pdtProductUpdateProductToSolrEventModel);

    pdtProductCombinedUpdateToSolrEventListener.onDomainEventConsumed(json);

    Mockito.verify(objectMapper).readValue(json, PDTProductCombinedUpdateToSolrEventModel.class);
    Mockito.verify(solrVendorCollectionService).updateSolrOnApprovalOrSave(pdtProductUpdateProductToSolrEventModel);
  }

  @Test
   void onDomainEventConsumedDeleteToSolrTest() throws Exception {
    PDTProductCombinedUpdateToSolrEventModel pdtProductCombinedUpdateToSolrEventModel =
        PDTProductCombinedUpdateToSolrEventModel.builder()
            .productCode(PRODUCT_CODE).build();
    String json = mapper.writeValueAsString(pdtProductCombinedUpdateToSolrEventModel);

    Mockito.when(objectMapper.readValue(json, PDTProductCombinedUpdateToSolrEventModel.class))
        .thenReturn(pdtProductCombinedUpdateToSolrEventModel);
    Mockito.doNothing().when(solrVendorProductCollectionRepository)
        .deleteDocumentFromSolr(Arrays.asList(PRODUCT_CODE), false);

    pdtProductCombinedUpdateToSolrEventListener.onDomainEventConsumed(json);

    Mockito.verify(objectMapper).readValue(json, PDTProductCombinedUpdateToSolrEventModel.class);
    Mockito.verify(solrVendorProductCollectionRepository).deleteDocumentFromSolr(Arrays.asList(PRODUCT_CODE), false);
  }

  @Test
   void onDomainEventConsumedNoOperationTest() throws Exception {
    PDTProductCombinedUpdateToSolrEventModel pdtProductCombinedUpdateToSolrEventModel =
        PDTProductCombinedUpdateToSolrEventModel.builder().build();
    String json = mapper.writeValueAsString(pdtProductCombinedUpdateToSolrEventModel);

    Mockito.when(objectMapper.readValue(json, PDTProductCombinedUpdateToSolrEventModel.class))
        .thenReturn(pdtProductCombinedUpdateToSolrEventModel);

    pdtProductCombinedUpdateToSolrEventListener.onDomainEventConsumed(json);

    Mockito.verify(objectMapper).readValue(json, PDTProductCombinedUpdateToSolrEventModel.class);
  }

  @Test
   void onDomainEventConsumedErrorTest() throws Exception {
    PDTProductCombinedUpdateToSolrEventModel pdtProductCombinedUpdateToSolrEventModel =
        PDTProductCombinedUpdateToSolrEventModel.builder().build();
    String json = mapper.writeValueAsString(pdtProductCombinedUpdateToSolrEventModel);

    Mockito.when(objectMapper.readValue(json, PDTProductCombinedUpdateToSolrEventModel.class))
        .thenThrow(ApplicationRuntimeException.class);

    pdtProductCombinedUpdateToSolrEventListener.onDomainEventConsumed(json);

    Mockito.verify(objectMapper).readValue(json, PDTProductCombinedUpdateToSolrEventModel.class);
  }
}
