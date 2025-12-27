package com.gdn.mta.product.service.domainevent;

import java.util.Arrays;
import java.util.UUID;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gdn.mta.domain.event.config.ProductApprovalDetailStatus;
import com.gdn.mta.domain.event.modal.CreateProductSyncEvent;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.enums.PrioritySeller;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductVendorApprovedEventModel;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ProductItemSyncEvent;

public class ProductApprovalStatusPublisherTest {
  private static final String DEFAULT_PRODUCT_CODE = "MTA-0000001";
  private static final String STORE_ID = "10001";
  private static final String USERNAME = "username";
  private static final String ITEM_SKU = "itemSku";
  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String PICKUP_POINT_CODE = "pickupPoinrcode";

  @InjectMocks
  private ProductStatusPublisherServiceBean productStatusPublisherService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Test
  public void updateProductApprovalDetailStatusTest() {
    productStatusPublisherService.updateProductApprovalDetailStatus(DEFAULT_PRODUCT_CODE,
        ProductApprovalDetailStatus.PBP_UpdateProductContent_Response_Received_PCB);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(com.gdn.mta.domain.event.config.DomainEventName.PRODUCT_STATUS_UPDATE_TRACKER),
            Mockito.any(), Mockito.any());
  }

  @Test
  public void publishSolrProductCollectionUpdateEventTest() {
    productStatusPublisherService.publishSolrProductCollectionUpdateEvent(generateSolrCollectionDTO());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(com.gdn.mta.domain.event.config.DomainEventName.SOLR_PRODUCT_COLLECTION_UPDATE),
            Mockito.any(), Mockito.any());
  }

  private SolrProductCollectionDTO generateSolrCollectionDTO() {
    SolrProductCollectionDTO solrProductCollectionDTO = new SolrProductCollectionDTO();
    solrProductCollectionDTO.setId(UUID.randomUUID().toString());
    solrProductCollectionDTO.setProductCode(DEFAULT_PRODUCT_CODE);
    return solrProductCollectionDTO;
  }

  @Test
  public void publishPDTDomainEventModelToPBPTest() {
    productStatusPublisherService.publishPDTDomainEventModelToPBP(generatePDTDomainEventModel());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_QC_APPROVED_TASK_EVENT_NAME), Mockito.any(),
            Mockito.any());
  }

  @Test
  public void publishVendorApprovedEventToPBPTest() {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode(DEFAULT_PRODUCT_CODE);
    productCollection.setPostLive(true);
    productCollection.setReviewPending(true);
    PDTProductVendorApprovedEventModel response =
        productStatusPublisherService.publishVendorApprovedEventToPBP(productCollection);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_VENDOR_APPROVED_TASK_EVENT_NAME), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, response.getProductCode());
    Assertions.assertTrue(response.isPostLive());
    Assertions.assertTrue(response.isReviewPending());
  }

  @Test
  public void publishVendorApprovedEventToPBPPriority1Test() {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode(DEFAULT_PRODUCT_CODE);
    productCollection.setPostLive(true);
    productCollection.setReviewPending(true);
    productCollection.setPrioritySeller(PrioritySeller.PRIORITY_1.getPrioritySeller());
    PDTProductVendorApprovedEventModel response =
        productStatusPublisherService.publishVendorApprovedEventToPBP(productCollection);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_VENDOR_APPROVED_TASK_PRIORITY_1_EVENT_NAME), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, response.getProductCode());
    Assertions.assertTrue(response.isPostLive());
    Assertions.assertTrue(response.isReviewPending());
  }

  @Test
  public void publishVendorApprovedEventToPBPPriority2Test() {
    ProductCollection productCollection = new ProductCollection();
    productCollection.setProductCode(DEFAULT_PRODUCT_CODE);
    productCollection.setPostLive(true);
    productCollection.setReviewPending(true);
    productCollection.setPrioritySeller(PrioritySeller.PRIORITY_2.getPrioritySeller());
    PDTProductVendorApprovedEventModel response =
        productStatusPublisherService.publishVendorApprovedEventToPBP(productCollection);
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(DomainEventName.PRODUCT_VENDOR_APPROVED_TASK_PRIORITY_2_EVENT_NAME), Mockito.any(),
            Mockito.any());
    Assertions.assertEquals(DEFAULT_PRODUCT_CODE, response.getProductCode());
    Assertions.assertTrue(response.isPostLive());
    Assertions.assertTrue(response.isReviewPending());
  }

  @Test
  public void publishCreateProductSyncEventTest() {
    CreateProductSyncEvent createProductSyncEvent = productStatusPublisherService
        .publishCreateProductSyncEvent(STORE_ID, USERNAME, Arrays.asList(ITEM_SKU), BUSINESS_PARTNER_CODE,
            PICKUP_POINT_CODE);
    Assertions.assertEquals(Arrays.asList(ITEM_SKU), createProductSyncEvent.getSourceItemSkus());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(com.gdn.mta.domain.event.config.DomainEventName.CREATE_PRODUCT_SYNC_EVENT),
            Mockito.any(), Mockito.any());
  }

  @Test
  public void publishProductSyncSuccessEventTest() {
    ProductItemSyncEvent productItemSyncEvent =
        productStatusPublisherService.publishProductSyncSuccessEvent(STORE_ID, ITEM_SKU, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(ITEM_SKU, productItemSyncEvent.getItemSku());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(ProductDomainEventName.FBB_ITEM_SYNC_EVENT), Mockito.any(), Mockito.any());
  }

  private PDTProductDomainEventModel generatePDTDomainEventModel() {
    PDTProductDomainEventModel pdtProductDomainEventModel = new PDTProductDomainEventModel();
    pdtProductDomainEventModel.setProductCode(DEFAULT_PRODUCT_CODE);
    return pdtProductDomainEventModel;
  }

  @Test
  public void productAutoApprovalCheckTest() {
    productStatusPublisherService.productAutoApprovalCheck(new AutoApprovalTypeRequest());
    Mockito.verify(kafkaProducer)
        .send(com.gdn.mta.domain.event.config.DomainEventName.PRODUCT_AUTO_APPROVAL_CHECK, null,
            new AutoApprovalTypeRequest());
  }

  @Test
  public void publishVendorCombinedEventTest() {
    AutoApprovalTypeRequest autoApprovalTypeRequest = new AutoApprovalTypeRequest();
    autoApprovalTypeRequest.setProductCode(DEFAULT_PRODUCT_CODE);
    Mockito.when(kafkaTopicProperties.getVendorCombinedEventNoPriority())
        .thenReturn(DomainEventName.REVISED_PRODUCT_VENDOR_APPROVED_TASK_EVENT_NAME);
    productStatusPublisherService.publishVendorCombinedEvent(autoApprovalTypeRequest);
    Mockito.verify(kafkaProducer).send(Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }
}
