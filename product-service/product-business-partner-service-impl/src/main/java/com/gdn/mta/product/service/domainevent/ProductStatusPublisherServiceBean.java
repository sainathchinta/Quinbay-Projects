package com.gdn.mta.product.service.domainevent;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.config.ProductApprovalDetailStatus;
import com.gdn.mta.domain.event.modal.AddProductToVendorCombinedEventModel;
import com.gdn.mta.domain.event.modal.AutoApprovalTypeRequestModel;
import com.gdn.mta.domain.event.modal.CreateProductSyncEvent;
import com.gdn.mta.domain.event.modal.ProductApprovalDetailStatusEvent;
import com.gdn.mta.domain.event.modal.SolrProductCollectionUpdateEvent;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.enums.PrioritySeller;
import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import com.gdn.partners.pbp.commons.constants.DomainKeyConstants;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductVendorApprovedEventModel;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ProductItemSyncEvent;

import java.util.List;

/**
 * @author akshay.bhatt
 */

@Service
public class ProductStatusPublisherServiceBean implements ProductStatusPublisherService {
  private static final Logger LOGGER = LoggerFactory.getLogger(ProductStatusPublisherServiceBean.class);

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Override
  public ProductApprovalDetailStatusEvent updateProductApprovalDetailStatus(String productCode,
      ProductApprovalDetailStatus status) {
    ProductApprovalDetailStatusEvent productApprovalDetailStatusEvent =
        new ProductApprovalDetailStatusEvent(productCode, status, System.currentTimeMillis());
    kafkaProducer.send(DomainEventName.PRODUCT_STATUS_UPDATE_TRACKER, productApprovalDetailStatusEvent.getProductCode(),
        productApprovalDetailStatusEvent);
    return productApprovalDetailStatusEvent;
  }

  @Override
  public SolrProductCollectionUpdateEvent publishSolrProductCollectionUpdateEvent(
      SolrProductCollectionDTO solrProductCollectionDTO) {
    LOGGER.info("publish product collection update Request {}, to PBP", solrProductCollectionDTO.getProductCode());
    SolrProductCollectionUpdateEvent solrProductCollectionUpdateEvent =
        new SolrProductCollectionUpdateEvent(solrProductCollectionDTO);
    kafkaProducer.send(DomainEventName.SOLR_PRODUCT_COLLECTION_UPDATE,
        solrProductCollectionUpdateEvent.getSolrProductCollectionDTO().getProductCode(),
        solrProductCollectionUpdateEvent);
    return solrProductCollectionUpdateEvent;
  }

  @Override
  public PDTProductDomainEventModel publishPDTDomainEventModelToPBP(
      PDTProductDomainEventModel pdtProductDomainEventModel) {
    LOGGER.info("Publishing event com.gdn.x.mta.distributiontask.product.qc.approved for productCode : {}",
        pdtProductDomainEventModel.getProductCode());
    kafkaProducer
        .send(com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName.PRODUCT_QC_APPROVED_TASK_EVENT_NAME,
            pdtProductDomainEventModel.getProductCode(), pdtProductDomainEventModel);
    return pdtProductDomainEventModel;
  }

  @Override
  public PDTProductVendorApprovedEventModel publishVendorApprovedEventToPBP(ProductCollection productCollection) {
    LOGGER.info("Publishing event com.gdn.x.mta.distributiontask.product.vendor.approved for productCode : {}",
        productCollection.getProductCode());
    PDTProductVendorApprovedEventModel pdtProductVendorApprovedEventModel =
        getPdtProductVendorApprovedEventModel(productCollection);
    String vendorApprovedTaskEventName =
        com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName.PRODUCT_VENDOR_APPROVED_TASK_EVENT_NAME;
    if (productCollection.getPrioritySeller() == PrioritySeller.PRIORITY_1.getPrioritySeller()) {
      vendorApprovedTaskEventName =
          com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName.PRODUCT_VENDOR_APPROVED_TASK_PRIORITY_1_EVENT_NAME;
    } else if (productCollection.getPrioritySeller() == PrioritySeller.PRIORITY_2.getPrioritySeller()) {
      vendorApprovedTaskEventName =
          com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName.PRODUCT_VENDOR_APPROVED_TASK_PRIORITY_2_EVENT_NAME;
    }
    kafkaProducer.send(vendorApprovedTaskEventName,
        pdtProductVendorApprovedEventModel.getProductCode(), pdtProductVendorApprovedEventModel);
    return pdtProductVendorApprovedEventModel;
  }

  private PDTProductVendorApprovedEventModel getPdtProductVendorApprovedEventModel(
      ProductCollection productCollection) {
    PDTProductVendorApprovedEventModel pdtProductVendorApprovedEventModel = new PDTProductVendorApprovedEventModel();
    pdtProductVendorApprovedEventModel.setProductCode(productCollection.getProductCode());
    pdtProductVendorApprovedEventModel.setPostLive(productCollection.isPostLive());
    pdtProductVendorApprovedEventModel.setReviewPending(productCollection.isReviewPending());
    return pdtProductVendorApprovedEventModel;
  }

  @Override
  public CreateProductSyncEvent publishCreateProductSyncEvent(String storeId, String username, List<String> productItemSkus,
      String businessPartnerCode, String pickupPointCode) {
    LOGGER
        .info("publishing message to clone product item {} for business partner: {}, pickup point: {}", productItemSkus,
            businessPartnerCode, pickupPointCode);
    CreateProductSyncEvent createProductSyncEvent =
        CreateProductSyncEvent.builder().storeId(storeId).username(username).sourceItemSkus(productItemSkus)
            .pickupPointCode(pickupPointCode).businessPartnerCode(businessPartnerCode).build();
    kafkaProducer.send(DomainEventName.CREATE_PRODUCT_SYNC_EVENT, createProductSyncEvent.getBusinessPartnerCode(),
        createProductSyncEvent);
    return createProductSyncEvent;
  }

  @Override
  public ProductItemSyncEvent publishProductSyncSuccessEvent(String storeId, String itemSku, String partnerCode) {
    LOGGER.info("publishing message to update product item {} for business partner: {}", itemSku, partnerCode);
    ProductItemSyncEvent event = new ProductItemSyncEvent();
    event.setStoreId(storeId);
    event.setItemSku(itemSku);
    event.setLinkedPartnerCode(partnerCode);
    kafkaProducer.send(ProductDomainEventName.FBB_ITEM_SYNC_EVENT, event.getItemSku(), event);
    return event;
  }

  @Override
  public AutoApprovalTypeRequest productAutoApprovalCheck(AutoApprovalTypeRequest autoApprovalTypeRequest) {
    kafkaProducer.send(DomainEventName.PRODUCT_AUTO_APPROVAL_CHECK, autoApprovalTypeRequest.getProductCode(),
        autoApprovalTypeRequest);
    LOGGER.info("Publish message {} with message: {} ", DomainEventName.PRODUCT_AUTO_APPROVAL_CHECK,
        autoApprovalTypeRequest);
    return autoApprovalTypeRequest;
  }

  @Override
  public void publishVendorCombinedEvent(AutoApprovalTypeRequest autoApprovalTypeRequest) {
    AutoApprovalTypeRequestModel autoApprovalTypeRequestModel = new AutoApprovalTypeRequestModel();
    BeanUtils.copyProperties(autoApprovalTypeRequest, autoApprovalTypeRequestModel);
    LOGGER.info("Publish vendor combined event {} and message {} ",
        kafkaTopicProperties.getVendorCombinedEventNoPriority(),
        AddProductToVendorCombinedEventModel.builder().autoApprovalTypeRequestModel(autoApprovalTypeRequestModel)
            .build());
    kafkaProducer
        .send(kafkaTopicProperties.getVendorCombinedEventNoPriority(), autoApprovalTypeRequestModel.getProductCode(),
            autoApprovalTypeRequestModel);
  }
}