package com.gdn.mta.product.service.domainevent.publisher;

import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gdn.mta.domain.event.config.ProductApprovalDetailStatus;
import com.gdn.mta.domain.event.modal.AddProductToVendorCombinedEventModel;
import com.gdn.mta.domain.event.modal.CreateProductSyncEvent;
import com.gdn.mta.domain.event.modal.ProductApprovalDetailStatusEvent;
import com.gdn.mta.domain.event.modal.SolrProductCollectionUpdateEvent;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductDomainEventModel;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductVendorApprovedEventModel;
import com.gdn.x.product.domain.event.model.ProductItemSyncEvent;

import java.util.List;

public interface ProductStatusPublisherService {

  /**
   * publish product approval detail state in product collection
   *
   * @param productCode
   * @param status
   * @return
   */
  ProductApprovalDetailStatusEvent updateProductApprovalDetailStatus(String productCode,
      ProductApprovalDetailStatus status);

  /**
   * publish product collection update to solr
   *
   * @param solrProductCollectionDTO must not be null
   * @return
   */
  SolrProductCollectionUpdateEvent publishSolrProductCollectionUpdateEvent(SolrProductCollectionDTO solrProductCollectionDTO);

  /**
   * publish pdt domain event to PBP to approve Image
   *
   * @param pdtProductDomainEventModel
   */
  PDTProductDomainEventModel publishPDTDomainEventModelToPBP(PDTProductDomainEventModel pdtProductDomainEventModel);

  /**
   *
   * @param productCollection
   * @return
   */
  PDTProductVendorApprovedEventModel publishVendorApprovedEventToPBP(ProductCollection productCollection);

  /**
   * Event will be published to create duplicate / clone product with similar details for different store
   * <p>
   * process will be same as creating product using flow 2 but automatically
   *
   * @param storeId     store identifier
   * @param username    username
   * @param itemSku     item sku which is being copied / cloned to new merchant
   * @param partnerCode business partner code
   * @param pickupPoint business partner pickup point
   * @return Create product sync event
   */
  CreateProductSyncEvent publishCreateProductSyncEvent(String storeId, String username, List<String> itemSku,
    String partnerCode, String pickupPoint);

  /**
   * Event will be published to update x-product with linked partner details
   *
   * @param storeId     store identifier
   * @param itemSku     item sku which is being copied / cloned to new merchant
   * @param partnerCode business partner code
   *
   * @return product sync event
   */
  ProductItemSyncEvent publishProductSyncSuccessEvent(String storeId, String itemSku, String partnerCode);

  /**
   * Check for auto approval of product
   *
   * @param autoApprovalTypeRequest
   * @return
   */
  AutoApprovalTypeRequest productAutoApprovalCheck(AutoApprovalTypeRequest autoApprovalTypeRequest);

  /**
   * @param autoApprovalTypeRequest
   * @return
   */
  void publishVendorCombinedEvent(AutoApprovalTypeRequest autoApprovalTypeRequest);
}
