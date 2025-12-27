package com.gdn.mta.product.service;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.modal.ItemStatusDomainEvent;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.enums.ProductStatus;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;

@Service
public class ItemServiceBean implements ItemService {

  private static final Logger LOGGER = LoggerFactory.getLogger(ItemServiceBean.class);

  @Autowired
  private ItemStatusPublisherService itemStatusPublisherService;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Override
  public void publishItemStatusEvent(List<ProductBusinessPartner> productBusinessPartners,
      ProductStatus productStatus, String storeId) throws Exception {
    ItemStatusDomainEvent itemStatusDomainEvent = ItemStatusDomainEvent.builder()
        .productStatus(productStatus).itemSkus(new HashSet<>()).storeId(storeId).build();
    if (CollectionUtils.isNotEmpty(productBusinessPartners)) {
      for (ProductBusinessPartner productBusinessPartner : productBusinessPartners) {
        Set<String> itemSkus = productBusinessPartner.getProductItemBusinessPartners().stream()
            .map(ProductItemBusinessPartner::getGdnProductItemSku).collect(Collectors.toSet());
        itemStatusDomainEvent.getItemSkus().addAll(itemSkus);
      }
      if (CollectionUtils.isNotEmpty(itemStatusDomainEvent.getItemSkus())) {
        itemStatusPublisherService.publishItemStatusDomainEvent(itemStatusDomainEvent);
      }
    }
  }

  @Override
  public void publishItemStatusEvent(String productCode, ProductStatus productStatus) {
    String storeId = GdnMandatoryRequestParameterUtil.getStoreId();
    try {
      ProductCollection productCollection =
          productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
      List<ProductBusinessPartner> productBusinessPartners = this.productBusinessPartnerRepository
          .findByStoreIdAndProductId(storeId, productCollection.getProductId());
      this.publishItemStatusEvent(productBusinessPartners, productStatus, storeId);
    } catch (Exception ex) {
      LOGGER.error(
          "Error when publishing active item sku event for productCode : {} Exception : {}",
          productCode, ex.getStackTrace());
    }
  }
}
