package com.gdn.x.product.service.impl;

import com.gdn.x.product.dao.api.ItemPickupPointRepository;
import com.gdn.x.product.dao.api.ProductRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.model.BackFillFbbFlagRequest;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.service.api.BackFillFbbFlagService;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.util.CommonUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

@Slf4j
@Service
public class BackFillFbbFlagServiceImpl implements BackFillFbbFlagService {

  @Autowired
  private CacheEvictHelperService cacheEvictHelperService;

  @Autowired
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private ItemPickupPointRepository itemPickupPointRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Value("${publish.l5.event}")
  private boolean publishL5Event;

  @Value("${newly.added.item.pickup.point.data.change.event.types}")
  private String newlyAddedItemPickupPointDataChangeEventTypes;

  @Value("${new.l5.data.change.event.types.enabled}")
  private boolean newL5DataChangeTypeEnabled;

  @Override
  public void backFillFbbFlag(BackFillFbbFlagRequest backFillFbbFlagRequest) {
    String productSku = backFillFbbFlagRequest.getIdentifier();
    log.info("Updating fbb flag for sku {} ", productSku);
    Product product = productRepository.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(
      backFillFbbFlagRequest.getStoreId(), productSku, false);
    if (Objects.nonNull(product)) {
      List<ItemPickupPoint> itemPickupPointList =
        itemPickupPointRepository.findByStoreIdAndProductSkuAndPickupPointCodeAndMarkForDeleteFalse(
            backFillFbbFlagRequest.getStoreId(), productSku,
            backFillFbbFlagRequest.getPickupPointCode()).stream()
          .filter(itemPickupPoint -> !itemPickupPoint.isFbbActivated())
          .collect(Collectors.toList());
      List<ItemPickupPoint> updatedItemPickupPointList = new ArrayList<>();
      Optional.of(itemPickupPointList).orElseGet(ArrayList::new).forEach(
        itemPpCode -> updatedItemPickupPointList.addAll(new ArrayList<>(
          itemPickupPointRepository.updateFbbFlagByProductSkuAndPickupPointCode(
            itemPpCode.getStoreId(), itemPpCode.getProductSku(), itemPpCode.getPickupPointCode(),
            true))));
      if (!product.isFbbActivated()) {
        Product result =
          productRepository.updateFbbFlagAtProduct(backFillFbbFlagRequest.getStoreId(), productSku,
            true);
        cacheEvictHelperService.evictProductData(backFillFbbFlagRequest.getStoreId(), result);
        updateFbbFlagInSolr(result);
      }
      if (publishL5Event) {
        updatedItemPickupPointList.forEach(this::publishL5WithFbbEventType);
      }
    } else {
      log.error("Product sku not found {} ", productSku);
    }
  }

  private void updateFbbFlagInSolr(Product product) {
    log.info("Publishing solr update for fbb event : {}, product-sku : {}",
      ProductDomainEventName.UPDATE_TO_SOLR, product.getProductSku());
    productAndItemSolrIndexerService.updateProductAndItemDetailsInSolr(product, new ArrayList<>(),
      false);
  }

  private void publishL5WithFbbEventType(ItemPickupPoint itemPickupPoint) {
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel =
        objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint, false);
    BusinessPartner businessPartner =
        businessPartnerService.getBusinessPartnerByBusinessPartnerCode(itemPickupPointDataChangeEventModel.getStoreId(),
            itemPickupPointDataChangeEventModel.getMerchantCode());
    itemPickupPointDataChangeEventModel.setSellerChannel(
        CommonUtil.generateSellerChannelFromBusinessPartner(businessPartner));
    itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypes(
      Collections.singletonList(ItemPickupPointChangeEventType.FBB_MIGRATION));
    setV2ChangeTypeAndRemoveNewlyAddedChangeTypes(itemPickupPointDataChangeEventModel);
    log.info("Publishing fbb event : {}, itemPickupPointChangeEventModel : {}",
      ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME,
      itemPickupPointDataChangeEventModel);
    kafkaPublisher.send(ProductDomainEventName.ITEM_PICKUP_POINT_DATA_CHANGE_EVENT_NAME,
      itemPickupPoint.getItemSku() + Constants.HYPHEN + itemPickupPoint.getPickupPointCode(),
      itemPickupPointDataChangeEventModel);
  }

  private void setV2ChangeTypeAndRemoveNewlyAddedChangeTypes(
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel) {
    if (newL5DataChangeTypeEnabled) {
      Optional.ofNullable(itemPickupPointDataChangeEventModel.getItemPickupPointChangeEventTypes())
        .ifPresent(types -> {
          List<String> eventTypesV2 = types.stream().map(Objects::toString).collect(Collectors.toList());
          itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypesV2(eventTypesV2);
          List<ItemPickupPointChangeEventType> updatedEventTypes = CommonUtil.removeNewlyAddedEventTypesForL5DataChange(types,
              newlyAddedItemPickupPointDataChangeEventTypes);
          itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypes(updatedEventTypes);
        });
    }
  }



}
