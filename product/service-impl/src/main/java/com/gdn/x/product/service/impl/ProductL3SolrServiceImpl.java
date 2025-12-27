package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.solr.common.SolrInputDocument;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.x.product.dao.solr.api.ProductSolrRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ProductL3SolrService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.util.CommonUtil;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductL3SolrServiceImpl implements ProductL3SolrService {

  @Autowired
  private ProductSolrRepository productSolrRepository;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Value("${event.based.solr.update.enabled}")
  private boolean eventBasedSolrUpdateEnable;

  @Value("${populate.label.for.upcoming.promo}")
  private boolean populateLabelForUpcomingPromo;

  @Value("${populate.label.for.pwp.promo}")
  private boolean populateLabelForPwpPromo;

  @Override
  public void updateStockStatusInL3Solr(String productSku, String status, String merchantCode) {
    boolean stockStatus = Constants.AVAILABLE.equalsIgnoreCase(status) ? true : false;
    productSolrRepository.updateStockStatus(productSku, stockStatus, merchantCode);
  }

  @Override
  public <T> void updatePromoOrWholesaleItemSkus(List<T> itemsOrItemPickupPoints, boolean isPromoUpdateOnly) {
    productSolrRepository.updatePromoOrWholesaleItemSkus(itemsOrItemPickupPoints, isPromoUpdateOnly);
  }

  @Override
  public void updatePromoOrWholesaleItemSkusByItemPickupPoint(ItemPickupPoint itemPickupPoint) {
    List<ItemPickupPoint> itemPickUpPoints =
        itemPickupPointService.findByStoreIdAndItemSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
            itemPickupPoint.getItemSku());
    if (CollectionUtils.isNotEmpty(itemPickUpPoints)) {
      itemPickUpPoints = new ArrayList<>(itemPickUpPoints);
      itemPickUpPoints.removeIf(
          itemPickupPoint1 -> itemPickupPoint1.getPickupPointCode().equals(itemPickupPoint.getPickupPointCode()));
      itemPickUpPoints.add(itemPickupPoint);
      if (eventBasedSolrUpdateEnable) {
        Map<String, Object> fieldsAndValues =
            CommonUtil.getFieldsAndValuesL3ForPromoAndWholesaleUpdate(itemPickUpPoints, populateLabelForUpcomingPromo,
                populateLabelForPwpPromo);
        log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
            ProductDomainEventName.UPDATE_TO_SOLR, itemPickupPoint.getProductSku(), fieldsAndValues);
        kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, itemPickupPoint.getProductSku(),
          new ProductAndItemEventModel(itemPickupPoint.getProductSku(), fieldsAndValues,
            itemPickupPoint.getMerchantCode()));
      } else {
        SolrInputDocument solrInputDocument = CommonUtil.getSolrInputDocumentL3ForPromoAndWholesaleUpdate(itemPickUpPoints,
            itemPickupPoint.getProductSku(), populateLabelForUpcomingPromo, populateLabelForPwpPromo);
        productSolrRepository.executeSolrDocumentsAtomicUpdate(Arrays.asList(solrInputDocument));
      }
    }
  }

  @Override
  public void updatePromoOrWholesaleItemSkusByItemPickupPoint(List<ItemPickupPoint> updatedItemPickupPoints) {
    Map<String, List<ItemPickupPoint>> productSkuItemPickupPointMap =
        updatedItemPickupPoints.stream().collect(Collectors.groupingBy(ItemPickupPoint::getProductSku));
    for (Map.Entry<String, List<ItemPickupPoint>> entry : productSkuItemPickupPointMap.entrySet()) {
      Set<String> updatedItemPickupPointIds = CommonUtil.getItemPickupPointIds(entry.getValue());
      List<ItemPickupPoint> itemPickUpPoints =
          itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
              entry.getKey());
      if (CollectionUtils.isNotEmpty(itemPickUpPoints)) {
        itemPickUpPoints = new ArrayList<>(itemPickUpPoints);
        itemPickUpPoints.removeIf(
            fetchedItemPickupPoint -> updatedItemPickupPointIds.contains(fetchedItemPickupPoint.getId()));
        itemPickUpPoints.addAll(entry.getValue());
        if (eventBasedSolrUpdateEnable) {
          Map<String, List<ItemPickupPoint>> itemPickupPointsGroupedByMerchantCode =
              itemPickUpPoints.stream().filter(itemPickupPoint -> Objects.nonNull(itemPickupPoint.getMerchantCode()))
                  .collect(Collectors.groupingBy(ItemPickupPoint::getMerchantCode));
          for (Map.Entry<String, List<ItemPickupPoint>> merchantCodeEntry : itemPickupPointsGroupedByMerchantCode.entrySet()) {
            Map<String, Object> fieldsAndValues =
                CommonUtil.getFieldsAndValuesL3ForPromoAndWholesaleUpdate(merchantCodeEntry.getValue(),
                    populateLabelForUpcomingPromo, populateLabelForPwpPromo);
            log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
                ProductDomainEventName.UPDATE_TO_SOLR, entry.getKey(), fieldsAndValues);
            kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, entry.getKey(),
                new ProductAndItemEventModel(entry.getKey(), fieldsAndValues, merchantCodeEntry.getKey()));
          }
        } else {
          SolrInputDocument solrInputDocument =
              CommonUtil.getSolrInputDocumentL3ForPromoAndWholesaleUpdate(itemPickUpPoints, entry.getKey(),
                  populateLabelForUpcomingPromo, populateLabelForPwpPromo);
          productSolrRepository.executeSolrDocumentsAtomicUpdate(Arrays.asList(solrInputDocument));
        }
      }
    }
  }
}
