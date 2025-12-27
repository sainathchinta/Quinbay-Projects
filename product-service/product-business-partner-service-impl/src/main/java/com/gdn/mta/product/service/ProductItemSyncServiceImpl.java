package com.gdn.mta.product.service;

import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Map;
import java.util.ArrayList;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import com.gdn.mta.product.entity.ProductItemSyncProcessSummary;
import com.gdn.mta.product.entity.ProductItemSyncStatus;
import com.gdn.mta.product.enums.ProductSyncStatus;
import com.gdn.mta.product.repository.ProductItemSyncStatusRepository;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * @author anand
 * @since Sep 2019
 */
@Slf4j
@Service
public class ProductItemSyncServiceImpl implements ProductItemSyncService {

  @Autowired
  private ProductItemSyncStatusRepository syncStatusRepository;

  @Autowired
  private ProductStatusPublisherService productStatusPublisherService;

  @Override
  public ProductItemSyncStatus save(ProductItemSyncStatus syncStatus) {
    return syncStatusRepository.save(syncStatus);
  }

  @Override
  public ProductItemSyncStatus findByItemSkuAndBusinessPartnerCode(String storeId, String itemSku,
    String partnerCode) {
    return syncStatusRepository.findByStoreIdAndBusinessPartnerCodeAndGdnItemSku(storeId, partnerCode, itemSku);
  }

  @Override
  public List<String> findItemsAlreadyInSyncProcess(String storeId, String businessPartnerCode,
    String linkedPartnerCode) {
    List<ProductItemSyncStatus> productItemSyncStatuses = syncStatusRepository
      .findByStoreIdAndBusinessPartnerCodeAndLinkedBusinessPartnerCodeAndProductSyncStatus(storeId,
        businessPartnerCode, linkedPartnerCode, ProductSyncStatus.IN_PROGRESS);

    return productItemSyncStatuses.stream()
      .map(ProductItemSyncStatus::getGdnItemSku)
      .collect(Collectors.toList());
  }

  @Override
  public List<ProductItemSyncStatus> findSyncStatusByItemSkuAndLinkedPartner(String storeId, String businessPartnerCode,
    String linkedPartnerCode, List<String> itemSKUs) {
    return syncStatusRepository
      .findByStoreIdAndBusinessPartnerCodeAndLinkedBusinessPartnerCodeAndGdnItemSkuIn(storeId, businessPartnerCode,
        linkedPartnerCode, itemSKUs);
  }

  @Override
  public void updateProductItemSyncStatus(String storeID, long syncRetryDuration) {

    Date syncRetryDate = new Date(System.currentTimeMillis() - TimeUnit.HOURS.toMillis(syncRetryDuration));
    syncStatusRepository.updateSyncStatusForSyncRetry(storeID, syncRetryDate, ProductSyncStatus.IN_PROGRESS,
      ProductSyncStatus.FAIL);
  }

  @Override
  public List<ProductItemSyncProcessSummary> productCopyStatusForProcessID(String storeId, String processId) {
    return syncStatusRepository
      .getCountByProcessIdGroupByProductSyncStatus(storeId, processId);
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public Integer copy(String storeId, String username, String partnerCode, String pickupPointCode, boolean isRetryAttempt,
    String processId, Map<String, List<String>> productSkusItemSkus, String linkedPartnerCode) {
    log.info("copying product items {} for partner {} pickup point {}", productSkusItemSkus,
      partnerCode, pickupPointCode);
    Integer itemEligibleForSync = 0;
    for (Map.Entry<String, List<String>> itemSkus : productSkusItemSkus.entrySet()) {
      List<String> validItemSkus = new ArrayList<>();
      for (String itemSku : itemSkus.getValue()) {
        ProductItemSyncStatus syncStatus = this.findByItemSkuAndBusinessPartnerCode(storeId, itemSku, partnerCode);

        if (Objects.isNull(syncStatus)) {
          syncStatus = ProductItemSyncStatus.builder()
            .gdnItemSku(itemSku)
            .businessPartnerCode(partnerCode)
            .linkedBusinessPartnerCode(linkedPartnerCode)
            .build();
          syncStatus.setStoreId(storeId);
        }

        if (validateProductItemSyncStatus(syncStatus, isRetryAttempt)) {
          syncStatus.setProductSyncStatus(ProductSyncStatus.IN_PROGRESS);
          syncStatus.setProcessId(processId);
          syncStatus.setAttempt(syncStatus.getAttempt() + 1);
          this.save(syncStatus);
          itemEligibleForSync = itemEligibleForSync + 1;
          validItemSkus.add(itemSku);
        }
      }

      if (CollectionUtils.isNotEmpty(validItemSkus)) {
        productStatusPublisherService
          .publishCreateProductSyncEvent(storeId, username, validItemSkus, partnerCode, pickupPointCode);
      }

    }
    return itemEligibleForSync;
  }

  private boolean validateProductItemSyncStatus(ProductItemSyncStatus itemSyncProcess, boolean isRetryAttempt) {
    if (ProductSyncStatus.SUCCESS.equals(itemSyncProcess.getProductSyncStatus())) {
      log.warn("item {} has already been copied to business partner {}", itemSyncProcess.getGdnItemSku(),
        itemSyncProcess.getBusinessPartnerCode());
      return false;
    }

    if (!isRetryAttempt && ProductSyncStatus.IN_PROGRESS.equals(itemSyncProcess.getProductSyncStatus())) {
      log.warn("item {} already in progress to copy to {}", itemSyncProcess.getGdnItemSku(),
        itemSyncProcess.getBusinessPartnerCode());
      return false;
    }
    return true;
  }

}
