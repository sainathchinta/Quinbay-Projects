package com.gdn.x.product.service.impl;

import com.gdn.x.product.dao.api.ItemPickupPointMigrationRepository;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ItemMigrationStatus;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.ItemPickupPointMigration;
import com.gdn.x.product.service.api.ItemPickupPointMigrationService;
import com.gdn.x.product.service.api.SystemParameterService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
@Slf4j
public class ItemPickupPointMigrationServiceImpl implements ItemPickupPointMigrationService {

  @Autowired
  private ItemPickupPointMigrationRepository itemPickupPointMigrationRepository;

  @Autowired
  private SystemParameterService systemParameterService;

  @Override
  public List<ItemPickupPointMigration> findByStatusAndLimit(String status, int limit) {
    return itemPickupPointMigrationRepository.getItemsByStatusAndLimit(status, limit);
  }

  @Override
  public List<ItemPickupPointMigration> saveCollection(List<ItemPickupPointMigration> itemPickupPointMigrationList) {
    if (CollectionUtils.isEmpty(itemPickupPointMigrationList)) {
      return itemPickupPointMigrationList;
    }
    return this.itemPickupPointMigrationRepository.saveAll(itemPickupPointMigrationList);
  }

  @Override
  public ItemPickupPointMigration findByItemSku(String itemSku) {
    if (StringUtils.isEmpty(itemSku)) {
      return null;
    } else {
      return this.itemPickupPointMigrationRepository.findByItemSku(itemSku);
    }
  }

  @Override
  public void updateStatusByItemSku(List<String> itemSkuList, String status) {
    this.itemPickupPointMigrationRepository.updateStatusByItemSkus(itemSkuList, status);
  }

  @Override
  public void updateFailedStatusByItemSku(Map<String, String> failedItemSku) {
    List<ItemPickupPointMigration> itemPickupPointMigrations =
      this.itemPickupPointMigrationRepository.findByItemSkuIn(failedItemSku.keySet());
    for (ItemPickupPointMigration itemPickupPointMigration : itemPickupPointMigrations) {
      itemPickupPointMigration.setStatus(ItemMigrationStatus.FAILED.name());
      itemPickupPointMigration.setErrorMessage(failedItemSku.get(itemPickupPointMigration.getItemSku()));
      itemPickupPointMigration.setEndTime(new Date());
    }
    this.itemPickupPointMigrationRepository.saveAll(itemPickupPointMigrations);
  }

  @Override
  public void insertItemSkuByState(List<String> itemSkuList, String state) {
    if (Boolean.FALSE.equals(Boolean.parseBoolean(
      this.systemParameterService.findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
        SystemParameterNames.MIGRATION_ITEM_CREATION_CHANGE_SWITCH).getValue()))) {
      return;
    }
    List<ItemPickupPointMigration> itemPickupPointMigrations =
      Optional.ofNullable(itemSkuList).orElse(new ArrayList<>()).stream()
        .map(itemSku -> ItemPickupPointMigration.builder().itemSku(itemSku).status(state).build())
        .collect(Collectors.toList());
    try {
      this.itemPickupPointMigrationRepository.saveAll(itemPickupPointMigrations);
    } catch (Exception e) {
      log.error(
        "Exception on storing item migration entry for itemSkuList : {}, state : {}, error - ",
        itemSkuList, state, e);
    }
  }
}
