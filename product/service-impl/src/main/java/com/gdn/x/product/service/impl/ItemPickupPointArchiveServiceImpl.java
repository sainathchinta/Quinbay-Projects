package com.gdn.x.product.service.impl;

import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.dao.api.ItemPickupPointArchiveRepository;
import com.gdn.x.product.model.entity.ItemPickupPointArchive;
import com.gdn.x.product.service.api.ItemPickupPointArchiveService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ItemPickupPointArchiveServiceImpl implements ItemPickupPointArchiveService {

  @Autowired
  private ItemPickupPointArchiveRepository itemPickupPointArchiveRepository;

  @Override
  public List<ItemPickupPointArchive> addItemPickupPointsToItemPickupPointArchive(
      List<ItemPickupPointArchive> itemPickupPointArchiveList) {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemPickupPointArchiveList),
        ErrorMessages.ITEM_PICKUP_POINT_ARCHIVE_LIST_MUST_NOT_BE_EMPTY);
    return itemPickupPointArchiveRepository.saveAll(itemPickupPointArchiveList);
  }
}
