package com.gdn.x.product.service.impl;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.dao.api.ItemArchiveRepository;
import com.gdn.x.product.model.entity.ItemArchive;
import com.gdn.x.product.service.api.ItemArchiveService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class ItemArchiveServiceImpl implements ItemArchiveService {

  @Autowired
  private ItemArchiveRepository itemArchiveRepository;

  @Override
  public List<ItemArchive> addItemsToItemArchive(List<ItemArchive> itemArchiveList) {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemArchiveList),
        ErrorMessages.ITEM_ARCHIVE_LIST_MUST_NOT_BE_EMPTY);
    return itemArchiveRepository.saveAll(itemArchiveList);
  }
}
