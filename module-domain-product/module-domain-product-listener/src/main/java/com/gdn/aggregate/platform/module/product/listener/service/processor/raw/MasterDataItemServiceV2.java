package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataItem;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.MasterDataItemRepository;

@Component
public class MasterDataItemServiceV2 {

  @Autowired
  private MasterDataItemRepository masterDataItemRepository;

  public List<MasterDataItem> findAllByIds(Set<String> ids) {
    return masterDataItemRepository.findByIdIn(ids);
  }

  public MasterDataItem getExistingMasterDataItem(String id, List<MasterDataItem> allMasterDataItems) {
    return Optional.ofNullable(allMasterDataItems).orElseGet(ArrayList::new)
        .stream()
        .filter(masterDataItem -> Optional.ofNullable(id).orElse(StringUtils.EMPTY).equals(masterDataItem.getId()))
        .findFirst()
        .orElse(null);
  }

  public MasterDataItem getSingleMasterDataItemById(String id) {
    Optional<MasterDataItem> masterDataItem = masterDataItemRepository.findById(id);
    return masterDataItem.orElse(null);
  }
}
