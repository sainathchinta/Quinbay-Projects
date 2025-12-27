package com.gdn.aggregate.platform.module.product.listener.service.processor.processed;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaItem;
import com.gdn.aggregate.platform.module.product.listener.repository.processed.SivaItemRepository;

@Component
public class SivaItemServiceV2 {

  @Autowired
  private SivaItemRepository sivaItemRepository;

  public List<SivaItem> findAllByIds(Set<String> ids) {
    return sivaItemRepository.findByIdIn(ids);
  }

  public SivaItem getExistingSivaItem(String id, List<SivaItem> allSivaItems) {
    return Optional.ofNullable(allSivaItems).orElseGet(ArrayList::new)
        .stream()
        .filter(sivaItem -> Optional.ofNullable(id).orElse(StringUtils.EMPTY).equals(sivaItem.getId()))
        .findFirst().orElse(null);
  }
}
