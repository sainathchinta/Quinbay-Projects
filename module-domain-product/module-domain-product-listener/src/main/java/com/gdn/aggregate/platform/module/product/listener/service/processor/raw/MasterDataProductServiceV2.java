package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataProduct;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.MasterDataProductRepository;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.MasterDataRepository;

@Component
public class MasterDataProductServiceV2 {

  @Autowired
  private MasterDataProductRepository masterDataProductRepository;

  public List<MasterDataProduct> findAllByIds(Set<String> ids) {
    return masterDataProductRepository.findByIdIn(ids);
  }

  public MasterDataProduct getExistingMasterDataProduct(String id, List<MasterDataProduct> allMasterDataProduct) {
    return Optional.ofNullable(allMasterDataProduct).orElseGet(ArrayList::new).stream()
        .filter(masterDataProduct -> Optional.ofNullable(id).orElse(StringUtils.EMPTY).equals(id)).findFirst()
        .orElse(null);
  }
}
