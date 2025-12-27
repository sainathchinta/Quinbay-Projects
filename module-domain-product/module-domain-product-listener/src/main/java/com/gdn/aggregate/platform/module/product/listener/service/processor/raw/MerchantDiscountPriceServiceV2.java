package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.model.raw.MerchantDiscountPrice;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.MerchantDiscountPriceRepository;

@Component
public class MerchantDiscountPriceServiceV2 {

  @Autowired
  private MerchantDiscountPriceRepository merchantDiscountPriceRepository;

  public Optional<MerchantDiscountPrice> findById(String id) {
    return merchantDiscountPriceRepository.findById(id);
  }

  public PickupPoint.DiscountPrice getDiscountPrice(String id, List<MerchantDiscountPrice> allMerchantDiscountPrice) {
    return Optional.ofNullable(id)
        .map(_id -> getExistingMerchantDiscountPrice(_id, allMerchantDiscountPrice))
        .filter(val -> !val.isMarkForDelete())
        .map(MerchantDiscountPrice::getPrice)
        .orElseGet(HashSet::new)
        .stream()
        .filter(Objects::nonNull)
        .findFirst()
        .map(PickupPoint.Price::getMerchantPromoDiscountPrice)
        .orElse(null);
  }

  public PickupPoint.Price getFullPrice(String id,
    List<MerchantDiscountPrice> allMerchantDiscountPrice) {
    return Optional.ofNullable(id)
      .map(_id -> getExistingMerchantDiscountPrice(_id, allMerchantDiscountPrice))
      .filter(val -> !val.isMarkForDelete()).map(MerchantDiscountPrice::getPrice)
      .orElseGet(HashSet::new).stream().filter(Objects::nonNull).findFirst().orElse(null);
  }

  public MerchantDiscountPrice getExistingMerchantDiscountPrice(String id, List<MerchantDiscountPrice> allMerchantDiscountPrice) {
    return Optional.ofNullable(allMerchantDiscountPrice).orElseGet(ArrayList::new)
        .stream()
        .filter(merchantDiscountPrice -> Optional.ofNullable(id).orElse(StringUtils.EMPTY).equals(merchantDiscountPrice.getId()))
        .findFirst()
        .orElse(null);
  }
}
