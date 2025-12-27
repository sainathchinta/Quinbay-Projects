package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignType;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductPublished;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.CampaignProductRepository;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;

@Component
public class CampaignProductServiceV2 {

  @Autowired
  private CampaignProductRepository campaignProductRepository;

  public List<CampaignProduct> findAllByProductSku(Set<String> itemSkus) {
    return campaignProductRepository.findBySku_ItemSkuIn(itemSkus);
  }

  public SivaProduct.CampaignPriceSchedule getUpcomingCampaignPriceSchedule(List<CampaignProduct> campaignProducts, Long baseDate) {
    return campaignProducts.stream()
        .filter(val -> !ModuleProductUtil.isItAlreadyStarted(val.getPromotionStartTime(),baseDate))
        .min(Comparator
            .comparing(CampaignProduct::getPromotionStartTime)
            .thenComparing(val -> StringUtils.isNotEmpty(val.getFlashSaleCampaignName()),Comparator.reverseOrder())
            .thenComparing(CampaignProduct::isExclusive,Comparator.reverseOrder())
            .thenComparing(val -> CampaignType.REGULAR.equals(val.getCampaignType()),Comparator.reverseOrder()))
        .map(val -> SivaProduct.CampaignPriceSchedule.builder()
            .campaignCode(val.getCampaignCode())
            .start(val.getPromotionStartTime())
            .end(val.getPromotionEndTime())
            .discount(val.getSku().getDiscount())
            .quota(val.getSku().getQuota())
            .campaignPrice(val.getSku().getCampaignPrice())
            .campaignTag(val.getTagLabel())
            .finalPrice(val.getSku().getFinalPrice())
            .build())
        .orElse(null);
  }

  public List<CampaignProduct> getCampaignProductsByL5(String itemSku, String pickupPointCode, List<CampaignProduct> allCampaignProducts) {
    return Optional.ofNullable(allCampaignProducts).orElseGet(ArrayList::new)
        .stream()
        .filter(campaignProduct -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(Optional.ofNullable(campaignProduct).map(CampaignProduct::getSku).map(
            CampaignProductPublished.ProductSkuEventModel::getItemSku).orElse(StringUtils.EMPTY)))
        .filter(campaignProduct -> Optional.ofNullable(pickupPointCode).orElse(StringUtils.EMPTY).equals(Optional.ofNullable(campaignProduct).map(CampaignProduct::getSku).map(
            CampaignProductPublished.ProductSkuEventModel::getPickupPointCode).orElse(StringUtils.EMPTY)))
        .filter(Objects::nonNull)
        .filter(CampaignProduct::isActive)
        .sorted(Comparator.comparing(adj -> MainUtil.toNotNullString(adj.toId())))
        .collect(Collectors.toList());
  }

  public CampaignProduct getExistingCampaignProduct(String id, List<CampaignProduct> allCampaignProducts) {
    return Optional.ofNullable(allCampaignProducts).orElseGet(ArrayList::new).stream()
        .filter(campaignProduct -> Optional.ofNullable(id).orElse(StringUtils.EMPTY).equals(campaignProduct.getId()))
        .findFirst().orElse(null);
  }
}
