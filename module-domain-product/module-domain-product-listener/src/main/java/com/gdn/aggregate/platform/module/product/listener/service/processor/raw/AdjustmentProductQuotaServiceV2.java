package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignOwner;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductQuota;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Quota;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.AdjustmentProductQuotaRepository;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;

@Component
public class AdjustmentProductQuotaServiceV2 {

  @Autowired
  private AdjustmentProductQuotaRepository adjustmentProductQuotaRepository;

  public List<AdjustmentProductQuota> findAllByItemSkuIn(Set<String> itemSkus) {
    return adjustmentProductQuotaRepository.findByItemSkuIn(itemSkus);
  }

  public List<Quota> toQuotaAdjustments(List<PickupPoint> pickupPoints, String campaignCode, List<AdjustmentProductQuota> allAdjustmentProductQuotas) {
    return Optional.ofNullable(pickupPoints)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(val -> toQuotaAdjustments(val.getItemSku(), val.getPickupPointCode(), campaignCode, allAdjustmentProductQuotas))
        .filter(Objects::nonNull)
        .min(Comparator.comparing(ModuleProductUtil::toComparingPercentage))
        .orElse(null);
  }

  public List<Quota> toQuotaAdjustments(String itemSku, String pickupPointCode, String campaignCode, List<AdjustmentProductQuota> allAdjustmentProductQuotas) {
    List<Quota> result = new ArrayList<>();
    Quota quotaBlibli = ModuleProductUtil.initializeQuota(null);
    Quota quotaMerchant = ModuleProductUtil.initializeQuota(null);

    getAdjustmentProductQuotasByCampaignDetail(itemSku, pickupPointCode, campaignCode, allAdjustmentProductQuotas).stream()
        .forEach(quota -> {
          if(CampaignOwner.BLIBLI.equals(quota.getBudgetOwner())) {
            int used = (quotaBlibli.getUsed() + quota.getUsedQuota());
            int qt = (quotaBlibli.getQuota() + quota.getQuota());
            int remaining = qt - used;
            quotaBlibli.setItemSku(quota.getItemSku());
            quotaBlibli.setPickupPointCode(quota.getPickupPointCode());
            quotaBlibli.setOwner(CampaignOwner.BLIBLI);
            quotaBlibli.setUsed(used);
            quotaBlibli.setQuota(qt);
            quotaBlibli.setRemaining(remaining);
          } else {
            int used = (quotaMerchant.getUsed() + quota.getUsedQuota());
            int qt = (quotaMerchant.getQuota() + quota.getQuota());
            int remaining = qt - used;
            quotaMerchant.setItemSku(quota.getItemSku());
            quotaMerchant.setPickupPointCode(quota.getPickupPointCode());
            quotaMerchant.setOwner(CampaignOwner.MERCHANT);
            quotaMerchant.setUsed(used);
            quotaMerchant.setQuota(qt);
            quotaMerchant.setRemaining(remaining);
          }
        });

    ModuleProductUtil.insertQuotaAdjustments(CampaignOwner.BLIBLI,quotaBlibli,result);
    ModuleProductUtil.insertQuotaAdjustments(CampaignOwner.MERCHANT,quotaMerchant,result);

    return Optional.ofNullable(result)
        .filter(CollectionUtils::isNotEmpty)
        .orElse(null);
  }

  public List<AdjustmentProductQuota> getAdjustmentProductQuotasByCampaignDetail(String itemSku, String pickupPointCode, String campaignCode, List<AdjustmentProductQuota> allAdjustmentProductQuotas) {
    return Optional.ofNullable(itemSku)
        .map(l4Id -> getAdjustmentProductQuotas(itemSku, pickupPointCode, ModuleProductUtil.toRealCampaignCode(campaignCode), allAdjustmentProductQuotas))
        .filter(CollectionUtils::isNotEmpty)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .sorted(Comparator.comparing(adj -> MainUtil.toNotNullString(adj.toId())))
        .collect(Collectors.toList());
  }

  private List<AdjustmentProductQuota> getAdjustmentProductQuotas(String itemSku, String pickupPointCode,
      String campaignCode, List<AdjustmentProductQuota> allAdjustmentProductQuotas) {
    return Optional.ofNullable(
            getAdjustmentProductQuotasByL4AndL5(itemSku, pickupPointCode, campaignCode, allAdjustmentProductQuotas))
        .filter(CollectionUtils::isNotEmpty)
        .orElseGet(() -> getAdjustmentProductQuotasByL4AndL5(itemSku, pickupPointCode, null, allAdjustmentProductQuotas));
  }

  private List<AdjustmentProductQuota> getAdjustmentProductQuotasByL4AndL5(String itemSku, String pickupPointCode,
      String campaignCode, List<AdjustmentProductQuota> allAdjustmentProductQuotas) {
    return Optional.ofNullable(pickupPointCode)
        .map(val -> getAdjustmentProductQuotasByL5(itemSku, pickupPointCode, campaignCode, allAdjustmentProductQuotas))
        .orElseGet(() -> getAdjustmentProductQuotasByL4(itemSku, campaignCode, allAdjustmentProductQuotas));
  }

  private List<AdjustmentProductQuota> getAdjustmentProductQuotasByL5(String itemSku, String pickupPointCode,
      String campaignCode, List<AdjustmentProductQuota> allAdjustmentProductQuotas) {
    return Optional.ofNullable(allAdjustmentProductQuotas).orElseGet(ArrayList::new).stream()
        .filter(adjustmentProductQuota -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(adjustmentProductQuota.getItemSku()))
        .filter(adjustmentProductQuota -> Optional.ofNullable(pickupPointCode).orElse(StringUtils.EMPTY).equals(adjustmentProductQuota.getPickupPointCode()))
        .filter(adjustmentProductQuota -> Optional.ofNullable(campaignCode).orElse(StringUtils.EMPTY).equals(adjustmentProductQuota.getCampaignCode()))
        .sorted(Comparator.comparing(adj -> MainUtil.toNotNullString(adj.toId()))).collect(Collectors.toList());
  }

  private List<AdjustmentProductQuota> getAdjustmentProductQuotasByL4(String itemSku, String campaignCode,
      List<AdjustmentProductQuota> allAdjustmentProductQuotas) {
    return Optional.ofNullable(allAdjustmentProductQuotas).orElseGet(ArrayList::new).stream()
        .filter(adjustmentProductQuota -> Optional.ofNullable(itemSku).orElse(StringUtils.EMPTY).equals(adjustmentProductQuota.getItemSku()))
        .filter(adjustmentProductQuota -> Optional.ofNullable(campaignCode).orElse(StringUtils.EMPTY).equals(adjustmentProductQuota.getCampaignCode()))
        .sorted(Comparator.comparing(adj -> MainUtil.toNotNullString(adj.toId()))).collect(Collectors.toList());
  }
}
