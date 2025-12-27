package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignOwner;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.constants.ProductType;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.sub.Quota;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductQuota;
import com.gdn.aggregate.platform.module.product.listener.model.raw.AdjustmentProductSaved;
import com.gdn.aggregate.platform.module.product.listener.repository.processed.SivaProductRepository;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.AdjustmentProductQuotaRepository;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Component
public class AdjustmentProductQuotaService {

  private static final String SAVE_COMMAND = "saveAdjustmentProductQuota";

  @Autowired
  private DBService dbService;

  @Autowired
  private AdjustmentProductQuotaRepository adjustmentProductQuotaRepository;

  @Autowired
  private SivaProductRepository sivaProductRepository;

  /*Save*/
  public Mono<Boolean> save(AdjustmentProductQuota adjustmentProductQuota, SaveParam saveParam) {
    return Mono.fromCallable(() -> SaveRequest.builder()
            .index(Collections.ADJUSTMENT_PRODUCT_QUOTA)
            .domain(adjustmentProductQuota)
            .clazz(AdjustmentProductQuota.class)
            .mongo(true)
            .elasticsearch(false)
            .republish(ParamUtil.isRepublish(saveParam))
            .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
            .build())
        .flatMap(saveRequest -> dbService.save(saveRequest))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(adjustmentProductQuota,SAVE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(adjustmentProductQuota,t,SAVE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult);
  }

  public void setMandatory(AdjustmentProductQuota adjustmentProductQuota, SaveParam saveParam) {
    Optional.ofNullable(adjustmentProductQuota)
        .ifPresent(val -> {
          val.setId(val.toId());
          val.setPickupPointCode(getPickupPointCodeForSaving(val));
          val.setCampaignCode(getCampaignCodeForSaving(val));
        });
  }

  public String getPickupPointCodeForSaving(AdjustmentProductQuota adjustmentProductQuota) {
    return Optional.ofNullable(adjustmentProductQuota)
        .map(AdjustmentProductQuota::getItemSku)
        .flatMap(sivaProductRepository::findById)
        .filter(val -> ProductType.DIGITAL.equals(val.getProductType()))
        .map(val -> ProductType.DIGITAL)
        .orElseGet(() -> ModuleProductUtil.getAdjustmentProductQuotaPickupPointCode(adjustmentProductQuota));
  }

  public String getCampaignCodeForSaving(AdjustmentProductQuota adjustmentProductQuota) {
    return Optional.ofNullable(adjustmentProductQuota)
        .filter(val -> Objects.isNull(val.getCampaignCode()))
        .map(this::getExistingAdjustmentProductQuotaWithObject)
        .map(AdjustmentProductQuota::getCampaignCode)
        .orElseGet(() -> ModuleProductUtil.getAdjustmentProductQuotaCampaignCode(adjustmentProductQuota));
  }
  /*End of Save*/

  /*Getters*/
  public AdjustmentProductQuota getExistingAdjustmentProductQuota(String id) {
    return Optional.ofNullable(id)
        .flatMap(adjustmentProductQuotaRepository::findById)
        .orElse(null);
  }

  public AdjustmentProductQuota getExistingAdjustmentProductQuotaWithObject(AdjustmentProductQuota adjustmentProductQuota) {
    return Optional.ofNullable(adjustmentProductQuota)
        .map(AdjustmentProductQuota::toId)
        .map(this::getExistingAdjustmentProductQuota)
        .orElse(null);
  }

  public AdjustmentProductQuota getExistingAdjustmentProductQuota(AdjustmentProductSaved adjustmentProductSaved) {
    return Optional.ofNullable(adjustmentProductSaved)
        .map(val -> {
          AdjustmentProductQuota result = getExistingAdjustmentProductQuota(val.toId());
          if (Objects.isNull(result)) {
            result = new AdjustmentProductQuota();
            BeanUtils.copyProperties(val,result);
          } else {
            result.setTimestamp(val.getTimestamp());
            result.setQuota(val.getQuota());
          }
          result.setAdjustmentProductId(val.getAdjustmentId());
          result.setPriority(val.getPriority());
          return result;
        })
        .orElse(null);
  }

  public List<AdjustmentProductQuota> getAdjustmentProductQuotasByCampaignDetail(String itemSku, String pickupPointCode, String campaignCode) {
    return Optional.ofNullable(itemSku)
        .map(l4Id -> getAdjustmentProductQuotas(itemSku,pickupPointCode,ModuleProductUtil.toRealCampaignCode(campaignCode)))
        .filter(CollectionUtils::isNotEmpty)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .sorted(Comparator.comparing(adj -> MainUtil.toNotNullString(adj.toId())))
        .collect(Collectors.toList());
  }

  private List<AdjustmentProductQuota> getAdjustmentProductQuotas(String itemSku, String pickupPointCode, String campaignCode) {
    return Optional.ofNullable(getAdjustmentProductQuotasByL4AndL5(itemSku,pickupPointCode,campaignCode))
        .filter(CollectionUtils::isNotEmpty)
        .orElseGet(() -> getAdjustmentProductQuotasByL4AndL5(itemSku,pickupPointCode,null));
  }

  private List<AdjustmentProductQuota> getAdjustmentProductQuotasByL4AndL5(String itemSku, String pickupPointCode, String campaignCode) {
    return Optional.ofNullable(pickupPointCode)
        .map(val -> getAdjustmentProductQuotasByL5(itemSku,pickupPointCode,campaignCode))
        .orElseGet(() -> getAdjustmentProductQuotasByL4(itemSku,campaignCode));
  }

  private List<AdjustmentProductQuota> getAdjustmentProductQuotasByL4(String itemSku, String campaignCode) {
    return Optional.ofNullable(itemSku)
        .map(l4Id -> {
          try(Stream<AdjustmentProductQuota> quotaAdjustmentProductStream = adjustmentProductQuotaRepository.streamAllByItemSkuAndCampaignCode(itemSku,campaignCode)) {
            return quotaAdjustmentProductStream
                .filter(Objects::nonNull)
                .sorted(Comparator.comparing(adj -> MainUtil.toNotNullString(adj.toId())))
                .collect(Collectors.toList());
          }
        })
        .orElseGet(ArrayList::new);
  }

  private List<AdjustmentProductQuota> getAdjustmentProductQuotasByL5(String itemSku, String pickupPointCode, String campaignCode) {
    return Optional.ofNullable(ModuleProductUtil.toPickupPointId(itemSku,pickupPointCode))
        .map(l5Id -> {
          try(Stream<AdjustmentProductQuota> quotaAdjustmentProductStream = adjustmentProductQuotaRepository.streamAllByItemSkuAndPickupPointCodeAndCampaignCode(itemSku,pickupPointCode,campaignCode)) {
            return quotaAdjustmentProductStream
                .filter(Objects::nonNull)
                .sorted(Comparator.comparing(adj -> MainUtil.toNotNullString(adj.toId())))
                .collect(Collectors.toList());
          }
        })
        .orElseGet(ArrayList::new);
  }

  private List<AdjustmentProductQuota> getAdjustmentProductQuotasByItemSkuAndCampaignCode(String itemSku, String campaignCode) {
    return Optional.ofNullable(itemSku)
        .map(itmSku -> {
          try(Stream<AdjustmentProductQuota> quotaAdjustmentProductStream = adjustmentProductQuotaRepository.streamAllByItemSkuAndCampaignCode(itmSku,campaignCode)) {
            return quotaAdjustmentProductStream
                .filter(Objects::nonNull)
                .sorted(Comparator.comparing(adj -> MainUtil.toNotNullString(adj.toId())))
                .collect(Collectors.toList());
          }
        })
        .orElseGet(ArrayList::new);
  }

  public List<Quota> toQuotaAdjustments(List<PickupPoint> pickupPoints, String campaignCode) {
    return Optional.ofNullable(pickupPoints)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(val -> toQuotaAdjustments(val.getItemSku(), val.getPickupPointCode(), campaignCode))
        .filter(Objects::nonNull)
        .min(Comparator.comparing(ModuleProductUtil::toComparingPercentage))
        .orElse(null);
  }

  public List<Quota> toQuotaAdjustments(String itemSku, String pickupPointCode, String campaignCode) {
    List<Quota> result = new ArrayList<>();
    Quota quotaBlibli = ModuleProductUtil.initializeQuota(null);
    Quota quotaMerchant = ModuleProductUtil.initializeQuota(null);

    getAdjustmentProductQuotasByCampaignDetail(itemSku,pickupPointCode,campaignCode).stream()
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
  /*End of Getters*/
  
}
