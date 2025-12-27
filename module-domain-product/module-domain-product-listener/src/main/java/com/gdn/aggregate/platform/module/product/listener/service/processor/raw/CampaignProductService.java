package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.TimeService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.CampaignType;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import com.gdn.aggregate.platform.module.product.listener.model.processed.SivaProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProduct;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignProductPublished;
import com.gdn.aggregate.platform.module.product.listener.model.raw.CampaignTeaserLive;
import com.gdn.aggregate.platform.module.product.listener.properties.CampaignProperties;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.CampaignProductRepository;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
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
public class CampaignProductService {

  private static final String SAVE_COMMAND = "saveCampaignProduct";

  @Autowired
  private DBService dbService;

  @Autowired
  private CampaignProductRepository campaignProductRepository;

  @Autowired
  private TimeService timeService;

  @Autowired
  private CampaignProperties campaignProperties;

  public Mono<Boolean> save(CampaignProduct campaignProduct, SaveParam saveParam) {
    return Mono.fromCallable(() -> SaveRequest.builder()
            .index(Collections.CAMPAIGN_PRODUCT)
            .domain(campaignProduct)
            .clazz(CampaignProduct.class)
            .mongo(true)
            .elasticsearch(false)
            .republish(ParamUtil.isRepublish(saveParam))
            .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
            .build())
        .flatMap(saveRequest -> dbService.save(saveRequest))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(campaignProduct,SAVE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(campaignProduct,t,SAVE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult);
  }

  public void setMandatory(CampaignProduct campaignProduct, SaveParam saveParam) {
    Optional.ofNullable(campaignProduct)
        .ifPresent(val -> {
          val.setId(val.toId());
          val.setExpiryTime(timeService.getExpiryTime(val.getPromotionEndTime(),MainUtil.toFriendlyClassName(val)));
        });
  }

  public List<CampaignProduct> getExistingCampaignProducts(List<String> ids) {
    return Optional.ofNullable(ids)
        .filter(CollectionUtils::isNotEmpty)
        .map(campaignProductRepository::findAllByIdIn)
        .orElse(null);
  }

  public CampaignProduct getExistingCampaignProduct(String id) {
    return Optional.ofNullable(id)
        .flatMap(campaignProductRepository::findById)
        .orElse(null);
  }

  public CampaignProduct getExistingCampaignProductWithObject(CampaignProduct campaignProduct) {
    return Optional.ofNullable(campaignProduct)
        .map(CampaignProduct::toId)
        .map(this::getExistingCampaignProduct)
        .orElse(null);
  }

  public List<CampaignProduct> getActiveCampaignProductsByCampaignSessions(List<String> campaignCodeSessionIds) {
    return Optional.ofNullable(campaignCodeSessionIds)
        .filter(CollectionUtils::isNotEmpty)
        .map(ids -> {
          try(Stream<CampaignProduct> campaignProductStream = campaignProductRepository.streamAllByCampaignCodeSessionIdInAndActiveTrue(ids)) {
            return campaignProductStream
                .filter(Objects::nonNull)
                .sorted(Comparator.comparing(adj -> MainUtil.toNotNullString(adj.toId())))
                .collect(Collectors.toList());
          }
        })
        .orElseGet(ArrayList::new);
  }

  public List<CampaignProduct> getCampaignProductsByL5(String itemSku, String pickupPointCode) {
    return Optional.ofNullable(itemSku)
        .map(itmSku -> {
          try(Stream<CampaignProduct> campaignProductStream = campaignProductRepository.streamAllBySku_ItemSkuAndSku_PickupPointCode(itmSku,pickupPointCode)) {
            return campaignProductStream
                .filter(Objects::nonNull)
                .filter(CampaignProduct::isActive)
                .sorted(Comparator.comparing(adj -> MainUtil.toNotNullString(adj.toId())))
                .collect(Collectors.toList());
          }
        })
        .orElseGet(ArrayList::new);
  }

  public List<CampaignProduct> getCampaignProductsByCampaignCodeAndTagLabel(String campaignCode, String tagLabel) {
    return campaignProductRepository.findAllByCampaignCodeAndTagLabelNot(campaignCode,tagLabel,PageRequest.of(0,campaignProperties.getBatchSize()));
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

  public List<CampaignProduct> fromCampaignProductPublishedToCampaignProducts(CampaignProductPublished campaignProductPublished) {
    return Optional.ofNullable(campaignProductPublished)
        .map(CampaignProductPublished::getSkuList)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(val -> {
          CampaignProduct campaignProduct = new CampaignProduct();
          BeanUtils.copyProperties(campaignProductPublished,campaignProduct,"skuList","skuCheapestPriceDetails");
          CampaignProductPublished.ProductSkuEventModel item = new CampaignProductPublished.ProductSkuEventModel();
          BeanUtils.copyProperties(val,item);
          campaignProduct.setSku(item);
          campaignProduct.setId(campaignProduct.toId());
          campaignProduct.setCampaignCodeSessionId(ModuleProductUtil.toCampaignCodeSessionId(campaignProduct.getCampaignCode(),item.getSessionId()));
          campaignProduct.setTimestamp(campaignProductPublished.getTimestamp());
          if (val.isMarkForDelete() || campaignProductPublished.isEmptyQuota()) {
            campaignProduct.setActive(false);
          } else {
            campaignProduct.setActive(true);
          }
          return campaignProduct;
        })
        .collect(Collectors.toList());
  }

  public List<CampaignProduct> fromCampaignProductTagLabelToCampaignProducts(CampaignTeaserLive campaignTeaserLive) {
    return Optional.ofNullable(campaignTeaserLive)
        .map(val -> getCampaignProductsByCampaignCodeAndTagLabel(val.getCampaignCode(),val.getTagLabel()))
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(campaignProduct -> {
          campaignProduct.setTimestamp(campaignTeaserLive.getTimestamp());
          campaignProduct.setTagLabel(campaignTeaserLive.getTagLabel());
          return campaignProduct;
        })
        .collect(Collectors.toList());
  }

}
