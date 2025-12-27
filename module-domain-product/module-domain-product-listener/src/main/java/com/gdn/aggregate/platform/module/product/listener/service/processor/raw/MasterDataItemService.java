package com.gdn.aggregate.platform.module.product.listener.service.processor.raw;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import com.gdn.aggregate.modules.agp.engagement.common.util.model.param.SaveParam;
import com.gdn.aggregate.modules.agp.engagement.common.util.model.request.SaveRequest;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.DBService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.LoggerUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.MainUtil;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.ParamUtil;
import com.gdn.aggregate.platform.module.product.listener.constants.Collections;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterData;
import com.gdn.aggregate.platform.module.product.listener.model.raw.MasterDataItem;
import com.gdn.aggregate.platform.module.product.listener.repository.raw.MasterDataItemRepository;
import reactor.core.publisher.Mono;

@Component
public class MasterDataItemService {

  private static final String SAVE_COMMAND = "saveMasterDataItem";

  @Autowired
  private DBService dbService;

  @Autowired
  private MasterDataItemRepository masterDataItemRepository;

  /*Save*/
  public Mono<Boolean> save(MasterDataItem masterDataItem, SaveParam saveParam) {
    return Mono.fromCallable(() -> SaveRequest.builder()
            .index(Collections.MASTER_DATA_ITEM)
            .domain(masterDataItem)
            .clazz(MasterDataItem.class)
            .mongo(true)
            .elasticsearch(false)
            .republish(ParamUtil.isRepublish(saveParam))
            .ignoreTimestamp(ParamUtil.isIgnoreTimestamp(saveParam))
            .build())
        .flatMap(saveRequest -> dbService.save(saveRequest))
        .doOnSuccess(s -> LoggerUtil.sendSuccessExecuteLog(masterDataItem,SAVE_COMMAND, saveParam.getTraceId()))
        .doOnError(t -> LoggerUtil.sendErrorExecuteLog(masterDataItem,t,SAVE_COMMAND, saveParam.getTraceId()))
        .onErrorResume(MainUtil::errorResult);
  }

  public void setMandatory(MasterDataItem masterDataItem) {
    Optional.ofNullable(masterDataItem)
        .ifPresent(val -> {
          val.setId(val.toId());
        });
  }
  /*End of Save*/

  /*Getters*/
  public MasterDataItem getExistingMasterDataItem(String id) {
    return Optional.ofNullable(id)
        .flatMap(masterDataItemRepository::findById)
        .orElse(null);
  }

  public List<MasterDataItem> toMasterDataItems(MasterData masterData) {
    return Optional.ofNullable(masterData)
        .map(MasterData::getProductItems)
        .orElseGet(ArrayList::new)
        .stream()
        .filter(Objects::nonNull)
        .map(val -> toMasterDataItem(val,masterData))
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  public MasterDataItem toMasterDataItem(MasterData.ProductItemDomainEventModel productItem, MasterData masterData) {
    return Optional.ofNullable(productItem)
        .filter(val -> Objects.nonNull(val.getSkuCode()))
        .map(val -> {
          MasterDataItem result = MasterDataItem.builder()
              .timestamp(masterData.getTimestamp())
              .productCode(masterData.getProductCode())
              .skuCode(val.getSkuCode())
              .generatedItemName(val.getGeneratedItemName())
              .upcCode(val.getUpcCode())
              .activated(val.isActivated())
              .viewable(val.isViewable())
              .dangerousLevel(MainUtil.toNotNullInteger(val.getDangerousGoodsLevel()))
              .newData(val.getNewData())
              .markForDelete(val.isMarkForDelete())
              .masterDataItemImages(toMasterDataItemImages(val.getImages()))
              .reviewPending(masterData.isReviewPending())
              .build();
          result.setId(result.toId());
          return result;
        })
        .orElse(null);
  }

  private List<MasterDataItem.MasterDataItemImage> toMasterDataItemImages(List<MasterData.ImageDomainEventModel> images) {
    return Optional.ofNullable(images)
        .orElseGet(ArrayList::new)
        .stream()
        .map(this::toMasterDataItemImage)
        .filter(Objects::nonNull)
        .sorted(Comparator.comparing(img -> MainUtil.toNotNullString(img.getLocationPath())))
        .collect(Collectors.toList());
  }

  private MasterDataItem.MasterDataItemImage toMasterDataItemImage(MasterData.ImageDomainEventModel image) {
    return Optional.ofNullable(image)
        .map(img -> MasterDataItem.MasterDataItemImage.builder()
            .mainImage(img.isMainImage())
            .locationPath(img.getLocationPath())
            .sequence(img.getSequence())
            .build())
        .orElse(null);
  }
  /*End of Getters*/

}
